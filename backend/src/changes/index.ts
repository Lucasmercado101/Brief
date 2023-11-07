import { Elysia, t } from "elysia";
import { prisma, requiredCookieSession } from "../index";
import { Label, Note } from "@prisma/client";
import {
  NewLabel,
  NewNote,
  ID,
  EditNoteOp,
  ChangeLabelNameOp
} from "./schemas";
import validateBody from "./validation";

const POSIX = t.Number();

const body = t.Object({
  operations: t.Array(t.Unknown(), { minItems: 1 }),
  lastSyncedAt: POSIX,
  currentData: t.Object({
    labels: t.Array(t.Number()),
    notes: t.Array(t.Number())
  })
});

export default () =>
  new Elysia()
    // error handler Copy and pasted from index.ts
    .onError(({ error, set, code }) => {
      console.log(code);
      switch (code) {
        case "VALIDATION":
          set.status = 400;
          return error.message;
        case "NOT_FOUND":
          set.status = 404;
          return "Endpoint not found.";
        case "INVALID_COOKIE_SIGNATURE":
          set.status = 401;
          return error.message;
        // TODO: better
        case "PARSE":
        case "UNKNOWN":
          if (error.message.includes("JSON Parse error")) {
            set.status = 400;
            return "Invalid JSON.";
          }
        case "INTERNAL_SERVER_ERROR":
        default:
          console.log("ERROR\n");
          console.log("CODE:", code);
          console.log(error);
          console.log("\nERROR END");
          set.status = 500;
          return "An unexpected error has occurred.";
      }
    })
    .post(
      "/changes",
      async ({ body, cookie: { session } }) => {
        const userId = session.value;

        let {
          changeLabelsName,
          createLabels: createLabelsOp,
          createNotes,
          deleteLabelIds,
          deleteNoteIds,
          editNotes
        } = validateBody(body.operations);

        let labelsNotCreated: NewLabel[] = [];
        let notesNotCreated: NewNote[] = [];
        let notesNotEdited: EditNoteOp[] = [];
        let labelsNameNotChanged: ChangeLabelNameOp[] = [];

        let newLabels: (Label & { offlineId: string | undefined })[] = [];

        let newNotes: (Note & {
          offlineId: string;
        })[] = [];

        let editedNotes: (Note & {
          offlineId: ID;
        })[] = [];

        let labelsNameEdited: Label[] = [];

        // * delete labels
        if (deleteLabelIds.length > 0) {
          await prisma.label.deleteMany({
            where: { id: { in: deleteLabelIds }, ownerId: userId }
          });
        }

        // * delete notes
        if (deleteNoteIds.length > 0) {
          await prisma.note.deleteMany({
            where: { id: { in: deleteNoteIds }, userId: userId }
          });
        }

        // * create labels
        if (createLabelsOp.length > 0) {
          await prisma.label.createMany({
            skipDuplicates: true,
            data: createLabelsOp.map(({ name }) => ({
              name,
              ownerId: userId
            }))
          });

          newLabels = await prisma.label
            .findMany({
              where: {
                name: {
                  in: createLabelsOp.map(({ name }) => name)
                },
                ownerId: userId
              }
            })
            .then((labels) => {
              return labels.map((label) => ({
                ...label,
                offlineId: createLabelsOp.find(
                  ({ name }) => name === label.name
                )?.offlineId
              }));
            });

          createNotes = createNotes.map((note) => ({
            ...note,
            labels: note.labels?.map(
              (label) =>
                newLabels.find((l) => l.offlineId === label)?.id ?? label
            )
          }));

          editNotes = editNotes.map((note) => ({
            ...note,
            labels: note.labels?.map(
              (label) =>
                newLabels.find((l) => l.offlineId === label)?.id ?? label
            )
          }));

          changeLabelsName = changeLabelsName.map((label) => ({
            ...label,
            id: newLabels.find((l) => l.offlineId === label.id)?.id ?? label.id
          }));

          // NOTE: don't inquire further, if it couldn't create then don't
          // retry, don't fail, just send as "not created" in response
          labelsNotCreated = createLabelsOp.filter(
            ({ name }) => !newLabels.find((label) => label.name === name)
          );
        }

        // * create notes
        if (createNotes.length > 0) {
          let currentNotesAmount = await prisma.note
            .findFirst({
              where: {
                userId: userId
              },
              select: { order: true },
              orderBy: {
                order: "desc"
              },
              take: 1
            })
            .then((e) => {
              if (e?.order) {
                return e.order + 1;
              } else {
                return 1;
              }
            });

          const newNotesPromises = createNotes.map(
            ({ title, content, pinned, labels, offlineId, order }) => {
              const newNote = prisma.note.create({
                data: {
                  title,
                  content,
                  pinned,
                  userId,
                  order: order ?? currentNotesAmount++,
                  labels: {
                    // if some label failed to create or just got given
                    // offline id then just ignore it and don't connect it
                    connect:
                      labels
                        ?.filter((e) => typeof e === "number")
                        .map((label) => ({
                          id: label as number
                        })) ?? []
                  }
                }
              });
              return newNote.then((e) => ({ ...e, offlineId }));
            }
          );

          (await Promise.allSettled(newNotesPromises)).forEach((e) => {
            if (e.status === "fulfilled") {
              newNotes.push(e.value);
            }
          });

          editNotes = editNotes.map((note) => {
            const noteOnlineId = newNotes.find(
              (n) => n.offlineId === note.id
            )?.id;
            return {
              ...note,
              id: noteOnlineId ?? note.id
            };
          });

          // NOTE: don't inquire further, if it couldn't create then don't
          // retry, don't fail, just send as "not created" in response
          notesNotCreated = createNotes.filter(
            ({ offlineId }) =>
              !newNotes.find((note) => note.offlineId === offlineId)
          );
        }

        // * edit notes
        if (editNotes.length > 0) {
          // NOTE: It IS possible to mess up order
          // as i removed the unique order number per userId restraint
          // on the schema, so call changes endpoint with care

          const editNotesPromises = editNotes
            .filter((e) => typeof e.id === "number")
            .map(({ id, title, content, pinned, labels, order }) => {
              // if some label failed to create or just got given
              // offline id then just ignore it and don't connect it
              const labelsIds = labels
                ?.filter((e): e is number => typeof e === "number")
                .map((label) => ({
                  id: label
                }));

              const editNote = prisma.note
                .update({
                  where: {
                    id: id as number,
                    userId: userId
                  },
                  select: {
                    id: true,
                    title: true,
                    content: true,
                    pinned: true,
                    order: true,
                    createdAt: true,
                    updatedAt: true,
                    userId: true,
                    labels: {
                      select: { id: true }
                    }
                  },
                  data: {
                    title,
                    content,
                    pinned,
                    order,
                    ...(labels !== undefined && {
                      labels: {
                        set: labelsIds
                      }
                    })
                  }
                })
                .then((e) => ({ ...e, offlineId: id }));
              return editNote;
            });

          (await Promise.allSettled(editNotesPromises)).forEach((e) => {
            if (e.status === "fulfilled") {
              editedNotes.push(e.value);
            }
          });

          // NOTE: don't inquire further, if it couldn't create then don't
          // retry, don't fail, just send as "not edited" in response
          notesNotEdited = editNotes.filter(
            ({ id }) => !editedNotes.find((note) => note.offlineId === id)
          );
        }

        // * change labels names
        if (changeLabelsName.length > 0) {
          const changeLabelsNamePromises = changeLabelsName
            .filter((e) => typeof e.id === "number")
            .map(({ id, name }) => {
              const changeLabelName = prisma.label.update({
                where: {
                  id: id as number,
                  ownerId: userId
                },
                data: {
                  name
                }
              });
              return changeLabelName;
            });

          (await Promise.allSettled(changeLabelsNamePromises)).forEach((e) => {
            if (e.status === "fulfilled") {
              labelsNameEdited.push(e.value);
            }
          });

          labelsNameNotChanged = changeLabelsName.filter(
            ({ id }) => !labelsNameEdited.find((label) => label.id === id)
          );
        }

        const lastSyncedAt = new Date(body.lastSyncedAt);

        const toDownSyncNotes = await prisma.note
          .findMany({
            where: {
              updatedAt: {
                gt: lastSyncedAt
              },
              userId: userId
            },
            include: {
              labels: {
                select: { id: true }
              }
            }
          })
          .then((res) =>
            res.map((e) => ({ ...e, labels: e.labels.map((e) => e.id) }))
          );

        const toDownSyncLabels = await prisma.label.findMany({
          where: {
            updatedAt: {
              gt: lastSyncedAt
            },
            ownerId: userId
          }
        });

        const dbNotes = await prisma.note.findMany({
          where: {
            userId,
            id: { in: body.currentData.notes.map((id) => id) }
          },
          select: {
            id: true
          }
        });

        const deletedNotes = body.currentData.notes.filter(
          (id) => !dbNotes.find((label) => label.id === id)
        );

        const dbLabels = await prisma.label.findMany({
          where: {
            ownerId: userId,
            id: { in: body.currentData.labels.map((id) => id) }
          },
          select: {
            id: true
          }
        });

        const deletedLabels = body.currentData.labels.filter(
          (id) => !dbLabels.find((label) => label.id === id)
        );

        return {
          data: {
            notes: toDownSyncNotes
              .map((n) => ({
                ...n,
                updatedAt: n.updatedAt.valueOf(),
                createdAt: n.createdAt.valueOf()
              }))
              .map((e) => {
                const offlineId = newNotes.find(
                  (n) => n.id === e.id
                )?.offlineId;
                if (offlineId) return { ...e, offlineId };
                else return e;
              }),
            labels: toDownSyncLabels
              .map((n) => ({
                ...n,
                updatedAt: n.updatedAt.valueOf(),
                createdAt: n.createdAt.valueOf()
              }))
              .map((e) => {
                const offlineId = newLabels.find(
                  (l) => l.id === e.id
                )?.offlineId;
                if (offlineId) return { ...e, offlineId };
                else return e;
              })
          },
          deleted: {
            notes: deletedNotes,
            labels: deletedLabels
          },
          failedToCreate: [
            ...labelsNotCreated.map((e) => e.offlineId),
            ...notesNotCreated.map((e) => e.offlineId)
          ],
          failedToEdit: {
            notes: notesNotEdited,
            labels: labelsNameNotChanged
          },
          justSyncedAt: new Date().valueOf()
        };
      },
      {
        body: body,
        cookie: requiredCookieSession
      }
    );
