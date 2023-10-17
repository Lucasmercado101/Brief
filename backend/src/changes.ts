import { Elysia, t } from "elysia";
import { prisma, requiredCookieSession } from "./index";

const POSIX = t.Number();

const body = t.Object({
  operations: t.Array(t.Unknown(), { minItems: 1 }),
  lastSync: POSIX,
  currentData: t.Object({
    labels: t.Array(t.Number()),
    notes: t.Array(t.Number())
  })
});

export default () =>
  new Elysia().post(
    "/changes",
    async ({ body, set, cookie: { session } }) => {
      const userId = session.value;

      if (body.operations.some((e) => (e as any).operation === undefined)) {
        set.status = 400;
        return;
      }

      // check if operation is a valid one
      if (
        body.operations.some((e) => {
          const op: string = (e as any).operation;
          switch (op) {
            case Operations.DELETE_LABELS:
              return false;
            case Operations.CREATE_LABELS:
              return false;
            case Operations.DELETE_NOTES:
              return false;
            case Operations.CREATE_NOTES:
              return false;
            case Operations.EDIT_NOTE:
              return false;
            case Operations.CHANGE_LABEL_NAME:
              return false;
            default:
              return true;
          }
        })
      ) {
        set.status = 400;
        return "Invalid operation";
      }

      const operations = getOperations(body.operations as Operation[]);

      const { deleteLabelIds, deleteNoteIds } = operations;
      let createLabels = operations.createLabels;
      let createNotes = operations.createNotes;
      let editNotes = operations.editNotes;
      let changeLabelsName = operations.changeLabelsName;

      let labelsNotCreated: NewLabel[] = [];
      let notesNotCreated: NewNote[] = [];
      let notesNotEdited: EditNote[] = [];
      let labelsNameNotChanged: ChangeLabelName[] = [];

      let newLabels: {
        offlineId: string | undefined;
        id: number;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        ownerId: number;
      }[] = [];

      let newNotes: {
        offlineId: string;
        id: number;
        title: string | null;
        content: string;
        pinned: boolean;
        createdAt: Date;
        updatedAt: Date;
        userId: number;
      }[] = [];

      let editedNotes: {
        offlineId: ID;
        id: number;
        title: string | null;
        content: string;
        pinned: boolean;
        createdAt: Date;
        updatedAt: Date;
        userId: number;
      }[] = [];

      let labelsNameEdited: {
        id: number;
        name: string;
        createdAt: Date;
        updatedAt: Date;
        ownerId: number;
      }[] = [];

      if (deleteLabelIds.length > 0) {
        await prisma.label.deleteMany({
          where: {
            id: {
              in: deleteLabelIds
            },
            ownerId: userId
          }
        });
      }

      if (deleteNoteIds.length > 0) {
        await prisma.note.deleteMany({
          where: {
            id: {
              in: deleteNoteIds
            },
            userId: userId
          }
        });
      }

      if (createLabels.length > 0) {
        // remove labels that already exist
        const existingLabels = await prisma.label.findMany({
          where: {
            name: {
              in: createLabels.map(({ name }) => name)
            }
          }
        });

        createLabels = createLabels.filter(
          ({ name }) => !existingLabels.find((label) => label.name === name)
        );

        // create new labels
        await prisma.label.createMany({
          skipDuplicates: true,
          data: createLabels.map(({ name }) => ({
            name,
            ownerId: userId
          }))
        });

        newLabels = await prisma.label
          .findMany({
            where: {
              name: {
                in: createLabels.map(({ name }) => name)
              },
              ownerId: userId
            }
          })
          .then((labels) => {
            return labels.map((label) => ({
              ...label,
              offlineId: createLabels.find(({ name }) => name === label.name)
                ?.offlineId
            }));
          });

        createNotes = createNotes.map((note) => ({
          ...note,
          labels: note.labels.map((label) => {
            const onlineLabel = newLabels.find((l) => l.offlineId === label);
            // replace offline label with db label
            if (onlineLabel) return onlineLabel.id;
            else return label;
          })
        }));

        editNotes = editNotes.map((note) => ({
          ...note,
          labels: note.labels.map((label) => {
            const onlineLabel = newLabels.find((l) => l.offlineId === label);
            // replace offline label with db label
            if (onlineLabel) return onlineLabel.id;
            else return label;
          })
        }));

        changeLabelsName = changeLabelsName.map((label) => ({
          ...label,
          id: newLabels.find((l) => l.offlineId === label.id)?.id ?? label.id
        }));

        // NOTE: don't inquire further, if it couldn't create then don't
        // retry, don't fail, just send as "not created" in response
        labelsNotCreated = createLabels.filter(
          ({ name }) => !newLabels.find((label) => label.name === name)
        );
      }

      if (createNotes.length > 0) {
        const newNotesPromises = createNotes.map(
          ({ title, content, pinned, labels, offlineId }) => {
            const newNote = prisma.note.create({
              data: {
                title,
                content,
                pinned,
                userId,
                labels: {
                  // if some label failed to create or just got given
                  // offline id then just ignore it and don't connect it
                  connect: labels
                    .filter((e) => typeof e === "number")
                    .map((label) => ({
                      id: label as number
                    }))
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

      if (editNotes.length > 0) {
        const editNotesPromises = editNotes
          .filter((e) => e.id === "number")
          .map(({ id, title, content, pinned, labels }) => {
            const editNote = prisma.note
              .update({
                where: {
                  id: id as number
                },
                data: {
                  title,
                  content,
                  pinned,
                  labels: {
                    // if some label failed to create or just got given
                    // offline id then just ignore it and don't connect it
                    connect: labels
                      .filter((e) => typeof e === "number")
                      .map((label) => ({
                        id: label as number
                      }))
                  }
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
          ({ id }) => !newNotes.find((note) => note.offlineId === id)
        );
      }

      if (changeLabelsName.length > 0) {
        const changeLabelsNamePromises = changeLabelsName
          .filter((e) => e.id === "number")
          .map(({ id, name }) => {
            const changeLabelName = prisma.label.update({
              where: {
                id: id as number
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

      const lastSync = new Date(body.lastSync);

      const toDownSyncNotes = await prisma.note.findMany({
        where: {
          updatedAt: {
            gt: lastSync
          },
          id: userId
        }
      });
      const toDownSyncLabels = await prisma.label.findMany({
        where: {
          updatedAt: {
            gt: lastSync
          },
          id: userId
        }
      });

      const deletedNotes = await prisma.note
        .findMany({
          where: {
            userId,
            id: { in: body.currentData.notes.map((id) => id) }
          },
          select: {
            id: true
          }
        })
        .then((e) =>
          e.filter(
            (note) => !body.currentData.notes.find((id) => id === note.id)
          )
        );

      const deletedLabels = await prisma.label
        .findMany({
          where: {
            ownerId: userId,
            id: { in: body.currentData.labels.map((id) => id) }
          },
          select: {
            id: true
          }
        })
        .then((e) =>
          e.filter(
            (label) => !body.currentData.labels.find((id) => id === label.id)
          )
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
              const offlineId = newNotes.find((n) => n.id === e.id)?.offlineId;
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
              const offlineId = newLabels.find((l) => l.id === e.id)?.offlineId;
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
        justSyncedAt: new Date().valueOf()
      };
    },
    {
      body: body,
      cookie: requiredCookieSession
    }
  );

enum Operations {
  DELETE_LABELS = "DELETE_LABELS",
  CREATE_LABELS = "CREATE_LABELS",
  DELETE_NOTES = "DELETE_NOTES",
  CREATE_NOTES = "CREATE_NOTES",
  EDIT_NOTE = "EDIT_NOTE",
  CHANGE_LABEL_NAME = "CHANGE_LABEL_NAME"
}

interface Operation {
  operation: Operations;
  [key: string]: unknown;
}

type offlineId = string;
type databaseId = number;
type ID = offlineId | databaseId;

type DeleteLabels = {
  operation: Operations.DELETE_LABELS;
  ids: databaseId[];
};

//

type NewLabel = {
  offlineId: offlineId;
  name: string;
};

type CreateLabels = {
  operation: Operations.CREATE_LABELS;
  labels: NewLabel[];
};

//

type DeleteNotes = {
  operation: Operations.DELETE_NOTES;
  ids: databaseId[];
};

//

type NewNote = {
  offlineId: offlineId;
  title: string;
  content: string;
  pinned: boolean;
  labels: (string | number)[];
};

type CreateNotes = {
  operation: Operations.CREATE_NOTES;
  notes: NewNote[];
};

//

type EditNote = {
  operation: Operations.EDIT_NOTE;
  id: ID;
  title: string;
  content: string;
  pinned: boolean;
  labels: ID[];
};

type ChangeLabelName = {
  operation: Operations.CHANGE_LABEL_NAME;
  id: ID;
  name: string;
};

function getOperations(operations: Operation[]): {
  deleteLabelIds: DeleteLabels["ids"];
  deleteNoteIds: DeleteNotes["ids"];
  createLabels: CreateLabels["labels"];
  createNotes: CreateNotes["notes"];
  editNotes: EditNote[];
  changeLabelsName: ChangeLabelName[];
} {
  const [[deleteLabels], r1] = partition(
    operations,
    (mutation): mutation is DeleteLabels =>
      mutation.operation === Operations.DELETE_LABELS
  );

  const [[createLabels], r2] = partition(
    r1,
    (mutation): mutation is CreateLabels =>
      mutation.operation === Operations.CREATE_LABELS
  );

  const [[deleteNotes], r3] = partition(
    r2,
    (mutation): mutation is DeleteNotes =>
      mutation.operation === Operations.DELETE_NOTES
  );

  const [[createNotes], r4] = partition(
    r3,
    (mutation): mutation is CreateNotes =>
      mutation.operation === Operations.CREATE_NOTES
  );

  const [editNotes, r5] = partition(
    r4,
    (mutation): mutation is EditNote =>
      mutation.operation === Operations.EDIT_NOTE
  );

  const [changeLabelsName] = partition(
    r5,
    (mutation): mutation is ChangeLabelName =>
      mutation.operation === Operations.CHANGE_LABEL_NAME
  );

  return {
    deleteLabelIds: deleteLabels.ids,
    deleteNoteIds: deleteNotes.ids,
    createLabels: createLabels.labels,
    createNotes: createNotes.notes,
    editNotes,
    changeLabelsName
  };
}

function partition<T, X extends T, Y extends T>(
  arr: T[],
  predicate: (element: T) => element is X
): [X[], Y[]] {
  const res1: X[] = [];
  const res2: Y[] = [];

  arr.forEach((element) => {
    if (predicate(element)) {
      res1.push(element);
    } else {
      res2.push(element as Y);
    }
  });
  return [res1, res2];
}
