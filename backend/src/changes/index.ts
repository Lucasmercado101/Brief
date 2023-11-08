import { prisma } from "../index";
import { Label, Note } from "@prisma/client";
import {
  NewLabel,
  NewNote,
  ID,
  EditNoteOp,
  ChangeLabelNameOp
} from "./schemas";
import validateBody from "./validation";

export async function changesEndpoint(userId: number, body: unknown) {
  const validatedBody = validateBody(body);
  let {
    changeLabelsName,
    createLabels: createLabelsOp,
    createNotes,
    deleteLabelIds,
    deleteNoteIds,
    editNotes
  } = validatedBody.operations;

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
          offlineId: createLabelsOp.find(({ name }) => name === label.name)
            ?.offlineId
        }));
      });

    createNotes = createNotes.map((note) => ({
      ...note,
      labels: note.labels?.map(
        (label) => newLabels.find((l) => l.offlineId === label)?.id ?? label
      )
    }));

    editNotes = editNotes.map((note) => ({
      ...note,
      labels: note.labels?.map(
        (label) => newLabels.find((l) => l.offlineId === label)?.id ?? label
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
      .then((e) => (e?.order ? e.order + 1 : 1));

    const newNotesPromises = createNotes.map(
      ({ title, content, pinned, labels, offlineId, order }) => {
        // if some label failed to create or just got given an
        // offline id then just ignore it and don't connect it
        const labelsData =
          labels
            ?.filter((e): e is number => typeof e === "number")
            .map((label) => ({
              id: label
            })) ?? [];

        return prisma.note
          .create({
            data: {
              title,
              content,
              pinned,
              userId,
              order: order ?? currentNotesAmount++,
              labels: {
                connect: labelsData
              }
            }
          })
          .then((e) => ({ ...e, offlineId }));
      }
    );

    (await Promise.allSettled(newNotesPromises)).forEach((e) => {
      if (e.status === "fulfilled") {
        newNotes.push(e.value);
      }
    });

    editNotes = editNotes.map((note) => ({
      ...note,
      id: newNotes.find((n) => n.offlineId === note.id)?.id ?? note.id
    }));

    // NOTE: don't inquire further, if it couldn't create then don't
    // retry, don't fail, just send as "not created" in response
    notesNotCreated = createNotes.filter(
      ({ offlineId }) => !newNotes.find((note) => note.offlineId === offlineId)
    );
  }

  // * edit notes
  if (editNotes.length > 0) {
    // NOTE: It IS possible to mess up order
    // as i removed the unique order number per userId restraint
    // on the schema, so call changes endpoint with care

    const editNotesPromises = editNotes
      .filter((e): e is EditNoteOp & { id: number } => typeof e.id === "number")
      .map(({ id, title, content, pinned, labels, order }) => {
        // if some label failed to create or just got given
        // offline id then just ignore it and don't connect it
        const labelsIds = labels
          ?.filter((e): e is number => typeof e === "number")
          .map((label) => ({
            id: label
          }));

        return prisma.note
          .update({
            where: {
              id: id,
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
      .filter(
        (e): e is ChangeLabelNameOp & { id: number } => typeof e.id === "number"
      )
      .map(({ id, name }) =>
        prisma.label.update({
          where: {
            id: id,
            ownerId: userId
          },
          data: {
            name
          }
        })
      );

    (await Promise.allSettled(changeLabelsNamePromises)).forEach((e) => {
      if (e.status === "fulfilled") {
        labelsNameEdited.push(e.value);
      }
    });

    labelsNameNotChanged = changeLabelsName.filter(
      ({ id }) => !labelsNameEdited.find((label) => label.id === id)
    );
  }

  const toDownSyncNotes = await prisma.note
    .findMany({
      where: {
        updatedAt: {
          gt: validatedBody.lastSyncedAt
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
        gt: validatedBody.lastSyncedAt
      },
      ownerId: userId
    }
  });

  const dbNotes = await prisma.note.findMany({
    where: {
      userId,
      id: { in: validatedBody.currentData.notes.map((id) => id) }
    },
    select: {
      id: true
    }
  });

  const deletedNotes = validatedBody.currentData.notes.filter(
    (id) => !dbNotes.find((label) => label.id === id)
  );

  const dbLabels = await prisma.label.findMany({
    where: {
      ownerId: userId,
      id: { in: validatedBody.currentData.labels.map((id) => id) }
    },
    select: {
      id: true
    }
  });

  const deletedLabels = validatedBody.currentData.labels.filter(
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
    failedToEdit: {
      notes: notesNotEdited,
      labels: labelsNameNotChanged
    },
    justSyncedAt: new Date().valueOf()
  };
}
