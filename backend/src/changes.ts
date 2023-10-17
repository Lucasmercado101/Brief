import { Elysia, t } from "elysia";
import { prisma, requiredCookieSession } from "./index";

const POSIX = t.Number();

const body = t.Object({
  operations: t.Array(t.Unknown()),
  lastSync: POSIX,
  data: t.Object({
    labels: t.Array(
      t.Object({
        id: t.Number(),
        updatedAt: POSIX
      })
    ),
    notes: t.Array(
      t.Object({
        id: t.Number(),
        updatedAt: POSIX
      })
    )
  })
});

export default new Elysia().post(
  "/changes",
  async ({ body, cookie: { session } }) => {
    const userId = session.value;

    const operations = getOperations(body.operations as Operation[]);

    const deleteLabelIds = operations.deleteLabels.ids;
    const deleteNoteIds = operations.deleteNotes.ids;
    const createLabels = operations.createLabels.labels;
    const createNotes = operations.createNotes.notes;
    const editNote = operations.editNote;
    const changeLabelName = operations.changeLabelName;

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
  },
  {
    body: body,
    cookie: requiredCookieSession
  }
);

enum Operations {
  DELETE_LABELS,
  CREATE_LABELS,
  DELETE_NOTES,
  CREATE_NOTES,
  EDIT_NOTE,
  CHANGE_LABEL_NAME
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
  deleteLabels: DeleteLabels;
  createLabels: CreateLabels;
  deleteNotes: DeleteNotes;
  createNotes: CreateNotes;
  editNote: EditNote[];
  changeLabelName: ChangeLabelName[];
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

  const [editNote, r5] = partition(
    r4,
    (mutation): mutation is EditNote =>
      mutation.operation === Operations.EDIT_NOTE
  );

  const [changeLabelName] = partition(
    r5,
    (mutation): mutation is ChangeLabelName =>
      mutation.operation === Operations.CHANGE_LABEL_NAME
  );

  return {
    deleteLabels,
    createLabels,
    deleteNotes,
    createNotes,
    editNote,
    changeLabelName
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
