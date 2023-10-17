import { Elysia, t } from "elysia";
import { requiredCookieSession } from "./index";

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
  ({ body }) => {
    const operations = body.operations as Operation[];
    const [
      newNote,
      deleteNote,
      editNote,
      newLabel,
      changeLabelName,
      deleteLabel,
      deleteLabels,
      createLabels,
      deleteNotes,
      createNotes
    ] = getOperations(operations);
  },
  {
    body: body,
    cookie: requiredCookieSession
  }
);

enum Operations {
  // note
  NEW_NOTE,
  DELETE_NOTE,
  EDIT_NOTE,
  // label
  NEW_LABEL,
  CHANGE_LABEL_NAME,
  DELETE_LABEL,
  // Mutation Aggregations
  DELETE_LABELS,
  CREATE_LABELS,
  DELETE_NOTES,
  CREATE_NOTES
}

type offlineId = string;
type databaseId = number;
type ID = offlineId | databaseId;

interface Operation {
  operation: Operations;
  [key: string]: unknown;
}

type NewNote = {
  operation: Operations.NEW_NOTE;
  offlineId: offlineId;
  title: string;
  content: string;
  pinned: boolean;
  labels: (string | number)[];
};

type DeleteNote = {
  operation: Operations.DELETE_NOTE;
  id: databaseId;
};

type EditNote = {
  operation: Operations.EDIT_NOTE;
  id: ID;
  title: string;
  content: string;
  pinned: boolean;
  labels: ID[];
};

type NewLabel = {
  operation: Operations.NEW_LABEL;
  offlineId: offlineId;
  name: string;
};

type ChangeLabelName = {
  operation: Operations.CHANGE_LABEL_NAME;
  id: ID;
  name: string;
};

type DeleteLabel = {
  operation: Operations.DELETE_LABEL;
  id: databaseId;
};

type DeleteLabels = {
  operation: Operations.DELETE_LABELS;
  ids: databaseId[];
};

type CreateLabels = {
  operation: Operations.CREATE_LABELS;
  labels: Omit<NewLabel, "operation">[];
};

type DeleteNotes = {
  operation: Operations.DELETE_NOTES;
  ids: databaseId[];
};

type CreateNotes = {
  operation: Operations.CREATE_NOTES;
  notes: Omit<NewNote, "operation">[];
};

function getOperations(
  operations: Operation[]
): [
  NewNote | undefined,
  DeleteNote | undefined,
  EditNote | undefined,
  NewLabel | undefined,
  ChangeLabelName | undefined,
  DeleteLabel | undefined,
  DeleteLabels[],
  CreateLabels[],
  DeleteNotes[],
  CreateNotes[]
] {
  const [[newNote], r1] = partition(
    operations,
    (mutation): mutation is NewNote =>
      mutation.operation === Operations.NEW_NOTE
  );

  const [[deleteNote], r2] = partition(
    r1,
    (mutation): mutation is DeleteNote =>
      mutation.operation === Operations.DELETE_NOTE
  );

  const [[editNote], r3] = partition(
    r2,
    (mutation): mutation is EditNote =>
      mutation.operation === Operations.EDIT_NOTE
  );

  const [[newLabel], r4] = partition(
    r3,
    (mutation): mutation is NewLabel =>
      mutation.operation === Operations.NEW_LABEL
  );

  const [[changeLabelName], r5] = partition(
    r4,
    (mutation): mutation is ChangeLabelName =>
      mutation.operation === Operations.CHANGE_LABEL_NAME
  );

  const [[deleteLabel], r6] = partition(
    r5,
    (mutation): mutation is DeleteLabel =>
      mutation.operation === Operations.DELETE_LABEL
  );

  const [deleteLabels, r7] = partition(
    r6,
    (mutation): mutation is DeleteLabels =>
      mutation.operation === Operations.DELETE_LABELS
  );

  const [createLabels, r8] = partition(
    r7,
    (mutation): mutation is CreateLabels =>
      mutation.operation === Operations.CREATE_LABELS
  );

  const [deleteNotes, r9] = partition(
    r8,
    (mutation): mutation is DeleteNotes =>
      mutation.operation === Operations.DELETE_NOTES
  );

  const [createNotes] = partition(
    r9,
    (mutation): mutation is CreateNotes =>
      mutation.operation === Operations.CREATE_NOTES
  );

  return [
    newNote,
    deleteNote,
    editNote,
    newLabel,
    changeLabelName,
    deleteLabel,
    deleteLabels,
    createLabels,
    deleteNotes,
    createNotes
  ];
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
