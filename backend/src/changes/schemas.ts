import { z } from "zod";

const offlineId = z.string();
const databaseId = z.number();

const ID = z.union([offlineId, databaseId]);

const deleteLabelsOp = z.object({
  operation: z.literal("DELETE_LABELS"),
  ids: z.array(z.number())
});

//

const newLabel = z.object({
  offlineId,
  name: z.string()
});

const createLabelsOp = z.object({
  operation: z.literal("CREATE_LABELS"),
  labels: z.array(newLabel)
});

//

const deleteNotesOp = z.object({
  operation: z.literal("DELETE_NOTES"),
  ids: z.array(z.number())
});

//

const newNote = z.object({
  offlineId,
  title: z.string().optional(),
  content: z.string(),
  pinned: z.boolean(),
  order: z.number().optional(),
  labels: z.array(ID).optional()
});

const createNotesOp = z.object({
  operation: z.literal("CREATE_NOTES"),
  notes: z.array(newNote)
});

//

const editNoteOp = z.object({
  operation: z.literal("EDIT_NOTE"),
  id: ID,
  title: z.string().optional(),
  content: z.string().optional(),
  pinned: z.boolean().optional(),
  order: z.number().optional(),
  labels: z.array(ID).optional()
});

//

const changeLabelNameOp = z.object({
  operation: z.literal("CHANGE_LABEL_NAME"),
  id: ID,
  name: z.string()
});

export type ID = z.infer<typeof ID>;

export type NewLabel = z.infer<typeof newLabel>;
export type NewNote = z.infer<typeof newNote>;

export type DeleteLabelsOp = z.infer<typeof deleteLabelsOp>;
export type CreateLabelsOp = z.infer<typeof createLabelsOp>;
export type DeleteNotesOp = z.infer<typeof deleteNotesOp>;
export type CreateNotesOp = z.infer<typeof createNotesOp>;
export type EditNoteOp = z.infer<typeof editNoteOp>;
export type ChangeLabelNameOp = z.infer<typeof changeLabelNameOp>;

const operation = z.discriminatedUnion("operation", [
  createLabelsOp,
  deleteLabelsOp,
  createNotesOp,
  deleteNotesOp,
  editNoteOp,
  changeLabelNameOp
]);

export const operations = z.array(operation);
