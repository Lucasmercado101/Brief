import {
  operations as operationsSchema,
  ChangeLabelNameOp,
  CreateLabelsOp,
  CreateNotesOp,
  DeleteLabelsOp,
  DeleteNotesOp,
  EditNoteOp
} from "./schemas";

export default function validateBody(body: unknown[]) {
  const operations = operationsSchema.parse(body);
  return operations.reduce(
    (acc, next) => {
      switch (next.operation) {
        case "DELETE_LABELS": {
          acc.deleteLabelIds = next.ids;
          return acc;
        }

        case "DELETE_NOTES": {
          acc.deleteNoteIds = next.ids;
          return acc;
        }
        case "CREATE_LABELS": {
          acc.createLabels = next.labels;
          return acc;
        }

        case "CREATE_NOTES": {
          acc.createNotes = next.notes;
          return acc;
        }

        case "EDIT_NOTE": {
          acc.editNotes.push(next);
          return acc;
        }

        case "CHANGE_LABEL_NAME": {
          acc.changeLabelsName.push(next);
          return acc;
        }

        default:
          return acc;
      }
    },
    {
      deleteLabelIds: [] as DeleteLabelsOp["ids"],
      deleteNoteIds: [] as DeleteNotesOp["ids"],
      createLabels: [] as CreateLabelsOp["labels"],
      createNotes: [] as CreateNotesOp["notes"],
      editNotes: [] as EditNoteOp[],
      changeLabelsName: [] as ChangeLabelNameOp[]
    }
  );
}
