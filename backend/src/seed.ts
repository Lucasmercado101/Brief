// Dev seed
import { faker } from "@faker-js/faker";
import { Label, Note, Prisma, PrismaClient } from "@prisma/client";
export const prisma = new PrismaClient();

type omitDBGenerated<T> = Omit<
  T,
  "id" | "createdAt" | "userId" | "ownerId" | "updatedAt"
>;

function keepUniqueStr(arr: string[]): string[] {
  const all: any = {};
  return arr.filter((e) => {
    if (!all[e]) {
      all[e] = true;
      return true;
    }
    return false;
  });
}

function generateNote(currNote: number): omitDBGenerated<Note> {
  return {
    content: faker.lorem.sentences({ min: 1, max: 5 }),
    pinned: false,
    title: currNote % 4 == 0 ? null : faker.lorem.words({ min: 1, max: 5 })
  };
}

function generateLabelName(currNote: number): string {
  if (currNote % 4 == 0) {
    return faker.word.words({ count: { min: 1, max: 10 } }).slice(0, 50);
  } else {
    return faker.word.sample({ length: { min: 1, max: 50 } });
  }
}

const notes = [];
const labelNames = [];

for (let index = 0; index < 50; index++) {
  notes.push(generateNote(index));
  labelNames.push(generateLabelName(index));
}

const user = await prisma.user.create({
  data: {
    email: "a@a.com",
    password: await Bun.password.hash("test")
  }
});

const labelsToCreate = keepUniqueStr(labelNames);

await prisma.label.createMany({
  data: labelsToCreate.map((e) => ({
    name: e,
    ownerId: user.id
  }))
});

const labels = await prisma.label.findMany({ where: { ownerId: user.id } });

function randNumBetween(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

// recursive
function getRandomLabels(
  amount: number,
  allLabels: Label[],
  resp: Label[]
): Label[] {
  if (amount === 0) {
    return resp;
  }
  const randElement: Label =
    allLabels[Math.floor(Math.random() * allLabels.length)];

  return getRandomLabels(
    amount - 1,
    allLabels.filter((e) => e.name !== randElement.name),
    [...resp, randElement]
  );
}

const notesToCreate = notes.map((e, i) => {
  return prisma.note.create({
    data: {
      ...e,
      userId: user.id,
      labels: {
        connect:
          i % 3 === 0
            ? []
            : getRandomLabels(randNumBetween(1, 8), labels, []).map((e) => ({
                id: e.id
              }))
      }
    }
  });
});

await prisma.$transaction(notesToCreate);

console.log("seeded DB");
