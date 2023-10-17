import { Elysia, t } from "elysia";
import { Prisma, PrismaClient } from "@prisma/client";
import { logger } from "./logger";
import PrismaError from "./prismaErrorCodes";
import changesEndpoint from "./changes";

// in seconds
const MINUTE = 60;
const HOUR = MINUTE * 60;
const DAY = HOUR * 24;

const COOKIE_MAX_AGE = DAY * 14;

export const prisma = new PrismaClient();

const PORT = process.env.PORT || 4000;

//NOTE: t.Number() === userID in this case:
const cookieSessionDTO = t.Number();

export const cookieSecret = {
  secrets: process.env.COOKIE_SECRETS || "Fischl von Luftschloss Narfidort",
  sign: ["session"]
};

export const requiredCookieSession = t.Cookie(
  {
    session: cookieSessionDTO
  },
  cookieSecret
);

new Elysia()
  .use(logger())
  // NOTE: CORS -----------
  // TODO: currently allows any origin
  .options("/*", ({ set, request }) => {
    const origin = request.headers.get("Origin") ?? "";
    set.headers["Vary"] = origin;
    set.headers["Access-Control-Allow-Origin"] = origin;
    set.headers["Access-Control-Allow-Credentials"] = "true";
    set.headers["Access-Control-Allow-Headers"] = "content-type";
    set.headers["Access-Control-Allow-Methods"] =
      request.headers.get("Access-Control-Request-Method") ?? "POST";
    // default max age
    set.headers["Access-Control-Max-Age"] = "5";
    set.status = 204;
  })
  .onRequest(({ set, request }) => {
    const origin = request.headers.get("Origin") ?? "";
    set.headers["Access-Control-Allow-Origin"] = origin;
    set.headers["Access-Control-Allow-Credentials"] = "true";
    set.headers["Access-Control-Allow-Headers"] = "content-type";
    set.headers["Access-Control-Allow-Methods"] =
      request.headers.get("Access-Control-Request-Method") ?? "POST";
    // default max age
    set.headers["Access-Control-Max-Age"] = "5";
  })
  // NOTE: CORS END -------
  .onError(({ error, set, code }) => {
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
    "/register",
    async ({ set, body }) => {
      try {
        const passwordHashed = await Bun.password.hash(body.password);
        await prisma.user.create({
          data: {
            email: body.email,
            password: passwordHashed
          }
        });
        return "User created";
      } catch (e) {
        if (e instanceof Prisma.PrismaClientKnownRequestError) {
          // "There is a unique constraint violation, a new user cannot be created with this email"
          if (e.code === PrismaError.P2002) {
            set.status = 409;
            return "A User with that email already exists.";
          }
        }
        throw e;
      }
    },
    {
      body: t.Object({
        email: t.String(),
        password: t.String()
      })
    }
  )
  // TODO: handle user having tampered cookie
  // error when unsigning doesn't let me reassign or remove cookie
  .post(
    "/login",
    async ({ set, cookie: { session }, body }) => {
      const userExists = await prisma.user.findUnique({
        where: {
          email: body.email
        }
      });

      if (!userExists) {
        set.status = 404;
        return "User or password is incorrect";
      }

      const passwordCorrect = await Bun.password.verify(
        body.password,
        userExists.password
      );

      if (!passwordCorrect) {
        set.status = 404;
        return "User or password is incorrect";
      }

      session.value = userExists.id;
      session.maxAge = COOKIE_MAX_AGE;
      session.sameSite = "none";
      session.secure = true;

      return userExists.id;
    },
    {
      body: t.Object({
        email: t.String(),
        password: t.String()
      }),
      cookie: t.Cookie(
        {
          session: t.Optional(cookieSessionDTO)
        },
        cookieSecret
      )
    }
  )
  .onBeforeHandle(({ cookie: { session } }) => {
    session.maxAge = COOKIE_MAX_AGE;
  })
  .post(
    "/note",
    async ({ body, cookie: { session } }) => {
      if (body.labels) {
        const labelsExist = await prisma.label.findMany({
          where: {
            id: {
              in: body.labels
            },
            ownerId: session.value
          },
          select: {
            id: true
          }
        });

        if (labelsExist.length !== body.labels.length) {
          return `Label(s) ${body.labels.filter(
            (e) => !labelsExist.some((l) => l.id === e)
          )} not found`;
        }
      }

      return await prisma.note
        .create({
          data: {
            userId: session.value,
            title: body.title,
            content: body.content,
            labels: {
              connect: body.labels?.map((i) => ({ id: i })) ?? []
            }
          }
        })
        .then((n) => ({
          ...n,
          createdAt: n.createdAt.valueOf(),
          updatedAt: n.updatedAt.valueOf()
        }));
    },
    {
      body: t.Object({
        title: t.Optional(t.String()),
        content: t.String(),
        labels: t.Optional(t.Array(t.Number(), { uniqueItems: true })),
        pinned: t.Optional(t.Boolean())
      }),
      cookie: requiredCookieSession
    }
  )
  .put(
    "/note/:id",
    async ({ body, params: { id: noteID }, set, cookie: { session } }) => {
      if (body.labels) {
        const labelsExist = await prisma.label.findMany({
          where: {
            id: {
              in: body.labels
            },
            ownerId: session.value
          },
          select: {
            id: true
          }
        });

        if (labelsExist.length !== body.labels.length) {
          return `Label(s) ${body.labels.filter(
            (e) => !labelsExist.some((l) => l.id === e)
          )} not found`;
        }
      }

      const noteExists = await prisma.note.findUnique({
        where: { id: noteID, userId: session.value }
      });

      if (!noteExists) {
        set.status = 404;
        return "Note not found";
      }

      return prisma.note
        .update({
          where: { id: noteID },
          select: {
            userId: false,
            id: true,
            title: true,
            content: true,
            pinned: true,
            createdAt: true,
            updatedAt: true,
            labels: {
              select: {
                name: true,
                id: true,
                createdAt: true,
                updatedAt: true
              }
            }
          },
          data: {
            title: body.title,
            content: body.content,
            pinned: body.pinned,
            ...(body.labels !== undefined
              ? { labels: { set: body.labels!.map((i) => ({ id: i })) } }
              : {})
          }
        })
        .then((e) => {
          const labels = e.labels.map((l) => ({
            ...l,
            createdAt: l.createdAt.valueOf(),
            updatedAt: l.updatedAt.valueOf()
          }));

          return {
            ...e,
            createdAt: e.createdAt.valueOf(),
            updatedAt: e.updatedAt.valueOf(),
            labels
          };
        });
    },
    {
      body: t.Object({
        title: t.Optional(t.Nullable(t.String())),
        content: t.Optional(t.String()),
        labels: t.Optional(t.Array(t.Number(), { uniqueItems: true })),
        pinned: t.Optional(t.Boolean())
      }),
      params: t.Object({
        id: t.Numeric()
      }),
      cookie: requiredCookieSession
    }
  )
  .delete(
    "/note/:id",
    async ({ params: { id: noteID }, set }) => {
      const noteExists = await prisma.note.findUnique({
        where: { id: noteID }
      });

      if (!noteExists) {
        set.status = 404;
        return "Note not found";
      }

      return prisma.note.delete({
        where: { id: noteID }
      });
    },
    {
      params: t.Object({
        id: t.Numeric()
      }),
      cookie: requiredCookieSession
    }
  )
  .post(
    "/notes",
    async ({ body, cookie: { session } }) => {
      return await prisma.note.createMany({
        data: body.map((data) => ({
          userId: session.value,
          title: data.title,
          content: data.content
        }))
      });
    },
    {
      body: t.Array(
        t.Object({
          title: t.Optional(t.String()),
          content: t.String(),
          labels: t.Optional(t.Array(t.String()))
        }),
        { minItems: 1 }
      ),
      cookie: requiredCookieSession
    }
  )
  .get(
    "/notes",
    async ({ cookie: { session } }) => {
      return await prisma.note
        .findMany({
          where: {
            userId: session.value
          },
          include: {
            labels: {
              select: {
                name: true,
                id: true,
                createdAt: true,
                updatedAt: true
              }
            }
          }
        })
        .then((e) => {
          return e.map((n) => {
            const labels = n.labels.map((l) => ({
              ...l,
              createdAt: l.createdAt.valueOf(),
              updatedAt: l.updatedAt.valueOf()
            }));

            return {
              ...n,
              createdAt: n.createdAt.valueOf(),
              updatedAt: n.updatedAt.valueOf(),
              labels
            };
          });
        });
    },
    {
      cookie: requiredCookieSession
    }
  )
  .post(
    "/label",
    async ({ body, set, cookie: { session } }) => {
      if (body.noteID) {
        const note = await prisma.note.findUnique({
          where: { id: body.noteID, userId: session.value }
        });
        if (!note) {
          set.status = 404;
          return "Note not found";
        }
      }

      try {
        return await prisma.label
          .create({
            data: {
              name: body.name,
              ownerId: session.value,
              ...(body.noteID
                ? { Note: { connect: { id: body.noteID! } } }
                : {})
            }
          })
          .then((e) => {
            return {
              ...e,
              createdAt: e.createdAt.valueOf(),
              updatedAt: e.updatedAt.valueOf()
            };
          });
      } catch (e) {
        if (e instanceof Prisma.PrismaClientKnownRequestError) {
          // "Unique constraint failed on the fields: (`name`,`ownerId`)"
          if (e.code === PrismaError.P2002) {
            set.status = 409;
            return "A label with that name already exists.";
          }
        }
        throw e;
      }
    },
    {
      body: t.Object({
        name: t.String(),
        noteID: t.Optional(t.Number())
      }),
      cookie: requiredCookieSession
    }
  )
  .put(
    "/label/:id",
    async ({ body, set, params: { id }, cookie: { session } }) => {
      const label = await prisma.label.findFirst({
        where: { id: id, ownerId: session.value }
      });

      if (!label) {
        set.status = 404;
        return "Label not found";
      }

      return prisma.label.update({
        where: { id: id },
        data: {
          name: body.name
        }
      });
    },
    {
      body: t.Object({
        name: t.String()
      }),
      params: t.Object({
        id: t.Numeric()
      }),
      cookie: requiredCookieSession
    }
  )
  .get(
    "/labels",
    ({ cookie: { session } }) =>
      prisma.label.findMany({
        where: { ownerId: session.value }
      }),
    { cookie: requiredCookieSession }
  )
  .get(
    "/label/:id",
    async ({ params: { id }, set, cookie: { session } }) => {
      const label = await prisma.label.findFirst({
        where: { id: id, ownerId: session.value }
      });

      if (!label) {
        set.status = 404;
        return "Label not found";
      }

      return label;
    },
    {
      params: t.Object({
        id: t.Numeric()
      }),
      cookie: requiredCookieSession
    }
  )
  .delete(
    "/label/:id",
    async ({ params: { id }, set, cookie: { session } }) => {
      const label = await prisma.label.findFirst({
        where: { id: id, ownerId: session.value }
      });

      if (!label) {
        set.status = 404;
        return "Label not found";
      }

      return prisma.label.delete({
        where: { id: id }
      });
    },
    {
      params: t.Object({
        id: t.Numeric()
      }),
      cookie: requiredCookieSession
    }
  )
  .get(
    "/full-sync",
    async ({ cookie: { session } }) => {
      const userID = session.value;
      const notes = await prisma.note
        .findMany({
          where: {
            userId: userID
          },
          include: {
            labels: {
              select: {
                id: true
              }
            }
          }
        })
        .then((e) =>
          e.map((l) => ({
            ...l,
            userId: undefined,
            createdAt: l.createdAt.valueOf(),
            updatedAt: l.updatedAt.valueOf(),
            labels: l.labels.map((v) => v.id)
          }))
        );

      const labels = await prisma.label.findMany({
        where: {
          ownerId: userID
        }
      });

      return {
        notes,
        labels: labels.map((e) => ({
          id: e.id,
          name: e.name,
          createdAt: e.createdAt.valueOf(),
          updatedAt: e.updatedAt.valueOf()
        }))
      };
    },
    { cookie: requiredCookieSession }
  )
  .use(changesEndpoint())
  .listen(PORT, (server) => {
    console.log(`🦊 Elysia is running at ${server?.hostname}:${server?.port} `);
  });
