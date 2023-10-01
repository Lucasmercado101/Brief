import { Elysia, t } from "elysia";
import { Prisma, PrismaClient } from "@prisma/client";
import { logger } from "./logger";
import PrismaError from "./prismaErrorCodes";

// in seconds
const MINUTE = 60;
const HOUR = MINUTE * 60;
const DAY = HOUR * 24;

const COOKIE_MAX_AGE = DAY * 14;

const prisma = new PrismaClient();

const PORT = process.env.PORT || 4000;

//NOTE: t.Number() === userID in this case:
const cookieSessionDTO = t.Number();

const cookieSecret = {
  secrets: process.env.COOKIE_SECRETS || "Fischl von Luftschloss Narfidort",
  sign: ["session"]
};

const requiredCookieSession = t.Cookie(
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
    set.headers["Access-Control-Allow-Method"] = "POST";
    // default max age
    set.headers["Access-Control-Max-Age"] = "5";
    set.status = 204;
  })
  .onRequest(({ set, request }) => {
    const origin = request.headers.get("Origin") ?? "";
    set.headers["Access-Control-Allow-Origin"] = origin;
    set.headers["Access-Control-Allow-Credentials"] = "true";
    set.headers["Access-Control-Allow-Headers"] = "content-type";
    set.headers["Access-Control-Allow-Method"] = "POST";
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
  // error when unsigning doesn't let me reassign and resign
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
  .post(
    "/note",
    async ({ body, cookie: { session } }) => {
      return await prisma.note.create({
        data: {
          userId: session.value,
          title: body.title,
          content: body.content
        }
      });
    },
    {
      beforeHandle({ cookie: { session } }) {
        session.maxAge = COOKIE_MAX_AGE;
      },
      body: t.Object({
        title: t.Optional(t.String()),
        content: t.String(),
        labels: t.Optional(t.Array(t.String()))
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
        return await prisma.label.create({
          data: {
            name: body.name,
            Note: { connect: { id: body.noteID } },
            ownerId: session.value
          }
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
  .listen(PORT, (server) => {
    console.log(`🦊 Elysia is running at ${server?.hostname}:${server?.port} `);
  });
