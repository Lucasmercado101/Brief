import { Elysia, t } from "elysia";
import { Prisma, PrismaClient } from "@prisma/client";
import { logger } from "./logger";
import PrismaError from "./prismaErrorCodes";

const prisma = new PrismaClient();

const PORT = process.env.PORT || 4000;

const cookieSessionDTO = t.Object({
  userID: t.String()
});

new Elysia()
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
      case "PARSE":
      case "UNKNOWN":
        // TODO: better
        console.log("ERROR\n");
        console.log(error);
        console.log("\nERROR END");
        set.status = 500;
        return error.message;
      case "INTERNAL_SERVER_ERROR":
        set.status = 500;
        return "An unexpected error has occurred.";
    }
  })
  .use(logger())
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
      session.value = { userID: "a" };
      return "Logged in";
    },
    {
      body: t.Object({
        email: t.String(),
        password: t.String()
      }),
      cookie: t.Cookie(
        {
          profile: t.Optional(cookieSessionDTO)
        },
        {
          secrets:
            process.env.COOKIE_SECRETS || "Fischl von Luftschloss Narfidort",
          sign: ["profile"]
        }
      )
    }
  )
  .post(
    "/note",
    async ({ body }) => {
      prisma.note.create({
        data: {
          userId: 1,
          title: body.title,
          content: body.content
        }
      });
    },
    {
      body: t.Object({
        title: t.Optional(t.String()),
        content: t.String(),
        labels: t.Optional(t.Array(t.String()))
      })
    }
  )
  .listen(PORT, (server) => {
    console.log(`🦊 Elysia is running at ${server?.hostname}:${server?.port} `);
  });
