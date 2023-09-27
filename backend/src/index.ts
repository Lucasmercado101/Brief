import { Elysia, t } from "elysia";
import { Prisma, PrismaClient } from "@prisma/client";
import { logger } from "./logger";
import PrismaError from "./prismaErrorCodes";

const prisma = new PrismaClient();

const PORT = process.env.PORT || 4000;

new Elysia({
  cookie: {
    secrets: process.env.COOKIE_SECRETS || "Fischl von Luftschloss Narfidort",
    sign: ["user"]
  }
})
  .onError(({ error, set, code }) => {
    switch (code) {
      case "VALIDATION":
        set.status = 400;
        return error.message;
      case "NOT_FOUND":
        set.status = 404;
        return "Endpoint not found.";
      case "PARSE":
      case "INVALID_COOKIE_SIGNATURE":
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
  .post("/login", async ({ cookie: { user } }) => {
    user.value = { something: "abcasd" };
    return "logged in";
  })
  .listen(PORT, (server) => {
    console.log(`🦊 Elysia is running at ${server?.hostname}:${server?.port} `);
  });
