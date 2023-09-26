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
  .onError(({ error, set }) => {
    // TODO: better
    console.log("ERROR\n");
    console.log(error);
    console.log("\nERROR END");
    set.status = 500;
    return "An unexpected error has occurred.";
  })
  .use(logger())
  .post(
    "/register",
    async ({ set }) => {
      let user;
      try {
        await prisma.user.create({
          data: {
            email: "test@gmail.com",
            password: Bun.password.hashSync("test")
          }
        });
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
        email: t.String({ format: "email" }),
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
