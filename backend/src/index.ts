import { Elysia, t } from "elysia";
import { PrismaClient } from "@prisma/client";
import { logger } from "./logger";

const prisma = new PrismaClient();

const PORT = process.env.PORT || 4000;

new Elysia({
  cookie: {
    secrets: process.env.COOKIE_SECRETS || "Fischl von Luftschloss Narfidort",
    sign: ["user"]
  }
})
  .use(logger())
  .post(
    "/register",
    async () => {
      return await prisma.user.create({
        data: {
          email: "test@gmail.com",
          password: Bun.password.hashSync("test")
        }
      });
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
  .get("/", () => "Hello Elysia")
  .listen(PORT, (server) => {
    console.log(`🦊 Elysia is running at ${server?.hostname}:${server?.port} `);
  });
