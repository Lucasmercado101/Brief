import { Elysia } from "elysia";
import { PrismaClient } from "@prisma/client";
import { logger } from "./logger";

const prisma = new PrismaClient();

const PORT = process.env.PORT || 4000;

const app = new Elysia()
  .use(logger())
  .post("/register", async () => {
    return await prisma.user.create({
      data: {
        email: "test@gmail.com",
        password: "test"
      }
    });
  })
  .get("/", () => "Hello Elysia")
  .listen(PORT);

console.log(
  `🦊 Elysia is running at ${app.server?.hostname}:${app.server?.port} `
);
