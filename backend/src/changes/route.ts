import { Elysia } from "elysia";
import { changesEndpoint } from "./index";
import { requiredCookieSession } from "..";

export default () =>
  new Elysia()
    // error handler Copy and pasted from index.ts
    .onError(({ error, set, code }) => {
      console.log(code);
      switch (code) {
        case "VALIDATION":
          set.status = 400;
          // TODO: maybe human readable error?
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
      "/changes",
      async ({ body, cookie: { session } }) => {
        return await changesEndpoint(session.value, body);
      },
      {
        cookie: requiredCookieSession
      }
    );
