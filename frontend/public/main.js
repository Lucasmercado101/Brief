"use strict";

const cookieName = "session";
// regex obtained from: https://stackoverflow.com/a/25617724
const hasSessionCookie = document.cookie.match(
  new RegExp(`^(.*;)?\s*${cookieName}\s*=\s*[^;]+(.*)?$`)
);

const app = Elm.Main.init({
  flags: { seeds: Array.from(self.crypto.getRandomValues(new Uint32Array(4))) },
  node: document.getElementById("root")
});

app.ports.requestRandomValues.subscribe(function () {
  app.ports.receiveRandomValues.send(
    Array.from(self.crypto.getRandomValues(new Uint32Array(4)))
  );
});
