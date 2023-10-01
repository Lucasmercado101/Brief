"use strict";

// DB

if ("indexedDB" in self) {
  const openReq = indexedDB.open("notes", 1);

  openReq.onsuccess = function () {
    console.log("open success");
  };
}

// DB end ------

const cookieName = "session";
// regex obtained from: https://stackoverflow.com/a/25617724
const hasSessionCookie = document.cookie.match(
  new RegExp(`^(.*;)?\s*${cookieName}\s*=\s*[^;]+(.*)?$`)
);

const app = Elm.Main.init({
  flags: {
    seeds: Array.from(self.crypto.getRandomValues(new Uint32Array(4))),
    hasSessionCookie: !!hasSessionCookie
  },
  node: document.getElementById("root")
});

app.ports.requestRandomValues.subscribe(function () {
  app.ports.receiveRandomValues.send(
    Array.from(self.crypto.getRandomValues(new Uint32Array(4)))
  );
});
