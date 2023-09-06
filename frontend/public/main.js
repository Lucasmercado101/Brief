"use strict";

const app = Elm.Main.init({
  flags: { seeds: Array.from(self.crypto.getRandomValues(new Uint32Array(4))) },
  node: document.getElementById("root")
});

app.ports.requestRandomValues.subscribe(function () {
  app.ports.receiveRandomValues.send(
    Array.from(self.crypto.getRandomValues(new Uint32Array(4)))
  );
});
