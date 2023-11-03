// DB

if ("indexedDB" in self) {
  let db: undefined | IDBDatabase;
  const openReq = indexedDB.open("notesDB", 1);

  openReq.onupgradeneeded = function (event) {
    console.log("open success");
    db = openReq.result;
    // const db = event.target.results;
    if (!db.objectStoreNames.contains("notes")) {
      db.createObjectStore("notes", { keyPath: "id" });
    }
  };

  setTimeout(() => {
    const note = {
      id: 1,
      title: "Note 1",
      content: "Content 1",
      pinned: false,
      labels: ["label 1", "label 2"],
      createdAt: new Date(),
      updatedAt: new Date()
    };

    const tx = db!.transaction("notes", "readwrite");

    const store = tx.objectStore("notes");
    const request = store.add(note);

    tx.oncomplete = function (e) {
      console.log("tx complete", e);
    };

    request.onsuccess = function (e) {
      console.log("request success", e);
    };

    tx.onerror = function (event) {
      console.log("tx error", event);
    };

    request.onerror = function (event) {
      console.log("request error", event);
    };
  }, 5000);
}

// DB end ------

const cookieName = "session";
// regex obtained from: https://stackoverflow.com/a/25617724
const hasSessionCookie = document.cookie.match(
  new RegExp(`^(.*;)?\s*${cookieName}\s*=\s*[^;]+(.*)?$`)
);

let lastSyncedAt = localStorage.getItem("lastSyncedAt");
// @ts-ignore
const app = Elm.Main.init({
  flags: {
    windowSize: { width: window.innerWidth, height: window.innerHeight },
    online: window.navigator.onLine,
    seeds: Array.from(self.crypto.getRandomValues(new Uint32Array(4))),
    hasSessionCookie: !!hasSessionCookie,
    lastSyncedAt: lastSyncedAt ? Number(lastSyncedAt) : 1
  },
  node: document.getElementById("root")
});

window.addEventListener("online", function () {
  app.ports.backOnline.send(null);
});
window.addEventListener("offline", () => {
  app.ports.goneOffline.send(null);
});

window.addEventListener("resize", () => {
  const width = window.innerWidth;
  const height = window.innerHeight;
  app.ports.windowResized.send({ width, height });
});

app.ports.reqWindowSize.subscribe(function () {
  const width = window.innerWidth;
  const height = window.innerHeight;
  app.ports.windowResized.send({ width, height });
});

app.ports.requestRandomValues.subscribe(function () {
  app.ports.receiveRandomValues.send(
    Array.from(self.crypto.getRandomValues(new Uint32Array(4)))
  );
});

app.ports.updateLastSyncedAt.subscribe(function (lastSyncedAt: string) {
  localStorage.setItem("lastSyncedAt", lastSyncedAt);
});
