//collect session if any
let session = localStorage.getItem("session")

// embed application to html
const app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: session
})

// subscribe to session changes from elm
app.ports.storeSession.subscribe(function (sessionData) {
    localStorage.setItem("session", sessionData);

    setTimeout(function () { app.ports.sessionChanged.send(sessionData); }, 0);
});
