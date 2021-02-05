import {Elm} from "./Main.elm"

const settings = localStorage.getItem("settings")
const highScores = localStorage.getItem("highScores")

const app = Elm.Main.init({
    node: document.querySelector("main"),
    flags: {
        settings: settings ? JSON.parse(settings) : null,
        highScores: highScores ? JSON.parse(highScores) : null
    }
})

/**
 * Persists the settings in local storage.
 */
app.ports.persistSettings.subscribe(settings => localStorage.setItem("settings", JSON.stringify(settings)))
