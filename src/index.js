import {Elm} from "./Main.elm"

// Read the settings and high scores from local storage. This is sent into the Elm app as JSON which is decoded using
// Elm. Any errors in decoding (or null values) result in the default settings and high scores being used instead.
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

/**
 * Persists the settings in local storage.
 */
app.ports.persistHighScores.subscribe(highScores => localStorage.setItem("highScores", JSON.stringify(highScores)))
