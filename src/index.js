import {Elm} from "./Main.elm"

const settings = localStorage.getItem("settings")
const highScores = localStorage.getItem("highScores")

Elm.Main.init({
    node: document.querySelector("main"),
    flags: {
        settings: settings ? JSON.parse(settings) : null,
        highScores: highScores ? JSON.parse(highScores) : null
    }
})
