# Elm Tetris

[![Actions Status](https://github.com/yonigibbs/yaet/workflows/Node.js%20CI/badge.svg)](https://github.com/yonigibbs/yaet/actions)

This repo contains a version of Tetris, written in Elm.

#### Play the game [here](https://yonigibbs.github.io/yaet/).

## Development

To run the project locally clone the repo, run `npm install` (only needed once), then run `npm run serve`. This will
start [Parcel](https://parceljs.org/) to serve the app on http://localhost:1234.

The entry point to the app is [index.html](src/index.html). This loads [index.js](src/index.js), which in turn loads the
Elm app itself. The entry point to the Elm code is [Main.elm](src/Main.elm). This module, like the rest, contains
comments at the top that explain a bit about it, and should help you find your way around the code.

## Possible future enhancements

* Allow game to be played on more devices
    * Add buttons to control the game, for devices without keyboards
    * Make the UI responsive
* Prevent default browser behaviour on key presses, otherwise arrow keys can cause the viewport to move if the browser
  window is small (see https://github.com/elm/browser/issues/89)
* Some complex moves currently might not be fully possible: investigate T-spin triple, for example
* Let user pause/resume by clicking on game
* Add bonus points for hard drop
* Improve Game Over animation - drop the "Game Over" message down from top of board
* Use `elm-animator` instead of doing animations manually
* Add smoother transitions (e.g. fade out/in) between welcome screen and game screen
* Minor issues with some corner cases around trapping Enter key on modal dialogs - if the Cancel (or Restore Defaults)
  button has focus and user presses Enter, what should happen? Is it even normal in web UI to treat Enter as Submit, in
  modals with no editable controls?
