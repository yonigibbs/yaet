# Elm Tetris

[![Actions Status](https://github.com/yonigibbs/yaet/workflows/Node.js%20CI/badge.svg)](https://github.com/yonigibbs/yaet/actions)

This repo contains a version of Tetris, written in Elm, as a learning exercise.

### Play the game [here](https://yonigibbs.github.io/yaet/).

Current state: **WIP.**

## TODO

- [x] Add `Game` module to control the game
- [x] Render board and dropping shape
- [x] Generate random next shape
- [x] Timer to drop shapes
- [x] Handle arrow keys to move shapes left/right/down
- [x] Handle arrow keys to rotate pieces
- [x] Make lines disappear when one completed
- [x] When using down arrow to drop piece, if it lands, don't immed start dropping down next piece quickly (possibly by
  adding animation on drop?)
- [x] Increase speed as lines disappear
- [x] Add animation for lines disappearing (fade out? flash?) and maybe also shapes landing?
- [x] Calculate when game over
- [x] Create unit tests for the Game module
- [x] When rotating pieces, if rotation means one of the blocks goes off edge, move piece back into board?
- [x] Prettify the UI (add `elm-ui`?)
- [x] Add animation to welcome screen (drop letters of Tetris, flash them in/out, then have random shapes with lower
  opacity falling behind the letters)
- [x] Publish on GitHub pages
- [x] GitHub actions for automatically running unit tests
- [x] Complete UI for each of the different states (instructions, playing, paused, game ended, etc)
- [x] Constantly rotating when at the lowest possible position restarts the timer drop so user can stop the game from
  progressing at all by just holding down the rotate key.
- [x] Improve playability wrt long holds of buttons
- [x] Fix issue where if two empty rows available and user pressing the down arrow, the new shape appears then
  disappears briefly just before Game Over appears.
- [x] Show upcoming piece
- [x] Show where currently dropping shape would land
- [x] Add "Hold" feature
- [x] Add key binding for dropping current piece immediately (space bar?)
- [x] Add ability to pause game
- [x] Fix issue where guidelines don't show for one frame after line removed
- [x] Settings/preferences, e.g. keyboard bindings (persisted to local storage)
    - [x] Let user close screen by pressing Escape key
- [ ] Update terminology in code - use soft/hard drop, and proper shape names, etc
- [ ] Scores
- [ ] High scores (persisted to local storage)
- [ ] Add link to get to source code, etc.
- [ ] Some moves currently not possible? Investigate T-spin triple, for example.
- [ ] Consolidate `id` fields used in messages/subscriptions/etc - put into a module and expose as opaque type?
  (e.g. `PlayingModel.timerDropMessageId`, `HighlightAnimation.Id`, etc.)
- [ ] Prevent default on key presses? (Otherwise arrow keys can move viewport about if browser window small)
    - [ ] See https://github.com/elm/browser/issues/89 - might need to use ports?
- [ ] Make UI responsive.
    - [ ] Buttons to use instead of keyboard for phones/tablets?
- [ ] Let user pause/resume by clicking on game
- [ ] Improve Game Over animation - drop the "Game Over" message down from top of board (use SVG for this?)
- [ ] Use `elm-animator` instead of doing animations manually?
- [ ] Add smoother transitions (e.g. fade out/in) between welcome screen and game screen
- [ ] TODOs in the code
- [ ] Some corner cases around trapping Enter key on modal dialogs - if the Cancel (or Restore Defaults) button has
  focus and user presses Enter, what should happen? Is it even normal in web UI to treat Enter as Submit, in modals with
  no editable controls?
