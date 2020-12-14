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
- [x] When using down arrow to drop piece, if it lands, don't immed start dropping down next piece quickly (possibly
      by adding animation on drop?)
- [x] Increase speed as lines disappear
- [x] Add animation for lines disappearing (fade out? flash?) and maybe also shapes landing?
- [x] Calculate when game over
- [x] Create unit tests for the Game module
- [x] When rotating pieces, if rotation means one of the blocks goes off edge, move piece back into board?
- [x] Prettify the UI (add `elm-ui`?)
- [x] Add animation to welcome screen (drop letters of Tetris, flash them  in/out, then have random shapes with lower
      opacity falling behind the letters)
- [x] Publish on GitHub pages
- [x] GitHub actions for automatically running unit tests
- [x] Complete UI for each of the different states (instructions, playing, paused, game ended, etc)
- [x] Constantly rotating when at the lowest possible position restarts the timer drop so user can stop the game from
      progressing at all by just holding down the rotate key.
- [x] Improve playability wrt long holds of buttons
- [ ] Add more unit tests for the above
- [ ] Fix issue where if two empty rows available and user pressing the down arrow, the new shape appears then
      disappears briefly just before Game Over appears. 
- [ ] Add key binding for dropping current piece immediately (space bar?)
- [ ] Add ability to pause game
- [ ] Show upcoming pieces
- [ ] Scores
- [ ] High scores (persisted to local storage)
- [ ] Settings/preferences, e.g. keyboard bindings (persisted to local storage)
- [ ] Add link to get to source code, etc.
- [ ] Consolidate `id` fields used in messages/subscriptions/etc - put into a module and expose as opaque type?
      (e.g. `PlayingModel.timerDropMessageId`, `HighlightAnimation.Id`, etc.)
- [ ] Make UI responsive.
  - [ ] Buttons to use instead of keyboard for phones/tablets?
- [ ] Improve Game Over animation - drop the "Game Over" message down from top of board (use SVG for this?)
- [ ] Show where currently dropping shape would land (brighter outline on cells on bottom-most valid rows?)
- [ ] Use `elm-animator` instead of doing animations manually?
- [ ] Add smoother transitions (e.g. fade out/in) between welcome screen and game screen
- [ ] Investigate "Hold" feature in some versions
- [ ] Look into probabilities of shapes - should some shapes be more likely than others (e.g. straight line)?
- [ ] TODOs in the code
