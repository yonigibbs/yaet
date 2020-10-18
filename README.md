Tetris, written in Elm, as a learning exercise.

Current state: WIP.

# TODO
- [x] Add `Game` module to control the game
- [x] Render board and dropping shape
- [x] Generate random next shape
- [x] Timer to drop shapes
- [x] Handle arrow keys to move shapes left/right/down
- [x] Handle arrow keys to rotate pieces
- [ ] Add arrow keys to drop piece immediately
- [ ] When using down arrow to drop piece, if it lands, don't start dropping down next piece quickly
- [x] Make lines disappear when one completed
- [ ] More unit tests
- [ ] Calculate when game over
- [ ] When rotating pieces, if rotation means one of the blocks goes off edge, move piece back into board?
- [ ] Increase speed as lines disappear
- [ ] GitHub actions for automatically running unit tests?
- [ ] Prettify the UI (add `elm-ui`?)
- [ ] UI for different states (instructions, playing, paused, game ended, etc)
- [ ] Add animation for lines disappearing (fade out? flash?)
- [ ] Show upcoming pieces
- [ ] Scores
- [ ] High scores (persisted to local storage)
- [ ] Settings/preferences, e.g. keyboard bindings (persisted to local storage)
- [ ] Buttons to use instead of keyboard for phones/tablets?
- [ ] Investigate "Hold" feature in some versions
- [ ] TODOs in the code
