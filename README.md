Tetris, written in Elm, as a learning exercise.

Current state: WIP.

# TODO
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
- [ ] When rotating pieces, if rotation means one of the blocks goes off edge, move piece back into board?
- [ ] Improve playability wrt long holds of buttons
- [ ] Add key binding for dropping current piece immediately (space bar?)
- [ ] Unit tests for the Game module
- [ ] GitHub actions for automatically running unit tests?
- [ ] Prettify the UI (add `elm-ui`?)
- [ ] UI for different states (instructions, playing, paused, game ended, etc)
- [ ] Show upcoming pieces
- [ ] Show where currently dropping shape would land (brigher outline on cells on bottom-most valid rows?)
- [ ] Add ability to pause game
- [ ] Scores
- [ ] High scores (persisted to local storage)
- [ ] Settings/preferences, e.g. keyboard bindings (persisted to local storage)
- [ ] Buttons to use instead of keyboard for phones/tablets?
- [ ] Investigate "Hold" feature in some versions
- [ ] Look into probabilities of shapes - should some shapes be more likely than others (e.g. straight line)?
- [ ] TODOs in the code
