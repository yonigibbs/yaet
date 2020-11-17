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
- [x] Create unit tests for the Game module
- [x] When rotating pieces, if rotation means one of the blocks goes off edge, move piece back into board?
- [ ] Constantly rotating when at the lowest possible position restarts the timer drop so user can stop the game from
      progressing at all by just holding down the rotate key.
- [ ] Improve playability wrt long holds of buttons
- [ ] Add key binding for dropping current piece immediately (space bar?)
- [ ] GitHub actions for automatically running unit tests?
- [ ] Publish on GitHub pages?
- [ ] Prettify the UI (add `elm-ui`?)
- [ ] UI for different states (instructions, playing, paused, game ended, etc)
- [ ] Show upcoming pieces
- [ ] Show where currently dropping shape would land (brighter outline on cells on bottom-most valid rows?)
- [ ] Add ability to pause game
- [ ] Scores
- [ ] High scores (persisted to local storage)
- [ ] Settings/preferences, e.g. keyboard bindings (persisted to local storage)
- [ ] Buttons to use instead of keyboard for phones/tablets?
- [ ] Investigate "Hold" feature in some versions
- [ ] Look into probabilities of shapes - should some shapes be more likely than others (e.g. straight line)?
- [ ] TODOs in the code
