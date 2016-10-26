# Go Game

## Description

This project contains a small go game implementation in scala.
The game can be played in command line at the moment, recognizing a few commands :
- pass
- undo
- restart
- quit
- [A-S][0-9]{1,2} to put a stone at the given coordinates

The implementation recognize only basic rules (the one I know :) ) and is able to detect when a string of stones is surrounded by the opposite color

## TODO
- Repetition rule (implemented but doesn't work)
- Detecting territories at the end of the game
- Computing score using Japanese/Chinese rules
- Implement a deep-NN AI to compete with Google Deepmind ... ;)