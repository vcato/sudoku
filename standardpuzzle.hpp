#include "puzzle.hpp"


struct StandardPuzzle : Puzzle {
  bool boardIsValid(const Board &board) const override
  {
    return Checker(board,/*show_violations*/false).checkUniqueness();
  }
};
