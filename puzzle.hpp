#ifndef PUZZLE_HPP_
#define PUZZLE_HPP_


struct Puzzle {
  virtual bool boardIsValid(const Board &baord) const = 0;
};


#endif /* PUZZLE_HPP_ */
