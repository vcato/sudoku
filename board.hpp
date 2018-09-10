#include <iostream>
#include "grid.hpp"


struct Board : Grid<Number> {
  public:
    Board() : Grid<Number>(' ') { }

    Board(const Number (&cell_values)[size][size+1])
    : Board()
    {
      for (auto row : rowIndices()) {
        for (auto col : columnIndices()) {
          (*this)[row][col] = cell_values[row][col];
        }
        assert(cell_values[row][size]=='\0');
      }
    }

    bool cellIsEmpty(Index row,Index col) const
    {
      return (*this)[row][col]==' ';
    }
};


inline void show(const char *desc,const Board &board,std::ostream &stream)
{
  stream << desc << ":\n";
  for (auto row : board.rowIndices()) {
    stream << "  ";
    if (row>0 && row%3==0) {
      for (auto col : board.columnIndices()) {
        if (col>0 && col%3==0) {
          stream << "+";
        }
        stream << "-";
      }
      stream << "\n";
      stream << "  ";
    }
    for (auto col : board.columnIndices()) {
      if (col>0 && col%3==0) {
        stream << "|";
      }
      stream << board[row][col];
    }
    stream << "\n";
  }
}


inline Numbers boardRowNumbers(const Board &board,Index row)
{
  Numbers result;
  result.reserve(board.size);
  for (auto column : board.columnIndices()) {
    result.push_back(board[row][column]);
  }
  return result;
}


inline Numbers boardColumnNumbers(const Board &board,Index column)
{
  Numbers result;
  result.reserve(board.size);
  for (auto row : board.rowIndices()) {
    result.push_back(board[row][column]);
  }
  return result;
}


inline Numbers boardRegionNumbers(const Board &board,Index region)
{
  assert(region>=0 && region<9);
  assert(board.size==9);
  Numbers result;
  result.reserve(9);
  int x = region%3;
  int y = region/3;
  for (int i=0; i!=3; ++i) {
    for (int j=0; j!=3; ++j) {
      result.push_back(board[y*3+i][x*3+j]);
    }
  }
  assert(result.size()==9);
  return result;
}


struct Checker {
  const Board &board;
  bool report;

  Checker(const Board &board,bool show_violations)
  : board(board),
    report(show_violations)
  {
  }

  void showBoard() { show("failure",board,std::cerr); }

  bool checkNumbersAreUnique(const Numbers &cells)
  {
    std::vector<bool> used(board.size,false);

    for (auto i : irange(cells)) {
      Number cell = cells[i];
      if (cell!=' ') {
        int value = cell-'1';
        assert(value>=0);
        assert(value<static_cast<int>(used.size()));
        if (used[value]) {
          if (report) {
            std::cerr << cell << " is used more than once.\n";
          }
          return false;
        }
        used[value] = true;
      }
    }
    return true;
  }


  bool checkAllCellsAreFilled()
  {
    for (auto i : board.rowIndices()) {
      for (auto j : board.columnIndices()) {
        if (board.cellIsEmpty(i,j)) {
          if (report) {
            std::cerr << "row " << i+1 << " column " << j+1 << " is not filled.\n";
            showBoard();
          }
          return false;
        }
      }
    }

    return true;
  }

  bool allRowsHaveUniqueNumbers()
  {
    for (auto i : board.rowIndices()) {
      if (!checkNumbersAreUnique(boardRowNumbers(board,i))) {
        if (report) {
          std::cerr << " on row " << i+1 << "\n";
          showBoard();
        }
        return false;
      }
    }
    return true;
  }

  bool allColumnsHaveUniqueNumbers()
  {
    for (auto i : board.columnIndices()) {
      if (!checkNumbersAreUnique(boardColumnNumbers(board,i))) {
        if (report) {
          std::cerr << " on column " << i+1 << "\n";
          showBoard();
        }
        return false;
      }
    }
    return true;
  }

  bool allRegionsHaveUniqueNumbers()
  {
    for (auto i : board.regionIndices()) {
      if (!checkNumbersAreUnique(boardRegionNumbers(board,i))) {
        if (report) {
          std::cerr << " in region " << i+1 << "\n";
          showBoard();
        }
        return false;
      }
    }
    return true;
  }

  bool checkUniqueness()
  {
    return allRowsHaveUniqueNumbers() &&
           allColumnsHaveUniqueNumbers() &&
           allRegionsHaveUniqueNumbers();
  }

  bool checkIsSolved()
  {
    return checkAllCellsAreFilled() && checkUniqueness();
  }
};


template <typename Func>
void forEachEmptyCell(const Board &board,const Func &f)
{
  for (auto row : board.rowIndices()) {
    for (auto col : board.columnIndices()) {
      if (board.cellIsEmpty(row,col)) {
        f(row,col);
      }
    }
  }
}
