#include <cassert>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using std::cerr;
using std::cout;
using std::vector;
using std::ostream;
using std::string;

using Index = int;
using Number = char;
using Numbers = vector<Number>;
using BoardState = Number [9][10];
using SumSpec = const char [19][20];


static const BoardState test_board_1 = {
  "92  8  17",
  "1    26  ",
  " 7   34  ",
  "   7352  ",
  " 652 813 ",
  "  8961   ",
  "  96   7 ",
  "  78    4",
  "84  5  61"
};


static const BoardState test_board_2 = {
  "      1  ",
  "    3   9",
  "   1   82",
  " 4 3     ",
  "  2     5",
  "  1  586 ",
  "13 498 5 ",
  "6       3",
  "    6 9  "
};


static const BoardState test_board_3 = {
  "5     291",
  "1 82  4  ",
  "      8  ",
  "6  45 7  ",
  "  53 2 4 ",
  "2     3 8",
  "9  6    2",
  "    7    ",
  "  31     "
};


static const BoardState test_board_4 = {
  "        5",
  "   94  2 ",
  "     597 ",
  "9    8   ",
  "   5  6  ",
  " 18 2    ",
  "1764532  ",
  "4  871   ",
  "385692741",
};


static const BoardState test_board_5 = {
  "        5",
  "5  94  2 ",
  "     597 ",
  "95  68   ",
  "   5  6  ",
  "618 2    ",
  "1764532  ",
  "4  871   ",
  "385692741",
};


static const BoardState test_board_6 = {
  "        5",
  "   94  2 ",
  "     597 ",
  "9    8   ",
  "   5  6  ",
  " 18 2    ",
  " 76  32  ",
  "4   7    ",
  " 856 2  1",
};


static const BoardState test_board_7 = {
  "      5  ",
  "  4    6 ",
  "    69   ",
  "         ",
  "   47 6 9",
  " 7 8 1  3",
  " 2  9 3  ",
  "6    8 15",
  "    43 82",
};


static const BoardState test_board_8 = {
  "   6 5  2",
  "49    5  ",
  "5  4  73 ",
  "1  5   9 ",
  "   8 7  4",
  "8 3 6    ",
  " 61      ",
  "3  71    ",
  "     6   ",
};


static const BoardState test_board_9 = {
  "         ",
  "     7   ",
  "         ",
  "       5 ",
  "         ",
  " 7       ",
  "         ",
  "   9     ",
  "         ",
};


static SumSpec test_sum_spec_9 = {
  "+-+-+-+-+-+-+-+-+-+",
  "|16     |25 |2|21 |",
  "+-+-+-+-+   +5+ + +",
  "|25     |   | |   |",
  "+-+-+-+-+-+ + +-+ +",
  "|3|12 |13 | | |1| |",
  "+ +-+-+ +-+-+ +4+-+",
  "| |16 | |1|1| |   |",
  "+-+-+ +-+4+1+-+-+-+",
  "|13 | |9| | |6|15 |",
  "+-+-+-+ + +-+ +-+-+",
  "|14 |1| | |1|   |1|",
  "+-+ +1+-+-+1+-+-+6+",
  "|3| | |3|   |13 | |",
  "+5+-+ +5+-+-+-+-+-+",
  "|   | |   |14     |",
  "+   + +   +-+-+-+-+",
  "|   | |   |18     |",
  "+-+-+-+-+-+-+-+-+-+",
};


template <typename T,typename Predicate>
static size_t indexWhere(const vector<T> &v,const Predicate &f)
{
  size_t index = 0;

  for (const auto &x : v) {
    if (f(x)) {
      return index;
    }

    ++index;
  }

  assert(false);

  return 0;
}


template <typename T,typename Predicate>
static const T& elementWhere(const vector<T> &v,const Predicate &f)
{
  return v[indexWhere(v,f)];
}


static bool isValidCellValue(Number c)
{
  return (c==' ' || (c>='1' && c<='9'));
}


static bool isValidCellValue(const Numbers &)
{
  return true;
}


namespace {
  template <typename Index>
  struct IndexRange {
    Index begin_index;
    Index end_index;

    struct iterator :
      std::iterator<
        std::forward_iterator_tag,
        Index
      >
    {
      iterator(Index index) : index(index) { }

      Index operator*() const { return index; }
      bool operator==(const iterator &that) const { return index==that.index; }
      bool operator!=(const iterator &that) const { return !(*this==that); }

      iterator& operator++()
      {
        ++index;
        return *this;
      }

      private:
        Index index;
    };

    iterator begin() const { return iterator{begin_index}; }
    iterator end()   const { return iterator{end_index}; }
  };
}


template <
  typename T,
  typename U,
  typename TU = typename std::common_type<T,U>::type
>
static inline IndexRange<TU> irange(T first,U last)
{
  return {first,last};
}


template <typename T>
static inline IndexRange<typename T::size_type> irange(const T& container)
{
  return {0,container.size()};
}


namespace {
  struct IndexPair {
    Index row;
    Index col;

    IndexPair(Index row_arg,Index col_arg)
    : row(row_arg), col(col_arg)
    {
    }

    bool operator==(const IndexPair &arg) const
    {
      return row==arg.row && col==arg.col;
    }

    bool operator!=(const IndexPair &arg) const
    {
      return !operator==(arg);
    }
  };
}


static ostream& operator<<(ostream& stream,const IndexPair &cell)
{
  stream << "(" << cell.row << "," << cell.col << ")";
  return stream;
}


static IndexPair regionCell(Index region,Index region_cell_index)
{
  Index region_row = region / 3;
  Index region_col = region % 3;
  Index cell_row = region_cell_index / 3;
  Index cell_col = region_cell_index % 3;

  return {cell_row + region_row*3,cell_col + region_col*3};
}


namespace {
  enum { grid_size = 9 };
}

namespace {
  template <typename CellValue>
  class Grid {
    public:
      enum {size = grid_size};

      struct CellRef {
        Grid &grid;
        Index row, column;

        operator CellValue &() const { return grid.cell(row,column); }

        CellValue &value() const { return grid.cell(row,column); }

        CellRef operator=(CellValue value)
        {
          grid.setCell(row,column,value);
          return *this;
        }
      };

      struct RowRef {
        Grid &grid;
        Index row;

        CellRef operator[](Index column) { return {grid,row,column}; }
      };

      struct ConstRowRef {
        const Grid &grid;
        Index row;

        const CellValue &operator[](Index column)
        {
          return grid.cell(row,column);
        }
      };

      bool operator==(const Grid &arg) const
      {
        return cells==arg.cells;
      }

      bool operator!=(const Grid &arg) const
      {
        return !operator==(arg);
      }

      Grid(CellValue v) : cells(size*size,v) { }
      RowRef operator[](Index index) { return {*this,index}; }
      ConstRowRef operator[](Index index) const { return {*this,index}; }
      CellRef operator[](IndexPair cell) { return {*this,cell.row,cell.col}; }

      const CellValue &operator[](IndexPair cell) const
      {
        return this->cell(cell.row,cell.col);
      }

      size_t cellIndex(Index row,Index column) const
      {
        assert(row>=0);
        assert(row<size);
        assert(column>=0);
        assert(column<size);
        size_t cell_index = row*size+column;
        assert(cell_index<cells.size());
        return cell_index;
      }

      const CellValue &cell(Index row,Index column) const
      {
        return cells[cellIndex(row,column)];
      }

      CellValue &cell(Index row,Index column)
      {
        return cells[cellIndex(row,column)];
      }

      void setCell(Index row,Index column,CellValue cell_value)
      {
        assert(isValidCellValue(cell_value));
        cells[cellIndex(row,column)] = cell_value;
      }

      template <typename Func>
      void forEachPairOfCellsInRow(Index row,const Func &func)
      {
        Index n_cols = size;

        for (Index col1 : irange(0,n_cols)) {
          for (Index col2 : irange(col1+1,n_cols)) {
            func(IndexPair{row,col1},{row,col2});
          }
        }
      }

      template <typename Func>
      void forEachPairOfCellsInColumn(Index col,const Func &func)
      {
        Index n_rows = size;

        for (Index row1 : irange(0,n_rows)) {
          for (Index row2 : irange(row1+1,n_rows)) {
            func(IndexPair{row1,col},{row2,col});
          }
        }
      }

      template <typename Func>
      void forEachPairOfCellsInRegion(Index region,const Func &func)
      {
        Index n_cells = size;

        for (Index cell1 : irange(0,n_cells)) {
          for (Index cell2 : irange(cell1+1,n_cells)) {
            func(regionCell(region,cell1),regionCell(region,cell2));
          }
        }
      }

      IndexRange<Index> rowIndices()    const { return {0,size}; }
      IndexRange<Index> columnIndices() const { return {0,size}; }
      IndexRange<Index> regionIndices() const { return {0,size}; }

    private:
      vector<CellValue> cells;
  };
}


namespace {
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
}


static void show(const char *desc,const Board &board,ostream &stream)
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


namespace {
  struct Area {
    vector<IndexPair> cell_indices;
  };
}


namespace {
  static bool isValidCellValue(const IndexPair &)
  {
    return true;
  }

  struct IndexPairUnionFind {
    Grid<IndexPair> parents;

    IndexPairUnionFind()
    : parents(nullIndexPair())
    {
    }

    static IndexPair nullIndexPair()
    {
      return IndexPair{-1,-1};
    }

    IndexPair rootOf(const IndexPair &cell) const
    {
      IndexPair parent = parents.cell(cell.row,cell.col);

      if (parent == nullIndexPair()) {
        return cell;
      }

      return rootOf(parent);
    }

    void join(const IndexPair &a,const IndexPair &b)
    {
      IndexPair a_root = rootOf(a);
      IndexPair b_root = rootOf(b);

      if (a_root==b_root) {
        return;
      }

      parents[a_root] = b_root;
    }
  };
}


static int numericValue(char c)
{
  assert(isdigit(c));
  return c - '0';
}


namespace {
struct SumSpecAnalyzer {
  const SumSpec &sum_spec;
  using CellIndex = IndexPair;
  using CharIndex = IndexPair;

  static bool isConnectingChar(char c)
  {
    return c==' ' || (c>='1' && c<='9');
  }

  static CharIndex charIndex(const IndexPair &cell_index)
  {
    int char_row = cell_index.row*2 + 1;
    int char_col = cell_index.col*2 + 1;

    return CharIndex{char_row,char_col};
  }

  char relativeChar(const CellIndex &cell_index,int row_offset,int col_offset) const
  {
    CharIndex char_index = charIndex(cell_index);
    Index row = char_index.row + row_offset;
    Index col = char_index.col + col_offset;
    return sum_spec[row][col];
  }

  bool cellIsConnectedToPrevRow(const IndexPair &cell_index) const
  {
    return isConnectingChar(relativeChar(cell_index,-1,0));
  }

  bool cellIsConnectedToPrevCol(const IndexPair &cell_index) const
  {
    return isConnectingChar(relativeChar(cell_index,0,-1));
  }

  vector<Area> makeAreas() const
  {
    IndexPairUnionFind union_find;
    Index n_rows = grid_size;
    Index n_cols = grid_size;

    for (Index row : irange(0,n_rows)) {
      for (Index col : irange(0,n_cols)) {
        IndexPair cell_index = {row,col};

        if (row>0) {
          if (cellIsConnectedToPrevRow(cell_index)) {
            union_find.join(cell_index,IndexPair{row-1,col});
          }
        }

        if (col>0) {
          if (cellIsConnectedToPrevCol(cell_index)) {
            union_find.join(cell_index,IndexPair{row,col-1});
          }
        }
      }
    }

    vector<Area> areas;

    for (Index root_row : irange(0,n_rows)) {
      for (Index root_col : irange(0,n_cols)) {
        IndexPair root = {root_row,root_col};

        if (union_find.rootOf(root)==root) {
          Area area;

          for (Index member_row : irange(0,n_rows)) {
            for (Index member_col : irange(0,n_cols)) {
              IndexPair member = {member_row,member_col};

              if (union_find.rootOf(member)==root) {
                area.cell_indices.push_back(member);
              }
            }
          }

          areas.push_back(area);
        }
      }
    }

    return areas;
  }

  static int numericValue(char c1,char c2)
  {
    return ::numericValue(c1)*10 + ::numericValue(c2);
  }

  int areaSum(const Area &area)
  {
    const CellIndex &first_cell = area.cell_indices[0];
    char center = relativeChar(first_cell,0,0);
    char right = relativeChar(first_cell,0,1);
    char down = relativeChar(first_cell,1,0);

    assert(isdigit(center));

    if (isdigit(right)) {
      return numericValue(center,right);
    }

    if (isdigit(down)) {
      return numericValue(center,down);
    }

    return ::numericValue(center);
  }
};
}


static void testRowIndices()
{
  Board board;
  auto row_indices = board.rowIndices();
  vector<Index>
    indices(row_indices.begin(),row_indices.end()),
    expected_indices(board.size);
  std::iota(expected_indices.begin(),expected_indices.end(),0);
  assert(indices==expected_indices);
}


static void testShowingASudokuBoard()
{
  Board board;
  board[0][0] = '1';

  std::ostringstream stream;

  show("board",board,stream);

  const char *expected_result =
    "board:\n"
    "  1  |   |   \n"
    "     |   |   \n"
    "     |   |   \n"
    "  ---+---+---\n"
    "     |   |   \n"
    "     |   |   \n"
    "     |   |   \n"
    "  ---+---+---\n"
    "     |   |   \n"
    "     |   |   \n"
    "     |   |   \n";

  if (stream.str()!=expected_result) {
    cerr << stream.str() << "\n";
  }

  assert(stream.str()==expected_result);
}


static Numbers boardRowNumbers(const Board &board,Index row)
{
  Numbers result;
  result.reserve(board.size);
  for (auto column : board.columnIndices()) {
    result.push_back(board[row][column]);
  }
  return result;
}


static Numbers boardColumnNumbers(const Board &board,Index column)
{
  Numbers result;
  result.reserve(board.size);
  for (auto row : board.rowIndices()) {
    result.push_back(board[row][column]);
  }
  return result;
}


static Numbers boardRegionNumbers(const Board &board,Index region)
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


static inline ostream& operator<<(ostream& stream,const Numbers &cell_values)
{
  for (auto cell_value : cell_values) {
    stream << cell_value;
  }
  return stream;
}


template <typename Func>
static void forEachEmptyCell(const Board &board,const Func &f)
{
  for (auto row : board.rowIndices()) {
    for (auto col : board.columnIndices()) {
      if (board.cellIsEmpty(row,col)) {
        f(row,col);
      }
    }
  }
}


namespace {
  struct SumConstraint {
    Area area;
    const int required_sum;
  };
}


using SumConstraints = vector<SumConstraint>;


static SumConstraints makeSumConstraints(const SumSpec &sum_spec)
{
  vector<SumConstraint> constraints;
  SumSpecAnalyzer analyzer{sum_spec};
  vector<Area> areas = analyzer.makeAreas();

  for (auto &area : areas) {
    int sum = analyzer.areaSum(area);
    constraints.push_back(SumConstraint{area,sum});
  }

  return constraints;
}


template <typename CellRangeCalculator>
static int
  areaMinimumSum(
    const Area &area,
    const CellRangeCalculator &cell_range_calculator
  )
{
  int result = 0;

  for (const IndexPair &cell : area.cell_indices) {
    result += cell_range_calculator.minimumValue(cell);
  }

  return result;
}


template <typename CellRangeCalculator>
static int
  areaMaximumSum(
    const Area &area,
    const CellRangeCalculator &cell_range_calculator
  )
{
  // Can we take advantage of the works grid to raise the lower bound of
  // the result here?

  int result = 0;

  for (const IndexPair &cell : area.cell_indices) {
    result += cell_range_calculator.maximumValue(cell);
  }

  return result;
}


template <typename CellRangeCalculator>
static bool
  sumConstraintIsSatisfied(
    const SumConstraint &constraint,
    const CellRangeCalculator &cell_range_calculator
  )
{
  int min_sum = areaMinimumSum(constraint.area,cell_range_calculator);
  int max_sum = areaMaximumSum(constraint.area,cell_range_calculator);
  int required_sum = constraint.required_sum;

  return (required_sum>=min_sum && required_sum<=max_sum);
}



template <typename CellRangeCalculator>
bool
  sumConstraintsAreSatisfied(
    const SumConstraints &sum_constraints,
    const CellRangeCalculator &cell_range_calculator
  )
{
  for (const SumConstraint &constraint : sum_constraints) {
    if (!::sumConstraintIsSatisfied(constraint,cell_range_calculator)) {
      return false;
    }
  }

  return true;
}


namespace {
struct BoardCellRangeCalculator {
  const Board &board;

  int minimumValue(const IndexPair &cell) const
  {
    if (board.cellIsEmpty(cell.row,cell.col)) {
      return 1;
    }

    return numericValue(board.cell(cell.row,cell.col));
  }

  int maximumValue(const IndexPair &cell) const
  {
    if (board.cellIsEmpty(cell.row,cell.col)) {
      return 9;
    }

    return numericValue(board.cell(cell.row,cell.col));
  }
};
}



namespace {
  struct Checker {
    const Board &board;
    bool report;

    Checker(const Board &board,bool show_violations)
    : board(board),
      report(show_violations)
    {
    }

    void showBoard() { show("failure",board,cerr); }

    bool checkNumbersAreUnique(const Numbers &cells)
    {
      vector<bool> used(board.size,false);

      for (auto i : irange(cells)) {
        Number cell = cells[i];
        if (cell!=' ') {
          int value = cell-'1';
          assert(value>=0);
          assert(value<static_cast<int>(used.size()));
          if (used[value]) {
            if (report) {
              cerr << cell << " is used more than once.\n";
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
              cerr << "row " << i+1 << " column " << j+1 << " is not filled.\n";
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
            cerr << " on row " << i+1 << "\n";
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
            cerr << " on column " << i+1 << "\n";
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
            cerr << " in region " << i+1 << "\n";
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
}


static bool
  boardSumConstraintsAreSatisfied(
    const Board &board,
    const SumConstraints &sum_constraints
  )
{
  return
    sumConstraintsAreSatisfied(
      sum_constraints,BoardCellRangeCalculator{board}
    );
}


static void testNonReportingChecker()
{
  Board board(test_board_1);
  assert(!Checker(board,/*show_violations*/false).checkIsSolved());
}


namespace {
struct Puzzle {
  virtual bool boardIsValid(const Board &baord) const = 0;
};
}


namespace {
struct StandardPuzzle : Puzzle {
  bool boardIsValid(const Board &board) const override
  {
    return Checker(board,/*show_violations*/false).checkUniqueness();
  }
};
}


static bool
  boardWithSumConstraintsIsValid(
    const Board &board,
    const SumConstraints &sum_constraints
  )
{
  Checker checker(board,/*show_violations*/false);

  if (!checker.checkUniqueness()) {
    return false;
  }

  if (!boardSumConstraintsAreSatisfied(board,sum_constraints)) {
    return false;
  }

  return true;
}


template <typename T>
static T& deref(T* ptr)
{
  assert(ptr);
  return *ptr;
}


namespace {
struct SumPuzzle : Puzzle {
  const SumConstraints &sum_constraints;

  SumPuzzle(const SumConstraints *sum_constraints_ptr)
  : sum_constraints(deref(sum_constraints_ptr))
  {
  }

  bool boardIsValid(const Board &board) const override
  {
    return boardWithSumConstraintsIsValid(board,sum_constraints);
  }
};
}


template <typename Func>
static void forEachNumber(const Func &f)
{
  for (Number value='1'; value<='9'; ++value) {
    f(value);
  }
}


template <typename BoardIsValidFunction>
static Numbers
  cellValuesThatWork(
    const Board &board,
    const BoardIsValidFunction &board_is_valid_function,
    Index row,
    Index col
  )
{
  if (!board.cellIsEmpty(row,col)) {
    return {board[row][col]};
  }

  Numbers workable;
  Board test_board = board;
  assert(test_board.cellIsEmpty(row,col));

  forEachNumber([&](Number value){
    test_board[row][col] = value;

    if (board_is_valid_function(test_board)) {
      workable.push_back(value);
    }
  });

  return workable;
}


static auto puzzleBoardIsValidFunction(const Puzzle &puzzle)
{
  return [&puzzle](const Board &board){ return puzzle.boardIsValid(board); };
}


static Numbers
  puzzleCellValuesThatWork(
    const Board &board,
    const Puzzle &puzzle,
    Index row,
    Index col
  )
{
  auto board_is_valid_function = puzzleBoardIsValidFunction(puzzle);
  return cellValuesThatWork(board,board_is_valid_function,row,col);
}


static bool isSorted(const Numbers &v)
{
  int n = v.size();
  if (v.empty()) return true;

  for (int i=1; i!=n; ++i) {
    assert(v[i-1]!=v[i]);
    if (v[i-1]>v[i]) return false;
  }

  return true;
}


static bool contains(const Numbers &one_of_these,Number v)
{
  auto b = one_of_these.begin();
  auto e = one_of_these.end();
  return std::find(b,e,v)!=e;
}


static bool contains(const vector<IndexPair> &one_of_these,const IndexPair& v)
{
  auto b = one_of_these.begin();
  auto e = one_of_these.end();
  return std::find(b,e,v)!=e;
}


template <typename Func>
static void removeIf(Numbers &values,const Func &f)
{
  values.erase(std::remove_if(values.begin(),values.end(),f),values.end());
}


static Index regionOf(IndexPair cell)
{
  return (cell.row/3)*3 + cell.col/3;
}


namespace {
  struct WorksGrid : Grid<Numbers> {
    bool check_empty = true;

    WorksGrid() : Grid<Numbers>({}) { }

    bool isMatchingPair(IndexPair cell1,IndexPair cell2) const
    {
      const Numbers &works1 = (*this)[cell1];
      const Numbers &works2 = (*this)[cell2];
      if (works1.size()!=2) return false;
      if (works2.size()!=2) return false;
      assert(isSorted(works1));
      assert(isSorted(works2));
      if (works1!=works2) return false;
      return true;
    }

    bool
      isMatchingTriple(IndexPair cell1,IndexPair cell2,IndexPair cell3) const
    {
      const Numbers &works1 = (*this)[cell1];
      const Numbers &works2 = (*this)[cell2];
      const Numbers &works3 = (*this)[cell3];
      if (works1.size()!=3) return false;
      if (works2.size()!=3) return false;
      if (works3.size()!=3) return false;
      assert(isSorted(works1));
      assert(isSorted(works2));
      assert(isSorted(works3));
      if (works1!=works2) return false;
      if (works1!=works3) return false;
      return true;
    }


    bool cellContains(IndexPair cell,Number v)
    {
      const Numbers &cell_values = (*this)[cell];
      return contains(cell_values,v);
    }

    void
      eliminateFrom(
        Numbers &values,
        const Numbers &to_remove
      )
    {
      assert(!check_empty || !values.empty());
      removeIf(values,[&](Number v){ return contains(to_remove,v); });
      assert(!check_empty || !values.empty());
    }

    bool anyEmpty() const
    {
      for (auto row : rowIndices()) {
        for (auto col : columnIndices()) {
          if ((*this)[row][col].empty()) {
            return true;
          }
        }
      }

      return false;
    }

    void eliminatePairFromColumn(IndexPair cell1,IndexPair cell2)
    {
      assert(isMatchingPair(cell1,cell2));
      assert(cell1.col==cell2.col);
      Index col = cell1.col;

      for (auto row : irange(0,size)) {
        IndexPair cell = {row,col};
        if (cell!=cell1 && cell!=cell2) {
          eliminateFrom((*this)[cell],(*this)[cell1]);
        }
      }
    }

    void eliminateUsingTriplesInRegions(bool debug = false)
    {
      Index n_cells = size;

      for (Index region : regionIndices()) {
        for (Index c1 : irange(0,n_cells)) {
          IndexPair cell1 = regionCell(region,c1);
          const Numbers &numbers1 = (*this)[cell1];
          if (numbers1.size()==3) {
            for (Index c2 : irange(c1+1,n_cells)) {
              IndexPair cell2 = regionCell(region,c2);
              const Numbers &numbers2 = (*this)[cell2];
              if (numbers2==numbers1) {
                for (Index c3 : irange(c2+1,n_cells)) {
                  IndexPair cell3 = regionCell(region,c3);
                  const Numbers &numbers3 = (*this)[cell3];
                  if (numbers3==numbers1) {
                    if (debug) {
                      cerr << "Found triple " <<
                        cell1 << "," << cell2 << "," << cell3 << "\n";
                    }
                    eliminateTripleFromRegion(cell1,cell2,cell3);
                  }
                }
              }
            }
          }
        }
      }
    }

    void eliminateUsingPairs(bool debug = false)
    {
      // Find pairs of cells in the same row, column, or region that
      // only have the same two values.  Eliminate those two values
      // from any other cells in the same row, col, or region.

      for (;;) {
        WorksGrid old_state = *this;

        for (auto row : rowIndices()) {
          forEachPairOfCellsInRow(row,
            [&](IndexPair cell1,IndexPair cell2){
              if (isMatchingPair(cell1,cell2)) {
                if (debug) {
                  cerr << "pair in " << cell1 << " and " << cell2 << "\n";
                }
                eliminatePairFromRow(cell1,cell2);
              }
            }
          );
        }

        for (auto col : columnIndices()) {
          forEachPairOfCellsInColumn(col,
            [&](IndexPair cell1,IndexPair cell2){
              if (isMatchingPair(cell1,cell2)) {
                eliminatePairFromColumn(cell1,cell2);
              }
            }
          );
        }

        for (auto region : regionIndices()) {
          forEachPairOfCellsInRegion(region,
            [&](IndexPair cell1,IndexPair cell2){
              if (isMatchingPair(cell1,cell2)) {
                eliminatePairFromRegion(cell1,cell2);
              }
            }
          );
        }

        if (*this==old_state) break;
      }
    }

    void eliminateInRow(IndexPair cell1)
    {
      const Numbers &values_to_eliminate = (*this)[cell1];
      Index row = cell1.row;

      for (Index col : irange(0,size)) {
        IndexPair cell = {row,col};
        if (cell!=cell1) {
          Numbers old_values = (*this)[cell];
          eliminateFrom((*this)[cell],values_to_eliminate);
        }
      }
    }

    void eliminateInColumn(IndexPair cell1)
    {
      const Numbers &values_to_eliminate = (*this)[cell1];
      Index col = cell1.col;

      for (auto row : rowIndices()) {
        IndexPair cell = {row,col};
        if (cell!=cell1) {
          eliminateFrom((*this)[cell],values_to_eliminate);
        }
      }
    }

    void eliminateInRegion(IndexPair cell1)
    {
      const Numbers &values_to_eliminate = (*this)[cell1];
      Index region = regionOf(cell1);

      for (Index c : irange(0,size)) {
        IndexPair cell = regionCell(region,c);
        if (cell!=cell1) {
          eliminateFrom((*this)[cell],values_to_eliminate);
        }
      }
    }

    void setCellNumber(Index row,Index col,Number v)
    {
      assert(contains(cell(row,col),v));
      setCell(row,col,{v});
      eliminateInRow({row,col});
      eliminateInColumn({row,col});
      eliminateInRegion({row,col});
    }

    void eliminatePairFromRow(IndexPair cell1,IndexPair cell2)
    {
      assert(isMatchingPair(cell1,cell2));
      assert(cell1.row==cell2.row);
      Index row = cell1.row;

      for (auto col : columnIndices()) {
        IndexPair cell = {row,col};
        if (cell!=cell1 && cell!=cell2) {
          eliminateFrom((*this)[cell],(*this)[cell1]);
        }
      }
    }

    void eliminatePairFromRegion(IndexPair cell1,IndexPair cell2)
    {
      assert(isMatchingPair(cell1,cell2));
      assert(regionOf(cell1)==regionOf(cell2));
      Index region = regionOf(cell1);

      for (Index rc : irange(0,size)) {
        IndexPair cell = regionCell(region,rc);
        if (cell!=cell1 && cell!=cell2) {
          eliminateFrom((*this)[cell],(*this)[cell1]);
        }
      }
    }

    void
      eliminateTripleFromRegion(
        IndexPair cell1,
        IndexPair cell2,
        IndexPair cell3
      )
    {
      assert(isMatchingTriple(cell1,cell2,cell3));
      Index region = regionOf(cell1);
      assert(regionOf(cell2)==region);
      assert(regionOf(cell3)==region);

      for (Index rc : irange(0,size)) {
        IndexPair cell = regionCell(region,rc);

        if (cell!=cell1 && cell!=cell2 && cell!=cell3) {
          eliminateFrom((*this)[cell],(*this)[cell1]);
        }
      }
    }

    vector<Index> rowsThatWorkFor(Number v,Index col)
    {
      vector<Index> result;

      for (auto row : rowIndices()) {
        IndexPair cell = {row,col};

        if (contains((*this)[cell],v)) {
          result.push_back(row);
        }
      }

      return result;
    }

    vector<Index> colsThatWorkFor(Number v,Index row)
    {
      vector<Index> result;

      for (auto col : rowIndices()) {
        IndexPair cell = {row,col};

        if (contains((*this)[cell],v)) {
          result.push_back(col);
        }
      }

      return result;
    }

    vector<IndexPair> cellsThatWorkFor(Number v,Index region)
    {
      vector<IndexPair> result;
      Index n_cells = size;

      for (Index cell_number : irange(0,n_cells)) {
        IndexPair cell = regionCell(region,cell_number);
        if (contains((*this)[cell],v)) {
          result.push_back(cell);
        }
      }

      return result;
    }

    void showCell(Index row,Index col) const
    {
      const Numbers &workable = (*this)[row][col];
      cerr << "row=" << row << ",col=" << col << ",works=";

      for (auto x : workable) {
        cerr << x;
      }

      cerr << "\n";
    }

    void show() const
    {
      for (auto row : rowIndices()) {
        for (auto col : columnIndices()) {
          showCell(row,col);
        }
      }
    }
  };
}


template <typename BoardIsValidFunction>
static WorksGrid
  makeWorksGrid(
    const Board &board,
    const BoardIsValidFunction &board_is_valid_function
  )
{
  WorksGrid works;

  for (auto row : board.rowIndices()) {
    for (auto col : board.columnIndices()) {
      works[row][col] =
        cellValuesThatWork(board,board_is_valid_function,row,col);
    }
  }

  return works;
}


static WorksGrid makePuzzleWorksGrid(const Board &board,const Puzzle &puzzle)
{
  return makeWorksGrid(board,puzzleBoardIsValidFunction(puzzle));
}



static void
  compareWorksGrids(const WorksGrid &temp_works,const WorksGrid &works)
{
  bool has_differences = false;

  if (temp_works!=works) {
    for (auto row : works.rowIndices()) {
      for (auto col : works.columnIndices()) {
        const Numbers temp_values = temp_works[row][col];
        const Numbers values = works[row][col];
        if (temp_values!=values) {
          cerr << "row=" << row << ", col=" << col << ", ";
          cerr << "temp_works[row][col]=" << temp_values << ", ";
          cerr << "works[row][col]=" << values << "\n";
          has_differences = true;
        }
      }
    }
  }

  if (has_differences) {
    cerr << "works:\n";
    works.show();
    cerr << "temp_works:\n";
    temp_works.show();
  }

  assert(!has_differences);
}


namespace {
struct WorkingBoard {
  Board &board;
  WorksGrid &works;
  const bool debug;

  void fillCell(Index row,Index col,Number v)
  {
    board[row][col] = v;
    works.setCellNumber(row,col,v);
  }

  void fillSinglesInRow(Index row)
  {
    forEachNumber([&](Number v){
      vector<Index> cols_that_work = works.colsThatWorkFor(v,row);

      if (cols_that_work.size()==1) {
        Index col = cols_that_work[0];
        IndexPair cell = {row,col};
        if (board.cellIsEmpty(row,col)) {
          if (debug) {
            cerr << v << " can only go in column " << col << " of row " <<
              row << "\n";
          }
          fillCell(cell.row,cell.col,v);
        }
      }
    });
  }

  void fillSinglesInColumn(Index col)
  {
    forEachNumber([&](Number v){
      vector<Index> rows_that_work = works.rowsThatWorkFor(v,col);

      if (rows_that_work.size()==1) {
        Index row = rows_that_work[0];
        IndexPair cell = {row,col};
        if (board.cellIsEmpty(row,col)) {
          if (debug) {
            cerr << v << " can only be in row " << row << " of column " <<
              col << "\n";
          }
          board[cell] = v;
          works[cell] = {v};
          works.eliminateInRow(cell);
          works.eliminateInRegion(cell);
        }
      }
    });
  }

  void fillSinglesInRegion(Index region)
  {
    forEachNumber([&](Number v){
      vector<IndexPair> cells_that_work = works.cellsThatWorkFor(v,region);

      if (cells_that_work.size()==1) {
        IndexPair cell = cells_that_work[0];
        Index row = cell.row;
        Index col = cell.col;

        if (board.cellIsEmpty(row,col)) {
          if (debug) {
            cerr << v << " can only be in cell " << cell <<
              " in region " << region << "\n";
          }
          board[cell] = v;
          works[cell] = {v};
          works.eliminateInRow(cell);
          works.eliminateInColumn(cell);
        }
      }
    });
  }

  void fillSinglesInCell(Index row,Index col)
  {
    assert(board.cellIsEmpty(row,col));
    Numbers &values_that_work = works[row][col];
    if (values_that_work.size()==1) {
      Number v = values_that_work[0];
      if (debug) {
        cerr << v << " is the only value that can go in cell " <<
          row << "," << col << "\n";
      }
      board[row][col] = v;
      works.eliminateInRow({row,col});
      works.eliminateInColumn({row,col});
      works.eliminateInRegion({row,col});
    }
  }

  void fillRowSingles()
  {
    for (auto row : board.rowIndices()) {
      fillSinglesInRow(row);
    }
  }

  void fillColumnSingles()
  {
    for (auto col : board.columnIndices()) {
      fillSinglesInColumn(col);
    }
  }

  void fillRegionSingles()
  {
    for (auto region : board.regionIndices()) {
      fillSinglesInRegion(region);
    }
  }

  void fillCellSingles()
  {
    for (auto row : board.rowIndices()) {
      for (auto col : board.columnIndices()) {
        if (board.cellIsEmpty(row,col)) {
          fillSinglesInCell(row,col);
        }
      }
    }
  }

  void fillSingles()
  {
    fillRowSingles();
    fillColumnSingles();
    fillRegionSingles();
    fillCellSingles();
  }
};
}


namespace {
  struct WorksSolver {
    Board &board;
    const Puzzle &puzzle;
    WorksGrid works;
    bool debug;

    WorksSolver(Board &,const Puzzle *,bool debug_arg = false);

    void shallowSolve();
    bool tryNumber(Index row,Index col,Number v);
    void deepSolve();
    void showBoard() { show("board",board,cerr); }
    void handlePairs(bool debug = false) { works.eliminateUsingPairs(debug); }

    bool isMatchingPair(const IndexPair cell1,const IndexPair cell2) const
    {
      return works.isMatchingPair(cell1,cell2);
    }

    WorkingBoard workingBoard()
    {
      return WorkingBoard{board,works,debug};
    }

    void fillRowSingles()
    {
      workingBoard().fillRowSingles();
    }

    void fillColumnSingles()
    {
      workingBoard().fillColumnSingles();
    }

    void fillCellSingles()
    {
      workingBoard().fillCellSingles();
    }

    void fillCell(Index row,Index col,Number v)
    {
      workingBoard().fillCell(row,col,v);
    }

    void check()
    {
      WorksGrid temp_works = makePuzzleWorksGrid(board,puzzle);
      temp_works.check_empty = works.check_empty;
      temp_works.eliminateUsingPairs();
      compareWorksGrids(temp_works,works);
    }
  };
}


WorksSolver::WorksSolver(
  Board &board_arg,
  const Puzzle *puzzle_ptr,
  bool debug_arg
)
: board(board_arg),
  puzzle(deref(puzzle_ptr)),
  works(makePuzzleWorksGrid(board_arg,deref(puzzle_ptr))),
  debug(debug_arg)
{
}


void WorksSolver::shallowSolve()
{
  for (int pass_number=1;;++pass_number) {
    if (debug) {
      cerr << "Pass " << pass_number << ":\n";
    }

    WorksGrid old_works = works;

    for (;;) {
      Board old_board = board;
      workingBoard().fillSingles();

      if (board==old_board) {
        break;
      }
    }

    handlePairs();

    if (works.check_empty) {
      check();
    }

    if (works==old_works) {
      break;
    }
  }
}


bool WorksSolver::tryNumber(Index row,Index col,Number v)
{
  Board old_board = board;
  works.check_empty = false;
  fillCell(row,col,v);
  shallowSolve();
  bool is_valid = !works.anyEmpty();
  board = old_board;
  works = makePuzzleWorksGrid(board,puzzle);
  assert(works.check_empty);
  works.eliminateUsingPairs();
  check();
  return is_valid;
}


void WorksSolver::deepSolve()
{
  shallowSolve();

  forEachEmptyCell(board,[&](Index row,Index col){
    Numbers valid_values = works[row][col];
    removeIf(valid_values,[&](Number v){return !tryNumber(row,col,v);});

    if (valid_values.size()==1) {
      Number v = valid_values[0];
      fillCell(row,col,v);
      shallowSolve();
    }
  });
}


static void solve(Board &board)
{
  StandardPuzzle puzzle;
  WorksSolver solver(board,&puzzle);
  solver.deepSolve();
}


static IndexPair multiValuedCell(const Area &area,const WorksGrid &works)
{
  for (IndexPair cell : area.cell_indices) {
    if (works[cell].size()!=1) {
      return cell;
    }
  }

  assert(false);

  return {0,0};
}


static bool
  sumConstraintIsSatisfiedByWorks(
    const SumConstraint &constraint,
    const WorksGrid &works
  )
{
  // First, subtract the value of any cells that have only a single
  // possible value.
  int required_sum = constraint.required_sum;
  int n_multi_valued_cells = 0;

  for (IndexPair cell : constraint.area.cell_indices) {
    if (works[cell].size()==1) {
      required_sum -= numericValue(works[cell].front());
    }
    else {
      ++n_multi_valued_cells;
    }
  }

  if (n_multi_valued_cells==0) {
    return required_sum==0;
  }

  if (n_multi_valued_cells==1) {
    // If only a single cell remains, then see if one of its values
    // matches our expected sum.
    IndexPair cell = multiValuedCell(constraint.area,works);

    for (Number n : works[cell]) {
      if (numericValue(n)==required_sum) {
        return true;
      }
    }

    return false;
  }

  if (n_multi_valued_cells==required_sum) {
    // There is more than 1 multi-valued cell, and they all must have
    // a value of 1.  If the multi-valued cells are all in the same
    // region, then this isn't valid.
    Index unset_region(-1);
    Index common_region = unset_region;
    bool in_common_region = true;

    for (IndexPair cell : constraint.area.cell_indices) {
      if (works[cell].size()!=1) {
        assert(works[cell].size()>1);
        Index cell_region = regionOf(cell);

        if (common_region==unset_region) {
          common_region = cell_region;
        }
        else {
          if (cell_region!=common_region) {
            in_common_region = false;
          }
        }
      }
    }

    if (in_common_region) {
      return false;
    }
  }

  int min = 0;
  int max = 0;

  // If multiple cells remain, get the minimum and maximum values
  // and see if our expected sum falls within that range.
  for (IndexPair cell : constraint.area.cell_indices) {
    if (works[cell].size()!=1) {
      assert(isSorted(works[cell]));
      min += numericValue(works[cell].front());
      max += numericValue(works[cell].back());
    }
  }

  return (required_sum>=min && required_sum<=max);
}


static bool
  sumConstraintsAreSatisfiedByWorks(
    const SumConstraints &constraints,
    const WorksGrid &works
  )
{
  for (auto &constraint : constraints) {
    if (!sumConstraintIsSatisfiedByWorks(constraint,works)) {
      return false;
    }
  }

  return true;
}


static bool areaContains(const Area &area,const IndexPair &cell_index)
{
  return contains(area.cell_indices,cell_index);
}


static SumConstraint
  sumConstraintContainingCell(
    const SumConstraints &sum_constraints,
    const IndexPair &cell_index
  )
{
  auto contains_cell_function =
    [&](const SumConstraint &constraint){
      return areaContains(constraint.area,cell_index);
    };
  return elementWhere(sum_constraints,contains_cell_function);
}


template <typename CellIndexFunction>
static bool
  allCellsInAreaHaveACommonIndex(
    const Area &area,const CellIndexFunction &cell_index_function
  )
{
  const Index no_index = -1;
  Index common_index = no_index;

  for (const IndexPair &cell : area.cell_indices) {
    Index index = cell_index_function(cell);

    if (common_index==no_index) {
      common_index = index;
    }
    else {
      if (common_index!=index) {
        return false;
      }
    }
  }

  return true;
}


static bool allCellsInAreaAreInTheSameRegion(const Area &area)
{
  auto region_index_function =
    [](const IndexPair &cell){ return regionOf(cell); };
  return allCellsInAreaHaveACommonIndex(area,region_index_function);
}


static bool allCellsInAreaAreInTheSameRow(const Area &area)
{
  auto row_index_function =
    [](const IndexPair &cell){ return cell.row; };
  return allCellsInAreaHaveACommonIndex(area,row_index_function);
}


static bool allCellsInAreaAreInTheSameColumn(const Area &area)
{
  auto column_index_function =
    [](const IndexPair &cell){ return cell.col; };
  return allCellsInAreaHaveACommonIndex(area,column_index_function);
}


static bool cellNumbersMustBeDistinctInArea(const Area &area)
{
  return
    allCellsInAreaAreInTheSameRegion(area) ||
    allCellsInAreaAreInTheSameRow(area) ||
    allCellsInAreaAreInTheSameColumn(area);
}


static Numbers
  numbersWithout(const Numbers &input_numbers,Number number_to_remove)
{
  Numbers result = input_numbers;
  removeIf(result,[&](Number n){ return number_to_remove==n; });
  return result;
}


static bool
  sumCanBeSatisifiedWithNumber(
    int required_sum,
    int n_cells,
    const Numbers &possible_numbers,
    const Number number
  )
{
  assert(required_sum>=0);

  int i = numericValue(number);

  if (i>required_sum) {
    return false;
  }

  int remaining_sum = required_sum - i;
  int remaining_cells = n_cells - 1;

  if (remaining_cells==0) {
    return remaining_sum==0;
  }

  for (Number number : possible_numbers) {
    bool can_be_satisified_with_number =
      sumCanBeSatisifiedWithNumber(
        remaining_sum,remaining_cells,possible_numbers,number
      );

    if (can_be_satisified_with_number) {
      return true;
    }
  }

  return false;
}


static bool
  distinctSumCanBeSatisifiedWithNumber(
    int required_sum,
    int n_cells,
    const Numbers &possible_numbers,
    const Number number
  )
{
  assert(required_sum>=0);

  int i = numericValue(number);

  if (i>required_sum) {
    return false;
  }

  Numbers remaining_numbers = numbersWithout(possible_numbers,number);
  int remaining_sum = required_sum - i;
  int remaining_cells = n_cells - 1;

  if (remaining_cells==0) {
    return remaining_sum==0;
  }

  for (Number number : remaining_numbers) {
    bool can_be_satisified_with_number =
      distinctSumCanBeSatisifiedWithNumber(
        remaining_sum,remaining_cells,remaining_numbers,number
      );

    if (can_be_satisified_with_number) {
      return true;
    }
  }

  return false;
}


static Numbers allNumbers()
{
  Numbers result;
  forEachNumber([&](Number n){ result.push_back(n); });
  return result;
}


static Numbers
  sumNumbers(
    int n_cells,
    int required_sum,
    const Numbers &possible_numbers
  )
{
  Numbers result;

  for (Number number : possible_numbers) {
    bool can_be_satisified_with_number =
      sumCanBeSatisifiedWithNumber(
        required_sum,
        n_cells,
        possible_numbers,
        number
      );

    if (can_be_satisified_with_number) {
      result.push_back(number);
    }
  }

  return result;
}


static Numbers
  distinctSumNumbers(
    int n_cells,
    int required_sum,
    const Numbers &possible_numbers
  )
{
  Numbers result;

  for (Number number : possible_numbers) {
    bool can_be_satisified_with_number =
      distinctSumCanBeSatisifiedWithNumber(
        required_sum,
        n_cells,
        possible_numbers,
        number
      );

    if (can_be_satisified_with_number) {
      result.push_back(number);
    }
  }

  return result;
}


static Numbers
  possibleNumbersInSumConstraint(const SumConstraint &constraint)
{
  Numbers possible_numbers = allNumbers();
  int n_cells = constraint.area.cell_indices.size();

  if (cellNumbersMustBeDistinctInArea(constraint.area)) {
    return
      distinctSumNumbers(n_cells,constraint.required_sum,possible_numbers);
  }
  else {
    return sumNumbers(n_cells,constraint.required_sum,possible_numbers);
  }
}


static WorksGrid
  reducedWorksBySums(
    const Board &board,
    const WorksGrid &works,
    const SumConstraints &sum_constraints
  )
{
  WorksGrid new_works;

  for (auto row : board.rowIndices()) {
    for (auto col : board.columnIndices()) {
      const Numbers &numbers = works[row][col];
      const SumConstraint &sum_constraint =
        sumConstraintContainingCell(sum_constraints,{row,col});
      assert(numbers.size()>=1);
      Numbers possible_numbers =
        possibleNumbersInSumConstraint(sum_constraint);

      if (numbers.size()>=2) {
        Numbers new_numbers;

        for (Number n : numbers) {
          if (contains(possible_numbers,n)) {
            WorksGrid new_works = works;
            new_works.check_empty = false;

            new_works.setCellNumber(row,col,n);

            if (!new_works.anyEmpty()) {
              if (sumConstraintIsSatisfiedByWorks(sum_constraint,new_works)) {
                new_numbers.push_back(n);
              }
            }
          }
        }
        assert(!new_numbers.empty());
        new_works[row][col] = new_numbers;
      }
      else {
        assert(numbers.size()==1);
        new_works[row][col] = numbers;
      }
      assert(!new_works[row][col].value().empty());
    }
  }

  return new_works;
}



#if 0
static bool deepBoardIsValid(Board &board,const Puzzle &puzzle,int depth)
{
  if (depth==0) {
    return puzzle.boardIsValid(board);
  }

  forEachEmptyCell(board,[](IndexPair cell){
    Numbers valid_numbers;

    forEachNumber([](Number n){
      board[cell] = n;
      bool is_valid = deepBoardIsValid(board,puzzle,depth-1);
      board[cell] = ' ';
      if (is_valid) {
        valid_numbers.push_back(n);
      }
    }

    if (valid_numbers.empty()) {
      return false;
    }

    if (valid_numbers.size()==1) {
      board[cell] = valid_numbers.front();
      bool is_valid = deepBoardIsValid(board,puzzle,depth);
      board[cell] = ' ';

      if (!is_valid) {
        return false;
      }
    }
  });

  return true;
}
#endif


#if 0
static bool // return true if we can prove it, return false if not.
  proveNumberCannotGoInCell(
    const Puzzle &,
    Board &,
    IndexPair /*cell*/,
    Number,
    int /*proof_size_limit*/
  )
{
  Index row = cell.row;
  Index col = cell.col;

  // We should never call this function if a cell is already filled,
  // because to fill that cell, we must have already proven that no
  // other number can go there, so anything else we might say about that
  // cell would be redundant.
  assert(board[cell].isEmpty());

  board[cell] = number;
  ValidResult result = proveBoardIsUnsolvable(board,puzzle,proof_size_limit);
  board[cell].setEmpty();
  return result.is_valid;
}
#endif


#if 0
static bool findGroupPositiveProof(group,proofs)
{
  // See if a group has only one unproven member
  // for each member in group {
  //   if !proofs[member] {
  //     if (unproven_member) {
  //       // Multiple unproven members
  //       return false
  //     }
  //     unproven_member = member
  //   }
  // }
  // if (unproven_member) {
  //   // There was only a single unproven member
  //   board[unproven_member.cell] = unproven_member.number
  //   proof = sum{member in group}(proofs[member])
  //   return proof
  // }
}
#endif


#if 0
static bool findAnyPositiveProof()
{
  // We try to make some forward progress, finding a proof that a certain
  // number must go in a certain cell.

  // Find negative proofs for each cell and number

  // for each cell {
  //   for each number {
  //     proofs[cell][number] = findNegativeProof(cell,number)
  //   }
  // }

  // See if each row,col has only one unproven number
  // for each cell {
  //   group = []
  //   for each number {
  //     group.push_back({cell,number})
  //   }
  //   if (findProof(group)) return proof
  // }
  //
  // See if each row,number has only one unproven col
  // for each row {
  //   for each number {
  //     group = []
  //     for each col {
  //       group.push_back({{row,col},number})
  //     }
  //     if (findProof(group)) return proof
  //   }
  // }
  //
  // See if each col,number has only one unproven row
  // for each col {
  //   for each number {
  //     group = []
  //     for each row {
  //       group.push_back({{row,col},number})
  //     }
  //     if (findProof(group)) return proof
  //   }
  // }
  //
  // See if each region,number has only one unproven row
  // for each region {
  //   for each number {
  //     group = []
  //     for each region_cell(region) {
  //       group.push_back({region_cell(region),number})
  //     }
  //     if (findProof(group)) return proof
  //   }
  // }
}
#endif


#if 0
static void
  runDeepeningSolver(const Puzzle &/*puzzle*/,Board &/*board*/)
{
  // We solve by finding cells we can fill one at a time.
  // For each cell we fill, we try to find the minimum proof that
  // the assignment is correct.

  int max_proof_length = 1;

  for (;;) {
    PositiveProofFinderResult result = findPositiveProof(max_proof_length)

    if (result.proof_was_found) {
      board[result.found_cell] = result.found_number;
      full_proof += result.proof;
      max_proof_length = 1;
    }
    else {
      ++max_proof_length;
    }
  }

  // It seems clear that we should go through each cell, see which
  // numbers work, and if there is only one number that works fill it in.

  // If there are no cells with a single option, do we then try the
  // cells with two options?

  // It seems like the main thing we are trying to do is prove that
  // a board isn't valid.  To do that, we have to find a cell that
  // has no valid options.

  // So we can do a level-1 check on each cell.  If a level-1 check
  // doesn't fill any cells, then we could look for cells with two
  // level-1 possibilities, and see how many level-2 possibilities it has.

  // The main thing is we have a trade-off between going deeper on one
  // cell or trying more cells.

  // If a board is valid, then we're going to have to go through
  // every cell anyway.  If the board isn't valid then it would be
  // best to find that quickly.

  // The top-level knowledge is the most valuable.
  // Which cells are filled.
  // The possible values for the cells.
  // Can we remember anything else?
  // The possible values for other cells if we use a particular number
  // for a cell?

  // It would be nice to have output like this:
  // Cell 0,0 can only be 4:
  //   Cell 0,0 can't be 2 because 2 is already in row 0
  //   Cell 0,0 can't be 3 because if we make it 3 then
  //    Cell 0,1 can't be 1 because there is already a 1 in row 0
  //    Cell 0,1 can't be 2 because there is already a 2 in row 0
  //    Cell 0,1 can't be 3 because there is already a 3 in column 0
  //    Cell 0,1 can't be 4 because if we made it 4 then
  //      ...
  //    ...
  //   Cell 0,0 can't be 5 because if we make it 5 then
  //    ...
  //   ...

  // In other words, the output would give us a minimal proof of the solution.
  // We find a cell which has the smallest proof that is is a certain number
  // and fill it.
  // That means we are looking for proofs of a certain length.
  // We try to find proofs that are length n, and if that doesn't work,
  // then we look for proofs of length n+1.
  // The elements of the proof are statements of conflicts, which implies
  // that the length of the proof is the number of conflicts.  That
  // means that we can count the number of conflicts to determine when to
  // stop.

  // So we go through each cell and see if we can prove it must have
  // a certain number in n steps.  If not, we try the next cell.
  // Also, we could try rows, columns, and regions.
  //   1 can only go in column 5 of row 0
  //     1 can't go in column 0 of row 0:
  //       ...
  //     1 can't go in column 1 of row 0:
  //       ...
  //     ...

  // We go to a cell and see if we can prove it can't be a certain number
  // in n steps.  If we prove it in k steps, then we reduce n by k.
  // If we eliminate all but one before n gets to 0, then we've proven it;
  // we fill and continue.  If n gets to 0, then we reset n and go to the next
  // possibility.
  // For each number we try, if there isn't an immediate conflict, then
  // we go deeper.
  // As soon as we find two numbers that we can't prove are invalid, then we
  // don't need to try this cell any more, since it isn't going to
  // contribute to the proof.

  // This approach seems right.  We are avoiding investigating anything
  // too deeply.  We stop going too deep because the deeper we go, the
  // more conflicts we need to make the proof.  We avoid trying too many
  // options at any particular depth because if we don't look very
  // deeply, then we easily find more than one option for a cell, which
  // lets us avoid looking any further.  So our depth limit is helping
  // us in two different ways.

  // Increasing the depth limit by 1 each time probably won't be efficient
  // enough, but we can try doubling it.  We won't get a minimal proof,
  // but it should then be within a factor of 2 of minimal (maybe?).
}
#endif


#if 0
static void testDeepeningSolver()
{
  Board board(test_board_1);
  StandardPuzzle puzzle;
  runDeepeningSolver(puzzle,board);
  Checker checker(board,/*show_violations*/false);
  assert(checker.checkIsSolved());
}
#endif


#if 0
static void
  solveWithSumConstraints(
    Board &board,
    const SumConstraints &sum_constraints,
    bool debug
  )
{
  SumPuzzle puzzle(&sum_constraints);
  WorksGrid works = makePuzzleWorksGrid(board,puzzle);
  works = reducedWorksBySums(board,works,sum_constraints);
  WorkingBoard working_board{board,works,debug};
  working_board.fillSingles();
  works = reducedWorksBySums(board,works,sum_constraints);
  working_board.fillSingles();
  works = reducedWorksBySums(board,works,sum_constraints);
  working_board.fillSingles();
  works.eliminateUsingTriplesInRegions();
  working_board.fillSingles();
  working_board.fillSingles();
  cerr << "---\n";
  works = reducedWorksBySums(board,works,sum_constraints);
  working_board.fillSingles();
  cerr << "---\n";
  works = reducedWorksBySums(board,works,sum_constraints);
  working_board.fillSingles();
  cerr << "---\n";
  works.eliminateUsingPairs();
  works = reducedWorksBySums(board,works,sum_constraints);
  working_board.fillSingles();
#if 0
  // works.show();
  // working_board.fillSingles();
  // cerr << "---\n";
#endif

  show("board",board,cerr);
  works.show();
}
#endif


static void testUniqueness()
{
  Board board(test_board_1);
  board[0][2] = '1';
  assert(!StandardPuzzle().boardIsValid(board));
}


static void showValid(const Board &board,const Puzzle &puzzle)
{
  forEachEmptyCell(board,[&](Index row,Index col) {
    Numbers workable = puzzleCellValuesThatWork(board,puzzle,row,col);
    cerr << "row=" << row << ",col=" << col << ",works=";
    cerr << workable << "\n";
  });
}


static void testSolvingAPuzzle(const BoardState &test_board,bool show_it)
{
  Board board(test_board);

  if (show_it) {
    show("initial",board,cout);
  }

  solve(board);
  bool solved = Checker(board,/*show_violations*/true).checkIsSolved();

  if (!solved) {
    showValid(board,StandardPuzzle());
    assert(solved);
  }

  if (show_it) {
    show("solution",board,cout);
  }
}


#if 0
static void
  testSolvingASumPuzzle(
    const BoardState &test_board,
    const SumSpec &sum_spec,
    bool /*show_it*/
  )
{
  Board board(test_board);
  SumConstraints sum_constraints = makeSumConstraints(sum_spec);

  solveWithSumConstraints(board,sum_constraints,/*debug*/true);

#if 0
  Checker checker(board,/*show_violations*/true);
  bool solved = checker.checkIsSolved();

  if (!solved) {
    assert(false);
  }

  if (!checker.sumConstraintsAreSatisfied(sum_constraints)) {
    assert(false);
  }

  if (show_it) {
    show("solution",board,cout);
  }
#endif
}
#endif


#if 0
static void
  testSolvingASumPuzzle2(const BoardState &test_board,const SumSpec &sum_spec)
{
  Board board(test_board);
  SumConstraints sum_constraints = makeSumConstraints(sum_spec);
  SumPuzzle puzzle(&sum_constraints);
  WorksSolver solver(board,&puzzle,/*debug*/true);
  solver.deepSolve();
  show("result",board,cerr);
}
#endif


static void testIsSorted()
{
  assert(isSorted({'1','2'}));
  assert(!isSorted({'2','1'}));
}


static void testRemoveIf()
{
  Numbers values = {'1','2','3'};
  removeIf(values,[](Number c){ return c=='2'; });
  Numbers expected = {'1','3'};
  assert(values==expected);
}


static void testRegionOf()
{
  {
    IndexPair cell = {0,0};
    assert(regionOf(cell)==0);
  }
  {
    IndexPair cell = {0,3};
    assert(regionOf(cell)==1);
  }
  {
    IndexPair cell = {3,0};
    assert(regionOf(cell)==3);
  }
}


static void testRegionCell()
{
  {
    IndexPair cell = regionCell(0,0);
    IndexPair expected = {0,0};
    assert(cell==expected);
  }
  {
    IndexPair cell = regionCell(0,1);
    IndexPair expected = {0,1};
    assert(cell==expected);
  }
  {
    IndexPair cell = regionCell(0,3);
    IndexPair expected = {1,0};
    assert(cell==expected);
  }
  {
    IndexPair cell = regionCell(1,0);
    IndexPair expected = {0,3};
    assert(cell==expected);
  }
  {
    IndexPair cell = regionCell(2,0);
    IndexPair expected = {0,6};
    assert(cell==expected);
  }
  {
    IndexPair cell = regionCell(3,0);
    IndexPair expected = {3,0};
    assert(cell==expected);
  }
}


static void testContains()
{
  Numbers values = {'1','3','5'};
  assert(contains(values,'1'));
  assert(!contains(values,'2'));
}


static void testWorksGrid()
{
  Board board = test_board_4;
  WorksGrid works_grid = makePuzzleWorksGrid(board,StandardPuzzle());
  {
    IndexPair cell = {0,5};
    Numbers works = works_grid[cell];
    Numbers expected = {'6','7'};
    assert(works==expected);
  }
  assert(works_grid.isMatchingPair({0,5},{1,5}));
  works_grid.eliminatePairFromColumn({0,5},{1,5});
  {
    IndexPair cell = {5,5};
    Numbers expected = {'4','9'};
    Numbers actual = works_grid[cell];
    assert(expected==actual);
  }
  works_grid.eliminatePairFromRow({7,1},{7,2});
  {
    IndexPair cell = {7,7};
    Numbers expected = {'3','5','6'};
    Numbers actual = works_grid[cell];
    assert(expected==actual);
  }
  works_grid.eliminatePairFromRegion({0,5},{1,5});
  {
    IndexPair cell = {0,4};
    assert(!contains(works_grid[cell],'6'));
  }
  assert(works_grid.rowsThatWorkFor('6',4)[0]==3);
  {
    IndexPair cell = {3,4};
    works_grid[cell] = {'6'};
    IndexPair test_cell = {3,1};
    assert(contains(works_grid[test_cell],'6'));
    works_grid.eliminateInRow(cell);
    assert(!contains(works_grid[test_cell],'6'));
  }
}


static void testWorksSolver()
{
  Board board = test_board_4;
  StandardPuzzle puzzle;
  WorksSolver solver(board,&puzzle);
  solver.handlePairs();
  {
    IndexPair cell = {4,5};
    assert(!contains(solver.works[cell],'7'));
  }
  {
    IndexPair cell = {7,7};
    assert(!contains(solver.works[cell],'9'));
  }
  {
    IndexPair cell = {0,4};
    assert(!contains(solver.works[cell],'6'));
  }
  assert(solver.isMatchingPair({0,5},{1,5}));
  solver.fillColumnSingles();
  solver.fillRowSingles();
  assert(!contains(solver.works[0][0],'6'));
}


static void testShallowSolve()
{
  const BoardState board_state = {
    "         ",
    "   7     ",
    "6 2    19",
    "  3      ",
    "  8 9   6",
    "7 9  1 83",
    " 4 1 7   ",
    " 369 5  1",
    "1 7  3592"
  };

  Board board = board_state;
  StandardPuzzle puzzle;
  WorksSolver solver(board,&puzzle);
  solver.shallowSolve();
  solver.check();
}


static void testHandlingPairs()
{
  const BoardState board_state = {
    "        7",
    "   7     ",
    "6 2    19",
    "  3 7 9  ",
    "  839 1 6",
    "7 9  1 83",
    "9451 7  8",
    " 369 5  1",
    "1 7  3592",
  };

  Board board = board_state;
  int row = 8;
  int col = 1;
  Number v = '8';
  StandardPuzzle puzzle;
  WorksGrid works = makePuzzleWorksGrid(board,puzzle);
  works.eliminateUsingPairs();
  board[row][col] = v;
  works.eliminateInRow({row,col});
  works.eliminateInColumn({row,col});
  works.eliminateInRegion({row,col});
  works.eliminateUsingPairs();
  WorksGrid temp_works = makePuzzleWorksGrid(board,puzzle);
  temp_works.eliminateUsingPairs();
  assert(temp_works==works);
}


static void testIndexPairUnionFind()
{
  IndexPairUnionFind union_find;

  IndexPair cell1 = {0,0};
  IndexPair cell2 = {1,0};
  assert(union_find.rootOf(cell1)==union_find.rootOf(cell1));
  assert(union_find.rootOf(cell1)!=union_find.rootOf(cell2));
  union_find.join(cell1,cell2);
  assert(union_find.rootOf(cell1)==union_find.rootOf(cell2));
}


static int totalCells(const vector<Area> &areas)
{
  int total = 0;

  for (const Area &area : areas) {
    total += area.cell_indices.size();
  }

  return total;
}


static const Area&
  areaContaining(const vector<Area> &areas,const IndexPair &cell_index)
{
  auto contains_cell_function =
    [&](const Area &area){ return areaContains(area,cell_index); };

  return elementWhere(areas,contains_cell_function);
}


static void testSumSpecAnalyzer()
{
  SumSpec sum_spec = {
    "+-+-+-+-+-+-+-+-+-+",
    "|16     |25 |2|21 |",
    "+-+-+-+-+   +5+ + +",
    "|25     |   | |   |",
    "+-+-+-+-+-+ + +-+ +",
    "|3|12 |13 | | |1| |",
    "+ +-+-+ +-+-+ +4+-+",
    "| |16 | |1|1| |   |",
    "+-+-+ +-+4+1+-+-+-+",
    "|13 | |9| | |5|15 |",
    "+-+-+-+ + +-+ +-+-+",
    "|14 |1| | | |   |1|",
    "+-+ +1+-+-+ +-+-+6+",
    "|3| | |3|11 |13 | |",
    "+5+-+ +5+-+-+-+-+-+",
    "|   | |   |14     |",
    "+   + +   +-+-+-+-+",
    "|   | |   |18     |",
    "+-+-+-+-+-+-+-+-+-+",
  };

#if 0
  // This demonstrates how there are 25 areas.
  SumSpec sum_spec = {
    "+-+-+-+-+-+-+-+-+-+",
    "|1111111|222|3|444|",
    "+-+-+-+-+222+3+444+",
    "|5555555|222|3|444|",
    "+-+-+-+-+-+2+3+-+4+",
    "|6|777|888|2|3|9|4|",
    "+6+-+-+8+-+-+3+9+-+",
    "|6|000|8|1|2|3|999|",
    "+-+-+0+-+1+2+-+-+-+",
    "|333|0|4|1|2|5|666|",
    "+-+-+-+4+1+-+5+-+-+",
    "|777|8|4|1|9|555|0|",
    "+-+7+8+-+-+9+-+-+0+",
    "|1|7|8|2|999|333|0|",
    "+1+-+8+2+-+-+-+-+-+",
    "|111|8|222|4444444|",
    "+111+8+222+-+-+-+-+",
    "|111|8|222|5555555|",
    "+-+-+-+-+-+-+-+-+-+",
  };
#endif

  SumSpecAnalyzer analyzer{sum_spec};
  assert(!analyzer.cellIsConnectedToPrevRow(IndexPair{1,0}));
  assert( analyzer.cellIsConnectedToPrevRow(IndexPair{1,4}));
  assert(!analyzer.cellIsConnectedToPrevCol(IndexPair{0,4}));
  assert( analyzer.cellIsConnectedToPrevCol(IndexPair{0,1}));

  {
    vector<Area> areas = analyzer.makeAreas();
    assert(areas.size()==25);
    assert(totalCells(areas)==9*9);
    assert(analyzer.areaSum(areaContaining(areas,IndexPair{0,0}))==16);
    assert(analyzer.areaSum(areaContaining(areas,IndexPair{0,6}))==25);
    assert(analyzer.areaSum(areaContaining(areas,IndexPair{2,0}))==3);
  }
}


static void testMakeSumConstraints()
{
  SumSpec sum_spec = {
    "+-+-+-+-+-+-+-+-+-+",
    "|16     |25 |2|21 |",
    "+-+-+-+-+   +5+ + +",
    "|25     |   | |   |",
    "+-+-+-+-+-+ + +-+ +",
    "|3|12 |13 | | |1| |",
    "+ +-+-+ +-+-+ +4+-+",
    "| |16 | |1|1| |   |",
    "+-+-+ +-+4+1+-+-+-+",
    "|13 | |9| | |5|15 |",
    "+-+-+-+ + +-+ +-+-+",
    "|14 |1| | |1|   |1|",
    "+-+ +1+-+-+1+-+-+6+",
    "|3| | |3|   |13 | |",
    "+5+-+ +5+-+-+-+-+-+",
    "|   | |   |14     |",
    "+   + +   +-+-+-+-+",
    "|   | |   |18     |",
    "+-+-+-+-+-+-+-+-+-+",
  };

  SumConstraints result = makeSumConstraints(sum_spec);

  assert(result.size()==25);
  assert(result.back().required_sum==18);
}


static void testCheckerWithSums()
{

  {
    const BoardState board_state = {
      "         ",
      "         ",
      "         ",
      "         ",
      "         ",
      "         ",
      "         ",
      "         ",
      "         ",
    };

    Board board(board_state);
    SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
    bool sums_are_satisified =
      boardSumConstraintsAreSatisfied(board,sum_constraints);
    assert(sums_are_satisified);
  }

  {
    const BoardState board_state = {
      "         ",
      "         ",
      "3        ",
      "1        ",
      "         ",
      "         ",
      "         ",
      "         ",
      "         ",
    };

    Board board(board_state);
    SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
    bool sums_are_satisified =
      boardSumConstraintsAreSatisfied(board,sum_constraints);
    assert(!sums_are_satisified);
  }

  {
    const BoardState board_state = {
      "         ",
      "         ",
      "1        ",
      "1        ",
      "         ",
      "         ",
      "         ",
      "         ",
      "         ",
    };

    Board board(board_state);
    SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
    bool sums_are_satisified =
      boardSumConstraintsAreSatisfied(board,sum_constraints);
    assert(!sums_are_satisified);
  }
}


static void testCellNumbersMustBeDistinctInArea()
{
  const SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);

  struct {
    const IndexPair cell;
    const bool expected_result;
  } const table[] = {
    {{6,0}, true},
    {{5,0}, false},
    {{0,0}, true},
    {{5,8}, true}
  };

  for (auto &entry : table) {
    const SumConstraint &constraint =
      sumConstraintContainingCell(sum_constraints,entry.cell);
    bool result = cellNumbersMustBeDistinctInArea(constraint.area);
    assert(result==entry.expected_result);
  }
}


static Numbers makeNumbers(const string &numbers_string)
{
  return vector<Number>(numbers_string.begin(),numbers_string.end());
}


static void testDistinctCellNumbers()
{
  Numbers result =
    distinctSumNumbers(
      /*n_cells*/4,
      /*required_sum*/16,
      /*possible_numbers*/allNumbers()
    );
  Numbers expected_result = makeNumbers("123456789");
  assert(result==expected_result);
}


static void testPossibleNumbersInSumConstraint()
{
  const Board board(test_board_9);
  const SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
  {
    // This is a case where the numbers have to be distinct
    const SumConstraint &sum_constraint =
      sumConstraintContainingCell(sum_constraints,{2,0});
    const Numbers numbers = possibleNumbersInSumConstraint(sum_constraint);
    const Numbers expected_numbers = makeNumbers("12");
    assert(numbers==expected_numbers);
  }
  {
    // This is a case where the numbers don't have to be distinct.
    const SumConstraint &sum_constraint =
      sumConstraintContainingCell(sum_constraints,{2,3});
    const Numbers numbers =
      possibleNumbersInSumConstraint(sum_constraint);
    const Numbers expected_numbers = makeNumbers("123456789");
    assert(numbers==expected_numbers);
  }
}


static void testSumConstraintIsSatisifiedByWorks()
{
  const Board board(test_board_9);
  const SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
  const SumPuzzle puzzle(&sum_constraints);
  const WorksGrid initial_works_grid = makePuzzleWorksGrid(board,puzzle);

  struct Tester {
    const SumConstraints &sum_constraints;
    const WorksGrid &initial_works_grid;

    bool isSatisfied(Index row,Index col,Number number) const
    {
      const SumConstraint constraint =
        sumConstraintContainingCell(sum_constraints,{row,col});
      WorksGrid works_grid = initial_works_grid;
      assert(contains(works_grid[row][col],number));
      works_grid.setCellNumber(row,col,number);
      return sumConstraintIsSatisfiedByWorks(constraint,works_grid);
    }
  };

  Tester tester{sum_constraints,initial_works_grid};

  assert(!tester.isSatisfied(/*row*/5,/*col*/8,/*number*/'8'));
  assert( tester.isSatisfied(/*row*/5,/*col*/8,/*number*/'9'));
  assert(!tester.isSatisfied(/*row*/5,/*col*/8,/*number*/'8'));
  assert( tester.isSatisfied(/*row*/2,/*col*/0,/*number*/'1'));
  assert(!tester.isSatisfied(/*row*/5,/*col*/6,/*number*/'4'));
  assert( tester.isSatisfied(/*row*/2,/*col*/0,/*number*/'1'));
}


static void testCellValuesThatWorkWithSums()
{
  Board board(test_board_9);
  SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);

  SumPuzzle puzzle(&sum_constraints);
  Numbers numbers_that_work =
    puzzleCellValuesThatWork(board,puzzle,/*row*/2,/*col*/0);

  Numbers expected_numbers = {'1','2'};
  assert(numbers_that_work == expected_numbers);
}


static void testBuildWorksGridWithSums()
{
  Board board(test_board_9);
  SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
  SumPuzzle puzzle(&sum_constraints);
  WorksGrid works_grid = makePuzzleWorksGrid(board,puzzle);
  vector<char> expected_numbers = {'1','2'};
  assert(works_grid[2][0].value()==expected_numbers);
}


#if 0
static bool allCellsAreNonEmpty(const WorksGrid &works_grid)
{
  for (auto row : works_grid.rowIndices()) {
    for (auto col : works_grid.columnIndices()) {
      if (works_grid[row][col].empty()) {
        return false;
      }
    }
  }

  return true;
}
#endif


namespace {
struct WorksCellRangeCalculator {
  const WorksGrid &works;

  int minimumValue(IndexPair cell) const
  {
    const Numbers &numbers = works[cell];
    assert(isSorted(numbers));
    return numericValue(numbers.front());
  }

  int maximumValue(IndexPair cell) const
  {
    const Numbers &numbers = works[cell];
    assert(isSorted(numbers));
    return numericValue(numbers.back());
  }
};
}


template <typename CellRangeCalculator>
static void
  verifySumConstraints(
    const SumConstraints &sum_constraints,
    const CellRangeCalculator &cell_range_calculator
  )
{
  int constraint_index = 0;

  for (auto &constraint : sum_constraints) {
    const Area &area = constraint.area;
    int min = areaMinimumSum(area,cell_range_calculator);
    int max = areaMaximumSum(area,cell_range_calculator);
    int required = constraint.required_sum;
    cerr << "Constraint " << constraint_index << ": min=" <<
      min << ", max=" << max << ", required=" << required << "\n";
    assert(required>=min);
    assert(required<=max);
    ++constraint_index;
  }
}


static void testReducedWorksBySums()
{
  Board board(test_board_9);
  SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
  SumPuzzle puzzle(&sum_constraints);
  WorksGrid works = makePuzzleWorksGrid(board,puzzle);
  assert(sumConstraintsAreSatisfiedByWorks(sum_constraints,works));

  WorksGrid new_works = reducedWorksBySums(board,works,sum_constraints);

  const Numbers &numbers_5_8 = new_works[5][8];
  const Numbers expected_5_8 = {'9'};
  const Numbers &numbers_6_8 = new_works[6][8];
  const Numbers expected_6_8 = {'7'};
  const Numbers &numbers_6_0 = new_works[6][0];
  assert(numbers_5_8==expected_5_8);
  assert(numbers_6_8==expected_6_8);

  // We have sum constraints that make it where the only place the '4'
  // could be is in the upper-middle cell of the lower-left region.
  assert(!contains(numbers_6_0,'4'));
}


static void testFillSingles()
{
  Board board(test_board_9);
  SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
  SumPuzzle puzzle(&sum_constraints);
  WorksGrid works = makePuzzleWorksGrid(board,puzzle);
  works = reducedWorksBySums(board,works,sum_constraints);
  WorkingBoard working_board{board,works,/*debug*/false};
  working_board.fillSingles();
  assert(board[6][1]=='4');
}


static void runTests()
{
  testRowIndices();
  testShowingASudokuBoard();
  testNonReportingChecker();
  testUniqueness();
  testIsSorted();
  testRemoveIf();
  testRegionOf();
  testRegionCell();
  testContains();
  testWorksGrid();
  testWorksSolver();
  testShallowSolve();
  testHandlingPairs();
  testIndexPairUnionFind();
  testSumSpecAnalyzer();
  testMakeSumConstraints();
  testCheckerWithSums();
  testCellNumbersMustBeDistinctInArea();
  testDistinctCellNumbers();
  testPossibleNumbersInSumConstraint();
  testSumConstraintIsSatisifiedByWorks();
  testCellValuesThatWorkWithSums();
  testBuildWorksGridWithSums();
  testReducedWorksBySums();
  testFillSingles();

  testSolvingAPuzzle(test_board_1,/*show*/false);
  testSolvingAPuzzle(test_board_2,/*show*/false);
  testSolvingAPuzzle(test_board_3,/*show*/false);
  testSolvingAPuzzle(test_board_4,/*show*/false);
  testSolvingAPuzzle(test_board_5,/*show*/false);
  testSolvingAPuzzle(test_board_6,/*show*/false);
  testSolvingAPuzzle(test_board_7,/*show*/false);
  testSolvingAPuzzle(test_board_8,/*show*/false);

  // testDeepeningSolver();
  // testSolvingASumPuzzle(test_board_9,test_sum_spec_9,/*show*/false);

  // Try using the standard solver
  // testSolvingASumPuzzle2(test_board_9,test_sum_spec_9);
}


int main(int argc,char **argv)
{
  if (argc==2 && string(argv[1])=="test") {
    runTests();
    return 0;
  }

  const Number board_state[][10] = {
    "19 6  8  ",
    "8 571 36 ",
    " 638 5 71",
    "6 24 8 9 ",
    " 3   2   ",
    "         ",
    "58    1 9",
    "         ",
    "  6   53 ",
  };

  Board board(board_state);
  solve(board);
  show("solution",board,cout);
}
