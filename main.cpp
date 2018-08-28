#include <cassert>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using std::cerr;
using std::cout;
using std::vector;
using std::ostream;

using Index = int;
using Number = char;
using Numbers = vector<Number>;
using BoardState = Number [9][10];
using SumSpec = const char [19][20];


static const BoardState test_board1 = {
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


static const BoardState test_board2 = {
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


static const BoardState test_board3 = {
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


static const BoardState test_board4 = {
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


static const BoardState test_board5 = {
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


static const BoardState test_board6 = {
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


static const BoardState test_board7 = {
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


static const BoardState test_board8 = {
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


#if 1
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
#endif


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

#if 0
    template <typename Function>
    int calculateSum(const Area &area,const Function &cell_value_function)
    {
      // Can we take advantage of the works grid to raise the lower bound of
      // the result here?

      int result = 0;

      for (const IndexPair &cell_index : area.cell_indices) {
        result += cell_value_function(cell_index);
      }

      return result;
    }
#endif

    int minimumValue(const IndexPair &cell)
    {
      if (board.cellIsEmpty(cell.row,cell.col)) {
        return 1;
      }

      return numericValue(board.cell(cell.row,cell.col));
    }

    int maximumValue(const IndexPair &cell)
    {
      if (board.cellIsEmpty(cell.row,cell.col)) {
        return 9;
      }

      return numericValue(board.cell(cell.row,cell.col));
    }

    int minimumSum(const Area &area)
    {
      // Can we take advantage of the works grid to raise the lower bound of
      // the result here?

      int result = 0;

      for (const IndexPair &cell : area.cell_indices) {
        result += minimumValue(cell);
      }

      return result;
    }

    int maximumSum(const Area &area)
    {
      // Can we take advantage of the works grid to raise the lower bound of
      // the result here?

      int result = 0;

      for (const IndexPair &cell : area.cell_indices) {
        result += maximumValue(cell);
      }

      return result;
    }

    bool sumConstraintIsSatisfied(const SumConstraint &constraint)
    {
      int min_sum = minimumSum(constraint.area);
      int max_sum = maximumSum(constraint.area);
      int required_sum = constraint.required_sum;

      return (required_sum>=min_sum && required_sum<=max_sum);
    }

    bool sumConstraintsAreSatisfied(const SumConstraints &sum_constraints)
    {
      for (const SumConstraint &constraint : sum_constraints) {
        if (!sumConstraintIsSatisfied(constraint)) {
          return false;
        }
      }

      return true;
    }
  };
}


static void testNonReportingChecker()
{
  Board board(test_board1);
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

  if (!checker.sumConstraintsAreSatisfied(sum_constraints)) {
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


static Numbers
  cellValuesThatWork(
    const Board &board,
    const Puzzle &puzzle,
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

    if (puzzle.boardIsValid(test_board)) {
      workable.push_back(value);
    }
  });

  return workable;
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
      const Numbers &works1 = (*this)[cell1.row][cell1.col];
      const Numbers &works2 = (*this)[cell2.row][cell2.col];
      if (works1.size()!=2) return false;
      if (works2.size()!=2) return false;
      assert(isSorted(works1));
      assert(isSorted(works2));
      if (works1!=works2) return false;
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

    void handlePairs(bool debug = false)
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


static WorksGrid buildWorksGrid(const Board &board,const Puzzle &puzzle)
{
  WorksGrid works;

  for (auto row : board.rowIndices()) {
    for (auto col : board.columnIndices()) {
      works[row][col] = cellValuesThatWork(board,puzzle,row,col);
    }
  }

  return works;
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
    void handlePairs(bool debug = false) { works.handlePairs(debug); }

    bool isMatchingPair(const IndexPair cell1,const IndexPair cell2) const
    {
      return works.isMatchingPair(cell1,cell2);
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

    void fillSinglesInColumn(Index col);
    void fillSinglesInRow(Index row);
    void fillSinglesInCell(Index row,Index col);

    void check()
    {
      WorksGrid temp_works = buildWorksGrid(board,puzzle);
      temp_works.check_empty = works.check_empty;
      temp_works.handlePairs();
      compareWorksGrids(temp_works,works);
    }

    void fill(Index row,Index col,Number v)
    {
      board[row][col] = v;
      works[row][col] = {v};
      works.eliminateInRow({row,col});
      works.eliminateInColumn({row,col});
      works.eliminateInRegion({row,col});
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
  works(buildWorksGrid(board_arg,deref(puzzle_ptr))),
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
      fillRowSingles();
      fillColumnSingles();
      fillCellSingles();

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
  fill(row,col,v);
  shallowSolve();
  bool is_valid = !works.anyEmpty();
  board = old_board;
  works = buildWorksGrid(board,puzzle);
  assert(works.check_empty);
  works.handlePairs();
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
      fill(row,col,v);
      shallowSolve();
    }
  });
}


void WorksSolver::fillSinglesInColumn(Index col)
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


void WorksSolver::fillSinglesInRow(Index row)
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
        board[cell] = v;
        works[cell] = {v};
        works.eliminateInColumn(cell);
        works.eliminateInRegion(cell);
      }
    }
  });
}


void WorksSolver::fillSinglesInCell(Index row,Index col)
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


static void solve(Board &board)
{
  StandardPuzzle puzzle;
  WorksSolver solver(board,&puzzle);
  solver.deepSolve();
}


#if 0
static void solveWithSumConstraints(Board &board,const SumConstraints &)
{
  WorksSolver solver(board);
  solver.deepSolve();
}
#endif


static void testUniqueness()
{
  Board board(test_board1);
  board[0][2] = '1';
  assert(!StandardPuzzle().boardIsValid(board));
}


static void showValid(const Board &board,const Puzzle &puzzle)
{
  forEachEmptyCell(board,[&](Index row,Index col) {
    Numbers workable = cellValuesThatWork(board,puzzle,row,col);
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
  testSolvingAPuzzleWithSums(
    const BoardState &test_board,
    const SumSpec &sum_spec,
    bool show_it
  )
{
  Board board(test_board);
  SumConstraints sum_constraints = makeSumConstraints(sum_spec);

  solveWithSumConstraints(board,sum_constraints);

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
  Board board = test_board4;
  WorksGrid works_grid = buildWorksGrid(board,StandardPuzzle());
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
  Board board = test_board4;
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
  WorksGrid works = buildWorksGrid(board,puzzle);
  works.handlePairs();
  board[row][col] = v;
  works.eliminateInRow({row,col});
  works.eliminateInColumn({row,col});
  works.eliminateInRegion({row,col});
  works.handlePairs();
  WorksGrid temp_works = buildWorksGrid(board,puzzle);
  temp_works.handlePairs();
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


static bool areaContains(const Area &area,const IndexPair &cell_index)
{
  return contains(area.cell_indices,cell_index);
}


static const Area& areaContaining(const vector<Area> &areas,const IndexPair &cell_index)
{
  for (const Area &area : areas) {
    if (areaContains(area,cell_index)) {
      return area;
    }
  }

  assert(false);

  return areas[0];
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
    Checker checker(board,/*show_violations*/false);
    bool sums_are_satisified =
      checker.sumConstraintsAreSatisfied(sum_constraints);
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
    Checker checker(board,/*show_violations*/false);
    bool sums_are_satisified =
      checker.sumConstraintsAreSatisfied(sum_constraints);
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
    Checker checker(board,/*show_violations*/false);
    bool sums_are_satisified =
      checker.sumConstraintsAreSatisfied(sum_constraints);
    assert(!sums_are_satisified);
  }
}


static void testCellValuesThatWorkWithSums()
{
  Board board(test_board_9);
  SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);

  SumPuzzle puzzle(&sum_constraints);
  Numbers numbers_that_work =
    cellValuesThatWork(board,puzzle,/*row*/2,/*col*/0);

  Numbers expected_numbers = {'1','2'};
  assert(numbers_that_work == expected_numbers);
}


#if 0
static void testBuildWorksGridWithSums()
{
  Board board(test_board_9);
  SumConstraints sum_constraints = makeSumConstraints(test_sum_spec_9);
  auto is_valid_function = isValidWithSumsFunction(sum_constraints);
  WorksGrid works_grid = buildWorksGrid(board,is_valid_function);

  for (auto row : board.rowIndices()) {
    for (auto col : board.columnIndices()) {
      cerr << row << "," << col << ": " << works_grid[row][col] << "\n";
    }
  }
}
#endif


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
  testCellValuesThatWorkWithSums();
  // testBuildWorksGridWithSums();

  testSolvingAPuzzle(test_board1,/*show*/false);
  testSolvingAPuzzle(test_board2,/*show*/false);
  testSolvingAPuzzle(test_board3,/*show*/false);
  testSolvingAPuzzle(test_board4,/*show*/false);
  testSolvingAPuzzle(test_board5,/*show*/false);
  testSolvingAPuzzle(test_board6,/*show*/false);
  testSolvingAPuzzle(test_board7,/*show*/false);
  testSolvingAPuzzle(test_board8,/*show*/false);

  // testSolvingAPuzzleWithSums(test_board_9,test_sum_spec_9,/*show*/false);
}


int main(int argc,char **argv)
{
  if (argc==2 && std::string(argv[1])=="test") {
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
