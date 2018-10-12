#include "board.hpp"
#include "testboards.hpp"
#include "standardpuzzle.hpp"
#include "optional.hpp"


namespace {
struct CellProofs;
struct Assignment;
struct NegativeAssignmentProof;
struct UnsolvableProof;
}

using std::ostream;
using std::vector;
using std::cerr;
using NegativeProofs = Grid<CellProofs>;
using Assignments = vector<Assignment>;


namespace {
struct Assignment {
  const IndexPair cell;
  const Number number;

  bool operator==(const Assignment &arg) const
  {
    return cell==arg.cell && number==arg.number;
  }

  bool operator!=(const Assignment &arg) const
  {
    return !operator==(arg);
  }
};
}


// A group represents a set of assignments which could conflict.
// All the numbers in a cell.
// All the columns for a row and a number.
// All the rows for a column and a number.
// All the cells for a region and a number.
namespace {
class Group {
  public:
    static Group makeCell(IndexPair cell)
    {
      Group result(Type::numbers_for_a_cell);
      new (&result.member.cell)IndexPair(cell);
      return result;
    }

    static Group makeRow(Index row,Number number)
    {
      Group result(Type::cols_for_a_row_and_number);
      new (&result.member.row_and_number)RowAndNumber{row,number};
      return result;
    }

    static Group makeCol(Index col,Number number)
    {
      Group result(Type::rows_for_a_col_and_number);
      new (&result.member.col_and_number)ColAndNumber{col,number};
      return result;
    }

    static Group makeRegion(Index region,Number number)
    {
      Group result(Type::cells_for_a_region_and_number);
      new (&result.member.region_and_number)RegionAndNumber{region,number};
      return result;
    }

    ~Group()
    {
      switch (type) {
        case Type::numbers_for_a_cell:
        {
          destroyObject(member.cell);
          break;
        }

        case Type::cols_for_a_row_and_number:
        {
          destroyObject(member.row_and_number);
          break;
        }

        case Type::rows_for_a_col_and_number:
        {
          destroyObject(member.col_and_number);
          break;
        }

        case Type::cells_for_a_region_and_number:
        {
          destroyObject(member.region_and_number);
          break;
        }
      }
    }

    enum class Type {
      numbers_for_a_cell,
      cols_for_a_row_and_number,
      rows_for_a_col_and_number,
      cells_for_a_region_and_number
    };

    using Cell = IndexPair;

    struct RowAndNumber {
      Index row;
      Number number;
    };

    struct ColAndNumber {
      Index col;
      Number number;
    };

    struct RegionAndNumber {
      Index region;
      Number number;

      bool operator==(const RegionAndNumber &arg) const
      {
        return region==arg.region && number==arg.number;
      }
    };

    const Type type;

    Cell cell() const
    {
      assert(type==Type::numbers_for_a_cell);
      return member.cell;
    }

    RowAndNumber rowAndNumber() const
    {
      assert(type==Type::cols_for_a_row_and_number);
      return member.row_and_number;
    }

    ColAndNumber colAndNumber() const
    {
      assert(type==Type::rows_for_a_col_and_number);
      return member.col_and_number;
    }

    RegionAndNumber regionAndNumber() const
    {
      assert(type==Type::cells_for_a_region_and_number);
      return member.region_and_number;
    }

    bool operator==(const Group &arg) const
    {
      if (type!=arg.type) return false;

      switch (type) {
        case Type::numbers_for_a_cell: assert(false);
        case Type::cols_for_a_row_and_number: assert(false);
        case Type::rows_for_a_col_and_number: assert(false);
        case Type::cells_for_a_region_and_number:
          return member.region_and_number == arg.member.region_and_number;
      }

      assert(false);
    }

  private:
    union Member {
      Member()
      {
      }

      Cell cell;
      RowAndNumber row_and_number;
      ColAndNumber col_and_number;
      RegionAndNumber region_and_number;
    } member;

    Group(Type type_arg)
    : type(type_arg)
    {
    }
};
}


namespace {
struct UnsolvableProof {
  Group group;
  vector<NegativeAssignmentProof> supporting_proofs;
};
}


namespace {
struct NegativeAssignmentProof {
  enum class Type {
    // Could possibly use a group here to condense the three
    // "number_already_exists" types into a single one.
    number_already_exists_in_row,
    number_already_exists_in_col,
    number_already_exists_in_region,
    assignment_would_create_an_unsolvable_board
  };

  NegativeAssignmentProof(const Assignment &assignment_arg,Type type_arg)
  : assignment(assignment_arg),
    type(type_arg)
  {
  }

  NegativeAssignmentProof(
    const Assignment &assignment_arg,
    UnsolvableProof proof
  )
  : assignment(assignment_arg),
    type(Type::assignment_would_create_an_unsolvable_board),
    maybe_unsolvable_proof(std::move(proof))
  {
  }

  NegativeAssignmentProof(NegativeAssignmentProof &&arg) = default;
  NegativeAssignmentProof(const NegativeAssignmentProof &arg) = default;

  NegativeAssignmentProof &operator=(NegativeAssignmentProof &&)
  {
    // Why do we need this assignment?
    assert(false);
    return *this;
  }

  int nSteps() const
  {
    if (type!=Type::assignment_would_create_an_unsolvable_board) {
      return 1;
    }

    assert(maybe_unsolvable_proof);
    int n_steps = 0;

    for (auto &supporting_proof : maybe_unsolvable_proof->supporting_proofs) {
      n_steps += supporting_proof.nSteps();
    }

    return n_steps;
  }

  bool operator==(const NegativeAssignmentProof &arg) const
  {
    return assignment==arg.assignment && type==arg.type;
  }

  const Assignment assignment;
  const Type type;
  Optional<UnsolvableProof> maybe_unsolvable_proof;
};
}


namespace {
struct CellProofs {
  CellProofs()
  : proofs(9)
  {
  }

  bool operator==(const CellProofs &arg)
  {
    assert(proofs.size()==arg.proofs.size());
    size_t n = proofs.size();

    for (size_t i=0; i!=n; ++i) {
      if (proofs[i]!=arg.proofs[i]) {
        return false;
      }
    }

    return true;
  }

  const Optional<NegativeAssignmentProof> &operator[](Number number) const
  {
    return proofs[indexOf(number)];
  }

  Optional<NegativeAssignmentProof> &operator[](Number number)
  {
    return proofs[indexOf(number)];
  }

  private:
    vector<Optional<NegativeAssignmentProof>> proofs;

    static size_t indexOf(Number number)
    {
      assert(number>='1' && number<='9');
      return number-'1';
    }
};
}


namespace {
struct ProofStep {
  enum class Type {
    number_already_exists_in_row
  };

  const Type type;
  const Index index;
};
}


namespace {
struct PositiveAssignmentProof {
  const Group group;
  // vector<NegativeAssignmentProof> steps;
};
}

namespace {
struct Proof {
  vector<ProofStep> steps;
  size_t size() const { return steps.size(); }

#if 0
  void addInvalidAssignment(Assignment,ProofStep::Type type)
  {
    ProofStep step{ProofStep::Type::number_already_exists_in_row,row};
    steps.push_back(step);
  }
#endif
};
}


#if 0
static Optional<Proof> noProof()
{
  return {};
}
#endif


static vector<IndexPair> rowCells(Index row)
{
  vector<IndexPair> result;
  result.reserve(grid_size);

  for (Index col : irange(0,grid_size)) {
    result.push_back(IndexPair(row,col));
  }

  return result;
}


static vector<IndexPair> colCells(Index col)
{
  vector<IndexPair> result;
  result.reserve(grid_size);

  for (Index row : irange(0,grid_size)) {
    result.push_back(IndexPair(row,col));
  }

  return result;
}


static vector<IndexPair> regionCells(Index region)
{
  vector<IndexPair> result;
  result.reserve(grid_size);

  for (Index cell : irange(0,grid_size)) {
    result.push_back(regionCell(region,cell));
  }

  return result;
}


static bool
  numberExistsInCells(
    Number number,
    const vector<IndexPair> &cells,
    const Board &board
  )
{
  for (auto cell : cells) {
    if (board[cell]==number) {
      return true;
    }
  }

  return false;
}


static void indent(ostream &stream,int level)
{
  for (int i=0; i!=level; ++i) {
    stream << "  ";
  }
}


static Optional<UnsolvableProof>
  proveBoardIsUnsolvable(
    Board &board,
    const Puzzle &puzzle,
    int max_proof_steps,
    int recursion_level,
    bool debug
  );


static Optional<NegativeAssignmentProof>
  proveAssignmentIsInvalid(
    const Assignment &assignment,
    int max_proof_steps,
    Board &board,
    const Puzzle &puzzle,
    int recursion_level,
    bool debug
  )
{
  if (max_proof_steps==0) {
    return Optional<NegativeAssignmentProof>();
  }

  assert(max_proof_steps>0);
  const IndexPair &cell = assignment.cell;
  Index row = cell.row;
  Index col = cell.col;
  Number number = assignment.number;

  if (numberExistsInCells(number,rowCells(row),board)) {
    if (debug) {
      indent(cerr,recursion_level);
      cerr << "Number already exists in row\n";
    }
    return
      NegativeAssignmentProof(
        assignment,
        NegativeAssignmentProof::Type::number_already_exists_in_row
      );
  }

  if (numberExistsInCells(number,colCells(col),board)) {
    if (debug) {
      indent(cerr,recursion_level);
      cerr << "Number already exists in column\n";
    }
    return
      NegativeAssignmentProof(
        assignment,
        NegativeAssignmentProof::Type::number_already_exists_in_col
      );
  }

  if (numberExistsInCells(number,regionCells(regionOf(cell)),board)) {
    if (debug) {
      indent(cerr,recursion_level);
      cerr << "Number already exists in region\n";
    }
    return
      NegativeAssignmentProof(
        assignment,
        NegativeAssignmentProof::Type::number_already_exists_in_region
      );
  }

  if (max_proof_steps>1) {
    if (debug) {
      indent(cerr,recursion_level);
      cerr << "Trying " << number << " in cell " << cell << " using " << max_proof_steps << " steps\n";
    }

    board[cell] = number;
    Optional<UnsolvableProof> maybe_proof =
      proveBoardIsUnsolvable(
        board,puzzle,max_proof_steps-1,recursion_level + 1,debug
      );
    board.setCellEmpty(cell.row,cell.col);

    if (debug) {
      indent(cerr,recursion_level);

      if (maybe_proof) {
        cerr << "Succeeded in negative proof\n";
      }
      else {
        cerr << "Failed negative proof\n";
      }

      cerr << "\n";
    }

    if (maybe_proof) {
      return NegativeAssignmentProof(assignment,*maybe_proof);
    }
  }

  return {};
}


#if 0
static bool
  hasNoValidAssignments(
    const Assignments &assignments,
    const NegativeProofs &negative_proofs,
    int max_proof_steps
  )
{
  int n_remaining_steps = max_proof_steps;

  for (auto &assignment : assignments) {
    assert(n_remaining_steps>=0);

    const Optional<NegativeAssignmentProof> &maybe_proof =
      negative_proofs[assignment.cell][assignment.number];

    if (!maybe_proof) {
      return false;
    }

    assert(maybe_proof->nSteps() <= max_proof_steps);

    if (maybe_proof->nSteps() > n_remaining_steps) {
      // It is going to take more than our given number of steps to
      // prove this.
      return false;
    }

    n_remaining_steps -= maybe_proof->nSteps();
  }

  return true;
}
#endif


static vector<NegativeAssignmentProof>
  negativeAssignmentProofs(
    const Assignments &assignments,
    const NegativeProofs &negative_proofs
  )
{
  vector<NegativeAssignmentProof> proofs;

  for (auto &assignment : assignments) {
    const Optional<NegativeAssignmentProof> &maybe_proof =
      negative_proofs[assignment.cell][assignment.number];

    assert(maybe_proof);
    proofs.push_back(*maybe_proof);
  }

  return proofs;
}


template <typename Function>
static void forEachCellAssignment(IndexPair cell,const Function &f)
{
  forEachNumber([&](Number number){
    f(Assignment{cell,number});
  });
}


template <typename Function>
static void
  forEachRegionAndNumberAssignment(
    const Board &,Index region,Number number,const Function &f
  )
{
  for (IndexPair cell : regionCells(region)) {
    f(Assignment{cell,number});
  }
}


template <typename Function>
static void
  forEachRowAndNumberAssignment(
    const Board &board,
    Index row,
    Number number,
    const Function &f
  )
{
  for (Index col : board.columnIndices()) {
    f(Assignment{{row,col}, number});
  }
}


template <typename Function>
static void
  forEachColAndNumberAssignment(
    const Board &board,
    Index col,
    Number number,
    const Function &f
  )
{
  for (Index row : board.rowIndices()) {
    f(Assignment{{row,col}, number});
  }
}


static vector<Assignment> cellAssignments(IndexPair cell)
{
  vector<Assignment> assignments;

  forEachCellAssignment(
    cell,
    [&](const Assignment &a){ assignments.push_back(a); }
  );

  return assignments;
}


template <typename Function>
static void
  forEachGroupAssignment(Group group,const Board &board,const Function &f)
{
  using Type = Group::Type;

  switch (group.type) {
    case Type::numbers_for_a_cell:
    {
      forEachCellAssignment(group.cell(),f);
      return;
    }
    case Type::cols_for_a_row_and_number:
    {
      Index row = group.rowAndNumber().row;
      Number number = group.rowAndNumber().number;
      forEachRowAndNumberAssignment(board,row,number,f);
      return;
    }
    case Type::rows_for_a_col_and_number:
    {
      Index col = group.colAndNumber().col;
      Number number = group.colAndNumber().number;
      forEachColAndNumberAssignment(board,col,number,f);
      return;
    }
    case Type::cells_for_a_region_and_number:
    {
      Index region = group.regionAndNumber().region;
      Number number = group.regionAndNumber().number;
      forEachRegionAndNumberAssignment(board,region,number,f);
      return;
    }
  }

  assert(false);
}


static Assignments possibleGroupAssignments(Group group,const Board &board)
{
  vector<Assignment> assignments;

  forEachGroupAssignment(
    group,board,
    [&](const Assignment &a){
      if (board.cellIsEmpty(a.cell.row,a.cell.col)) {
        assignments.push_back(a);
      }
    }
  );

  return assignments;
}


static void printGroupOn(ostream &stream,const Group &group)
{
  using Type = Group::Type;

  switch (group.type) {
    case Type::numbers_for_a_cell:
    {
      stream << "numbers in cell (" << group.cell() << ")";
      return;
    }
    case Type::cols_for_a_row_and_number:
    {
      Index row = group.rowAndNumber().row;
      Number number = group.rowAndNumber().number;
      stream << "number " << number << " in row " << row;
      return;
    }
    case Type::rows_for_a_col_and_number:
    {
      Index col = group.colAndNumber().col;
      Number number = group.colAndNumber().number;
      stream << "number " << number << " in col " << col;
      return;
    }
    case Type::cells_for_a_region_and_number:
    {
      Index region = group.regionAndNumber().region;
      Number number = group.regionAndNumber().number;
      stream << "number " << number << " in region " << region;
      return;
    }
  }

  assert(false);
}


static ostream& operator<<(ostream &stream,const Group &group)
{
  printGroupOn(stream,group);
  return stream;
}


#if 0
static ostream& operator<<(ostream& stream,const Group &group)
{
  using Type = Group::Type;

  switch (group.type) {
    case Type::numbers_for_a_cell:
    {
      stream << "numbers for cell " << group.cell();
      break;
    }
    case Type::cols_for_a_row_and_number:
    {
      Index row = group.rowAndNumber().row;
      Number number = group.rowAndNumber().number;
      stream << "cols for number " << number << " on row " << row;
      break;
    }
    case Type::rows_for_a_col_and_number:
    {
      Index col = group.colAndNumber().col;
      Number number = group.colAndNumber().number;
      stream << "cols for number " << number << " on col " << col;
      break;
    }
    case Type::cells_for_a_region_and_number:
    {
      Index region = group.regionAndNumber().region;
      Number number = group.regionAndNumber().number;
      stream << "cells for number " << number << " in region " << region;
      break;
    }
  }

  return stream;
}
#endif


static Optional<UnsolvableProof>
  proveGroupIsUnsolvable(
    Group group,
    const NegativeProofs &negative_proofs,
    int max_proof_steps,
    const Board &board,
    bool debug
  )
{
  // We're trying to prove that there is no possibilities for
  // this group.  For example, if the group is a cell, then we
  // want to show that there are no numbers that can go in that cell.
  // If the group is a number for a row, we need to show that there
  // are no places that the number can go.  That means we are going
  // to have one proof for each possible assignment.
  vector<Assignment> assignments = possibleGroupAssignments(group,board);
  int n_possible_assignments = 0;

  if (assignments.size()>=2) {
    assert(assignments[0] != assignments[1]);
  }

  for (auto &assignment : assignments) {
    if (!negative_proofs[assignment.cell][assignment.number]) {
      if (debug) {
        cerr << "Can't put number " << assignment.number <<
          " in cell " << assignment.cell << "\n";
      }
      ++n_possible_assignments;
    }
    else {
      if (debug) {
        cerr << "Can put number " << assignment.number <<
          " in cell " << assignment.cell << "\n";
      }
    }
  }

  if (debug) {
    cerr << "Group " << group << " has " << n_possible_assignments <<
      " possible assignments.\n";
  }

  if (n_possible_assignments > max_proof_steps) {
    // If there are more possible assignments than we have proof steps, then we
    // know we can't prove it.
    return {};
  }

  if (n_possible_assignments==0) {
    return
      UnsolvableProof{
        group,negativeAssignmentProofs(assignments,negative_proofs)
      };
  }

  // We could not prove that this group was unsolvable in the given
  // number of steps.
  return {};
}


static Group cellGroup(IndexPair cell)
{
  return Group::makeCell(cell);
}


static Group rowAndNumberGroup(Index row,Number number)
{
  return Group::makeRow(row,number);
}


static Group colAndNumberGroup(Index col,Number number)
{
  return Group::makeCol(col,number);
}


static Group regionAndNumberGroup(Index region,Number number)
{
  return Group::makeRegion(region,number);
}


template <typename Function>
static void forEachGroup(const Board &board,const Function &f)
{
  forEachEmptyCell(board,[&](Index row,Index col){
    f(cellGroup(IndexPair{row,col}));
  });

  for (Index row : board.rowIndices()) {
    forEachNumber(
      [&](Number number){
        f(rowAndNumberGroup(row,number));
      }
    );
  }

  for (Index col : board.columnIndices()) {
    forEachNumber(
      [&](Number number){
        f(colAndNumberGroup(col,number));
      }
    );
  }

  for (Index region : board.regionIndices()) {
    forEachNumber(
      [&](Number number){
        f(regionAndNumberGroup(region,number));
      }
    );
  }
}


static void
  extendNegativeProofs(
    Grid<CellProofs> &negative_proofs,
    int max_proof_steps,
    Board &board,
    const Puzzle &puzzle,
    int recursion_level,
    bool debug
  )
{
  forEachEmptyCell(board,[&](Index row,Index col){
    for (Assignment assignment : cellAssignments({row,col})) {
      assert(!negative_proofs[row][col][assignment.number]);

      if (debug) {
        indent(cerr,recursion_level);
        cerr << "Trying to prove you cannot put number " <<
          assignment.number << " in cell " << assignment.cell << "\n";
      }

      if (!negative_proofs[row][col][assignment.number]) {
        negative_proofs[row][col][assignment.number] =
          proveAssignmentIsInvalid(
            assignment,
            max_proof_steps,
            board,
            puzzle,
            recursion_level,
            debug
          );
      }
    }
  });
}


static Grid<CellProofs>
  makeNegativeProofs(
    int max_proof_steps,
    Board &board,
    const Puzzle &puzzle,
    int recursion_level,
    bool debug
  )
{
  Grid<CellProofs> negative_proofs(CellProofs{});

  extendNegativeProofs(
    negative_proofs,
    max_proof_steps,
    board,
    puzzle,
    recursion_level,
    debug
  );

  return negative_proofs;
}


static bool groupIsSet(const Group &group,const Board &board)
{
  using Type = Group::Type;

  switch (group.type) {
    case Type::numbers_for_a_cell:
      // Check that no numbers exist in the cell
      return !board.cellIsEmpty(group.cell().row,group.cell().col);
    case Type::cols_for_a_row_and_number:
      {
        Number number = group.rowAndNumber().number;
        Index row = group.rowAndNumber().row;
        return numberExistsInCells(number,rowCells(row),board);
      }
    case Type::rows_for_a_col_and_number:
      {
        Number number = group.colAndNumber().number;
        Index col = group.colAndNumber().col;
        return numberExistsInCells(number,colCells(col),board);
      }
    case Type::cells_for_a_region_and_number:
      {
        Number number = group.regionAndNumber().number;
        Number region = group.regionAndNumber().region;
        return numberExistsInCells(number,regionCells(region),board);
      }
  }

  assert(false);
}


#if 0
static Optional<Assignment>
  findSingleSolution(const Group &group,const Board &board)
{
  vector<Assignment> assignments = possibleGroupAssignments(group,board);

  // We should have already checked that the group has at least one
  // solution, so we could know early that the board was unsolvable.
  assert(!assignments.empty());

  if (assignments.size()==1) {
    return assignments[0];
  }

  return {};
}
#endif



#if 0
static ostream& operator<<(ostream& stream,const Assignment &assignment)
{
  stream << "Assignment("
    "cell={" << assignment.cell.row << "," << assignment.cell.col << "},"
    "number=" << assignment.number <<
  ")";
  return stream;
}
#endif


static Optional<UnsolvableProof>
  proveBoardIsUnsolvable(
    Board &board,
    const Puzzle &puzzle,
    int max_proof_steps,
    int recursion_level,
    bool debug
  )
{
  // This should only be called if there are no immediate conflicts.
  assert(puzzle.boardIsValid(board));

  // To prove that a board is unsolvable, we have to show that
  // either there is no number that can go in a cell,
  // no place a number can go in a row, no place a number can go
  // in a column, or no place a number can go in a region.

  if (max_proof_steps==0) {
    // No way to prove it if we can't have any evidence.
    assert(false);
    return {};
  }

  // First, we need to get our negative proofs for each empty cell.
  Grid<CellProofs> negative_proofs(CellProofs{});

  extendNegativeProofs(
    negative_proofs,
    /*max_proof_steps*/1,
    board,
    puzzle,
    recursion_level,
    debug
  );

  vector<Group> unset_groups;

  forEachGroup(board,[&](Group group){
    if (!groupIsSet(group,board)) {
      unset_groups.push_back(group);
    }
  });

  // See if any of the unset groups are unsolvable.

  for (const Group &group : unset_groups) {
    Optional<UnsolvableProof> maybe_proof =
      proveGroupIsUnsolvable(
        group,negative_proofs,max_proof_steps,board,debug
      );

    if (maybe_proof) {
      return maybe_proof;
    }
  }

#if 0
  // See if any of the unset groups have a single solution

  for (const Group &group : unset_groups) {
    Optional<Assignment> maybe_assignment =
      findSingleSolution(group,board);
    if (maybe_assignment) {
      const Assignment &assignment = *maybe_assignment;
      board[assignment.cell] = assignment.number;
    }
  }
#endif

  return {};
}


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


#if 1
static Optional<PositiveAssignmentProof>
  findGroupPositiveProof(const Group &group,const Board &board)
{
  Assignments assignments = possibleGroupAssignments(group,board);

  if (assignments.size()==1) {
    return PositiveAssignmentProof{group};
  }

  return {};
}
#endif


#if 0
static CellProofs noCellProofs()
{
  return vector<Optional<Proof>>(grid_size,noProof());
}
#endif


static vector<Group> groupsOf(const Board &board)
{
  vector<Group> result;

  forEachGroup(board,[&](Group group){
    result.push_back(group);
  });

  return result;
}


static Optional<PositiveAssignmentProof>
  findAnyPositiveProof(Board &board,int max_proof_length)
{
  assert(max_proof_length>0);
  vector<Group> groups = groupsOf(board);

  for (const Group &group : groups) {
    if (!groupIsSet(group,board)) {
      Optional<PositiveAssignmentProof> maybe_proof =
        findGroupPositiveProof(group,board);

      if (maybe_proof) {
        return maybe_proof;
      }
    }
  }

  if (max_proof_length==1) {
    // There's no steps available to
  }

  assert(false);
  // for each cell {
  //   if (board[cell].isEmpty()) {
  //     for each number {
  //       proofs[cell][number] =
  //         proveAssignmentIsInvalid(cell,number,max_proof_length);
  //     }
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


#if 0
static void
  runDeepeningSolver(const Puzzle &/*puzzle*/,Board &/*board*/)
{
  // We solve by finding cells we can fill one at a time.
  // For each cell we fill, we try to find the minimum proof that
  // the assignment is correct.

  int max_proof_length = 1;

  // We could reuse the top-level negative proofs that we find.  Also, the
  // negative proofs are still valid as we go deeper, although we may
  // be able to find even smaller proofs.  In that sense, maybe the
  // top-level proofs aren't all that useful since we may fail to find
  // certain positive proofs because it seems like the total length of
  // the negative proofs will be too long.

  for (;;) {
    PositiveProofFinderResult result = findAnyPositiveProof(max_proof_length)

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


template <typename T>
static bool allAreUnique(const vector<T> &v)
{
  size_t n = v.size();

  for (size_t i=0; i!=n; ++i) {
    for (size_t j=i+1; j!=n; ++j) {
      if (v[i]==v[j]) return false;
    }
  }

  return true;
}


static void testOptional()
{
  {
    Optional<int> v;
    assert(!v);
  }
  {
    Optional<int> v = 5;
    assert(v);
    assert(*v==5);
  }
}


static void testRowCells()
{
  vector<IndexPair> result = rowCells(/*row*/1);
  assert(result.size()==grid_size);
  assert(result[0]==IndexPair(1,0));
  assert(result[8]==IndexPair(1,8));
}


static void testNumberExistsInCells()
{
  Board board(testBoard1());
  assert( numberExistsInCells('1',rowCells(/*row*/0),board));
  assert(!numberExistsInCells('3',rowCells(/*row*/0),board));
}


static void
  testProveAssignmentIsInvalidWith(
    const BoardState &board_state,
    const Assignment &assignment,
    NegativeAssignmentProof::Type expected_type
  )
{
  Board board(board_state);
  StandardPuzzle puzzle;
  Optional<NegativeAssignmentProof> maybe_proof =
    proveAssignmentIsInvalid(
      assignment,
      /*max_proof_length*/1,
      board,
      puzzle,
      /*recursion_level*/0,
      /*debug*/false
    );
  NegativeAssignmentProof
    expected_proof(
      assignment,
      expected_type
    );
  assert(*maybe_proof==expected_proof);
}


static void testProveAssignmentIsInvalid()
{
  testProveAssignmentIsInvalidWith(
    testBoard1(),
    {{/*row*/4,/*col*/0},/*number*/'1'},
    NegativeAssignmentProof::Type::number_already_exists_in_row
  );

  testProveAssignmentIsInvalidWith(
    testBoard1(),
    {{/*row*/0,/*col*/2},/*number*/'5'},
    NegativeAssignmentProof::Type::number_already_exists_in_col
  );

  testProveAssignmentIsInvalidWith(
    testBoard1(),
    {{/*row*/2,/*col*/8},/*number*/'6'},
    NegativeAssignmentProof::Type::number_already_exists_in_region
  );
}


static void testGroupAssignments()
{
  {
    // Test cell group
    Board board(testBoard1());
    IndexPair cell{0,2};
    Assignments assignments = possibleGroupAssignments(cellGroup(cell),board);
    assert(assignments.size()==9);
    assert(assignments[0].cell==cell);
    assert(assignments[0].number=='1');
    assert(assignments[8].number=='9');
  }
  {
    // Test row and number group
    Board board(testBoard1());
    Group group = rowAndNumberGroup(0,'3');
    Assignments assignments = possibleGroupAssignments(group,board);
    assert(assignments.size()==4);
    Assignments expected_assignments = {
      {{0,2},'3'},
      {{0,3},'3'},
      {{0,5},'3'},
      {{0,6},'3'},
    };
    assert(assignments==expected_assignments);
  }
  {
    Board board(testBoard1());

    forEachGroup(board,[&](const Group &group){
      assert(allAreUnique(possibleGroupAssignments(group,board)));
    });
  }
}


static void testGroupIsSet()
{
  Board board(testBoard1());
  assert( groupIsSet(cellGroup({0,0}),board));
  assert(!groupIsSet(cellGroup({0,2}),board));
  assert( groupIsSet(rowAndNumberGroup(0,'1'),board));
  assert(!groupIsSet(rowAndNumberGroup(0,'3'),board));
  assert( groupIsSet(colAndNumberGroup(0,'9'),board));
  assert(!groupIsSet(colAndNumberGroup(0,'2'),board));
  assert( groupIsSet(regionAndNumberGroup(0,'9'),board));
  assert(!groupIsSet(regionAndNumberGroup(0,'3'),board));
}


static void testForEachGroup()
{
  Board board(testBoard1());
  int n_groups = 0;

  forEachGroup(board,[&](const Group &){
    ++n_groups;
  });

  assert(n_groups!=0);
}


static void testProveAssignmentIsInvalid2()
{
  Board board(testBoard1());
  const StandardPuzzle puzzle;

  forEachEmptyCell(board,[&](Index row,Index col){
    forEachNumber([&](Number number){
      Assignment assignment{{row,col},number};
      Optional<NegativeAssignmentProof> maybe_proof =
        proveAssignmentIsInvalid(
          assignment,
          /*max_proof_length*/1,
          board,
          puzzle,
          /*recursion_level*/0,
          /*debug*/false
        );
#if 0
      if (!maybe_proof) {
        cerr << "Couldn't prove " << assignment << "\n";
      }
#endif
    });
  });

  // Need to find a case where we can't find a negative proof easily.
}


#if 0
static void testProveAssignmentIsInvalid3()
{
  StandardPuzzle puzzle;
  Board board(testBoard1());
  Index row = 0;
  Index col = 2;
  Number number = '3';
  int max_proof_length = 2;
  Assignment assignment = {{row,col},number};
  int recursion_level = 0;

  cerr << "---\n";
  Optional<NegativeAssignmentProof> maybe_proof =
    proveAssignmentIsInvalid(
      assignment,
      max_proof_length,
      board,
      puzzle,
      recursion_level,
      /*debug*/false
    );
  cerr << "---\n";

  if (!maybe_proof) {
    cerr << "Couldn't prove number " <<
      number << " can't go in {" << row << "," << col << "}\n";
  }
}
#endif


static void testMakeNegativeProofs()
{
  const StandardPuzzle puzzle;
  { /* if we don't allow any proof steps, then we can't have any proofs */
    int max_proof_steps = 0;
    Board board(testBoard1());
    Grid<CellProofs> negative_proofs =
      makeNegativeProofs(
        max_proof_steps,
        board,
        puzzle,
        /*recursion_level*/0,
        /*debug*/false
      );
    assert(negative_proofs[0][0]==CellProofs());
  }
  { /* if we allow only 1 proof step, then we can only have very simple
       proofs */
    int max_proof_steps = 1;
    Board board(testBoard1());
    Grid<CellProofs> negative_proofs =
      makeNegativeProofs(
        max_proof_steps,
        board,
        puzzle,
        /*recursion_level*/0,
        /*debug*/false
      );
    assert(negative_proofs[0][2]['9'].hasValue());
  }
}


static void testProveBoardIsUnsolvable()
{
  StandardPuzzle puzzle;
  Board board(testBoard1());
  board[0][2] = '3';
  Optional<UnsolvableProof> result =
    proveBoardIsUnsolvable(
      board,
      puzzle,
      /*max_proof_steps*/1,
      /*recursion_level*/0,
      /*debug*/false
    );
  assert(!result);
}


#if 1
static void testFindGroupPositiveProof()
{
  Board board(testBoard1());
  Optional<PositiveAssignmentProof> maybe_proof =
    findGroupPositiveProof(regionAndNumberGroup(4,'4'),board);
  assert(maybe_proof);
}
#endif


static void testFindAnyPositiveProof()
{
  Board board(testBoard1());
  Optional<PositiveAssignmentProof> maybe_proof =
    findAnyPositiveProof(board,/*max_proof_steps*/1);
  assert(maybe_proof);
  assert(maybe_proof->group==regionAndNumberGroup(4,'4'));
}


static void testProveBoardIsUnsolvable2()
{
  StandardPuzzle puzzle;
  Board board(testBoard1());
  board[0][2] = '3';
  Optional<UnsolvableProof> result =
    proveBoardIsUnsolvable(
      board,
      puzzle,
      /*max_proof_steps*/2,
      /*recursion_level*/0,
      /*debug*/false
    );
  assert(!result);
}


#if 0
static void testDeepeningSolver()
{
  Board board(testBoard1());
  StandardPuzzle puzzle;
  runDeepeningSolver(puzzle,board);
  Checker checker(board,/*show_violations*/false);
  assert(checker.checkIsSolved());
}
#endif


int main()
{
  testOptional();
  testRowCells();
  testNumberExistsInCells();
  testProveAssignmentIsInvalid();
  testForEachGroup();
  testGroupAssignments();
  testGroupIsSet();
  testMakeNegativeProofs();
  testProveBoardIsUnsolvable();
  testFindGroupPositiveProof();
  testFindAnyPositiveProof();
  testProveAssignmentIsInvalid2();
  testProveBoardIsUnsolvable2();
  // testProveAssignmentIsInvalid3();
  // testFindAnyPositiveProof();
  // testDeepeningSolver();
}
