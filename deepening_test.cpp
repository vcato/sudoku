#include "board.hpp"
#include "testboards.hpp"
#include "standardpuzzle.hpp"

#define USE_PROVE_BOARD_IS_UNSOLVABLE 0
#define USE_PROVE_ASSIGNMENT_IS_INVALID2 0


using std::cerr;


namespace {
struct NegativeAssignmentProof;
}


// A group represents a set of assignments which could conflict.
// All the numbers in a cell.
// All the columns for a row and a number.
// All the rows for a column and a number.
// All the cells for a region and a number.
namespace {
struct Group {
  enum class Type {
    numbers_for_a_cell,
    cols_for_a_row_and_number,
    rows_for_a_col_and_number,
    cells_for_a_region_and_number
  };

  const Type type;

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
  };

  union {
    Cell cell;
    RowAndNumber row_and_number;
    ColAndNumber col_and_number;
    RegionAndNumber region_and_number;
  };
};
}


namespace {
template <typename T>
struct Optional {
  const bool has_value;

  Optional()
  : has_value(false)
  {
  }

  Optional(T arg)
  : has_value(true)
  {
    new (&value) T(std::move(arg));
  }

  ~Optional()
  {
    if (!has_value) {
      return;
    }

    value.~T();
  }

  explicit operator bool() const
  {
    return has_value;
  }

  const T& operator*() const
  {
    assert(has_value);
    return value;
  }

  union {
    T value;
  };
};
}


using std::vector;
using CellProofs = vector<Optional<NegativeAssignmentProof>>;


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
struct Assignment {
  const IndexPair cell;
  const Number number;

  bool operator==(const Assignment &arg) const
  {
    return cell==arg.cell && number==arg.number;
  }
};
}


namespace {
struct NegativeAssignmentProof {
  enum class Type {
    number_already_exists_in_row,
    number_already_exists_in_col,
    number_already_exists_in_region,
    assignment_would_create_an_invalid_board
  };

  NegativeAssignmentProof(const Assignment &assignment_arg,Type type_arg)
  : assignment(assignment_arg),
    type(type_arg)
  {
  }

  bool operator==(const NegativeAssignmentProof &arg) const
  {
    return assignment==arg.assignment && type==arg.type;
  }

  const Assignment assignment;
  const Type type;
};
}


namespace {
struct PositiveAssignmentProof {
  enum class Type {
    only_valid_number_in_cell,
    only_valid_col_in_row,
    only_valid_row_in_col,
    only_valid_cell_in_region
  };

  const Assignment assignment;
  const Type type;
  vector<NegativeAssignmentProof> steps;
};
}


namespace {
struct UnsolvableProof {
  enum class Type {
    no_valid_number_for_cell,
    no_valid_col_for_number_in_row,
    no_valid_row_for_number_in_col,
    no_valid_cell_for_number_in_region
  };

  const Type type;
  const vector<NegativeAssignmentProof> supporting_proofs;
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


static Optional<NegativeAssignmentProof>
  proveAssignmentIsInvalid(
    const Assignment &assignment,
    int max_proof_steps,
    Board &board
  )
{
  if (max_proof_steps==0) {
    assert(false);
    return Optional<NegativeAssignmentProof>();
  }

  assert(max_proof_steps>0);
  const IndexPair &cell = assignment.cell;
  Index row = cell.row;
  Index col = cell.col;
  Number number = assignment.number;

  if (numberExistsInCells(number,rowCells(row),board)) {
    return
      NegativeAssignmentProof(
        assignment,
        NegativeAssignmentProof::Type::number_already_exists_in_row
      );
  }

  if (numberExistsInCells(number,colCells(col),board)) {
    return
      NegativeAssignmentProof(
        assignment,
        NegativeAssignmentProof::Type::number_already_exists_in_col
      );
  }

  if (numberExistsInCells(number,regionCells(regionOf(cell)),board)) {
    return
      NegativeAssignmentProof(
        assignment,
        NegativeAssignmentProof::Type::number_already_exists_in_region
      );
  }

#if USE_PROVE_ASSIGNMENT_IS_INVALID2
  cerr << "cell: " << cell << "\n";
  cerr << "number: " << number << "\n";
  assert(false);
  board[cell] = number;
  Optional<Proof> maybe_proof = proveBoardIsUnsolvable(board,max_proof_steps);
  board[cell].setEmpty();

  if (maybe_proof) {
    assert(false);
    Proof proof;
    proof.addNumberMakesBoardInvalid(*maybe_proof):
    return proof;
  }

  return noProof();
#else
  assert(false);
#endif
}

#if 0
static Optional<Proof>
  hasNoValidAssignments(const vector<Assignments> &,int max_proof_steps)
{
  int n_remaining_steps = max_proof_steps;

  for (auto &assgnment : assignments) {
    Optional<Proof> maybe_assignment_proof =
      proveAssignmentIsInvalid(assignment,n_remaining_steps);

    if (!maybe_assignment_proof) {
      return false;
    }

    proof += *maybe_assignment_proof;
    n_remaining_steps -= maybe_assignment_proof->nSteps();
  }

  return proof;
}
#endif


#if 0
static vector<Assignment> cellAssignments(IndexPair cell)
{
}


static vector<Assignment> rowAssignments(Index row,Number number)
{
}
#endif


#if USE_PROVE_BOARD_IS_UNSOLVABLE
static Optional<UnsolvableProof>
  proveGroupIsUnsolvable(Group group,const NegativeProofs &negative_proofs)
{
  vector<Assignment> assignments = groupAssignments(group);

  if (hasNoValidAssignments(assignments)) {
    assert(false);
    return
      UnsolvableProof{
        group,
        assignmentProofs(assignments,negative_proofs)
      };
  }
}
#endif


#if 0
static void forEachGroup()
{
  forEachEmptyCell(board,[&](IndexPair cell){
    f(cellGroup(cell));
  }

  forEachRow(
    forEachNumber(
      f(rowGroup(row,number))
    )
  );

  forEachCol(
    forEachNumber(
      f(colGroup(col,number));
    )
  )

  forEachRegion(
    forEachNumber(
      f(regionGroup(region,number));
    )
  )
}
#endif


#if 0
static Optional<UnsolvableProof>
  proveBoardIsUnsolvable(Board &board,const Puzzle &puzzle,int depth)
{
  // This should only be called if there are no obvious conflicts.
  assert(puzzle.boardIsValid(board));

  // To prove that a board is unsolvable, we have to show that
  // either there is no number that can go in a cell,
  // no place a number can go in a row, no place a number can go
  // in a column, or no place a number can go in a region.

  if (depth==0) {
    // No way to prove it if we can't have any evidence.
    assert(false);
    return {};
  }

  // First, we need to get our negative proofs for each empty cell.
  Grid<CellProofs> negative_proofs = makeNegativeProofs():

  forEachGroup([](Group group){
    Optional<UnsolvableProof> maybe_proof =
      proveGroupIsUnsolvable(group,negative_proofs);
    if (maybe_proof) {
      return maybe_proof;
    }
  }):
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


namespace {
struct MaybeProvenAssignment {
  const bool has_value;
  const IndexPair cell;
  const Number number;
  const Proof proof;
};
}


#if 0
static CellProofs noCellProofs()
{
  return vector<Optional<Proof>>(grid_size,noProof());
}
#endif

// Need a function which will give us all the negative proofs

#if 0
static MaybeProvenAssignment
  findAnyPositiveProof(Board &board,int /*max_proof_length*/)
{
  // We try to make some forward progress, finding a proof that a certain
  // number must go in a certain cell.

  // Find negative proofs for each cell and number

  Grid<CellProofs> proofs(noCellProofs());

  forEachEmptyCell(board,[&](Index row,Index col){
    forEachNumber([&](Number number){
      proofs[{row,col}][number] = proveAssignmentIsInvalid(board,row,col,number);
    });
  });

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
#endif


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


#if 0
static void testFindAnyPositiveProof()
{
  Board board(testBoard1());
  // We should not be able to find a proof in one step.
  MaybeProvenAssignment maybe_assignment =
    findAnyPositiveProof(board,/*max_proof_length*/1);

  assert(!maybe_assignment.has_value);
}
#endif


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
  Optional<NegativeAssignmentProof> maybe_proof =
    proveAssignmentIsInvalid(
      assignment,
      /*max_proof_length*/1,
      board
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


#if USE_PROVE_ASSIGNMENT_IS_INVALID2
static void testProveAssignmentIsInvalid2()
{
  Board board(testBoard1());

  forEachEmptyCell(board,[&](Index row,Index col){
    forEachNumber([&](Number number){
      Optional<NegativeAssignmentProof> maybe_proof =
        proveAssignmentIsInvalid(
          /*assignment*/{{row,col},number},
          /*max_proof_length*/1,
          board
        );
    });
  });

  // Need to find a case where we can't find a negative proof easily.
}
#endif


#if USE_PROVE_BOARD_IS_UNSOLVABLE
static void testProveBoardIsUnsolvable()
{
  StandardPuzzle puzzle;
  Board board(testBoard1());
  board[0][2] = '3';
  Optional<UnsolvableProof> result = proveBoardIsUnsolvable(puzzle,board);
  assert(false);
}
#endif


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
#if USE_PROVE_BOARD_IS_UNSOLVABLE
  testProveBoardIsUnsolvable();
#endif
#if USE_PROVE_ASSIGNMENT_IS_INVALID2
  testProveAssignmentIsInvalid2();
#endif
  // testFindAnyPositiveProof();
  // testDeepeningSolver();
}
