#include "board.hpp"
#include "testboards.hpp"
#include "standardpuzzle.hpp"


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
namespace {
struct MaybeProvedAssignment {
  bool has_value;
  IndexPair cell;
  Number number;
  Proof proof;
};
}


static MaybeProvedAssignment findAnyPositiveProof(int max_proof_length)
{
  // We try to make some forward progress, finding a proof that a certain
  // number must go in a certain cell.

  // Find negative proofs for each cell and number

  // for each cell {
  //   if (board[cell].isEmpty()) {
  //     for each number {
  //       proofs[cell][number] =
  //         findNegativeProof(cell,number,max_proof_length);
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
  // testDeepeningSolver();
}
