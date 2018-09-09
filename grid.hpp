#ifndef GRID_HPP_
#define GRID_HPP_

#include <cstdlib>
#include <cassert>
#include <iterator>
#include <vector>


using Index = int;
using Number = char;
using Numbers = std::vector<Number>;


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


template <
  typename T,
  typename U,
  typename TU = typename std::common_type<T,U>::type
>
IndexRange<TU> irange(T first,U last)
{
  return {first,last};
}


template <typename T>
IndexRange<typename T::size_type> irange(const T& container)
{
  return {0,container.size()};
}


inline IndexPair regionCell(Index region,Index region_cell_index)
{
  Index region_row = region / 3;
  Index region_col = region % 3;
  Index cell_row = region_cell_index / 3;
  Index cell_col = region_cell_index % 3;

  return {cell_row + region_row*3,cell_col + region_col*3};
}


inline bool isValidCellValue(Number c)
{
  return (c==' ' || (c>='1' && c<='9'));
}


inline bool isValidCellValue(const Numbers &)
{
  return true;
}


enum { grid_size = 9 };


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
    std::vector<CellValue> cells;
};


#endif /* GRID_HPP_ */
