using BoardState = Number [9][10];
using SumSpec = const char [19][20];


inline const BoardState &testBoard1()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard2()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard3()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard4()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard5()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard6()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard7()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard8()
{
  static BoardState result = {
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

  return result;
}


inline const BoardState& testBoard9()
{
  static BoardState result = {
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

  return result;
}


inline SumSpec& testSumSpec9()
{
  static SumSpec result = {
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

  return result;
}
