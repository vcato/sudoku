using Number = char;


template <typename Func>
void forEachNumber(const Func &f)
{
  for (Number value='1'; value<='9'; ++value) {
    f(value);
  }
}


