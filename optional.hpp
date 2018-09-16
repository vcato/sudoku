template <typename T>
void destroyObject(T& arg)
{
  arg.~T();
}


template <typename T>
class Optional {
  public:
    Optional()
    : has_value(false)
    {
    }

    Optional(T arg)
    : has_value(true)
    {
      createValue(std::move(arg));
    }

    Optional(Optional &&arg)
    : has_value(arg.has_value)
    {
      if (has_value) {
        createValue(std::move(arg.member.value));
      }
    }

    Optional(const Optional &arg)
    : has_value(arg.has_value)
    {
      if (arg.has_value) {
        assert(false);
      }
    }

    ~Optional()
    {
      if (!has_value) {
        return;
      }

      destroyValue();
    }

    bool hasValue() const { return has_value; }

    explicit operator bool() const
    {
      return has_value;
    }

    const T& operator*() const
    {
      assert(has_value);
      return member.value;
    }

    const T* operator->() const
    {
      assert(has_value);
      return &member.value;
    }

    Optional &operator=(const T& /*arg*/)
    {
      if (has_value) {
        assert(false);
      }
      else {
        assert(false);
      }
    }

    Optional &operator=(Optional arg)
    {
      if (has_value) {
        if (arg.has_value) {
          member.value = std::move(arg.member.value);
        }
        else {
          destroyValue();
          has_value = false;
        }
      }
      else {
        if (arg.has_value) {
          createValue(std::move(arg.member.value));
          has_value = true;
        }
      }

      return *this;
    }
    
    bool operator==(const Optional &arg) const
    {
      if (has_value) {
        assert(false);
      }
      else {
        if (arg.has_value) {
          assert(false);
        }
        else {
          return true;
        }
      }
    }

    bool operator!=(const Optional &arg) const
    {
      return !operator==(arg);
    }

  private:
    union Member {
      Member() {}
      ~Member() {}
      T value;
    } member;
  
    bool has_value;

    void createValue(T arg)
    {
      new (&member.value) T(std::move(arg));
    }

    void destroyValue()
    {
      destroyObject(member.value);
    }
};
