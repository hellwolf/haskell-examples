#include <variant>
#include <map>
#include <iostream>

using namespace std;


// Substructural types compile-time tracking table
namespace substruct {

struct Linear {
    int counter;
    constexpr Linear() : counter(1) {}
};

typedef std::variant<Linear> System;

static std::map<std::string, System> _trackings;

constexpr int slen(const char *str) {
    return *str ? 1 + slen(str + 1) : 0;
}
// template<char... CHARS> struct tag {
template<int> struct tag {
    operator const std::string&() {
        // static const std::string str{ { CHARS... } };
        static const std::string str{ "hello" };
        return str;
    }
};
#define LINEAR_S(a) #a
#define LINEAR_TAG substruct::tag<substruct::slen(__FILE__ "" LINEAR_S(__LINE__))>

template <typename T, typename TAG>
class Type
{
public:
    constexpr Type(T v): _v(v) {
    }
    constexpr T take() {
        Linear& _s = std::get<Linear>(_trackings[TAG]);
        _s.counter--;
        //static_assert(_u >= 0);
        return _v;
    }
    constexpr int useLeft() {
        Linear& _s = std::get<Linear>(_trackings[TAG]);
        return _s.counter;
    }

private:
    T _v;
};

}

constexpr static int _foo_tag = 0;
template<typename TAG>
int foo(substruct::Type<int, TAG> a) {
    a.take();
    return a + 2;
}

int main()
{
    auto a = substruct::Type<int, LINEAR_TAG>(42);
    cout << "a.useLeft() " << a.useLeft() << endl;

    auto b = foo(a);
    auto c = foo(a);

    cout << "a.useLeft() " << a.useLeft() << endl;
    cout << "b.useLeft() " << b.useLeft() << endl;
}


// Links:
// https://stackoverflow.com/questions/32002200/enforcing-correct-state-transitions-at-compile-time
// http://insanitybit.github.io/2016/05/30/beyond-memory-safety-with-types
// https://www.reddit.com/r/ProgrammingLanguages/comments/6sq6nv/linear_or_affine_types/
// https://b.atch.se/posts/non-constant-constant-expressions/
// https://b.atch.se/posts/constexpr-counter/