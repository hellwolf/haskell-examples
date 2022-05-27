#include <iostream>
#include <cmath>
#include <string>
#include <vector>

// run-time/inclusion polymorphism classes:
class INum {
public:
    virtual void operator +=(const INum&) = 0;
    virtual void operator *=(const INum&) = 0;
};
class IShow {
public:
    virtual std::string show() const = 0;
};
class IAnyNum: public INum, public IShow {};

// generics/compile-time ad-hoc "type classes"
// T: type of the number
// C: container of the number
template <typename T, typename C> class Num {
    typedef Num<T, C> ThisNum;

protected:
    int _val;

public:
    Num(int val) { _val = val; }

    // default implementation should work for int/double/etc.
public:
    static C add(const ThisNum& a, const ThisNum& b) { return C(a._val + b._val); }
    static C mul(const ThisNum& a, const ThisNum& b) { return C(a._val * b._val); }
    static C abs(const ThisNum& a) { return C(std::abs(a._val)); }
    static C negate(const ThisNum& a) { return C(std::negate(a._val)); }
    static C signum(const ThisNum& a) { return C(T(std::signbit(a._val))); }
    static C fromInteger(T x) { return C(x); }
};

// NOTE:
// We cannot do existential type in C++, so this AnyNum is parameterized by T
template <typename T> class AnyNum
    : public IAnyNum
    , public Num<T, AnyNum<T>>
{
public:
    AnyNum<T> (T val): Num<T, AnyNum<T>>(val) {}

public:
    // INum
    void operator +=(const INum& b) override {
        this->_val += dynamic_cast<const AnyNum<T>&>(b)._val;
    }
    void operator *=(const INum& b) override {
        this->_val *= dynamic_cast<const AnyNum<T>&>(b)._val;
    }
    /// IShow
    std::string show() const override {
        return std::to_string(this->_val);
    }
};

template<typename T> AnyNum<T> operator +(const AnyNum<T>& a, const AnyNum<T>& b) {
    return AnyNum<T>::add(a, b);
}
template<typename T> AnyNum<T> operator *(const AnyNum<T>& a, const AnyNum<T>& b) {
    return AnyNum<T>::mul(a, b);
}

int main() {
    AnyNum<int> a(1);
    AnyNum<double> b(2);
    AnyNum<int> c(3);

    std::cout << "a = " << a.show() << std::endl;
    std::cout << "b = " << b.show() << std::endl;
    std::cout << "c = " << c.show() << std::endl;

    // using generics and compile-time polymorphism
    std::cout << "a + c = " << (a + c).show() << std::endl;
    std::cout << "a * c = " << (a * c).show() << std::endl;

    // run-time polymorphism
    //   failure with RTTI-check
    try {
        static_cast<INum&>(a) += static_cast<const INum&>(b);
    } catch(std::exception& e) {
        std::cout << "a + b = error: " << e.what() << std::endl;
    }
    //   successful with RTT-check
    static_cast<INum&>(a) += static_cast<const INum&>(c);
    std::cout << "a += c; a = " << a.show() << std::endl;
    //   Show interface
    std::vector<const IShow*> nums{&a, &b, &c};
    for (auto i: nums) std::cout << i->show() << ", ";
    std::cout << std::endl;

    return 0;
}

/*
Expected output:

a = 1
b = 2
c = 3
a + c = 4
a * c = 3
a + b = error: std::bad_cast
a += c; a = 4
4, 2, 3,
*/
