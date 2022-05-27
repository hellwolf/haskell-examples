Subject: Should there be a haskell template for inclusion polymorphism in Haskell?

Hi Haskellers!

Here are some of my thoughts about polymorphism in Haskell especially the inclusion polymorphism, with questions in the
end.

~IGNORE THESE LHS BOILER PLATES~

> {-# LANGUAGE GADTs            #-}
> module Main where
> import Unsafe.Coerce ( unsafeCoerce )

~IGNORE THESE LHS BOILER PLATES~

* Classification of Polymorphism

Let's assume that we are following the classification of polymorphism where there are four types of it:

?? Btw, does anyone have a good citation of this classification? I got it from all over the Internet instead:
e.g https://www.tutorialspoint.com/types-of-polymorphisms-ad-hoc-inclusion-parametric-and-coercion

** Overloading Polymorphism

It allows function with same name to act in different manner for different types.

** Coercion Polymorphism

Also called as casting sometimes.

** Inclusion Polymorphism

Also called as subtyping. This allows to point derived classes using base class pointers and references.

** Parametric Polymorphism

Also called early Binding, it allows to use same piece of code for different types. We can get it by using templates.

** Sub Classifications

In terms of being ad-hoc or universal:

- Overloading and Coercion are ad-hoc,
- Inclusion and Parametric are universal.

In terms of being compile-time vs run-time:

- Coercion, Overloading, Parametric are all compile-time polymorphism,
- while only Inclusion is run-time polymorphism.

* Different Polymorphism in Haskell

In terms of their counter parties in Haskell language:

** Overloading Polymorphismin Haskell

I think type classes basically provides such overloading ad-hoc polymorphism.

Some thinks so too: https://stackoverflow.com/questions/6636107/how-does-haskell-handle-overloading-polymorphism

> class Fooable a where
>     foo :: a -> Int
> instance Fooable Int where
>     foo = id
> instance Fooable Bool where
>     foo _ = 42

** Coercion Polymorphism in Haskell

I think Haskell has both `Coercible` & `unsafeCoerce` for handling representational equality in compile time.

But I am not sure if c/c++ style of conercion between similar types such as int/float/double is something Haskell would
want to inject these magic conversion code ever.

** Parametric Polymorphism in Haskell

This is probably what Haskell is best at the most, demonstrated by the classic `map` function definition:

> map' :: (a -> b) -> [a] -> [b]
> map' _ []     = []
> map' f (x:xs) = f x : map' f xs

Note that, one could also combine parametric and overloading together using constraints:

> double :: Num a => a -> a
> double x = x + x

This `double` function is parametric, but constrained only to work with Num type classes, hence their implementations
are overloaded.

** Inclusion Polymorphism in Haskell

Ok, finally inclusion polymorphism is what I want to focus on and have some questions here.

Before that, I do want to mention one observation, inclusion polymorphism probably is most likely the old OOP habits
that die hard. For me at least, since I consider myself a recovering OOP run-time polymorphism addict, and my guess is
that unless the use case do need run-time polymorphism, e.g. run-time execution layer abstration, it is often better off
using other type of polymorphism to achieve the same result. And even Bjarne Stroustrup is flirting with the idea:
[[https://www.youtube.com/watch?v=xcpSLRpOMJM][Bjarne Stroustrup - Object Oriented Programming without Inheritance - ECOOP 2015]]

Let's think of translating this piece of pseudo OOP style code into haskell.

```
interface Num {
  Num (+)(Num a, Num b);
  Num (*)(Num a, Num b);
  Num abs(Num a);
  Num negate(Num a);
  Num signum(Num a);
  Num fromInteger(Integer);
}

interface Show {
  String show(Show a);
}

class AnyNum<T> implements Num, Show {
  // trivially implements Num Show interfaces
}

// Now we can use AnyNum<Int>, AnyNum<Double>, etc. through their Num or Show interfaces.
```

In Haskell though, thanks to the ability of having existential type, AnyNum is actually quite nice to write:

> data AnyNum where
>   MkAnyNum :: (Num a, Show a) => a -> AnyNum

But in order to use AnyNum like using interfaces in our pseudo code, we would have to write these boiler plates and in
some cases using unsafeCoerce due to not using Typeable run-time information:

> instance Num AnyNum where
>   (+) (MkAnyNum a) (MkAnyNum b) = MkAnyNum $ a + unsafeCoerce b
>   (*) (MkAnyNum a) (MkAnyNum b) = MkAnyNum $ a + unsafeCoerce b
>   abs (MkAnyNum a) = MkAnyNum . abs $ a
>   negate (MkAnyNum a) = MkAnyNum . negate $ a
>   signum (MkAnyNum a) = MkAnyNum . signum $ a
>   fromInteger = MkAnyNum . fromInteger
> instance Show AnyNum where
>   show (MkAnyNum a) = show a

Here is some test code that demonstrate how distressful unsafeCoerce is:

> main = do
>   let a = MkAnyNum(1 :: Int)
>   let b = MkAnyNum(2 :: Double)
>   let c = MkAnyNum(3 :: Int)
>   print $ show $ a + b
>   print $ show $ a + c

Outputs are:
```
λ>
"4611686018427387905"
"4"
```

Few observations:

- If there is at most one occurance of the usage of the type class, there is no need for the unsafeCoerce.
- Otherwise, unsafeCoerce is required, or otherwise those functions could be left to "undefined".

So my central questions for discussions are:

- Is inclusion polymorphism something we should use at all in Haskell?
- These boiler plate code maybe better be generated using Haskell template instead, is there one already, or should
  there be one?


Cheers,
Miao

----
Reply-to: Aaron-v

 > Hi Miao,
 >
 > interface Num {
 >   Num (+)(Num a, Num b);
 >   Num abs(Num a);
 >   Num fromInteger(Integer);
 >   ...
 > }
 >
 > I'm trying to follow through this and I'm afraid I'm not seeing how your example OOP interface would work.
 > What would the implementation of `AnyNum<Double>.abs` be?  It seems like it has no way of knowing whether the argument actually contains a Double or not?
 >
 > Wouldn't the `Num` interface instead need to be along the lines of the following?
 >
 > ```
 > interface Num<T> {
 >   T (+)(T a, T b);
 >   T abs(T a);
 >   T fromInteger(Integer);
 > }
 > ```
 >
 > or possibly
 >
 > ```
 > interface Num<T> {
 >   Num<T> (+)(Num<T> a, Num<T> b);
 >   Num<T> abs(Num<T> a);
 >   Num<T> fromInteger(Integer);
 > }
 > ```

Hi Aaron,

You are right, I was a bit rushing in the end, and I should write a proper example instead of pseudo code. Here is a
rewrite in actual C++: https://github.com/hellwolf/haskell-examples/blob/master/2022-05-25-inclusion-polymorphism/AnyNum.cpp

While re-writing, it's clear that you cannot fit Num class in OOP, in C++ it's rather using generics

```c++
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
```

This looks very much similar to the Haskell `Num` type class.

And the OOP-style stuff for Num and Show could rather be:

```c++
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
```

So the question back to can we express the += and *= inclusion polymorphism for Haskell data types.

It seems to me that sub-typing libraries Olaf suggested could be something to look into.

 >
 > I'm thinking that the OOP version for your original Num interface would have the same unsafe casting issues that your haskell translation has.
 > Or could you add an example of how `AnyNum<Double>.abs` would be implemented in the OOP version?

Yes, indeed, without using `Typeable`, there is unsafe casting issue. In the C++ version I fixed, it uses dynamic_cast,
which is basically using RTTI (similar to Haskell Typeable) to throw a run-time error if mismatched.

Also you were right, there is no sensible abs for the OOP version. The OOP Num should be "object-oriented" ones such as
"+=", "*=", etc.
