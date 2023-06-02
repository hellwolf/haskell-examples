open import Haskell.Prelude
open import Relation.Nullary using (¬_)
open import Relation.Binary.PropositionalEquality using (_≢_)
open import Data.Product using (∃; ∃-syntax)

data _∈_ (x : a) : List a → Set where
  here  : ∀ {ys}            → x ∈ (x ∷ ys)
  there : ∀ {y ys} → x ∈ ys → x ∈ (y ∷ ys)

min-list-elem' : {{Ord a}} → a → List a → a
min-list-elem' x [] = x
min-list-elem' x (y ∷ ys) = min-list-elem' (if x < y then x else y) ys

min-list-elem'-correct : ∀ {{_ : Ord a}} (x : a) (xs : List a)
                       → ¬ (∃[ y ] (y ∈ xs → min-list-elem' y xs ≢ min-list-elem' x xs))
min-list-elem'-correct = {!!}
-- min-list-elem'-proof-exist x [] = here

min-list-elem : {{Ord a}} → List a → Maybe a
min-list-elem [] = Nothing
min-list-elem (x ∷ xs) = Just $ min-list-elem' x xs
