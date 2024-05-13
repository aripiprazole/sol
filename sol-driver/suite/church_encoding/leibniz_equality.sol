Eq : {a : U} -> a -> a -> U
Eq = |a x y| (p : a -> U) -> p x -> p y

Eq.refl : {a : U} -> {x : a} -> Eq a a
Eq.refl = |a px| px

Test_Eq_Int64_10_10 : Eq 10 10
Test_Eq_Int64_10_10 = Eq.refl
