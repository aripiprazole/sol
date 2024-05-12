// Natural numbers
Nat : U
Nat = (n : U) -> (n -> n) -> n

Succ : Nat -> Nat
Succ = |prev n succ$ zero$| succ$ (prev n succ$ zero$) zero$

Zero : Nat
Zero = |n succ$ zero$| zero$

// Maybe definition
Maybe : U -> U
Maybe = |t| (a : U) -> (t -> a) -> a -> a

Just : 'a -> Maybe 'a
Just = |value t just$ nothing$| just$ value

Nothing : Maybe 'a
Nothing = |t just$ nothing$| nothing$

Maybe.unwrap : Maybe -> 'a
Maybe.unwrap = |maybe|
  maybe $
    (|value| value) $
    sorry
