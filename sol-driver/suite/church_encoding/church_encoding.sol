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

Just : {a : U} -> a -> Maybe a
Just = |value t just$ nothing$| just$ value

Nothing : {a : U} -> Maybe a
Nothing = |t just$ nothing$| nothing$

Sorry : {a : U} -> a
Sorry = Sorry

Maybe.unwrap : {a : U} -> Maybe a -> a
Maybe.unwrap = |maybe|
  maybe (|value| value) Sorry
