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

Just : {A : U} -> A -> Maybe A
Just = |value t just$ nothing$| just$ value

Nothing : {A : U} -> Maybe A
Nothing = |t just$ nothing$| nothing$

Sorry : {A : U} -> A
Sorry = Sorry

Maybe.unwrap : {A : U} -> Maybe A -> A
Maybe.unwrap = |maybe|
  maybe (|value| value) Sorry
