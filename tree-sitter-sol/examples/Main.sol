// Natural numbers
Nat : * = (n : *) -> (n -> n) -> n

Succ (prev : Nat) : Nat = \n succ# zero# =>
  succ# (prev n succ# zero#) zero#

Zero : Nat = \n succ# zero# => zero#

// Maybe definition
Maybe (t : *) = (a : *) -> (t -> a) a -> a

Just (value : a) : Maybe a = \t just# nothing# =>
  just# value

Nothing : Maybe a = \t just# nothing# =>
  nothing#

Maybe.unwrap (maybe : Maybe a) : a =
  maybe // match maybe with
    (\value => value) // Just a  => a
    sorry             // Nothing => sorry
