//! Represents a natural number with a value of 0 or more, it does use
//! the Peano representation
inductive Nat {
  Zero : Nat,
  Succ : Nat -> Nat;
}

//! Represents a nullable value
inductive Maybe a {
  Just    : a -> Maybe a,
  Nothing : Maybe a;
}

`+ : Nat -> Nat -> Nat
`+ (Succ a) b = Succ (a + b)
`+ Zero b = b

`|> : a -> (a -> b) -> b
`|> a f = f a

error : String -> a = sorry

Maybe.unwrap : Maybe a -> a
Maybe.unwrap (Just a) = a
Maybe.unwrap Nothing = error "Cannot unwrap Nothing"

println : a -> IO ()
println a = println (show a) |> IO.println

Main {
  let message = "Hello world"
  let value = Just "Jose"
  println (Maybe.unwrap value)
  println message
}
