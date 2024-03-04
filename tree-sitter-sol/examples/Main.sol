//! Represents a nullable value
@stdlib
data Maybe (a) {
    Just : a -> Maybe a,
    Nothing : Maybe a;
}

`+ : Int -> Int -> Int
`+ = sum

println : a -> IO ()
println a = return putStrLn (show a)

Main {
    let message = "Hello world"
    args <- getStdArgs
    println message
}
