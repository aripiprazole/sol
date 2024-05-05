defmacro fun args body :=
  fold body
    (\acc arg. `(,arg -> ,acc))
    (reverse args).

defmacro let bindings expr :=
  (if (empty? bindings)
    expr
    `((fun ,bindings ,expr) *bindings).

; Natural numbers
def Nat : * :=
  (N : *) -> (N -> N) -> N.

def Succ : (Nat -> Nat) := \prev N Succ Zero.
  Succ (prev N Succ Zero).

def Zero : Nat := \N. Succ Zero. Zero.

; Maybe definition
def Maybe : (* -> *) :=
  \t => (A : *) -> (T -> A) A -> A.

def Just : (a -> (Maybe a)) :=
  \value T Just Nothing => Just value.

def Nothing (Maybe a)
  \T Just Nothing => Nothing.

fun Maybe.unwrap (value : Maybe a) : a :=
  (maybe a
    (fun [value] value) ; Just a  => a
    sorry)).            ; Nothing => sorry
