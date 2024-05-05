(defmacro fun [args body]
  (fold body
    (fn [acc arg] `(-> ,arg ,acc))
    (reverse args)))

(defmacro let [bindings expr]
  (if (empty? bindings)
    expr
    `(fun ,bindings ,expr))

; Natural numbers
(def Nat *
  (-> (N : *) (N -> N) N))

(def Succ (-> Nat Nat)
  (fun [prev N Succ Zero] (Succ (prev N Succ Zero))))

(def Zero Nat
  (fun [N Succ Zero] Zero))

; Maybe definition
(def Maybe (-> * *)
  (fun [T] (-> (A : *) (-> T A) A A)))

(def Just (-> a (Maybe a))
  (fun [value T Just Nothing] (Just value))

(def Nothing (Maybe a)
  (fun [T Just Nothing] Nothing))

(def Maybe.unwrap (-> (Maybe a) a
  (fun [maybe]
    (maybe a
      (fun [value] value) ; Just a -> a
      sorry)))            ; Nothing -> sorry
