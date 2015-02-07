(testing "(vector N)"
  given
    N is 3
    V is (vector N)
  verify
    V satisfies vector?
    (limit V) is N)

(testing "(<-vector V N)"
  given V is (vector-> (vector 2) 1 a-previously-stored-value)
  verify
    (<-vector V 1) is a-previously-stored-value
    (<-vector V -1) raises an error
    (<-vector V 0) raises "cannot access 0th element of a vector"
    (<-vector V 2) raises "vector element not found"
    (<-vector V 3) raises an error)

(define cn3
  X Y Z -> (cn X (cn Y Z)))

(testing "function application"
  verify
    (cn3 "a" "b" "c") is "abc"
    ((cn3 "a" "b") "c") is "abc"
    ((cn3 "a") "b" "c") is "abc"
    (((cn3 "a") "b") "c") is "abc")

(testing "abstraction application"
  given F is (/. X Y Z (cn X (cn Y Z)))
  verify
    (F "a" "b" "c") is "abc"
    ((F "a" "b") "c") is "abc"
    ((F "a") "b" "c") is "abc"
    (((F "a") "b") "c") is "abc")
