\\ ShenTest - a simple, expressive test framework for Shen programs
\\
\\ Examples:
\\
\\   (testing "(vector N)"
\\     given
\\       N is 3
\\       V is (vector N)
\\     verify
\\       V satisfies vector?
\\       (limit V) is N)
\\
\\   (testing "(<-vector V N)"
\\     given V is (vector-> (vector 2) 1 a-previously-stored-value)
\\     verify
\\       (<-vector V 1) is a-previously-stored-value
\\       (<-vector V -1) raises an error
\\       (<-vector V 0) raises "cannot access 0th element of a vector"
\\       (<-vector V 2) raises "vector element not found"
\\       (<-vector V 3) raises an error)

(package shen-test [testing given verify is raises an satisfies]

(defcc <spec>
  Description <given-assumptions> <verify-assertions> :=
    [evaluate-assertions Description
                         (supply-assumptions <given-assumptions>
                                             <verify-assertions>)
                         true];
  Description <verify-assertions> :=
    [evaluate-assertions Description <verify-assertions> true];)

(defcc <given-assumptions>
  given <assumptions> := <assumptions>;)

(defcc <assumptions>
  <assumption> <assumptions> := (append <assumption> <assumptions>);
  <assumption> := <assumption>;)

(defcc <assumption>
  Var is ValueExpr := [Var ValueExpr] where (variable? Var);)

(defcc <verify-assertions>
  verify <assertions> := <assertions>;)

(defcc <assertions>
  <assertion> <assertions> := [cons <assertion> <assertions>];
  <assertion> := [cons <assertion> []];)

(defcc <assertion>
  TestExpr raises an error
  := (let TestText (make-string "~S" TestExpr)
          TestVal (gensym (protect Var))
       [trap-error
        [let TestVal TestExpr
          [@p failure
              [make-string "~A to raise an error" TestText]
              [make-string "it succeeded with ~S" TestVal]]]
        [(intern "/.") _ pass]]);

  TestExpr raises Message
  := (let TestText (make-string "~S" TestExpr)
          TestVal (gensym (protect Var))
          Err (gensym (protect Var))
          ErrMsg (gensym (protect Var))
       [trap-error
        [let TestVal TestExpr
          [@p failure
              [make-string "~A to raise ~S" TestText Message]
              [make-string "it succeeded with ~S" TestVal]]]
        [(intern "/.") Err
            [let ErrMsg [error-to-string Err]
              [if [or [= ErrMsg Message]
                      [= ErrMsg [make-string "~A~%" Message]]]
                  pass
                  [@p failure
                      [make-string "~A to raise ~S" TestText Message]
                      [make-string "it raised ~S" ErrMsg]]]]]])
  where (string? Message);

  TestExpr is OtherExpr
  := (let TestText (make-string "~S" TestExpr)
          OtherText (make-string "~S" OtherExpr)
          TestVal (gensym (protect Var))
          OtherVal (gensym (protect Var))
          Err (gensym (protect Var))
       [trap-error [let TestVal (wrap-in-error-check TestExpr TestText)
                        OtherVal (wrap-in-error-check OtherExpr OtherText)
                     [if [= TestVal OtherVal]
                         pass
                         [@p failure
                             [make-string "~A to be ~A"
                                          TestText OtherText]
                             [make-string "it was ~S" TestVal]]]]
                   [(intern "/.") Err [@p error [error-to-string Err]]]]);

  TestExpr satisfies UnaryPredicate
  := (let TestText (make-string "~S" TestExpr)
          PredText (make-string "~A" [UnaryPredicate TestText])
          TestVal (gensym (protect Var))
          Err (gensym (protect Var))
       [trap-error [let TestVal (wrap-in-error-check TestExpr TestText)
                     [if (wrap-in-error-check [UnaryPredicate TestVal] PredText)
                         pass
                         [@p failure
                             [make-string "~A to satisfy ~A"
                                          TestText UnaryPredicate]
                             "it did not"]]]
                   [(intern "/.") Err [@p error [error-to-string Err]]]]);

  TestExpr BinaryPredicate OtherExpr
  := (let TestText (make-string "~S" TestExpr)
          OtherText (make-string "~S" OtherExpr)
          PredText (make-string "~A" [BinaryPredicate TestText OtherText])
          TestVal (gensym (protect Var))
          OtherVal (gensym (protect Var))
          Err (gensym (protect Var))
       [trap-error [let TestVal (wrap-in-error-check TestExpr TestText)
                        OtherVal (wrap-in-error-check OtherExpr OtherText)
                     [if (wrap-in-error-check [BinaryPredicate TestVal OtherVal]
                                              PredText)
                         pass
                         [@p failure
                             [make-string "~A to be ~A ~A"
                                          TestText BinaryPredicate OtherText]
                             "it was not"]]]
                   [(intern "/.") Err [@p error [error-to-string Err]]]])
  where (symbol? BinaryPredicate);)

(define wrap-in-error-check
  TestExpr TestText -> (let Err (gensym (protect Var))
                     [trap-error TestExpr
                                 [(intern "/.") Err
                                  [error "~A raised ~S" TestText
                                         [error-to-string Err]]]]))

(define supply-assumptions
  Bindings Assertions -> (map (/. Assertion [let | (append Bindings [Assertion])])
                              Assertions))

(define evaluate-assertions
  Description [] Result -> Result
  Description [Assertion | Assertions] Result ->
    (evaluate-assertions Description Assertions
                         (and (evaluate-assertion Description Assertion)
                              Result)))

(define evaluate-assertion
  Description Test -> (process-result Description (eval Test)))

(define process-result
  Description pass -> true
  Description (@p failure Expected Actual) ->
    (do (output "~%FAILED: ~A~%  Expected ~A,~%  but ~A.~%"
                Description Expected Actual)
        false)
  Description (@p error Error) ->
    (do (output "~%FAILED: ~A~%  Unexpected exception: ~A~%" Description Error)
        false))

(defmacro shen-test-testing
  [testing | Spec] -> (compile (function <spec>) Spec))

)
