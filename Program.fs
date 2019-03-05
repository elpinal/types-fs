open Expecto

type Term =
    | Var of int
    | Abs of Term
    | App of Term * Term

let rec shiftAbove c d t =
    match t with
        | Var n -> if c <= n then Var(n + d) else t
        | Abs t -> Abs (shiftAbove (c + 1) d t)
        | App (t1, t2) -> App(shiftAbove c d t1, shiftAbove c d t2)

let shift d = shiftAbove 0 d

let subst t j by =
    let rec aux c t =
        match t with
            | Var n -> if n = c + j then shift c by else t
            | Abs t -> Abs (aux (c + 1) t)
            | App (t1, t2) -> App(aux c t1, aux c t2)
    aux 0 t

let tests =
    testList "tests" [
        test "shift above" {
            Expect.sequenceEqual
                [ shiftAbove 0 10 (Var 1)
                ; shiftAbove 1 100 (Var 0)
                ; shiftAbove 0 10 (Abs (Var 1))
                ; shiftAbove 0 10 (Abs (Var 0))
                ; shiftAbove 0 0 (Var 0)
                ; shiftAbove 1 5 (App (Var 0, Abs (Var 2)))
                ; shiftAbove 3 (-1) (Abs (Abs (Var 4)))
                ; shiftAbove 3 (-1) (Abs (Abs (Var 5)))
                ]
                [ Var 11
                ; Var 0
                ; Abs (Var 11)
                ; Abs (Var 0)
                ; Var 0
                ; App (Var 0, Abs (Var 7))
                ; Abs (Abs (Var 4))
                ; Abs (Abs (Var 4))]
                "Indices under the first argument should be protected"
        }
        test "subst" {
            Expect.sequenceEqual
                [ subst (Var 0) 0 (Var 1)
                ; subst (Var 1) 0 (Var 88)
                ; subst (Var 3) 0 (Var 7)
                ; subst (Var 3) 3 (Var 7)
                ; subst (Abs (Var 0)) 0 (Var 7)
                ; subst (Abs (Var 1)) 0 (Var 7)
                ; subst (App (Abs (Var 7), Abs (Var 3))) 2 (App (Var 9, Abs (App (Var 0, Var 77))))
                ]
                [ Var 1
                ; Var 1
                ; Var 3
                ; Var 7
                ; Abs (Var 0)
                ; Abs (Var 8)
                ; App (Abs (Var 7), Abs (App (Var 10, Abs (App (Var 0, Var 78)))))
                ]
                "Substitution"
        }
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests
