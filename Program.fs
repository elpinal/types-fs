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

let tests =
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

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests
