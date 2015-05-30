#r @"C:\Projects\z3\build\Microsoft.Z3.dll"

type ArithmeticExpression =
    | Num of int
    | IntVar of string
    | Add of ArithmeticExpression * ArithmeticExpression
    | Mult of ArithmeticExpression * ArithmeticExpression

type BooleanExpression =
    | LessOrEqual of ArithmeticExpression * ArithmeticExpression
    | GreaterOrEqual of ArithmeticExpression * ArithmeticExpression
    | And of BooleanExpression * BooleanExpression
    | Or of BooleanExpression * BooleanExpression

type ArithmeticExpression with
    static member (+) (e1, e2) = Add (e1, e2)
    static member (*) (e1, e2) = Mult (e1, e2)
    static member (*) (x, e) = Mult (Num x, e)
    static member op_LessThanOrEqual (e1, e2) = LessOrEqual (e1, e2)
    static member (<=.) (e, x) = LessOrEqual (e, Num x)
    static member op_GreaterOrEqual (e1, e2) = GreaterOrEqual (e1, e2)
    static member (>=.) (e, x) = GreaterOrEqual (e, Num x)

type Problem =
    | Maximize of ArithmeticExpression * BooleanExpression list

type Result =
    | Satisfiable of string * (string * string) []
    | Unsatisfiable

module Z3 =
    open Microsoft.Z3

    let And be1 be2 = And (be1, be2)
    let all predicate = List.map predicate >> List.reduce And
    let maximize goal constraints = Maximize (goal, constraints)

    let solve (Maximize (objective, constraints)) =
        use ctx = new Context()

        let vars = System.Collections.Generic.Dictionary HashIdentity.Structural

        let rec evalArith = function
            | Num x -> ctx.MkInt x :> ArithExpr
            | IntVar name ->
                match vars.TryGetValue name with
                | false, _ ->
                    let var = ctx.MkIntConst name
                    vars.[name] <- var
                    var :> _
                | true, var ->
                    var :> _
            | Add (ae1, ae2) ->
                ctx.MkAdd (evalArith ae1, evalArith ae2)
            | Mult (ae1, ae2) ->
                ctx.MkMul (evalArith ae1, evalArith ae2)

        and evalBoolean = function
            | LessOrEqual (ae1, ae2) ->
                ctx.MkLe (evalArith ae1, evalArith ae2)
            | GreaterOrEqual (ae1, ae2) ->
                ctx.MkGe (evalArith ae1, evalArith ae2)
            | And (be1, be2) ->
                ctx.MkAnd (evalBoolean be1, evalBoolean be2)
            | Or (be1, be2) ->
                ctx.MkOr (evalBoolean be1, evalBoolean be2)
                
        let solver = ctx.MkOptimize()
        solver.Assert (constraints |> Seq.map evalBoolean |> Seq.toArray)

        let goal =
            evalArith objective
            |> solver.MkMaximize

        match solver.Check() with
        | Status.SATISFIABLE ->
            let model = solver.Model
            let result =
                model.Decls
                |> Array.map (fun f -> string f.Name, string (model.ConstInterp f))

            Satisfiable (string goal.Upper, result)
        | _ -> Unsatisfiable

[<AutoOpen>]
module Z3Operators =
    type Int = | Int
    
    let (?) Int name = IntVar name

Z3.maximize Int?x3 [
    Int?x1 >=. 1
    Int?x2 >=. 1
    Int?x1 + Int?x2 <=. 2
]
|> Z3.solve
    
let test =
    let prod1, prod2, prod3, prod4, prod5 =
        Int?prod1, Int?prod2, Int?prod3, Int?prod4, Int?prod5

    let profit = 550 * prod1 + 600 * prod2 + 350 * prod3 + 400 * prod4 + 200 * prod5

    let grinding = 12 * prod1 + 20 * prod2 + 25 * prod4 + 15 * prod5 <=. 3 * 16 * 6
    let drilling = 10 * prod1 + 8 * prod2 + 16 * prod3 <=. 2 * 16 * 6
    let labor = 20 * prod1 + 20 * prod2 + 20 * prod3 + 20 * prod4 + 20 * prod5 <=. 8 * 8 * 6

    let nonNegative =
        [prod1; prod2; prod3; prod4; prod5]
        |> Z3.all (fun x -> x >=. 0)

    Z3.maximize profit [
        grinding
        drilling
        labor
        nonNegative
    ]
    |> Z3.solve

type Stat =
    | AttackSpeed of int
    | Physique of int
    | Cunning of int
    | OffensiveAbility of int
    | DefensiveAbility of int
    | PercentPhysique of int
    | PercentCunning of int

type Location =
    | Head of Stat list
    | Ring of Stat list
    | Amulet of Stat list
    | Chest of Stat list
    | Shoulder of Stat list
    | Gloves of Stat list
    | Pants of Stat list
    | Boots of Stat list
    | Belt of Stat list
    | Medal of Stat list

let items =
    [
        Head [
            Physique 27
            Cunning 30
        ]
        Amulet [
            PercentPhysique 6
            OffensiveAbility 41
            DefensiveAbility 37
        ]
        Ring [
            Cunning 14
            PercentCunning 5
            AttackSpeed 5
        ]
        Ring [
            Physique 12
            Cunning 10
            OffensiveAbility 95
            DefensiveAbility 58
        ]
        Shoulder [
            Cunning 53
        ]
        Gloves [
            Cunning 14
            PercentCunning 6
            OffensiveAbility 17
        ]
        Boots [
            Physique 14
            Cunning 10
            PercentPhysique 3
        ]
        Medal [
            Physique 13
            Cunning 10
            PercentPhysique 6
        ]
        Belt [
            Physique 21
            Cunning 19
            PercentCunning 5
        ]
        Pants [
            Cunning 12
            PercentCunning 5
            OffensiveAbility 14
        ]
        Chest [
            Physique 36
            PercentCunning 4
        ]
    ]

let (MkInt basePhysique), (MkInt baseCunning), (MkInt basePercentPhysique) = 367, 661, 14
let (MkInt baseSpeed), (MkInt baseOffensive), (MkInt baseDefensive) = 125, (875 - 661 / 2), (728 - 367 / 2)

let variables =
    [
        "Attack Speed"
        "Cunning"
        "Physique"
        "Offensive Ability"
        "Defensive Ability"
        "% Physique"
        "% Cunning"
    ]
    |> Seq.collect (fun criteria ->
        [
            "ring1"
            "ring2"
            "amulet"
            "helm"
            "chest"
            "shoulder"
            "gloves"
            "pants"
            "boots"
            "belt"
            "medal"
        ]
        |> List.map (fun piece -> criteria, piece, ctx.MkIntConst (piece + "-" + criteria))
    )