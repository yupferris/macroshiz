open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let defMacro (name : string) (transform : Expr -> Expr) = ()
let useMacro (name : string) e = e

type Macro =
    {
        name : string;
        transform : Expr;
    }

let parseMacros e =
    let rec parseMacro acc = function
        | Call (_, m, exprList) when m.Name = "defMacro" ->
            let n =
                match exprList.Head with
                | Value (v, _) -> v.ToString()
                | _ -> failwith "fuck"
            let t =
                match exprList.Tail.Head with
                | Lambda (v, expr) -> Expr.Lambda(v, expr)
                | _ -> failwith "nope"
            { name = n; transform = t; } :: acc
        | Call (_, m, exprList) when m.Name = "useMacro" -> acc
        | Sequential (expr1, expr2) -> acc @ (parseMacro [] expr1) @ (parseMacro [] expr2)
        | Let (var, expr1, expr2) -> parseMacro acc expr2
        | x -> failwith (sprintf "WARNING: Unrecognized expression: %s" (x.ToString()))
    parseMacro [] e

type EnvVar =
    {
        name : string;
        value : obj;
    }

let EnvVar name value = { name = name; value = value; }

let rec eval env e =
    let evalList l = Array.ofList (List.map (eval env) l)
    let evalInstance i = function
        | Some x ->
            match eval env x with
            | null -> raise (System.NullReferenceException())
            | x -> x
        | None -> null

    match e with
    | Value (v, _) -> v
    | Var (v) ->
        match List.tryFind (fun x -> x.name = v.Name) env with
        | Some x -> x.value
        | x -> failwith (sprintf "Could not find variable: %s" (x.ToString()))
    | Call (_, m, exprList) when m.Name = "defMacro" -> null
    | Call (_, m, exprList) -> m.Invoke(null, evalList exprList)
    | Sequential (expr1, expr2) ->
        ignore (eval env expr1)
        eval env expr2
    | Lambda (v, b) ->
        let t = FSharpType.MakeFunctionType(v.Type, b.Type)
        let impl : obj -> obj =
            fun a ->
                let env' = (EnvVar v.Name a) :: env
                eval env' b
        FSharpValue.MakeFunction(t, impl)
    | Application (l, a) ->
        let l' = eval env l
        let lType = l'.GetType()
        let aType = a.Type
        let a' = eval env a
        let m = lType.GetMethod("Invoke", BindingFlags.Instance ||| BindingFlags.Public, null, [|aType|], null)
        m.Invoke(l', [|a'|])
    | Coerce (target, t) -> eval env target
    | NewObject (c, exprList) -> c.Invoke(evalList exprList)
    | Let (v, a, b) ->
        let env' = (EnvVar v.Name (eval env a)) :: env
        eval env' b
    | NewUnionCase (c, a) -> FSharpValue.MakeUnion(c, evalList a)
    | PropertyGet (i, p, a) -> p.GetValue(evalInstance env i, evalList a)
    | x -> failwith (sprintf "Unrecognized expression: %s" (x.ToString()))

let rec applyMacros macros = function
    | Call (_, m, exprList) when m.Name = "defMacro" -> Expr.Value null
    | Call (_, m, exprList) when m.Name = "useMacro" ->
        let n =
            match exprList.Head with
            | Value (v, _) -> v.ToString()
            | _ -> failwith "fuck"
        match List.tryFind (fun (x : Macro) -> x.name = n) macros with
        | Some x ->
            let e' = exprList.Tail.Head
            let l = eval [] x.transform
            let lt = l.GetType()
            let m = lt.GetMethod("Invoke", BindingFlags.Instance ||| BindingFlags.Public, null, [|typeof<Expr>|], null)
            m.Invoke(l, [|e'|]) :?> Expr
        | x -> failwith (sprintf "Unrecognized macro: %s" n)
    | Sequential (expr1, expr2) -> Expr.Sequential(applyMacros macros expr1, applyMacros macros expr2)
    | x -> failwith (sprintf "Unrecognized expression: %s" (x.ToString()))

let parseAndApplyMacros e = applyMacros (parseMacros e) e

let mi = typeof<string>.GetMethod("Concat", BindingFlags.Static ||| BindingFlags.Public, null, [|typeof<string>; typeof<string>|], null)

let e =
    <@
        defMacro
            "Chris Sucks Macro"
            (fun e ->
                let a = Var.Global("a", typeof<string>)
                let v = Expr.Value(" sucks")
                //let m = typeof<string>.GetMethod("Concat", BindingFlags.Static ||| BindingFlags.Public, null, [|typeof<string>; typeof<string>|], null)
                Expr.Let(a, v,
                    (Expr.Call ((*m*)mi, [e; Expr.Var a]))))
        useMacro "Chris Sucks Macro" "Chris"
    @>

let p = parseAndApplyMacros e
eval [] p
