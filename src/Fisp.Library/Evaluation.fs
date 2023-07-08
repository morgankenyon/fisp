namespace Fisp.Library

module Evaluation =
    open Objs

    type PrefixCase =
    | Twoary
    | Nary
    | Unsupported


    // let evalIntegerPrefixes operator results =
    //     let mutable running = 
    //         match operator with
    //         | "+" -> 0
    //         | "-" -> 0
    //         | "*" -> 1
    //         | "/" -> 1
    //         | _ -> 0 //return error instead of 0 since we don't want bad operators

    //     let mutable isFirstLoop = true
    //     for re in results do 
    //         match operator, re with 
    //         | "+", Int32Obj num ->
    //             running <- running + num.value
    //             ()
    //         | "*", Int32Obj num ->
    //             running <- running * num.value
    //         | "-", Int32Obj num ->
    //             if isFirstLoop then
    //                 running <- num.value
    //             else
    //                 running <- running - num.value
    //         | "/", Int32Obj num ->
    //             if isFirstLoop then
    //                 running <- num.value
    //             else
    //                 running <- running / num.value
    //         | _ -> ()

    //         isFirstLoop <- false //hacky mechanism
        
    //     Int32Obj { value = running }

    // let evalDoublePrefixes op results =
    //     let mutable running = 
    //         match op with
    //         | "+" -> 0.0
    //         | "-" -> 0.0
    //         | "*" -> 1.0
    //         | "/" -> 1.0
    //         | _ -> 0.0 //return error instead of 0 since we don't want bad operators

    //     let firstMinusArg isFirst run num  =
    //         if isFirst then
    //             num
    //         else
    //             run - num

    //     let firstDivideArg isFirst run num =
    //         if isFirst then
    //             num
    //         else
    //             run / num

    //     let mutable isFirstLoop = true
    //     for re in results do 
    //         match op, re with 
    //         | "+", DoubleObj num ->
    //             running <- running + num.value
    //         | "+", Int32Obj num ->
    //             running <- running + (double)num.value
    //         | "*", DoubleObj num ->
    //             running <- running * num.value
    //         | "*", Int32Obj num ->
    //             running <- running * (double)num.value
    //         | "-", DoubleObj num -> 
    //             running <- firstMinusArg isFirstLoop running num.value
    //         | "-", Int32Obj num -> 
    //             let dblNum = (double) num.value
    //             running <- firstMinusArg isFirstLoop running dblNum
    //         | "/", DoubleObj num -> 
    //             running <- firstDivideArg isFirstLoop running num.value
    //         | "/", Int32Obj num -> 
    //             let dblNum = (double) num.value
    //             running <- firstDivideArg isFirstLoop running dblNum
    //         | _ -> ()

    //         isFirstLoop <- false //hacky mechanism
        
    //     DoubleObj { value = running }

    let rec eval (expr: Ast.AstExpr) =
        match expr with
        | Ast.Int32 i32 ->
            Int32Obj { value = i32.value }
        | Ast.Double dbl ->
            DoubleObj { value = dbl.value }
        | Ast.Boolean bol ->
            BoolObj { value = bol.value }
        | Ast.String str ->
            StrObj { value = str.value }
        | Ast.PrefixExpr pe ->
            evalPrefixExpr pe
        | Ast.Program program ->
            evalProgram program
    
    and evalPrefixExpr (pe: Ast.PrefixExpression) =
        let supportedOps =
            match pe.operator with
            | "+" -> true
            | "-" -> true
            | "*" -> true
            | "/" -> true
            | ">" -> true
            | "<" -> true
            | _ -> false

        match supportedOps with
        | true -> evalPrefixExpression pe
        | false -> 
            let errorMsg = sprintf "%s is an unsupported operator" pe.operator
            ErrorObj { msg = errorMsg }
    
    and evalPrefixExpression (pe: Ast.PrefixExpression) =
        let firstResult = eval pe.firstValue
        let secondResult = eval pe.secondValue

        match pe.operator, firstResult, secondResult with
        | "+", Int32Obj first, Int32Obj second ->
            Int32Obj { value = first.value + second.value }
        | "+", DoubleObj first, DoubleObj second ->
            DoubleObj { value = first.value + second.value }
        | "-", Int32Obj first, Int32Obj second ->
            Int32Obj { value = first.value - second.value }
        | "-", DoubleObj first, DoubleObj second ->
            DoubleObj { value = first.value - second.value }
        | "/", Int32Obj first, Int32Obj second ->
            if second.value = 0 then
                ErrorObj { msg = "Error: Cannot divide by 0." }
            else
                Int32Obj { value = first.value / second.value }
        | "/", DoubleObj first, DoubleObj second ->
            if second.value = 0 then
                ErrorObj { msg = "Error: Cannot divide by 0." }
            else
                DoubleObj { value = first.value / second.value }
        | "*", Int32Obj first, Int32Obj second ->
            Int32Obj { value = first.value * second.value }
        | "*", DoubleObj first, DoubleObj second ->
            DoubleObj { value = first.value * second.value }
        | "<", Int32Obj first, Int32Obj second ->
            BoolObj { value = first.value < second.value }
        | "<", DoubleObj first, DoubleObj second ->
            BoolObj { value = first.value < second.value }
        | ">", Int32Obj first, Int32Obj second ->
            BoolObj { value = first.value > second.value }
        | ">", DoubleObj first, DoubleObj second ->
            BoolObj { value = first.value > second.value }
        | _, _, _ -> ErrorObj { msg = "Bad operator and/or type for prefix expression" }

    
    and evalProgram program =
        //this is junky, find a better way to only return the last one
        let results = new ResizeArray<Objs.Objects>()

        //if error, add early return step, check monkey
        for expr in program.expressions do 
            results.Add(eval expr)
            ()
        
        if results.Count > 1 then
            results.[results.Count - 1]
        else if results.Count = 1 then
            results.[0]
        else
            ErrorObj { msg = "Program did not produce any evaluated expressions" }

    let evaluate (expr: Ast.AstExpr) =
        //will add environment at some point
        eval expr
