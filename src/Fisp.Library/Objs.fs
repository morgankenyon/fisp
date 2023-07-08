namespace Fisp.Library

module Objs =

    type Integer32 =
        {
            value: int32
        }
    type Double =
        {
            value: double
        }
    type Boolean =
        {
            value: bool
        }
    type Error =
        {
            msg: string
        }
    type String =
        {
            value: string
        }
    and Objects =
    | Int32Obj of Integer32
    | DoubleObj of Double
    | BoolObj of Boolean
    | ErrorObj of Error
    | StrObj of String

    let printObj (obj: Objects) =
        match obj with
        | Int32Obj int ->
            sprintf "%d" int.value
        | DoubleObj dbl ->
            sprintf "%f" dbl.value
        | BoolObj bol ->
            if bol.value then "#t" else "#f"
        | StrObj str ->
            str.value
        | ErrorObj err ->
            err.msg