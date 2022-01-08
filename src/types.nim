import tables

type 
    MalTypeKind* = enum 
        Nil,
        True,
        False,
        Number,
        Symbol,
        String,
        List,
        Vector,
        HashMap,
        Fun,
        MalFun,
        Atom

    FunType = proc(a: varargs[MalType]): MalType

    MalFunType* = ref object
        fn*: FunType
        ast*: MalType
        params*: MalType
        env*: Env
        is_macro*: bool
    
    MalType* = ref object
        meta*: MalType
        case kind*: MalTypeKind
            of Nil, True, False:
                nil
            of Number:
                number*: int
            of String, Symbol:
                str*: string
            of List, Vector:
                list*: seq[MalType]
            of HashMap:
                hash_map*: Table[string, MalType]
            of Fun:
                fun*: FunType
                is_macro*: bool
            of MalFun:
                malfun*: MalFunType
            of Atom:
                val*: MalType
    
    Env* = ref object
        data*: Table[string, MalType]
        outer*: Env


let nilObj* = MalType(kind: Nil)
let trueObj* = MalType(kind: True)
let falseObj* = MalType(kind: False)

proc number*(x: int): MalType = MalType(kind: Number, number: x)

proc symbol*(x: string): MalType = MalType(kind: Symbol, str: x)

proc str*(x: string): MalType = MalType(kind: String, str: x)

proc keyword*(x: string): MalType = MalType(kind: String, str: "\xff" & x)

proc atom*(x: MalType): MalType =
    result = MalType(kind: Atom)
    result.val = x

proc list*(x_s: varargs[MalType]): MalType {.procvar.} =
    result = MalType(kind: List, list: newSeq[MalType](len(x_s)))
    for i, x in x_s:
        result.list[i] = x

proc vector*(x_s: varargs[MalType]): MalType {.procvar.} =
    result = MalType(kind: Vector, list: newSeq[MalType](len(x_s)))
    for i, x in x_s:
        result.list[i] = x

proc hash_map*(x_s: varargs[MalType]): MalType {.procvar.} =
    result = MalType(kind: HashMap, hash_map: initTable[string, MalType]())
    for i in countup(0, x_s.high, 2):
        let s =
            case x_s[i].kind
                of String:
                    x_s[i].str
                else:
                    x_s[i].str
        result.hash_map[s] = x_s[i+1]

proc fun_is_macro*(x: MalType): bool =
    if x.kind != Fun and x.kind != MalFun:
        raise newException(ValueError, "No function")
    if x.kind == Fun:
        result = x.is_macro
    elif x.kind == MalFun:
        result = x.malfun.is_macro

proc getFun*(x: MalType): FunType =
    if x.kind != Fun and x.kind != MalFun:
        raise newException(ValueError, "No function")
    if x.kind == Fun:
        result = x.fun
    elif x.kind == MalFun:
        result = x.malfun.fn

proc fun*(x: proc(x_s: varargs[MalType]): MalType, is_macro = false): MalType =
    MalType(kind: Fun, fun: x, is_macro: is_macro)

proc malfun*(fn: auto, ast, params: MalType, env: Env, is_macro = false): MalType =
    MalType(kind: MalFun, malfun: MalFunType(fn: fn, ast: ast, params: params, env: env, is_macro: is_macro))

proc boolObj*(flag: bool): MalType =
    result = if flag: trueObj else: falseObj

proc list_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == List)

proc vector_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == Vector)

proc seq_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind in {List, Vector})

proc hash_map_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == Hashmap)

proc empty_q*(x_s: varargs[Maltype]): MalType {.procvar.} =
    return boolObj(len(x_s[0].list) == 0)

proc nil_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == Nil)

proc true_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == True)

proc false_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == False)

proc string_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == String and (len(x_s[0].str) == 0 or x_s[0].str[0] != '\xff'))

proc symbol*(x_s: varargs[MalType]): MalType {.procvar.} =
    symbol(x_s[0].str)

proc symbol_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == Symbol)

proc keyword*(x_s: varargs[MalType]): Maltype {.procvar.} =
    if len(x_s[0].str) > 0 and x_s[0].str[0] == '\xff':
        result = x_s[0]
    else:
        result = keyword(x_s[0].str)

proc keyword_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == String and (len(x_s[0].str) > 0 and x_s[0].str[0] == '\xff'))

proc number_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == Number)

proc fn_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj((x_s[0].kind == Fun or x_s[0].kind == MalFun) and not x_s[0].fun_is_macro)

proc macro_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj((x_s[0].kind == Fun or x_s[0].kind == MalFun) and x_s[0].fun_is_macro)

proc atom*(x_s: varargs[MalType]): MalType {.procvar.} =
    atom(x_s[0])

proc atom_q*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0].kind == Atom)

proc count*(x_s: varargs[MalType]): MalType {.procvar.} =
    result = number(
        if x_s[0].kind == Nil:
            0
        else:
            len(x_s[0].list)
    )

proc `==`*(x, y: MalType): bool =
    if not (x.kind in {List, Vector} and y.kind in {List, Vector}):
        if x.kind != y.kind:
            return false
    result = 
        case x.kind
            of Nil, True, False:
                true
            of Number:
                x.number == y.number
            of Symbol, String:
                x.str == y.str
            of List, Vector:
                x.list == y.list
            of HashMap:
                x.hash_map == y.hash_map
            of Fun:
                x.fun == y.fun and x.is_macro == y.is_macro
            of MalFun:
                x.malfun == y.malfun
            of Atom:
                x.val == y.val

proc equal*(x_s: varargs[MalType]): MalType {.procvar.} =
    return boolObj(x_s[0] == x_s[1])