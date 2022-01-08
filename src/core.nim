import strutils, rdstdin, tables, times, sequtils, types, printer, reader

type MalError* = object of Exception
    t*: MalType

proc pr_str(x_s: varargs[MalType]): MalType =
    str(x_s.map(proc(x: MalType): string = x.pr_str(true)).join(" "))

proc do_str(x_s: varargs[MalType]): MalType =
    str(x_s.map(proc(x: MalType): string = x.pr_str(false)).join(""))

proc prn(x_s: varargs[MalType]): MalType =
    echo(x_s.map(proc(x: MalType): string = x.pr_str(true)).join(" "))
    result = nilObj

proc println(x_s: varargs[MalType]): MalType =
    echo(x_s.map(proc(x: MalType): string = x.pr_str(false)).join(" "))
    result = nilObj

proc read_str(x_s: varargs[MalType]): MalType =
    read_str(x_s[0].str)

proc readline(x_s: varargs[MalType]): MalType =
    str(readLineFromStdin(x_s[0].str))

proc slurp(x_s: varargs[MalType]): MalType =
    str(readFile(x_s[0].str))

proc cons(x_s: varargs[MalType]): MalType =
    result = list(x_s[0])
    for x in x_s[1].list:
        result.list.add(x)

proc concat(x_s: varargs[MalType]): MalType =
    result = list()
    for x in x_s:
        for e in x.list:
            result.list.add(e)

proc vec(x_s: varargs[MalType]): MalType =
    result = MalType(kind: Vector, list: newSeq[MalType](len(x_s[0].list)))
    for i, x in x_s[0].list:
        result.list[i] = x

proc nth(x_s: varargs[MalType]): MalType =
    if x_s[1].number < len(x_s[0].list):
        return x_s[0].list[x_s[1].number]
    else:
        raise newException(ValueError, "nth: index out of range")

proc first(x_s: varargs[MalType]): MalType =
    if x_s[0].kind in {List, Vector} and len(x_s[0].list) > 0:
        return x_s[0].list[0]
    else:
        return nilObj

proc rest(x_s: varargs[MalType]): MalType =
    if x_s[0].kind in {List, Vector} and len(x_s[0].list) > 0:
        return list(x_s[0].list[1..^1])
    else:
        return list()

proc throw(x_s: varargs[MalType]): MalType =
    raise (ref MalError)(t: list(x_s))

proc assoc(x_s: varargs[MalType]): MalType =
    result = hash_map()
    result.hash_map = x_s[0].hash_map
    for i in countup(1, x_s.high, 2):
        result.hash_map[x_s[i].str] = x_s[i+1]

proc dissoc(x_s: varargs[MalType]): MalType =
    result = hash_map()
    result.hash_map = x_s[0].hash_map
    for i in 1..x_s.high:
        if result.hash_map.hasKey(x_s[i].str):
            result.hash_map.del(x_s[i].str)

proc get(x_s: varargs[MalType]): MalType =
    if x_s[0].kind == HashMap:
        if x_s[1].str in x_s[0].hash_map:
            result = x_s[0].hash_map[x_s[1].str]
        if not result.isNil:
            return result
    else:
        return nilObj

proc contains_q(x_s: varargs[MalType]): MalType =
    boolObj(x_s[0].hash_map.hasKey(x_s[1].str))

proc keys(x_s: varargs[MalType]): MalType =
    result = list()
    for k in x_s[0].hash_map.keys():
        result.list.add(str(k))

proc vals(x_s: varargs[MalType]): MalType =
    result = list()
    for v in x_s[0].hash_map.values():
        result.list.add(v)

proc apply(x_s: varargs[MalType]): MalType =
    var s = newSeq[MalType]()
    if len(x_s) > 2:
        for j in 1..<x_s.high:
            s.add(x_s[j])
    s.add(x_s[x_s.high].list)
    result = x_s[0].getFun()(s)

proc map(x_s: varargs[MalType]): MalType =
    result = list()
    for i in 0..x_s[1].list.high:
        result.list.add(x_s[0].getFun()(list(x_s[1].list[i])))

proc conj(x_s: varargs[MalType]): MalType =
    if x_s[0].kind == List:
        result = list()
        for i in countdown(x_s.high, 1):
            result.list.add(x_s[i])
        result.list.add(x_s[0].list)
    else:
        result = vector()
        result.list.add(x_s[0].list)
        for i in 1..x_s.high:
            result.list.add(x_s[i])
    result.meta = x_s[0].meta

proc seq(x_s: varargs[MalType]): MalType =
    if x_s[0].kind == List:
        if len(x_s[0].list) == 0:
            return nilObj
        result = x_s[0]
    elif x_s[0].kind == Vector:
        if len(x_s[0].list) == 0:
            return nilObj
        result = list()
        result.list.add(x_s[0].list)
    elif x_s[0].kind == String:
        if len(x_s[0].str) == 0:
            return nilObj
        result = list()
        for i in 0..x_s[0].str.high:
            result.list.add(str(x_s[0].str.substr(i, i)))
    elif x_s[0] == nilObj:
        result = nilObj
    else:
        raise newException(ValueError, "seq: called on non-sequence")

proc with_meta(x_s: varargs[MalType]): MalType =
    new result
    result[] = x_s[0][]
    result.meta = x_s[1]

proc meta(x_s: varargs[MalType]): MalType =
    result = if not x_s[0].meta.isNil: x_s[0].meta else: nilObj

proc deref(x_s: varargs[MalType]): MalType = x_s[0].val

proc reset_bang(x_s: varargs[MalType]): MalType =
    x_s[0].val = x_s[1]
    result = x_s[0].val

proc swap_bang(x_s: varargs[MalType]): MalType =
    var args = @[x_s[0].val]
    for i in 2..x_s.high:
        args.add(x_s[i])
    x_s[0].val = x_s[1].getFun()(args)
    result = x_s[0].val

proc time_ms(x_s: varargs[MalType]): MalType =
    return number(int(epochTime()*1000))

template wrapNumberFun(op): untyped =
    fun proc(x_s: varargs[MalType]): MalType =
        result = number(op(x_s[0].number, x_s[1].number))

template wrapBoolFun(op): untyped =
    fun proc(x_s: varargs[MalType]): MalType =
        result = boolObj(op(x_s[0].number, x_s[1].number))

let ns* = {
    "+": wrapNumberFun(`+`),
    "-": wrapNumberFun(`-`),
    "*": wrapNumberFun(`*`),
    "/": wrapNumberFun(`div`),

    "<": wrapBoolFun(`<`),
    "<=": wrapBoolFun(`<=`),
    ">": wrapBoolFun(`>`),
    ">=": wrapBoolFun(`>=`),

    "list": fun list,
    "list?": fun list_q,
    "vector": fun vector,
    "vector?": fun vector_q,
    "hash-map": fun hash_map,
    "map?": fun hash_map_q,
    "empty?": fun empty_q,
    "assoc": fun assoc,
    "dissoc": fun dissoc,
    "get": fun get,
    "contains?": fun contains_q,
    "keys": fun keys,
    "vals": fun vals,

    "=": fun equal,

    "pr-str": fun pr_str,
    "str": fun do_str,
    "prn": fun prn,
    "println": fun println,
    "read-string": fun read_str,
    "slurp": fun slurp,
    "readline": fun readline,

    "sequential?": fun seq_q,
    "cons": fun cons,
    "concat": fun concat,
    "nth": fun nth,
    "first": fun first,
    "rest": fun rest,
    "vec": fun vec,
    "apply": fun apply,
    "map": fun map,

    "conj": fun conj,
    "seq": fun seq,

    "throw": fun throw,

    "nil?": fun nil_q,
    "true?": fun true_q,
    "false?": fun false_q,
    "string?": fun string_q,
    "symbol": fun symbol,
    "symbol?": fun symbol_q,
    "keyword": fun keyword,
    "keyword?": fun keyword_q,
    "number?": fun number_q,
    "fn?": fun fn_q,
    "macro?": fun macro_q,

    "with-meta": fun with_meta,
    "meta": fun meta,
    "atom": fun atom,
    "atom?": fun atom_q,
    "deref": fun deref,
    "reset!": fun reset_bang,
    "swap!": fun swap_bang,

    "time-ms": fun time_ms,
}