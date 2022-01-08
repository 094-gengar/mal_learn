import options, re, strutils, types

let
    tokenRE = re"""[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"""
    intRE = re"-?[0-9]+$"
    strRE = re"""^"(?:\\.|[^\\"])*"$"""

type
    Blank* = object of Exception
    Reader = object
        tokens: seq[string]
        position: int


proc next(r: var Reader): Option[string] =
    if r.position >= len(r.tokens):
        discard
    else:
        let t = r.tokens[r.position]
        inc r.position
        result = t.some

proc peek(r: Reader): Option[string] =
    if r.position >= len(r.tokens):
        discard
    else:
        return r.tokens[r.position].some
    
proc tokenize(str: string): seq[string] =
    result = @[]
    var pos = 0
    while pos < len(str):
        var matches: array[2, string]
        var l = str.findBounds(tokenRE, matches, pos)
        if l.first != -1 and l.last != -1 and l.last >= l.first:
            pos = l.last + 1
            if len(matches[0]) > 0 and matches[0][0] != ';':
                result.add(matches[0])
        else:
            inc pos

proc read_from(r: var Reader): MalType

proc read_seq(r: var Reader, fr, to: string): seq[MalType] =
    result = @[]
    var t = r.next
    if t.get("") != fr:
        raise newException(ValueError, "expected '" & fr & "'")
    t = r.peek
    while t.get("") != to:
        if t.get("") == "":
            raise newException(ValueError, "expected '" & to & "', got EOF")
        result.add(r.read_from)
        t = r.peek
    discard r.next

proc read_list(r: var Reader): MalType =
    result = list r.read_seq("(", ")")

proc read_vector(r: var Reader): MalType =
    result = vector r.read_seq("[", "]")

proc read_hash_map(r: var Reader): MalType =
    result = hash_map r.read_seq("{", "}")

proc read_atom(r: var Reader): MalType =
    let t = r.next.get("")
    if t.match(intRE):
        result = number(t.parseInt)
    elif t[0] == '"':
        if not t.match(strRE):
            raise newException(ValueError, "invalid string literal")
        result = str(t[1..len(t)-1].multiReplace(("\\\"", "\""), ("\\n", "\n"), ("\\\\", "\\")))
    elif t[0] == ':':
        result = keyword(t[1..len(t)-1])
    elif t == "nil":
        result = nilObj
    elif t == "true":
        result = trueObj
    elif t == "false":
        result = falseObj
    else:
        result = symbol(t)

proc read_from(r: var Reader): MalType =
    if r.peek.get("")[0] == ';':
        discard r.next
        return nilObj
    case r.peek.get("")
        of "'":
            discard r.next
            result = list(symbol("quote"), r.read_from)
        of "`":
            discard r.next
            result = list(symbol("quasiquote"), r.read_from)
        of "~":
            discard r.next
            result = list(symbol("unquote"), r.read_from)
        of "@":
            discard r.next
            result = list(symbol("deref"), r.read_from)
        of "^":
            discard r.next
            let meta = r.read_from
            result = list(symbol("with-meta"), r.read_from, meta)
        of "~@":
            discard r.next
            result = list(symbol("splice-unquote"), r.read_from)
        of "(":
            result = r.read_list
        of ")":
            raise newException(ValueError, "unexpected ')'")
        of "[":
            result = r.read_vector
        of "]":
            raise newException(ValueError, "unexpected ']'")
        of "{":
            result = r.read_hash_map
        of "}":
            raise newException(ValueError, "unexpected '}'")
        else:
            result = r.read_atom

proc read_str*(str: string): MalType =
    var r = Reader(tokens: str.tokenize)
    if len(r.tokens) == 0:
        raise newException(Blank, "Blank line")
    read_from(r)