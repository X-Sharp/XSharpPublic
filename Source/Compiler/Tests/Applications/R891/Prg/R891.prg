// See http://github.com/X-Sharp/XSharpPublic/issues/1231
FUNCTION Start( ) AS VOID
    local u as usual
    u := #abc
	? Test1(#abc)
	? Test1(u)
	u := NIL
	? Test1(u)
	Test1(NIL)
	? Test2(#def)
	Test2()
	? IsSymbolNil()
	? IsSymbolNil(NIL)
LOCAL s AS SYMBOL

s := NIL
? s
RETURN

FUNCTION Test1 (s as Symbol) AS String
    RETURN Symbol2String(s)

FUNCTION Test2 (s := NIL as Symbol) AS String
    RETURN Symbol2String(s)

function IsSymbolNil(sym := nil as Symbol) as logic
    return isNil(sym)

