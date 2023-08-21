FUNCTION Start() AS VOID STRICT
? IsSymbolNil()
? IsSymbolNil(#somesymbol)
wait
return

function IsSymbolNil(sym := nil as symbol) as logic
    return isNil(sym)
