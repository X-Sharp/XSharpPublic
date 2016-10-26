//205. error XS0266: Cannot implicitly convert type 'Vulcan.Codeblock' to 'Vulcan._Codeblock'
// vulcan incompatibility
FUNCTION Start() AS VOID
LOCAL cb AS CODEBLOCK
LOCAL _cb AS _CODEBLOCK
cb := {||1+1}
_cb := cb
cb := _cb
? Eval(cb)

