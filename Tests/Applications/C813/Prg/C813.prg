// 813. ICE with Type() in FoxPro dialect with /fox2+ enabled
FUNCTION Start() AS VOID
LOCAL pVarId
? Type(pVarId)
pVarId := "abc"
? Type(pVarId)

