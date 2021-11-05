// 813. ICE with Type() in FoxPro dialect with /fox2+ enabled
FUNCTION Start() AS VOID
MEMVAR testvar
LOCAL pVarId      
testvar := DATE()
? Type(pVarId)
pVarId := "testvar"
? Type(pVarId)   
? Valtype(testvar)

