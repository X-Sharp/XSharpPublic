// 351. error XS0019: Operator '==' cannot be applied to operands of type 'Codeblock' and 'string'
FUNCTION Start() AS VOID
LOCAL oValidBlock AS CODEBLOCK
oValidBlock := NULL_STRING
// works in vulcan with either /vo2+ or /vo2-
IF oValidBlock == NULL_STRING
    oValidBlock := MCompile( "1+1" )
ENDIF
? oValidBlock == ""
? Eval(oValidBlock)


