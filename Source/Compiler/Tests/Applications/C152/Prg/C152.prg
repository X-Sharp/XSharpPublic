// 152. Operator '??' cannot be applied to operands of type 'bool' and 'bool'
// it does compile correctly in vulcan dialect.
// Should we support the $ operator also in Core? If not, the error message should be improved.
FUNCTION Start() AS VOID
LOCAL cChar := "N" AS STRING
LOCAL l AS LOGIC
l := cChar $ "ANX9#"
IF .not. l
	THROW Exception{"$ Operation Returned FALSE"}
END IF
? l
