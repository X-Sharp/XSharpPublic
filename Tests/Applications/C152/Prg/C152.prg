// 152. Operator '??' cannot be applied to operands of type 'bool' and 'bool'
FUNCTION Start() AS VOID
LOCAL cChar := "N" AS STRING
LOCAL l AS LOGIC
l := cChar $ "ANX9#"
IF .not. l
	THROW Exception{"Returned FALSE"}
END IF

