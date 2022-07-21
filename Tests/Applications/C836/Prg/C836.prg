// 836. Float And Currency in Linq operations
// https://github.com/X-Sharp/XSharpPublic/issues/965

USING System.Collections.Generic
USING System.Linq
#pragma warnings(9043, off)

FUNCTION Start() AS VOID STRICT
    VAR valueList := List<ValueClass>{} { ValueClass{} { valueFloat := 11.11, valueReal8 := 22.22, valueCurrency := 33.33 } }

    VAR sumReal8 := valueList:Sum({ q => q:valueReal8 })
    VAR sumFloat := valueList:Sum({ q => q:valueFloat })
    VAR sumCur := valueList:Sum({ q  => q:valueCurrency })
    ? sumReal8
    ? sumFloat
    ? sumCur

    xAssert(sumReal8 == 22.22)
    xAssert(sumFloat == 11.11)
    xAssert(sumCur == 33.33)
RETURN

CLASS ValueClass
    PROPERTY valueFloat AS FLOAT AUTO
    PROPERTY valueReal8 AS REAL8 AUTO
    PROPERTY valueCurrency AS CURRENCY AUTO
END CLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
