// 307. error XS0457: Ambiguous user defined conversions '__Usual.implicit operator __Usual(object)' and '__Usual.implicit operator __Usual(sbyte)' when converting from 'decimal' to '__Usual'
// related to C166, but this one (307) happens only with /vo4+
#pragma warnings(219, off) // variable is never used
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL d := 12.34m AS Decimal
u := d
? u
u := 123.456m
? u
UsualFunc(d)

FUNCTION UsualFunc(u AS USUAL) AS VOID
? u


FUNCTION Usual_DecimalTest() AS VOID

LOCAL a := 0.0 AS USUAL
LOCAL b AS Decimal
b := (decimal) (OBJECT) a
RETURN
