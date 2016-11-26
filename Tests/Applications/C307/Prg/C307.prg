// 307. error XS0457: Ambiguous user defined conversions '__Usual.implicit operator __Usual(object)' and '__Usual.implicit operator __Usual(sbyte)' when converting from 'decimal' to '__Usual'
// related to C166, but this one (307) happens only with /vo4+
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := 123.456m
? u
