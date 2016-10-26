// 166. error XS0457: Ambiguous user defined conversions '__Usual.implicit operator byte(__Usual)' and '__Usual.implicit operator sbyte(__Usual)' when converting from '__Usual' to 'decimal'
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL d AS Decimal
u := 1.2m
d := (System.Decimal)u
? d

