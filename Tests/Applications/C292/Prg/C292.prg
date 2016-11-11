// 292. error XS0034: Operator '|' is ambiguous on operands of type 'word' and 'word'
// with /vo4+
FUNCTION Start() AS VOID
LOCAL nState := 129 AS WORD
? _OR( nState, WORD( 128 ) )

