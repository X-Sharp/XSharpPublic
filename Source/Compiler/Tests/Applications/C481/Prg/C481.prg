// C481. #translate cannot handle 4-letter abbreviations
#translate TEST1234 <xpr> => ? <"xpr">
#command ABCD1234 <xpr> => ? "command",<"xpr">
#command @<x>,<y> BBBB1234 <xpr> => ? <x>,<y>,<"xpr">
FUNCTION Start() AS VOID
TEST aaa
TEST1 bbb 

ABCD1234 aaa
ABCD1 bbb 
ABCD ccc

@1,2 BBBB1 ddd

