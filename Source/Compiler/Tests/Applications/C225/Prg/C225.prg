// 225. error XS0121: The call is ambiguous between the following methods or properties: 'Functions.VO_Sprintf(__Usual, params object[])' and 'Functions.VO_Sprintf(uint, params object[])'
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := 1
VO_Sprintf(5200,u,u )

