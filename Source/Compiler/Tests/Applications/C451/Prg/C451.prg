// error VN4011: 'ANY' : unknown type (are you missing an assembly reference?)
// Did you know that ANY is a synonym for USUAL in VO?!?
// (or is i not?) Found it in a few places..
//
// Logged this for the /VO dialect
FUNCTION Start() AS VOID
	LOCAL u AS ANY
	u := 1
	? u
RETURN
