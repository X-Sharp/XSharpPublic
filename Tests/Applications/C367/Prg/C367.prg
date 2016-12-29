// 367. error XS9002: Parser: unexpected input '#NIL'
// found this a lot of times
// vulcan requires a space between # and NIL
FUNCTION Start() AS VOID
LOCAL oOriginator AS USUAL

oOriginator := "asd"
? oOriginator#"asd" // ok

oOriginator := 123
? oOriginator#123 // ok

? oOriginator#NIL // error

oOriginator := NULL_ARRAY
? oOriginator#NULL_ARRAY // error

