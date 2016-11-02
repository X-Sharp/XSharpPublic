// 280. error XS1739: The best overload for 'Empty' does not have a parameter named 'c'
// vulcan dialect
FUNCTION Start() AS VOID
LOCAL c AS STRING
? Empty( c := "abc" )
? c

