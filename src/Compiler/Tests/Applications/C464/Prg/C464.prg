// 464. error XS1513: } expected

GLOBAL testglobal1 := {1,2,3}
GLOBAL testglobal2 := {1} AS ARRAY
STATIC GLOBAL testglobal3 := {1,2} AS ARRAY
STATIC GLOBAL testglobal4 := NULL_ARRAY AS ARRAY

DEFINE testdefine1 := {1,2,3,"A"}
DEFINE testdefine2 := NULL_ARRAY

FUNCTION Start( ) AS VOID
	? ALen(testglobal1)
	? ALen(testglobal2)
	? ALen(testglobal3)
	? ALen(testglobal4)

	? ALen(testdefine1)
	? ALen(testdefine2)
RETURN
