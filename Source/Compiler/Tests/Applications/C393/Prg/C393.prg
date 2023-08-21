// 393. (1,520): error XS0103: The name 'DebugInit' does not exist in the current context

// IMPORTANT!!!
// Also note the error lines reported, it is line 520 and 655!
// IMPORTANT!!!

GLOBAL ggg AS INT

STATIC PROCEDURE SomeInitialization() _INIT3
ggg := 123

STATIC PROCEDURE DebugInit _INIT1

FUNCTION Start( ) AS VOID
IF ggg != 123
	THROW Exception{"global not initialized"}
END IF
? "All OK!"

