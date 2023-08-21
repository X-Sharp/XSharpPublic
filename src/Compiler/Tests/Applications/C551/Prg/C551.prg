// 551. No compiler warnings on unused vars or assigned values in Vulcan/VO dialects
/*
In Core dialect, three warnings are correctly reported:

warning XS0168: The variable 'n' is declared but never used
warning XS0168: The variable 'c' is declared but never used
warning XS0219: The variable 'n2' is assigned but its value is never used

In Vulcan/VO dialects, no warnings are reported with this code at all
*/
#pragma warnings(168, off) // declared not used
#pragma warnings(219, off) // assigned not used

FUNCTION Start( ) AS VOID
	LOCAL n AS INT
	LOCAL c AS STRING

	LOCAL n2 AS INT
	n2 := 123
RETURN

