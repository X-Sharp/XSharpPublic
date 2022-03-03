// 838. Missing compiler errors on incorrect usa of OVERRIDE on ACCESSes
// https://github.com/X-Sharp/XSharpPublic/issues/981
/*
With no vo options enabled, the compiler does not report any errors or warnings, even though the two OVERRIDE clauses are incorrect, there's no property to override.
When using METHODs instead of ACCESSes, the compiler correctly reports two error XS0115: 'Method1': no suitable method found to override
*/
FUNCTION Start( ) AS VOID
	LOCAL o AS Child
	o := Child{}
	? o:Access1
	? o:Access2
RETURN

CLASS Parent
	OVERRIDE ACCESS Access1 AS STRING
	RETURN "parent1"
	PRIVATE ACCESS Access2 AS STRING
	RETURN "parent2"
END CLASS

CLASS Child INHERIT Parent
	OVERRIDE ACCESS Access2 AS STRING
	RETURN "child2"
END CLASS

