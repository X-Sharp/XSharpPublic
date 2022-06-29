// 29. error XS0052: Inconsistent accessibility: field type 'IntClass' is less accessible than field 'Test.o'
// Different behavior to vulcan so it breaks existing code. Maybe error can just become a warning
#pragma warnings(52, off) // inconsistent visibility
CLASS Test
PROTECT o AS IntClass
END CLASS

INTERNAL CLASS IntClass
END CLASS

