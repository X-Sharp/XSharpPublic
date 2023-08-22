GLOBAL ggg := 0 AS INT

FUNCTION AssignToNonStatic() AS VOID
? "must be zero", ggg
ggg := 1
? "must be one", ggg

