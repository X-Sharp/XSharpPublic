// 133. No warning on not using assigned value
FUNCTION Start() AS VOID
LOCAL n := 1 AS INT // correct warning XS0219 here
LOCAL o := System.Collections.ArrayList{} AS OBJECT // no warning

