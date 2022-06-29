// 715. error XS9002: Parser: unexpected input '[]'
#pragma warnings(219, off) //   assigned but not used
USING System.Collections.Generic
FUNCTION Start() AS VOID
	LOCAL a AS List<INT>[]
	a := List<INT>[]{2}
	a[1] := List<INT>{}
	a[1]:Add(123)
	a[1]:Add(-123)
	? a[1]

	LOCAL s1 AS SortedList<INT,STRING>[][]
	s1 := SortedList<INT,STRING>[][]{10}

	LOCAL s2 AS SortedList<INT,STRING>[,]
	s2 := SortedList<INT,STRING>[,]{5,10}
RETURN
