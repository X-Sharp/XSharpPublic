// 209. error XS0266: Cannot implicitly convert type 'uint' to 'int'. An explicit conversion exists
// vulcan dialect, not enabled vo4
FUNCTION Start( ) AS VOID
LOCAL a AS ARRAY
LOCAL n := 1 AS INT
LOCAL d := 1 AS DWORD
a := {{1}}

// no errors in the following 3 lines:
? a[d]
? a[n]
? a[n,1]

// error XS0266 in the following 3 lines:
? a[d,1]
? a[d,n]
? a[n,d]

