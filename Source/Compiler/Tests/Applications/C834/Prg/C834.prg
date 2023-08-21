// 834. Compiler error with the += and -= operators between INT/FLOAT
// /vo11+
#pragma options("vo11", on)     // conversions between different sizes and from fractional to integral
#pragma options("vo4", on)     // conversions between different sizes and from fractional to integral

FUNCTION Start( ) AS VOID
	LOCAL n := 1 AS INT
	LOCAL d := 2 AS DWORD
	LOCAL f := 3.0 AS FLOAT
	LOCAL r := 3.0 AS REAL8

	n := f
	n := n + f // OK
	n += f     // error XS0266: Cannot implicitly convert type 'FLOAT' to 'int'. An explicit conversion exists (are you missing a cast?)
	xAssert(n == 9)
	n -= f     // error XS0266: Cannot implicitly convert type 'FLOAT' to 'int'. An explicit conversion exists (are you missing a cast?)
	xAssert(n == 6)

	n := r
	n := n + r
	n += r
	xAssert(n == 9)
	n -= r
	xAssert(n == 6)

	d := f
	d := d + f // OK
	d += f     // error XS0266: Cannot implicitly convert type 'FLOAT' to 'dword'. An explicit conversion exists (are you missing a cast?)
	xAssert(d == 9)
	d -= f     // error XS0266: Cannot implicitly convert type 'FLOAT' to 'dword'. An explicit conversion exists (are you missing a cast?)
	xAssert(d == 6)

	d := r
	d := d + r
	d += r
	xAssert(d == 9)
	d -= r
	xAssert(d == 6)

    d := d + 1
    d += 1
    d -= 1
    d /= 1
    d *= 1

    d >>= 1
    n := d
    n := n + d
    n += d
    n -= d
    n *= d
    n >>= d


RETURN


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

