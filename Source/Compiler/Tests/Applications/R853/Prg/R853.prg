// https://www.xsharp.eu/forum/public-product/3046-begin-end-sequence-causes-compiler-error-xs0136
// https://github.com/X-Sharp/XSharpPublic/issues/1055
#pragma options("vo17", on)
FUNCTION Start( ) AS VOID
    local n as long
	BEGIN SEQUENCE
	    n := 0
	    ? "Do something that might fail"
	    ? 1 / n
	RECOVER
	    BEGIN SEQUENCE
	        ? "It failed as expected"
	    END SEQUENCE
	END SEQUENCE
RETURN
