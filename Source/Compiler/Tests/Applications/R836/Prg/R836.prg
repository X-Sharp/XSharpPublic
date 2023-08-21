// https://github.com/X-Sharp/XSharpPublic/issues/951
#pragma options("vo4", on)
#pragma options("vo7", on)
#pragma options("ovf", on)
#pragma warnings(9020, off)
FUNCTION Start( ) AS VOID
    LOCAL f := -1343.345 AS FLOAT  // "AS DOUBLE" throws an compile error
    LOCAL c := -1343.345 AS CURRENCY  // "AS DOUBLE" throws an compile error

    try
        Test (f )
        xAssert(FALSE)
    catch e as Exception
        ? "This exception was expected:"
        ? e:Message
    // this is expected
        xAssert(TRUE)
    end try

    try
        Test (c )
        xAssert(FALSE)
    catch e as Exception
        ? "This exception was expected:"
        ? e:Message
    // this is expected
        xAssert(TRUE)
    end try


    try
        f := 12345673.89
        Test (  f )
    catch e as Exception
        ? "This exception was expected:"
        ? e:Message
    // this is expected
        xAssert(TRUE)
    end try

    try
        c := $12345673.89
        Test (c )
        xAssert(FALSE)
    catch e as Exception
        ? "This exception was expected:"
        ? e:Message
    // this is expected
        xAssert(TRUE)
    end try

    try
        f := 42
        xAssert(TRUE)

        Test (  f )
    catch e as Exception
        ? "This exception should not happen"
        ? e:Message
    // this is expected
        xAssert(FALSE)
    end try
    try
        c := 42
        xAssert(TRUE)

        Test (  c )
    catch e as Exception
        ? e:Message
        ? e:ToString()
    // this is expected
        xAssert(FALSE)
    end try
RETURN


FUNCTION Test ( n AS BYTE ) AS INT
    ? n   // shows 193 and 73
RETURN 0

PROC xAssert(l AS LOGIC)
    IF .not. l
        THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
    END IF
    ? "Assertion passed"
RETURN
