// https://github.com/X-Sharp/XSharpPublic/issues/1147
// See https://www.xsharp.eu/forum/public-vfp/3201-searching-for-a-solution-with-do-varname
FUNCTION Start( ) AS VOID
    local cVar as string
    cVar := "Test"
    do &cVar
    //&(cVar+"()")            // this should also work
    //_CallClipFunc(cVar)     // should produce the same code as the DO &cVar
    cVar := "Test2"
    do &cVar with 1,2,3
    //& (cVar+"(1,2,3)")   // this should also work
    //_CallClipFunc(cVar,1,2,3) // should produce the same code as the DO &cVar
    wait
RETURN


Function Test()
    ? "Test"
    return nil
Function Test2(a,b,c)
    ? "Test2",a,b,c
    return a+b+c

