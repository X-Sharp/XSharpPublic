// the using System.Text is declared in test.prg
FUNCTION Start( ) AS VOID
    LOCAL x as StringBuilder
    x := StringBuilder{}
    x:Append("Hello world")
    ? x:ToString()
RETURN
