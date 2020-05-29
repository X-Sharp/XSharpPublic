USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharpModel


FUNCTION Start() AS VOID
LOCAL cFileName AS STRING
cFileName := "parsertest.prg"

VAR parser := ParserV2{XFile{cFileName}}
parser:Parse(TRUE, TRUE)
FOREACH VAR entity IN parser:EntityList
   ? entity:Name
NEXT
FOREACH VAR localvar IN parser:Locals
   ? localvar:Name, localvar:TypeName, localvar:Range, localvar:Interval, localVar:InitialExpression
NEXT

Console.ReadLine()
RETURN
