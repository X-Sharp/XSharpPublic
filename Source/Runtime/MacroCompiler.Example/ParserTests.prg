// ParserTests.prg
// Created by    : nvk
// Creation Date : 2/6/2021 11:31:16 AM
// Created for   : 
// WorkStation   : I7


USING System
USING System.Collections.Generic
USING System.Text

// Rudimentary parser tests
// Should parse without errors

FUNCTION ParserTestsFox(mc AS XSharp.Runtime.MacroCompiler) AS VOID
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "LPARAMETERS a,b as int, c",;
            "PARAMETERS a as string,b, c as int";
            }))
    RETURN

FUNCTION ParserTests(mc AS XSharp.Runtime.MacroCompiler) AS VOID
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "PRIVATE a,b[4,5][4],c := 1+1, d[5] := {1,2,3,4}",;
            "PUBLIC a as float,b[4,5][4],c := 1+1 as system.int, d[5] := {1,2,3,4}",;
            "MEMVAR x, y",;
            "MEMVAR q",;
            "LOCAL a, b := 1+2, c[2] := {P1,2}, d := 1234 as int, e as system.string",;
            "STATIC LOCAL a, b := 1+2, DIM c[2] := {P1,2}, d := 1234 as int",;
            "LOCAL STATIC a, const b := 1+2, c[2] := {P1,2}, d := 1234 as int",;
            "VAR a := 1+2, b := P1",;
            "LOCAL IMPLIED a := 1+2, b := P1",;
            "DIMENSION a[10] as int",;
            "DECLARE a[10][4][3+5,2] as int",;
            "x+1,2",;
            "y*x",;
            "NOP",;
            "NOP()";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "WHILE x > 3",;
                "x-=1",;
                "do while x > 0",;
                "--x",;
                "end while",;
                "do while x > 0",;
                "--x",;
                "end do",;
                "while x > 0",;
                "--x",;
                "enddo",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "FOR x := 1 to 10",;
            "NEXT",;
            "FOR x := 1 downto 10 step -1",;
                "FOR VAR y := 1 to 10",;
                    "y:=x+1",;
                "END",;
                "FOR LOCAL y := 1 as int to 10",;
                    "y:=x+1",;
                "END",;
            "END FOR";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "IF x > 0",;
                "y:=x+1",;
            "END IF";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "IF x > 0",;
                "y:=x+1",;
            "ELSE",;
                "y:=x-1",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "DO CASE",;
            "CASE x == 1",;
            "y = x",;
            "CASE x == 2",;
            "OTHERWISE",;
            "y = z",;
            "END CASE",;
            "DO CASE",;
            "OTHERWISE",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "FOREACH VAR x IN list",;
                "exit",;
            "END",;
            "FOREACH IMPLIED x IN list",;
                "write(x)",;
            "END FOR",;
            "FOR EACH x AS INT IN list",;
                "loop",;
            "NEXT";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "break",;
            "break e{}",;
            "return",;
            "return void",;
            "return 1+1";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "? 1, 2, a+b",;
            "?? 1, 2, a+b",;
            "? x",;
            "??";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "TRY",;
                "THROW Error{123}",;
            "CATCH",;
            "FINALLY",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "REPEAT",;
                "loop",;
            "UNTIL false";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN SWITCH e",;
                "CASE 1",;
                    "write(1)",;
                "CASE x AS INT",;
                    "write(x)",;
                "CASE M.x AS INT",;
                    "write(x)",;
                "CASE x AS List WHEN x:Count > 0",;
                    "write(x)",;
                "CASE 1 WHEN y < 0",;
                    "write(x)",;
                "OTHERWISE",;
                    "error()",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN SEQUENCE",;
                "write(x)",;
            "END SEQUENCE";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN SEQUENCE",;
                "write(x)",;
            "RECOVER USING x",;
                "test(x)",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN SEQUENCE",;
                "write(x)",;
            "FINALLY",;
                "test()",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN SEQUENCE",;
                "write(x)",;
            "RECOVER USING x",;
                "test(x)",;
            "FINALLY",;
                "test()",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN LOCK o:key",;
                "write(o)",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN LOCK key",;
                "write(o)",;
            "END LOCK";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN SCOPE",;
                "write(o)",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN SCOPE",;
                "write(o)",;
            "END SCOPE";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN USING o:f",;
                "write('thr')",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN USING file",;
                "file:write(o)",;
            "END USING";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN USING VAR file := File{'test'}",;
                "file:write(o)",;
            "END USING";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN USING VAR file := File{'test'}, o := Output{}",;
                "o:write(file)",;
            "END USING";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN USING LOCAL IMPLIED file := File{'test'}, o := Output{}",;
                "o:write(file)",;
            "END USING";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN USING LOCAL VAR file := File{'test'}, o := Output{}",;
                "o:write(file)",;
            "END USING";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN USING LOCAL file := File{'test'}, o := Output{} as System.File",;
                "o:write(file)",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN UNSAFE",;
                "write(o)",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN UNSAFE",;
                "write(o)",;
            "END UNSAFE";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN CHECKED",;
                "write(o)",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN CHECKED",;
                "write(o)",;
            "END CHECKED";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN UNCHECKED",;
                "write(o)",;
            "END";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN UNCHECKED",;
                "write(o)",;
            "END UNCHECKED";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN FIXED VAR file := File{'test'}, o := Output{}",;
                "o:write(file)",;
            "END FIXED";
            }))
        ParseScript(mc, String.Join(e"\n",<STRING>{;
            "BEGIN FIXED LOCAL file := File{'test'}, o := Output{} as System.File",;
                "o:write(file)",;
            "END";
            }))
    RETURN
