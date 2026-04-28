// 964. Error in codeblock execution with properties [#1860]
// https://github.com/X-Sharp/XSharpPublic/issues/1860

CLASS Test
    PRIVATE _val := 1 AS INT
    METHOD SetVal(n AS INT) AS VOID
		SELF:_val := n
    
    PUBLIC PROPERTY RecCount AS INT
        GET
            Console.WriteLine(ei"RecCount get: {_val}")
            RETURN _val
        END GET
        PROTECTED SET
            Console.WriteLine(ei"RecCount set: {value}")
            _val := value
        END SET
    END PROPERTY

    METHOD NoIVarPut(symField, xValue) AS USUAL CLIPPER
        Console.WriteLine(ei"NoIVarPut: {AsString(symField)},{AsString(xValue)}")
        xAssert(false) // should never come here
        RETURN NIL
END CLASS


FUNCTION Start() AS VOID STRICT
    VAR osql := Test{}

    // Works fine
    VAR cb1 := {|osql| NTrim(osql:RecCount)}
    ? Eval(cb1, osql)
    xAssert( Eval(cb1, osql) == "1" )
    
    osql:SetVal(2)

    // Calls first the getter and then the setter of RecCount, which is not expected
    VAR cb2 := &("{|osql| NTrim(osql:RecCount)}")
    ? Eval(cb2,osql)
    xAssert( Eval(cb2, osql) == "2" )


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
