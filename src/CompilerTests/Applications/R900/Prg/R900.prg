USING System.Reflection
BEGIN NAMESPACE ConsoleApp1
FUNCTION Start( ) AS VOID
    /*
    local oType := Typeof(test) as Type
    foreach oMem as MemberInfo in oType:GetMembers()
        if oMem is PropertyInfo VAR oProp
            ? "Prop", oProp:Name, oProp:PropertyType
            foreach var oAcc in oProp:GetAccessors()
                if oAcc:DeclaringType == oType
                    ? "Accessor", oAcc:Name, oAcc:ReturnType
                endif
            next
        elseif oMem is MethodInfo var oMethod
            if oMethod:DeclaringType == oType
                ? "Method", oMethod:Name, oMethod:ReturnType
            endif
        endif
    next
    ? "InterfaceMap"
    var map := oType:GetInterfaceMap(typeof( ITest))
    foreach m as methodInfo in  map:InterfaceMethods
        ? m:Name, m:ReturnType
    next
    */
    LOCAL oTest := Test{} AS Test
    // indexed SELF property
    oTest[1] := "abc"
    ? oTest[1]
    xAssert(oTest[1] == "abc1")
    LOCAL oTest2 := oTest AS ITest
    ? oTest2[1]
    oTest2[42] := "sometext" // sets it to "sometext42"
    ? oTest2[1]
    xAssert((STRING)oTest2[1] == "ITestsometext421")
    // Indexed expression body
    ? oTest:Foo[42]
    xAssert(oTest:Foo[42] == "42")
    ? oTest:Foo2[4242]
    ? oTest:Foo2[4242]
    xAssert(oTest:Foo2[4242] == "4242")
RETURN

CLASS Test IMPLEMENTS ITest
    PRIVATE sValue AS STRING

    PROPERTY SELF[nItem AS LONG] AS STRING
        GET
            RETURN sValue
        END GET
        SET
            sValue := value + nItem:ToString()
        END SET
    END PROPERTY
    PROPERTY ITest.SELF[nItem AS LONG] AS OBJECT
        GET
            RETURN "ITest"+ (STRING) sValue +nItem:ToString()
        END GET
        SET
            sValue := ((STRING) value) +nItem:ToString()
        END SET
    END PROPERTY
    PROPERTY Custom[cItem AS STRING] AS STRING
        GET
            RETURN sValue
        END GET
        SET
            sValue := value
        END SET
    END PROPERTY
    PROPERTY ITest.Custom[nItem AS LONG] AS OBJECT
        GET
            RETURN sValue
        END GET
        SET
            sValue := (STRING) value
        END SET
    END PROPERTY
    METHOD TestMe() AS STRING
        RETURN ""
    METHOD ITest.TestMe() AS OBJECT
        RETURN ""
    PROPERTY Foo[nItem AS LONG] AS STRING => nItem:ToString()
	PROPERTY Foo2(nItem AS LONG) AS STRING GET nItem:ToString()
END CLASS


INTERFACE ITest
    PROPERTY Custom[nItem AS LONG] AS OBJECT GET SET
    PROPERTY SELF[nItem AS LONG] AS OBJECT GET SET
    METHOD TestMe AS OBJECT
END INTERFACE
END NAMESPACE

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
