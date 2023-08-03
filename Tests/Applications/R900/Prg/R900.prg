using System.Reflection
BEGIN NameSpace ConsoleApp1
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
    local oTest := Test{} as Test
    // indexed SELF property
    oTest[1] := "abc"
    ? oTest[1]
    xAssert(oTest[1] == "abc1")
    LOCAL oTest2 := oTest as ITest
    ? oTest2[1]
    oTest2[42] := "sometext" // sets it to "sometext42"
    ? oTest2[1]
    xAssert((String)oTest2[1] == "ITestsometext421")
    // Indexed expression body
    ? oTest:Foo[42]
    xAssert(oTest:Foo[42] == "42")
    ? oTest:Foo2[4242]
    ? oTest:Foo2[4242]
    xAssert(oTest:Foo2[4242] == "4242")
RETURN

class Test IMPLEMENTS ITest
    PRIVATE sValue as sTRING

    PROPERTY SELF[nItem as LONG] AS String
        GET
            RETURN sValue
        END GET
        SET
            sValue := value + nItem:ToString()
        END SET
    END PROPERTY
    PROPERTY ITest.SELF[nItem as LONG] AS object
        GET
            RETURN "ITest"+ (string) sValue +nItem:ToString()
        END GET
        SET
            sValue := ((string) value) +nItem:ToString()
        END SET
    END PROPERTY
    PROPERTY Custom[cItem as STRING] AS String
        GET
            RETURN sValue
        END GET
        SET
            sValue := value
        END SET
    END PROPERTY
    PROPERTY ITest.Custom[nItem as LONG] AS OBJECT
        GET
            RETURN sValue
        END GET
        SET
            sValue := (string) value
        END SET
    END PROPERTY
    METHOD TestMe() as STRING
        RETURN ""
    METHOD ITest.TestMe() as Object
        RETURN ""
    PROPERTY Foo[nItem as LONG] AS STRING => nItem:ToString()
    PROPERTY Foo2(nItem as LONG) AS STRING GET nItem:ToString()

END class


INTERFACE ITest
    PROPERTY Custom[nItem as LONG] AS OBJECT GET SET
    PROPERTY SELF[nItem as LONG] AS OBJECT GET SET
    METHOD TestMe as OBJECT
END INTERFACE
END NAMESPACE

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
