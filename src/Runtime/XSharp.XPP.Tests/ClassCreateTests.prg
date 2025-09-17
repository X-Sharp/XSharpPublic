USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
BEGIN NAMESPACE XSharp.XPP.Tests

CLASS ClassCreateTests

    [Fact, Trait("Category", "Dynamic Classes")];
    METHOD ClassCreateTest AS VOID
        LOCAL oClass, oObject AS OBJECT
        LOCAL aMethod, aIVar AS ARRAY
        LOCAL nAttr AS LONG
        LOCAL bMethod AS CODEBLOCK
        oClass := ClassObject( "Abc" )
        Assert.Equal(oClass, null_object)
        nAttr   := CLASS_EXPORTED + VAR_INSTANCE
        aIVar := { {"FirstName", nAttr}, {"LastName",nAttr} , {"BirthDate", nAttr}}
        bMethod := { |oSelf| oSelf:FirstName + " " +oSelf:LastName}
        nAttr   := CLASS_EXPORTED + METHOD_INSTANCE + METHOD_ACCESS
        aMethod := {}
        AAdd(aMethod, { "FullName", nAttr, bMethod})
        nAttr   := CLASS_EXPORTED + METHOD_INSTANCE
        bMethod  :={ | oSelf | Age( oSelf:BirthDate) }
        AAdd(aMethod, { "CalcAge", nAttr, bMethod})
        bMethod  := { | oSelf | oSelf:FirstName := "None", oSelf:LastName := "None" , oSelf:BirthDate := 1900.01.01  }
        AAdd(aMethod, { "Init", nAttr, bMethod})
        oClass := ClassCreate("Abc",NIL, aIVar,aMethod)
        oObject := oClass:New()
//        Assert.Equal<STRING>("None", oObject:FirstName)
//        Assert.Equal<STRING>("None", oObject:LastName)
//        Assert.Equal<DATE>(1900.01.01, oObject:BirthDate)
//        Assert.Equal<DWORD>( Age(1900.01.01), oObject:CalcAge())

        oObject:FirstName := "Fabrice"
        oObject:LastName  := "Foray"
        oObject:BirthDate := 1966.09.21

//        Assert.Equal<STRING>("Fabrice Foray", oObject:FullName)
//        Assert.Equal<DWORD>( Age(1966.09.21), oObject:CalcAge())
        RETURN
END CLASS
END NAMESPACE





FUNCTION Age(dDate AS DATE) AS DWORD
    LOCAL sToday := Right(DTos(ToDay()),4) AS STRING
    LOCAL sDate  := Right(Dtos(dDate),4) AS STRING
    LOCAL nAge   AS DWORD
    nAge  := Year(Today()) - Year(dDate)
    IF sToday <= sDate
        nAge -= 1
    ENDIF
    RETURN nAge
