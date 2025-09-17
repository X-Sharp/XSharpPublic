USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
BEGIN NAMESPACE XSharp.XPP.Tests

CLASS ClassCreateTests

    [Fact, Trait("Category", "Dynamic Classes")];
    method ClassCreateTest as void
        local oClass, oObject as object
        local aMethod, aIVar as array
        local nAttr as long
        local bMethod as codeblock
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
        oClass := ClassCreate("Abc",nil, aIVar,aMethod)
        oObject := oClass:New()
        Assert.Equal<string>("None", oObject:FirstName)
        Assert.Equal<string>("None", oObject:LastName)
        Assert.Equal<date>(1900.01.01, oObject:BirthDate)
        Assert.Equal<dword>( Age(1900.01.01), oObject:CalcAge())

        oObject:FirstName := "Fabrice"
        oObject:LastName  := "Foray"
        oObject:BirthDate := 1966.09.21

        Assert.Equal<String>("Fabrice Foray", oObject:FullName)
        Assert.Equal<dword>( Age(1966.09.21), oObject:CalcAge())
        return
end class
end namespace





FUNCTION Age(dDate AS DATE) AS DWORD
    LOCAL sToday := Right(DTos(ToDay()),4) AS STRING
    LOCAL sDate  := Right(Dtos(dDate),4) AS STRING
    LOCAL nAge   AS DWORD
    nAge  := Year(Today()) - Year(dDate)
    IF sToday <= sDate
        nAge -= 1
    ENDIF
    RETURN nAge
