using System
using System.Collections.Generic
using System.Linq
using System.Text
using XUnit
begin namespace XSharp.XPP.Tests

class ClassCreateTests

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





function Age(dDate as date) as dword
    local sToday := Right(DTos(ToDay()),4) as string
    local sDate  := Right(Dtos(dDate),4) as string
    local nAge   as dword
    nAge  := Year(Today()) - Year(dDate)
    if sToday <= sDate
        nAge -= 1
    endif
    return nAge
