USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XSharp.XPP

//#translate SetDefault(@ <expr>, <value>) => <expr> := IIF( IsNil( <expr>) , <value>, <expr>)

#translate SetDefault(@ <expr>, <value>) => <expr> := SetDefault(<expr> , <value>)


FUNCTION MAIN(A,B,C) AS VOID clipper
    TRY
    ? XSharp.RuntimeState.Dialect:ToString() 
    LOCAL uTest := NIL as usual

    SetDefault(@uTest, 123)
    ? uTest

    ReportMemory("Before")    
    FOR var nI := 1 to 100
        testCLassCreate()
    NEXT
    ReportMemory("After")
    testDataObject()

    WAIT
    CATCH e as Exception
        ErrorDialog(e)
    END TRY
	RETURN

    FUNCTION ReportMemory(description AS STRING) AS VOID
        Console.WriteLine("Memory: {0} ({1})", GC:GetTotalMemory(TRUE),description)
        Console.WriteLine()
        RETURN

function testClassCreate() as void
    local oClass as OBJECT
    local oObject as OBJECT
    local aFields as array
    local aMethods as array
    aFields  := {{"FirstName", CLASS_EXPORTED + VAR_INSTANCE }, {"LastName", CLASS_EXPORTED + VAR_INSTANCE }}
    aMethods := { ;
        {"FullName", CLASS_EXPORTED + METHOD_INSTANCE +METHOD_ACCESS+METHOD_ASSIGN, {|oSelf, newValue| GetFullName(oSelf,newValue)} },;
        {"Init",    CLASS_EXPORTED + METHOD_INSTANCE , {|oSelf| InitObject(oSelf)} } ;
        }
    oClass := ClassCreate("Test",{},aFields,aMethods)
    //? oClass:GetType()
    oObject := oClass:New()
    //? oObject:ClassName()
    oObject:FirstName := "Robert"
    oObject:LastName  := "van der Hulst"
    //? oObject:FirstName, oObject:LastName
    //? oObject:FullName
    //oObject:FullName := "New Name"
    ClassDestroy(oClass)
    aadd(aFields, {"City", CLASS_EXPORTED + VAR_INSTANCE })
    oClass := ClassCreate("Test",NIL,aFields,aMethods)
    oObject := oClass:New()
    oObject:FirstName := "Marian"
    oObject:LastName  := "Koolhaas"
    //? oObject:FirstName, oObject:LastName
    //? oObject:FullName
    ClassDestroy(oClass)

function testDataObject() as VOID
    local oDO as OBJECT
    oDO := DataObject{}
    ? "LastName?", IsMemberVar(oDO, "lastname" )  // N
    ? oDO:LastName                   // NIL

  /* Add a member implicitly by value assignment,
   * the check for a member and output its value
   */
  oDO:LastName := "Picard"
  ? "LastName?", IsMemberVar(oDO, "lastname" )  // Y
  ? oDO:LastName                   // "Picard"

  /* Define a method via code block, execute
   * the method 
   */
  oDO:DefineMethod( "print" , {|oSelf|QOut("Hello "+oSelf:LastName)} )
  ? "Call Print"
  oDO:print()                      // "Hello Picard"

  /* Add another member, redefine an existing
   * method. Now implement the method using a 
   * function. Execute the method.
   */
  oDO:FirstName := "Jean-Luc"
  oDO:DefineMethod( "print", "displayname" )
  ? "Call Print (redefined)"
  oDO:print( "," )  



static function GetFullname(oSelf , newValue) CLIPPER
    IF IsNil(newValue)
        return "Full: "+oSelf:FirstName + " " + oSelf:LastName
    ELSE
        ? "Cannot assign FullName"
    ENDIF
    return NIL

static function InitObject(oSelf ) CLIPPER
    oSelf:FirstName := "John"
    oSelf:LastName  := "Doe"
    return oSelf


CLASS TestMe
    EXPORT oCb as CodeBlock

    METHOD CallMe(a params usual[]) AS USUAL
        return oCb:Eval(SELF)



END CLASS

FUNCTION DisplayName(oSelf, cSeparator )
  ? oSelf:FirstName+cSeparator+" "+oSelf:LastName
RETURN NIL


FUNCTION SetDefault( uExpr as USUAL, uValue as USUAL) AS USUAL
IF IsNil(uExpr)
    return uValue
ENDIF
RETURN uExpr
