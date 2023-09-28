// https://github.com/X-Sharp/XSharpPublic/issues/1244
#pragma options("vo3", on) // all methods virtual
#pragma options("vo12", on) // integer divisions
#pragma options("lb", on)
#pragma warnings(9201, disable)
// XPP The 'FROM' clause is interpreted as SHARING because variables in parent classes are always shared in the .Net Runtime.
procedure Main()
    local o
    local p
    o := DerivedExample889():new()
    ? o:SayHello()
    xAssert(o:SayHello() == "Hello, I am DerivedExample")
    o:Example889:SayHello()
    ? Example889():Test()
    xAssert(Example889():Test() == "Test")
    ? Example889():ClassVar
    xAssert(Example889():ClassVar == 42)
    p := PartList():New()
    p:Add("a",11)
    p:Add("b",20)
    p:Add("c",33)
    ? p:TotalCost()
    xAssert(p:TotalCost() == 64)
    ? p:AverageCost()
    xAssert(p:AverageCost() == 64/3)

    return


class DerivedExample889 from Example889
exported:
    inline method Name()
        return "DerivedExample"
endclass


class Example889
exported:
    inline class method Test()
        return "Test"
    class var ClassVar
    inline class method InitClass
        ::ClassVar := 42
    inline method Name()
        return "Example"

    inline method SayHello()
        return "Hello, I am " + ::Name()
endclass



// sample from XPP help with Final, Introduce etc.

CLASS PartList

   PROTECTED:

      VAR aParts, aPrice

   EXPORTED:

      METHOD init, add



      FINAL METHOD totalCost,averageCost

   ENDCLASS



   METHOD PartList:init

      ::aParts := {}

      ::aPrice := {}

   RETURN self



   METHOD PartList:add( cPart, nPrice )

      AAdd( ::aParts, cPart )

      AAdd( ::aPrice, nPrice )

   RETURN self



   // Calculation of total costs

   METHOD PartList:totalCost

      LOCAL nCost := 0

      AEval( ::aPrice, {|n| nCost += n } )

   RETURN nCost



   // Calculation of average costs

   METHOD PartList:averageCost

      LOCAL nCost := ::totalCost()

      IF ! Empty( ::aParts )

         nCost /= Len( ::aParts )

      ENDIF

   RETURN nCost


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
