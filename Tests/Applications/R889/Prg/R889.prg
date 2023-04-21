// https://github.com/X-Sharp/XSharpPublic/issues/1244
#pragma options("vo3", on) // all methods virtual
#pragma options("vo12", on) // integer divisions
#pragma options("lb", on)
#pragma warnings(9201, disable)
// XPP The 'FROM' clause is interpreted as SHARING because variables in parent classes are always shared in the .Net Runtime.
procedure Main()
    local o

    o := DerivedExample():new()
    ? o:SayHello()
    xAssert(o:SayHello() == "Hello, I am DerivedExample")

    o := PartList():New()
    o:Add("a",11)
    o:Add("b",20)
    o:Add("c",33)
    ? o:TotalCost()
    xAssert(o:TotalCost() == 64)
    ? o:AverageCost()
    xAssert(o:AverageCost() == 64/3)
    return


class DerivedExample from Example
exported:
    inline method Name()
        return "DerivedExample"
endclass


class Example
exported:
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
