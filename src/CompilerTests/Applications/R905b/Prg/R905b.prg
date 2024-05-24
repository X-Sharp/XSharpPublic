// Test late bound access to hidden members
// from within same class
// duplicate from R905, but with /xpp1 enabled
// https://github.com/X-Sharp/XSharpPublic/issues/1335
PROCEDURE Main()
    LOCAL o := BigNumber():NEW(3)   AS BigNumber
    LOCAL o2 := BigNumber():NEW(4)  AS BigNumber

    ? o:getNumber()
    XAssert(o:GetNumber() == 3)
    ? o2:getNumber()
    XAssert(o2:GetNumber() == 4)
    o:Increment(o2)
    ? o:getNumber()
    XAssert(o:GetNumber() == 7)
RETURN


CLASS BigNumber
exported:
   inline METHOD Init(a)
       ::_a := a
   RETURN

   inline METHOD Increment(o)
       //? ::_a, o:_a
       ::_a += o:_a // program falls, because o:_a is nil
   RETURN SELF

   inline METHOD getNumber()
   RETURN ::_a

HIDDEN:
   VAR _a
endclass

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
