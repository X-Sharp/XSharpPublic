// Test late bound access to hidden members
// from within same class
// https://github.com/X-Sharp/XSharpPublic/issues/1335
procedure Main()
    local o := BigNumber():new(3)   as BigNumber
    local o2 := BigNumber():new(4)  as BigNumber

    ? o:getNumber()
    XAssert(o:GetNumber() == 3)
    ? o2:getNumber()
    XAssert(o2:GetNumber() == 4)
    o:Increment(o2)
    ? o:getNumber()
    XAssert(o:GetNumber() == 7)
return


class BigNumber
exported:
   inline method Init(a)
       ::_a := a
   return

   inline method Increment(o)
       //? ::_a, o:_a
       ::_a += o:_a // program falls, because o:_a is nil
   return self

   inline method getNumber()
   return ::_a

hidden:
   var _a
endclass

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
