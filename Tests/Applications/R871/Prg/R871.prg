// https://github.com/X-Sharp/XSharpPublic/issues/1106


function Start() as void strict
     try
      Dummy{}:Test(42)
      xAssert(Dummy{}:Test(42) == 42)
      Dummy{}:Test("abc")
      xAssert(Dummy{}:Test("abc") == 0)
      catch e as Exception
          ? e:ToString()
     end try

return


class Dummy
    method Test(param as usual) as int
        local nParam := 0 as int

        if (param is int)
            nParam := (int)param
           self:test(nparam)
        else
            ? "Param not an int :" + param:ToString()
        endif

        return 0

    method Test(param as int?) as int
        ? "Param is int : " + (param ?? 0):toString()
        return param:Value
end class



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
