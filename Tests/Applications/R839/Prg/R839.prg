// https://github.com/X-Sharp/XSharpPublic/issues/984
function Start() as void strict
    TestClass{}:Test()
	Console.ReadLine()
    return

public class TestClass
    public method Test() as void strict
	local x := DateTime.Now as object
    self:TestString(x) // gives a FatalExecutionEngineError
	self:TestString((string)x) // gives InvalidCastException
        
        return
		
    public method TestString(c as string) as void
	c:Replace("X","y")
	return

end class
