// https://github.com/X-Sharp/XSharpPublic/issues/984
// mak sure compiler generated casts from object to reference type and value type
// are handled correctly.
// referencetype should generate castclass <expected type>
// valuetype should generate unbox.any <expected type>
#pragma options("vo7", on)
function Start() as void strict
    TestClass{}:Test()
    return

public class TestClass
    public method Test() as void strict
	local x := DateTime.Now as object     
	// test with implicit cast to string  
	try
        self:TestString(x) 
        xAssert(FALSE)
    catch e as exception
        xAssert(e is InvalidCastException)
	end try
	// test with explicit cast to string  
	try
	self:TestString((string)x) 
        xAssert(FALSE)
    catch e as exception
        xAssert(e is InvalidCastException)
	end try
    // test with implicit cast to dword               
    // even passing in an int should fail
    x := Int32.MaxValue
    #ifdef THISDOESNOTCOMPILE
    // implicit cast with /vo11 does not work for value types
	try
	self:testDword( x) // gives InvalidCastException 
        xAssert(FALSE)
    catch e as exception
        xAssert(e is InvalidCastException)
	end try                              
    #endif    
    // test with explicit cast to dword        
	try
	self:testDword((DWORD) x) // gives InvalidCastException 
        xAssert(FALSE)
    catch e as exception
        xAssert(e is InvalidCastException)
	end try                              
	// test with the conversion. this should work
    x := Int32.MaxValue
	try
	? self:testDword(Convert.ToUInt32( x))  // should work
        xAssert(true)
    catch 
        xAssert(false)
	end try                              
	
    return
		
    public method TestString(c as string) as void
    ? c
	c:Replace("X","y")
	return
    public method testDWord(dw as dword) as dword
    ? dw
	dw *= dw
	return  dw

end class


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
