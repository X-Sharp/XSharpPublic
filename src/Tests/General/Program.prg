public class TestClass
	public method TestMethod(args params usual[]) as void
		Console.WriteLine("TestMethod called with {0} arguments.", args:Length)
		return
	public method TestMethod2(arg as usual, args params usual[]) as void
		Console.WriteLine("TestMethod2 called with {0} arguments.", args:Length+1)
        return
	public method TestMethod3(arg as usual, arg2 as usual, args params usual[]) as void
		Console.WriteLine("TestMethod3 called with {0} arguments.", args:Length+2)
		return
    public method TestMethod4(arg as usual, arg2 as usual, arg3 as usual, args params usual[]) as void
        if args != null
            Console.WriteLine("TestMethod4 called with {0} arguments.", args:Length +3)
        else
            Console.WriteLine("TestMethod4 called with {0} arguments.", 3)
        endif
		return
end class



function Start() as void strict
	var test := TestClass{}
    try
	var testUntyped := (usual)TestClass{}

	test:TestMethod(1, 2, 3)
	test:TestMethod2(1, 2, 3)
	test:TestMethod3(1, 2, 3)
	test:TestMethod4(1, 2, 3,4,5,6)
	test:TestMethod4(1,2,3)
	test:TestMethod4(1,2,3)
	testUntyped:TestMethod(1, 2, 3)
	testUntyped:TestMethod2(1, 2, 3)
	testUntyped:TestMethod3(1, 2, 3)
	testUntyped:TestMethod4(1, 2, 3,4,5,6)
	testUntyped:TestMethod4(1,2,3)
    catch e as exception
        Console.WriteLine("Error: "+e:ToString())
        end try
	Console.ReadLine()
	return
