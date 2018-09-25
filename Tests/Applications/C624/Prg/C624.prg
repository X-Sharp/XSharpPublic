// 624. Runtime problem implementing interface with wrong calling convention
/*
Unhandled Exception: System.TypeLoadException: Method 'CreateDAO' in type 'Paren
t' from assembly 'C624, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null' d
oes not have an implementation.
   at C624.Exe.Functions.Start()
   
Problem is that the method in the interface is defined as STRICT, while it is being
implemented by a CLIPPER method, due to the /vo5 compiler option in the main app, so 
it does not really implement the interface. The compiler should be reporting an error on that.
*/

// the compiler should be reporting two errors also in the following incorrect implementation"
INTERFACE ITestInterface
	METHOD Test1() AS VOID CLIPPER
	METHOD Test2() AS VOID STRICT
END INTERFACE

CLASS TestClass    IMPLEMENTS ITestInterface
	METHOD Test1() AS VOID STRICT
	METHOD Test2() AS VOID CLIPPER
END CLASS


// original sample
FUNCTION Start() AS VOID STRICT
    LOCAL o AS IProvider
    o := Parent{}
    o:CreateDAO()
	RETURN
    
CLASS Parent IMPLEMENTS IProvider
//  METHOD CreateDAO() AS VOID CLIPPER // causes the same problem, without /vo5 enabled
//  METHOD CreateDAO() AS VOID STRICT // works correctly
    METHOD CreateDAO() AS VOID 
    	? "test"
    RETURN
END CLASS


