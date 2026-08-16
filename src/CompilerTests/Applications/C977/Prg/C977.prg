// 977. DataSession implementation
// https://github.com/X-Sharp/XSharpPublic/issues/2033

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

FUNCTION NewDataSession( )
	STATIC LOCAL nId := 1 AS INT
	nId ++
	LOCAL oNewDataSession AS  XSharp.RDD.DataSession
	oNewDataSession := XSharp.RDD.DataSession{ nId, "DataSession" + nId:ToString( ) }
	XSharp.RuntimeState.SetDataSession( oNewDataSession )
RETURN oNewDataSession

CLASS TestObject INHERIT XSharp.VFP.Custom
	CONSTRUCTOR( lCreatesNewDataSession AS LOGIC )
		super( )
		? "Constructor executing in context of datasession Id:", XSharp.RuntimeState.DataSession:Id
		IF lCreatesNewDataSession
			SELF:DataSession := NewDataSession( )
		ELSE
			SELF:DataSession := XSharp.RuntimeState.DataSession
		END IF
		? "Constructor exits in context of datasession Id:", XSharp.RuntimeState.DataSession:Id
		
	METHOD GetObjectDataSessionId( nExpectedToExecuteInDataSessionId AS INT ) AS INT
		? "Actual method code executing in context of datasession Id:", XSharp.RuntimeState.DataSession:Id
		xAssert( SELF:DataSession:Id == nExpectedToExecuteInDataSessionId )
		xAssert( XSharp.RuntimeState.DataSession:Id == nExpectedToExecuteInDataSessionId )
	RETURN SELF:DataSession:Id
END CLASS

FUNCTION Start() AS VOID
LOCAL o1,o2 AS TestObject

? "Object 1, created in DataSession 1"
o1 := TestObject{ FALSE }
? "Object reports working in DataSession Id:", o1:GetObjectDataSessionId( 1 )

? "Returned from object code, we should now continue execution in scope of DataSession 1. Current actual DataSession Id:", XSharp.RuntimeState.DataSession:Id
xAssert( XSharp.RuntimeState.DataSession:Id ==  1 )
	
? "-------------"	

? "Object 2, created in DataSession 1, but assigns itself DataSession 2"
	
o2 := TestObject{ TRUE }
? "Upon returning from the constructor, we should be back on DataSession 1 ('our' DataSession). Current actual DataSession Id:", XSharp.RuntimeState.DataSession:Id
xAssert( XSharp.RuntimeState.DataSession:Id ==  1 )
? "Object reports working in DataSession Id:", o2:GetObjectDataSessionId( 2 )

? "Returned from object code, we should now continue execution in scope of DataSession 1. Current actual DataSession Id:", XSharp.RuntimeState.DataSession:Id
xAssert( XSharp.RuntimeState.DataSession:Id ==  1 )

? "-------------"	

? "Object 1, it must use DataDession 1 again"
	
? "Object reports working in DataSession Id:", o1:GetObjectDataSessionId( 1 )

? "Returned from object code, we should now continue execution in scope of DataSession 1. Current actual DataSession Id:", XSharp.RuntimeState.DataSession:Id
xAssert( XSharp.RuntimeState.DataSession:Id ==  1 )

? "-------------"	

? "Explicitly selecting another DataSession. Object code should still execute in it's own DataSession context"

NewDataSession( )
? "New assigned DataSession Id:", XSharp.RuntimeState.DataSession:Id
xAssert( XSharp.RuntimeState.DataSession:Id ==  3 )

? "-------------"	

? "Object 1, must use DataDession 1 again"
? "Object reports working in DataSession Id:", o1:GetObjectDataSessionId( 1 )

? "-------------"	

? "Calling method on object 2 again, which should execute in the scope of DataSession 2:"
? "Object reports working in DataSession Id:", o2:GetObjectDataSessionId( 2 )

PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
//	? "FAILED!"
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
	? "Assertion passed"
RETURN
