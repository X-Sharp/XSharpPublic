//test primary constructor-core dialect/net sdk
FUNCTION Start( ) AS VOID
	LOCAL TypedCntr:=TestC1{1} AS TestC1
	xAssert(TypedCntr:Value()==1)
	
	LOCAL PrimaryCntr:=TestC2{2} AS TestC2
	xAssert(PrimaryCntr:Value==2)
	
	LOCAL BothCntr1:=TestC3{} AS TestC3
	xAssert(BothCntr1:Value()==0)
	LOCAL BothCntr2:=TestC3{3} AS TestC3
	xAssert(BothCntr2:Value()==3)
	LOCAL BothCntr3:=TestC3{3,1} AS TestC3
	xAssert(BothCntr3:Value()==3)
	
	LOCAL TypedStr:=TestS1{1} AS TestS1
	xAssert(TypedStr:Value()==1)
	
	LOCAL PrimaryStr:=TestS2{2} AS TestS2
	xAssert(PrimaryStr:Value==2)
	
	LOCAL BothStr1:=TestS3{} AS TestS3
	xAssert(BothStr1:Value()==0)
	LOCAL BothStr2:=TestS3{3} AS TestS3
	xAssert(BothStr2:Value()==3)
	LOCAL BothStr3:=TestS3{3,1} AS TestS3
	xAssert(BothStr3:Value()==3)
	
	LOCAL TypedRcd:=TestR1{1} AS TestR1
	xAssert(TypedRcd:Value()==1)
	
	LOCAL PrimaryRcd:=TestR2{2} AS TestR2
	xAssert(PrimaryRcd:Value==2)
	
	LOCAL BothRcd1:=TestR3{} AS TestR3
	xAssert(BothRcd1:Value()==0)
	LOCAL BothRcd2:=TestR3{3} AS TestR3
	xAssert(BothRcd2:Value()==3)
	LOCAL BothRcd3:=TestR3{3,1} AS TestR3
	xAssert(BothRcd3:Value()==3)
	
	LOCAL TypedRStr:=TestRS1{1} AS TestRS1
	xAssert(TypedRStr:Value()==1)
	
	LOCAL PrimaryRStr:=TestRS2{2} AS TestRS2
	xAssert(PrimaryRStr:Value==2)
	
	LOCAL BothRStr1:=TestRS3{} AS TestRS3
	xAssert(BothRStr1:Value()==0)
	LOCAL BothRStr2:=TestRS3{3} AS TestRS3
	xAssert(BothRStr2:Value()==3)
	LOCAL BothRStr3:=TestRS3{3,1} AS TestRS3
	xAssert(BothRStr3:Value()==3)	
	
RETURN

#region Class
CLASS TestC1 //Class with only a typed constructor
	PUBLIC CONSTRUCTOR (y AS INT)
	PUBLIC METHOD Value() AS INT
		RETURN 1
END CLASS

CLASS TestC2(x AS INT) //Class with just a primary constructor
	PUBLIC Value:= x AS INT
END CLASS

CLASS TestC3 (x AS INT) //Class with a primary construstor and two typed constructor
	PUBLIC CONSTRUCTOR ()
		SELF(0)
	PUBLIC CONSTRUCTOR(a AS INT, b AS INT)
		SELF(a)
	PUBLIC METHOD Value() AS INT
		RETURN x
END CLASS
#endregion

#region Struct
STRUCT TestS1 //Struct with only a typed constructor
	PUBLIC CONSTRUCTOR (y AS INT)
	PUBLIC METHOD Value() AS INT
		RETURN 1
END STRUCT

STRUCT TestS2(x AS INT) //Struct with just a primary constructor
	PUBLIC Value:= x AS INT
END STRUCT

STRUCT TestS3 (x AS INT) //Struct with a primary construstor and two typed constructor
	PUBLIC CONSTRUCTOR ()
		SELF(0)
	PUBLIC CONSTRUCTOR(a AS INT, b AS INT)
		SELF(a)
	PUBLIC METHOD Value() AS INT
		RETURN x
END STRUCT
#endregion
 
#region Record
RECORD TestR1 //Record with only a typed constructor
	PUBLIC CONSTRUCTOR (y AS INT)
	PUBLIC METHOD Value() AS INT
		RETURN 1
END RECORD

RECORD TestR2(x AS INT) //Record with just a primary constructor
	PUBLIC Value:= x AS INT
END RECORD

RECORD TestR3 (x AS INT) //Record with a primary construstor and two typed constructor
	PUBLIC CONSTRUCTOR ()
		SELF(0)
	PUBLIC CONSTRUCTOR(a AS INT, b AS INT)
		SELF(a)
	PUBLIC METHOD Value() AS INT
		RETURN x
END RECORD
#endregion

#region Record Struct
RECORD STRUCT TestRS1 //Record Struct with only a typed constructor
	PUBLIC CONSTRUCTOR (y AS INT)
	PUBLIC METHOD Value() AS INT
		RETURN 1
END STRUCT

RECORD STRUCT TestRS2(x AS INT) //Record Struct with just a primary constructor
	PUBLIC Value:= x AS INT
END STRUCT

RECORD STRUCT TestRS3 (x AS INT) //Record Struct with a primary construstor and two typed constructor
	PUBLIC CONSTRUCTOR ()
		SELF(0)
	PUBLIC CONSTRUCTOR(a AS INT, b AS INT)
		SELF(a)
	PUBLIC METHOD Value() AS INT
		RETURN x
END STRUCT
#endregion

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
