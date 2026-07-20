//test primary constructor-vo dialect/net sdk
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
	
	LOCAL ClipperCntr:=TestC4{"apple", 42} AS TestC4
	xAssert(ClipperCntr:Value()==4)
	
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
	
	LOCAL ClipperStr:=TestS4{"notapple"} AS TestS4
	xAssert(ClipperStr:Value()==4)
	
//	LOCAL RecordCntr:=TestR{1} AS TestR
//	xAssert(RecordCntr:Value()==4)
	
//	LOCAL RStructCntr:=TestRS{1} AS TestRS
//	xAssert(RStructCntr:Value()==4)
	
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

CLASS TestC4 //Class with clipper constructor
	PUBLIC CONSTRUCTOR(a) CLIPPER
	PUBLIC METHOD Value() AS INT
		RETURN 4
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

STRUCT TestS4
	PUBLIC CONSTRUCTOR(b) CLIPPER
	PUBLIC METHOD Value() AS INT
		RETURN 4
END STRUCT
#endregion 
//Record with clipper constructor throws internal compiler error
#region Record 
//RECORD TestR //Record with only a clipper constructor
//	PUBLIC CONSTRUCTOR (y) CLIPPER
//	PUBLIC METHOD Value() AS INT
//		RETURN 4
//END RECORD
#endregion

#region Record Struct
//RECORD STRUCT TestRS //Record Struct with only a clipper constructor
//	PUBLIC CONSTRUCTOR (y) CLIPPER
//	PUBLIC METHOD Value() AS INT
//		RETURN 4
//END STRUCT
#endregion

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
