// 586. Incorrect overload chosen for array argument
// problem only happens in VO/vulcan dialect, the OBJECT overload is used
// in core dialect it works as expected, the OBJECT[] overload is used

#using System.Data

FUNCTION Start() AS VOID
	LOCAL a AS OBJECT[]
	a := <OBJECT>{"A",123}
	xAssert( Test(a) == "OBJECT[] overload" )
	
	
	OriginalReport()
RETURN

FUNCTION Test(o AS OBJECT) AS STRING
	? "object plain"
RETURN "OBJECT overload"
FUNCTION Test(o AS OBJECT[]) AS STRING
	? "object[]"
RETURN "OBJECT[] overload"

FUNCTION OriginalReport() AS VOID
	LOCAL oCol AS DataColumn
	LOCAL oDT := DataTable{"Companies"} AS DataTable
	
   // Create DataColumns
	oCol:=DataColumn{"COMPANY_UID"}
	oDT:Columns:Add(oCol)
	oCol:=DataColumn{"CompanyName"}
	oDT:Columns:Add(oCol)
	oCol:=DataColumn{"MsgType"}
	oDT:Columns:Add(oCol)
	oCol:=DataColumn{"Destination"}
	oDT:Columns:Add(oCol)
	oCol:=DataColumn{"Attention"}
	oDT:Columns:Add(oCol)
	
   // Create a PK on oDT to use it in the Find() method
	LOCAL oPKs:=DataColumn[]{2} AS DataColumn[]
	oPKs[1]:=oDT:Columns["COMPANY_UID"]
	oPKs[2]:=oDT:Columns["Destination"]
	oDT:PrimaryKey:=oPKs
	
	LOCAL oValues:=OBJECT[]{2} AS OBJECT[]
	oValues[1] := "678"
	oValues[2] := "george@softway.gr"
	
	LOCAL oRow AS DataRow
	oRow := oDT:Rows:Find(oValues)
RETURN	




PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

