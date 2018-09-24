// TestDbfNtx.prg
// Created by    : fabri
// Creation Date : 9/16/2018 4:17:53 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING Xunit
USING XSharp.RDD
USING XSharp.Rdd.Support


BEGIN NAMESPACE XSharp.RDD.Tests

	/// <summary>
	/// The TestDbfNtx class.
	/// </summary>
	CLASS TestDbfNtx
	
		[Fact, Trait("DbfNtx", "Create")];
		METHOD CheckCreateDBFNtx() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
			VAR dbInfo := DbOpenInfo{ "TestNtx1.DBF", "TestNtx1", 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			LOCAL fieldInfo AS STRING[]
			LOCAL rddInfo AS RddFieldInfo[]
			rddInfo := RddFieldInfo[]{fields:Length}
			FOR VAR i := __ARRAYBASE__ TO fields:Length - (1-__ARRAYBASE__)
				// 
				LOCAL currentField AS RddFieldInfo
				fieldInfo := fields[i]:Split( ',' )
				currentField := RddFieldInfo{ fieldInfo[DBS_NAME], fieldInfo[DBS_TYPE], Convert.ToInt32(fieldInfo[DBS_LEN]), Convert.ToInt32(fieldInfo[DBS_DEC]) }
				rddInfo[i] := currentField
			NEXT
			//
			myDBF := DbfNtx{}
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			dbInfo:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(dbInfo:WorkArea, myDBF)
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			
			//
			LOCAL ntxInfo AS DbOrderCreateInfo
			ntxInfo := DbOrderCreateInfo{}
			ntxInfo:BagName := "TestNtx1"
			ntxInfo:Order := "TestNtx1"
			ntxInfo:Expression := "_FIELD->ID"
			//
			Assert.Equal( TRUE, myDBF:OrderCreate( ntxInfo ) )
			//
			myDBF:Close()
			RETURN
			
		[Fact, Trait("DbfNtx", "Open")];
		METHOD OpenDBFNtx() AS VOID
			// ID,N,5,0;NAME,C,20,0
			VAR dbInfo := DbOpenInfo{ "TestNTX2.DBF", "customer", 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			myDBF := DbfNtx{}
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			dbInfo:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(dbInfo:WorkArea, myDBF)
			Assert.Equal( TRUE, myDBF:Open( dbInfo ) )
			//
			LOCAL ntxInfo AS DbOrderInfo
			ntxInfo := DbOrderInfo{}
			ntxInfo:BagName := "TestNTX2"
			ntxInfo:Order := "TestNTX2"
			// FilePath NullOrEmpty => Will get the FilePath of the DBF
			Assert.Equal( TRUE, myDBF:OrderListAdd( ntxInfo, "" ) )
			//
			myDBF:Close()
			RETURN
			
		[Fact, Trait("DbfNtx", "Value")];
		METHOD ReadDBFNtx() AS VOID
			// ID,N,5,0;NAME,C,20,0
			VAR dbInfo := DbOpenInfo{ "TestNTX2.DBF", "customer", 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			myDBF := DbfNtx{}
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			dbInfo:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(dbInfo:WorkArea, myDBF)
			VAR success := myDBF:Open( dbInfo ) 
			Assert.Equal( TRUE, success )
			//
			LOCAL ntxInfo AS DbOrderInfo
			ntxInfo := DbOrderInfo{}
			ntxInfo:BagName := "TestNTX2"
			ntxInfo:Order := "TestNTX2"
			// FilePath NullOrEmpty => Will get the FilePath of the DBF
			Assert.Equal( TRUE, myDBF:OrderListAdd( ntxInfo, "" ) )
			//
			WHILE ! myDBF:EoF
				FOR VAR i := 1 TO myDBF:FIELDCount
					// 
					LOCAL oData AS OBJECT
					oData := myDBF:GetValue( i )
					LOCAL str AS STRING
					str := oData:ToString()
				NEXT
				myDBF:Skip(1)
			ENDDO
			//
			myDBF:Close()
			RETURN
			
	END CLASS
END NAMESPACE // XSharp.RDD.Tests