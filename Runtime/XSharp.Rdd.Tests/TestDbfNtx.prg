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
USING System.Diagnostics
USING Xsharp.Core

BEGIN NAMESPACE XSharp.RDD.Tests

	/// <summary>
	/// The TestDbfNtx class.
	/// </summary>
	PUBLIC CLASS TestDbfNtx

        METHOD InitTest() AS VOID
           // the next line ensures that we have included XSharp.RT as reference
            LOCAL d := __Date{0,0,0} AS __Date
            SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

            RETURN
	
		[Fact, Trait("DbfNtx", "Create")];
		METHOD CreateDBFNtx() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            InitTest()
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
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			dbInfo:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(dbInfo:WorkArea, myDBF)
			//
			LOCAL ntxInfo AS DbOrderCreateInfo
			ntxInfo := DbOrderCreateInfo{}
			ntxInfo:BagName := "TestNtx1"
			ntxInfo:Order := "TestNtx1"
			ntxInfo:Expression := "ID"
			//
			Assert.Equal( TRUE, myDBF:OrderCreate( ntxInfo ) )
			//
			RuntimeState.Workareas:CloseArea( dbInfo:WorkArea )
			RETURN
			
		[Fact, Trait("DbfNtx", "Open")];
		METHOD OpenDBFNtx() AS VOID
			// ID,N,5,0;NAME,C,20,0
            InitTest()
			VAR dbInfo := DbOpenInfo{ "TestNTX2.DBF", "customer", 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			myDBF := DbfNtx{}
			Assert.Equal( TRUE, myDBF:Open( dbInfo ) )
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			LOCAL area  := 0    AS DWORD
			area  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(area, myDBF)
			RuntimeState.Workareas:CurrentWorkAreaNO := area
			dbInfo:WorkArea := area
			//
			LOCAL ntxInfo AS DbOrderInfo
			ntxInfo := DbOrderInfo{}
			ntxInfo:BagName := "TestNTX2"
			ntxInfo:Order := "TestNTX2"
			// FilePath NullOrEmpty => Will get the FilePath of the DBF
			Assert.Equal( TRUE, myDBF:OrderListAdd( ntxInfo ) )
			//
			RuntimeState.Workareas:CloseArea( dbInfo:WorkArea )
			RETURN
			
		[Fact, Trait("DbfNtx", "Read")];
		METHOD ReadDBFNtx() AS VOID
			//
            InitTest()
			SELF:CheckOrder( "TestNTX2" )
			RETURN
			
			
		[Fact, Trait("DbfNtx", "CreateAppend")];
		METHOD CreateAppend() AS VOID
			// Create the DBF, Define a Ntx, then add a some Data
            InitTest()
            SELF:CreateAppendData( "XMenTestMe" )
			// Now, Verify
			SELF:CheckOrder( "XMenTestMe" )

		[Fact, Trait("DbfNtx", "CreateAppendSkipZero")];
		METHOD CreateAppendSkipZero() AS VOID
			// Create the DBF, Define a Ntx, then add a some Data
            InitTest()
			SELF:CreateAppendData( "XMenTest" )
			//
			VAR dbInfo := DbOpenInfo{ "XMenTest", "", 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			myDBF := DbfNtx{}
			
			VAR success := myDBF:Open( dbInfo ) 
			Assert.Equal( TRUE, success )
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			LOCAL area  := 0    AS DWORD
			area  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(area, myDBF)
			RuntimeState.Workareas:CurrentWorkAreaNO := area
			dbInfo:WorkArea := area
			//
			//FieldPos( "ID" )
			LOCAL ntxInfo AS DbOrderInfo
			ntxInfo := DbOrderInfo{}
			ntxInfo:BagName := "XMenTest"
			ntxInfo:Order := "XMenTest"
			// FilePath NullOrEmpty => Will get the FilePath of the DBF
			Assert.Equal( TRUE, myDBF:OrderListAdd( ntxInfo ) )
			LOCAL oData1, oData2 AS OBJECT
			//
			myDBF:GoTop()
			myDBF:Skip( 1 )
			// Field 1 == ID
			oData1 := myDBF:GetValue( 1 )
			myDBF:Skip( 0 )
			oData2 := myDBF:GetValue( 1 )
			//
			Assert.Equal( oData1, oData2 )
			//
			RuntimeState.Workareas:CloseArea( area )
			
		METHOD CreateAppendData( baseFileName AS STRING ) AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            InitTest()
            VAR dbInfo := DbOpenInfo{ baseFileName, "" , 1, FALSE, FALSE }
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
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			dbInfo:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(dbInfo:WorkArea, myDBF)
			//
			LOCAL ntxInfo AS DbOrderCreateInfo
			ntxInfo := DbOrderCreateInfo{}
			ntxInfo:BagName := baseFileName
			ntxInfo:Order := baseFileName
			ntxInfo:Expression := "ID"
			//
			Assert.Equal( TRUE, myDBF:OrderCreate( ntxInfo ) )
			//
			// Now, Add some Data
			//"ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
			LOCAL datas := "5,Diablo,T;2,Wolverine,T;4,Cyclops,T;3,Tornade,F;1,Professor Xavier,T" AS STRING
			LOCAL data := datas:Split( ';' ) AS STRING[]
			//
			FOR VAR i := __ARRAYBASE__ TO data:Length - (1-__ARRAYBASE__)
				// 
				LOCAL elt := data[i]:Split( ',' ) AS STRING[]
				myDBF:Append( FALSE )
				myDBF:PutValue( 1, Convert.ToInt32(elt[__ARRAYBASE__] ))
				myDBF:PutValue( 2, elt[__ARRAYBASE__+1])
				myDBF:PutValue( 3, String.Compare(elt[__ARRAYBASE__+2],"T",TRUE)==0 )
				myDBF:PutValue( 4, DateTime.Now )
			NEXT
			//myDBF:Close()
			RuntimeState.Workareas:CloseArea( dbInfo:WorkArea )
			
			RETURN
			
		[Fact, Trait("DbfNtx", "BigCreateAppend")];
		METHOD BigCreateAppend() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            InitTest()
			VAR dbInfo := DbOpenInfo{ "XMenTest.DBF", "XMenTest", 1, FALSE, FALSE }
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
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			dbInfo:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(dbInfo:WorkArea, myDBF)
			//
			LOCAL ntxInfo AS DbOrderCreateInfo
			ntxInfo := DbOrderCreateInfo{}
			ntxInfo:BagName := "XMenTest"
			ntxInfo:Order := "XMenTest"
			ntxInfo:Expression := "ID"
			//
			Assert.Equal( TRUE, myDBF:OrderCreate( ntxInfo ) )
			//
			// Now, Add some Data
			LOCAL rnd AS Random
			//
			rnd := Random{ (LONG)DateTime.Now.Ticks }
			//"ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
			// 3000 samples
			FOR VAR i := 1 TO 3000
				// 
				myDBF:Append( FALSE )
				myDBF:PutValue( 1,  rnd:@@NEXT( 1, 5000 ))
				myDBF:PutValue( 2,  LoremIpsum(20) )
				myDBF:PutValue( 3,  rnd:@@NEXT(0,2) == 1 )
				myDBF:PutValue( 4, DateTime.Now )
			NEXT
			//myDBF:Close()
			RuntimeState.Workareas:CloseArea( dbInfo:WorkArea )
			// Now, Verify
			SELF:CheckOrder( "XMenTest" )
			RETURN
			
		// Read a DBF/NTX who's first Field is a Number used as Index Key
		METHOD CheckOrder( baseFileName AS STRING ) AS VOID
			//
			VAR dbInfo := DbOpenInfo{ baseFileName, "", 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			myDBF := DbfNtx{}
            InitTest()
			VAR success := myDBF:Open( dbInfo ) 
			Assert.Equal( TRUE, success )
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			LOCAL area  := 0    AS DWORD
			area  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(area, myDBF)
			RuntimeState.Workareas:CurrentWorkAreaNO := area
			dbInfo:WorkArea := area
			//
			//FieldPos( "ID" )
			LOCAL ntxInfo AS DbOrderInfo
			ntxInfo := DbOrderInfo{}
			ntxInfo:BagName := baseFileName
			ntxInfo:Order := baseFileName
			// FilePath NullOrEmpty => Will get the FilePath of the DBF
			Assert.Equal( TRUE, myDBF:OrderListAdd( ntxInfo ) )
			myDBF:GoTop()
			//
			LOCAL idList AS List<INT>
			idList := List<INT>{}
			WHILE ! myDBF:EoF
				//				Debug.Write( "---===---" + Environment.NewLine )
				//				FOR VAR i := 1 TO myDBF:FIELDCount
				//					// 
				//					LOCAL oData AS OBJECT
				//					oData := myDBF:GetValue( i )
				//					LOCAL str AS STRING
				//					str := oData:ToString()
				//					Debug.Write( str +" - " )
				//				NEXT
				//				Debug.Write( Environment.NewLine )
				LOCAL oData AS OBJECT
				// Field 1 == ID
				oData := myDBF:GetValue( 1 )
				idList:Add( Convert.ToInt32(oData))
				myDBF:Skip(1)
			ENDDO
			// Check that the ID are sorted by value
			FOR VAR i := 0 TO (idList:Count-2)
				Assert.Equal( TRUE, idList[i] <= idList[i+1] )
			NEXT
			//
			RuntimeState.Workareas:CloseArea( area )
			//myDBF:Close()
			RETURN
			
			
	END CLASS
END NAMESPACE // XSharp.RDD.Tests
