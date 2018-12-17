// NtxTests.prg
// Created by    : fabri
// Creation Date : 10/19/2018 12:01:38 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING XSharp.Rdd.Support
USING System.Diagnostics
USING Xsharp.Core


CLASS NtxTests

		METHOD CreateAppendSkipZero() AS VOID
			// Create the DBF, Define a Ntx, then add a some Data
			SELF:CreateAppendData( "XMenTest" )
			//
			VAR dbInfo := DbOpenInfo{ "XMenTest", "", 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			myDBF := DbfNtx{}
			
			VAR success := myDBF:Open( dbInfo ) 
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
			myDBF:OrderListAdd( ntxInfo )
			// FilePath NullOrEmpty => Will get the FilePath of the DBF
			LOCAL oData1, oData2 AS OBJECT
			//
			myDBF:GoTop()
			myDBF:Skip( 1 )
			// Field 1 == ID
			oData1 := myDBF:GetValue( 1 )
			myDBF:Skip( 0 )
			oData2 := myDBF:GetValue( 1 )
			//
			//Assert.Equal( oData1, oData2 )
			//
			RuntimeState.Workareas:CloseArea( area )



		METHOD CreateAppendData( baseFileName AS STRING ) AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( c';' ) AS STRING[]
			VAR dbInfo := DbOpenInfo{ baseFileName, "" , 1, FALSE, FALSE }
			//
			LOCAL myDBF AS DbfNtx
			LOCAL fieldInfo AS STRING[]
			LOCAL rddInfo AS RddFieldInfo[]
			rddInfo := RddFieldInfo[]{fields:Length}
			FOR VAR i := __ARRAYBASE__ TO fields:Length - (1-__ARRAYBASE__)
				// 
				LOCAL currentField AS RddFieldInfo
				fieldInfo := fields[i]:Split( c',' )
				currentField := RddFieldInfo{ fieldInfo[DBS_NAME], fieldInfo[DBS_TYPE], Convert.ToInt32(fieldInfo[DBS_LEN]), Convert.ToInt32(fieldInfo[DBS_DEC]) }
				rddInfo[i] := currentField
			NEXT
			//
			myDBF := DbfNtx{}
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			myDBF:Create( dbInfo ) 
			// WE HAVE TO SET THE WORKAREA INFO !!!!
			dbInfo:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
			RuntimeState.Workareas:SetArea(dbInfo:WorkArea, myDBF)
			//
			LOCAL ntxInfo AS DbOrderCreateInfo
			ntxInfo := DbOrderCreateInfo{}
			ntxInfo:BagName := baseFileName
			ntxInfo:Order := baseFileName
			ntxInfo:Expression := "ID"
			myDBF:OrderCreate( ntxInfo )
			//
			// Now, Add some Data
			//"ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
			LOCAL datas := "5,Diablo,T;2,Wolverine,T;4,Cyclops,T;3,Tornade,F;1,Professor Xavier,T" AS STRING
			LOCAL data := datas:Split( c';' ) AS STRING[]
			//
			FOR VAR i := __ARRAYBASE__ TO data:Length - (1-__ARRAYBASE__)
				// 
				LOCAL elt := data[i]:Split( c',' ) AS STRING[]
				myDBF:Append( FALSE )
				myDBF:PutValue( 1, Convert.ToInt32(elt[__ARRAYBASE__] ))
				myDBF:PutValue( 2, elt[__ARRAYBASE__+1])
				myDBF:PutValue( 3, String.Compare(elt[__ARRAYBASE__+2],"T",TRUE)==0 )
				myDBF:PutValue( 4, DateTime.Now )
			NEXT
			//myDBF:Close()
			RuntimeState.Workareas:CloseArea( dbInfo:WorkArea )
			
			RETURN
			
END CLASS
