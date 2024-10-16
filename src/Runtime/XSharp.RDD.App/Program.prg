USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.Rdd
USING XSharp.Rdd.Support

USING STATIC XSharp.Core.Functions

USING XSharp.RDD.Tests

BEGIN NAMESPACE XSharp.RDD.App

    FUNCTION Start1 AS VOID
        LOCAL oDbfCdx AS DbfCdx
        VAR dbInfo := DbOpenInfo{ "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF", "TEST10K", 1, FALSE, FALSE }
        oDbfCdx := DbfCdx{}
        oDbfCdx:Open(dbInfo)
        WAIT
        RETURN


	FUNCTION Start() AS VOID
		//
		CoreDb.UseArea(TRUE, "DBF", "customer.DBF", "CUSTOMER", TRUE, TRUE)
		? CoreDb.Dbf()
		? "Fields", CoreDb.FCount(), "Records", CoreDb.LastRec(), "RecSize", CoreDb.RecSize()
		LOCAL i AS DWORD
		FOR i := 1 TO CoreDb.FCount()
			LOCAL oValue := NULL AS OBJECT
			IF CoreDb.FieldGet(i, REF oValue)
				? i, CoreDb.FieldName(i), oValue
			ELSE
				? i, CoreDb.FieldName(i), "** Error **"
			ENDIF
		NEXT
		CoreDb.Skip(1)
		FOR i := 1 TO CoreDb.FCount()
			LOCAL oValue := NULL AS OBJECT
			IF CoreDb.FieldGet(i, REF oValue)
				? i, CoreDb.FieldName(i), oValue
			ELSE
				? i, CoreDb.FieldName(i), "** Error **"
			ENDIF
		NEXT
		CoreDb.GoTop()
		? "GoTop Recno" , CoreDb.Recno(), "EOF", CoreDb.EOF(), "BOF", CoreDb.BOF()
		CoreDb.Skip(-1)
		? "After skip -1 Recno" , CoreDb.Recno(), "EOF", CoreDb.EOF(), "BOF", CoreDb.BOF()
		CoreDb.GoBottom()
		? "GoBottom Recno" , CoreDb.Recno(), "EOF", CoreDb.EOF(), "BOF", CoreDb.BOF()
		CoreDb.Skip(1)
		? "Skip 1 Recno" , CoreDb.Recno(), "EOF", CoreDb.EOF(), "BOF", CoreDb.BOF()
		CoreDb.CloseArea()
		Console.Read()
		//
		LOCAL myTest := TestDBF_Console{} AS TestDBF_Console
		myTest:OpenDBF()
		myTest:OpenDBFErr()
		myTest:OpenDBFShowFields()
		myTest:CheckFieldInfo()
		myTest:CheckFieldValue()
		myTest:WriteDataValue()
		myTest:CheckSkip()
		myTest:CheckAppend()
		myTest:CheckAppendData()
		myTest:CheckCreateDBF()
		myTest:CheckCreateAppendDBF()
		//myTest:CheckMemo()
		//
		LOCAL myNTXTest AS TestDbfNtx
		myNTXTest := TestDbfNtx{}
		myNTXTest:CreateAppendSkipZero()
		//
		Console.WriteLine("Hello World!")
		Console.WriteLine("Press any key to continue...")
		Console.ReadKey()


	STATIC CLASS MyAssert
		STATIC METHOD Equal( a AS OBJECT, b AS OBJECT ) AS VOID
			RETURN
    END CLASS


	CLASS TestDBF_Console

		METHOD OpenDBF() AS VOID
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			MyAssert.Equal( TRUE, myDBF:Open( dbInfo ) )
			//
			myDBF:Close()
			RETURN

		METHOD OpenDBFErr() AS VOID
			VAR dbInfo := DbOpenInfo{ "noFile.DBF", "noFile", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}

			MyAssert.Equal( FALSE, myDBF:Open( dbInfo ) )
			//
			myDBF:Close()
			RETURN

		METHOD OpenDBFShowFields() AS VOID
			VAR fields := <STRING>{ "CUSTNUM", "FIRSTNAME", "LASTNAME","ADDRESS","CITY","STATE","ZIP", "PHONE", "FAX" }
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			MyAssert.Equal( TRUE, myDBF:Open( dbInfo ) )
			//
			Console.WriteLine( "ArrayBase is " + Convert.ToString( __ARRAYBASE__ ) )
			//MyAssert.Equal( myDBF:FIELDCount, myDBF:_Fields:Length )
			FOR VAR i := 1 TO myDBF:FIELDCount
				VAR f1 := fields[i]
				VAR f2 := myDBF:FieldName( i )
				Console.WriteLine( i:ToString() + " : " + f1:ToString()  + " == " + f2:ToString()  )
			NEXT
			//
			Console.WriteLine( "RecCount : " + myDBF:RecCount:ToString() )
			//
			myDBF:Close()

			RETURN

		METHOD CloseDBF() AS VOID
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				// Should FAIL as currently no ClearScope, ClearRel, ...
				MyAssert.Equal( TRUE, myDBF:Close() )
			ENDIF
			RETURN

		METHOD WriteDataValue() AS VOID
			//
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				FOR VAR i := 1 TO myDBF:FIELDCount
					//
					LOCAL oData AS OBJECT
					oData := myDBF:GetValue( i )
					Console.WriteLine( myDBF:FieldInfo( i, DBS_NAME, NIL ):ToString()  + " == " + oData:ToString() )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckFields() AS VOID
			VAR fields := <STRING>{ "CUSTNUM", "FIRSTNAME", "LASTNAME","ADDRESS","CITY","STATE","ZIP", "PHONE", "FAX" }
			//VAR types :=  <STRING>{ "N", "C", "C","C","C","C","C", "C", "C" }
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				// Right number of Fields ?
				MyAssert.Equal(Fields:Length, myDBF:FieldCount)
				FOR VAR i := 1 TO myDBF:FIELDCount
					// Right Name decoding ?
					MyAssert.Equal( fields[i], myDBF:FieldName( i ) )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckFieldNames() AS VOID
			VAR fields := <STRING>{ "CUSTNUM", "FIRSTNAME", "LASTNAME","ADDRESS","CITY","STATE","ZIP", "PHONE", "FAX" }
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				// Right number of Fields ?
				MyAssert.Equal(Fields:Length, myDBF:FieldCount)
				FOR VAR i := 1 TO myDBF:FIELDCount
					// Right Name decoding ?
					MyAssert.Equal( i, myDBF:FieldIndex( fields[i] ) )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckFieldInfo() AS VOID
			VAR fieldDefs := "CUSTNUM,N,5,0;FIRSTNAME,C,10,0;LASTNAME,C,10,0;ADDRESS,C,25,0;CITY,C,15,0;STATE,C,2,0;ZIP,C,5,0;PHONE,C,13,0;FAX,C,13,0"
			VAR fields := fieldDefs:Split( c';' )
			//
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				// Right number of Fields ?
				MyAssert.Equal(fields:Length, myDBF:FieldCount)
				FOR VAR i := 1 TO myDBF:FIELDCount
					// Right decoding ?
					VAR fieldInfo := fields[i]:Split( c',' )
					MyAssert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_NAME, NIL ) )
					MyAssert.Equal( fieldInfo[DBS_TYPE], myDBF:FieldInfo( i, DBS_TYPE, NIL ) )
					MyAssert.Equal( Convert.ToInt32(fieldInfo[DBS_LEN]), myDBF:FieldInfo( i, DBS_LEN, NIL ) )
					MyAssert.Equal( Convert.ToInt32(fieldInfo[DBS_DEC]), myDBF:FieldInfo( i, DBS_DEC, NIL ) )
					MyAssert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_ALIAS, NIL ) )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckFieldValue() AS VOID
			VAR values := <OBJECT>{ 2, "Robert", "Evans","732 Johnson Street","New York","NY","11501", "(212)764-1246", "(212)764-1877" }
			//
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				FOR VAR i := 1 TO myDBF:FIELDCount
					//
					LOCAL oData AS OBJECT
					oData := myDBF:GetValue( i )
					MyAssert.Equal( values[i], oData )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckAppend() AS VOID
			//
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			IF myDBF:Open( dbInfo )
				//
				LOCAL nbrBefore := myDBF:RecCount AS LONG
				//
				myDBF:Append( FALSE )
				//
				MyAssert.Equal( nbrBefore+1, myDBF:RecCount )
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckAppendData() AS VOID
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			IF myDBF:Open( dbInfo )
				//
				LOCAL nbrBefore := myDBF:RecCount AS LONG
				//
				myDBF:Append( FALSE )
				//
				MyAssert.Equal(  myDBF:RecCount, nbrBefore+1 )
				// Now, Add some Data
				//"CUSTNUM,N,5,0;FIRSTNAME,C,10,0;LASTNAME,C,10,0;ADDRESS,C,25,0;CITY,C,15,0;STATE,C,2,0;ZIP,C,5,0;PHONE,C,13,0;FAX,C,13,0"
				myDBF:PutValue( 1, 5 )
				myDBF:PutValue( 2, "Fabrice" )
				MyAssert.Equal( 5, myDBF:GetValue( 1 ) )
				MyAssert.Equal( "Fabrice", myDBF:GetValue( 2 ) )
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckSkip() AS VOID
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				// Start Pos
				MyAssert.Equal( 1, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Bof )
				MyAssert.Equal( FALSE, myDBF:Eof )
				// Move Backward
				myDBF:Skip(-1)
				MyAssert.Equal( 1, myDBF:RecNo )
				MyAssert.Equal( TRUE, myDBF:Bof )
				// Move to Top
				myDBF:GoTop()
				MyAssert.Equal( 1, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Bof )
				// Move to Bottom
				myDBF:GoBottom()
				MyAssert.Equal( myDBF:RecCount, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Eof )
				// Move beyond bottom
				myDBF:Skip(1)
				MyAssert.Equal( myDBF:RecCount+1, myDBF:RecNo )
				MyAssert.Equal( TRUE, myDBF:Eof )
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckDeleteRecall() AS VOID
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			IF myDBF:Open( dbInfo )
				// Move to Bottom
				myDBF:GoBottom()
				MyAssert.Equal( myDBF:RecCount, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Eof )
				// Delete Record
				myDBF:Delete()
				MyAssert.Equal( TRUE, myDBF:Deleted )
				// Now, move and Come Back
				// Move to Top
				myDBF:GoTop()
				MyAssert.Equal( 1, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Bof )
				// Move to Bottom
				myDBF:GoBottom()
				MyAssert.Equal( myDBF:RecCount, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Eof )
				// Still Deleted ?
				MyAssert.Equal( TRUE, myDBF:Deleted )
				// Ok, now try to Recall
				myDBF:Recall()
				MyAssert.Equal( FALSE, myDBF:Deleted )
				// And again...
				// Move to Top
				myDBF:GoTop()
				MyAssert.Equal( 1, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Bof )
				// Move to Bottom
				myDBF:GoBottom()
				MyAssert.Equal( myDBF:RecCount, myDBF:RecNo )
				MyAssert.Equal( FALSE, myDBF:Eof )
				// Still Alive ?
				MyAssert.Equal( FALSE, myDBF:Deleted )
				//
				myDBF:Close()
			ENDIF
			RETURN

		METHOD CheckAddFields() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,10,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( c';' ) AS STRING[]
			//VAR dbInfo := DbOpenInfo{ "XSharpTest.DBF", "XSharpTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			// Let's say Three Fields
			myDBF:SetFieldExtent( fields:Length )
			MyAssert.Equal( fields:Length, myDBF:FieldCount )
			//
			LOCAL fieldInfo AS STRING[]
			FOR VAR i := 1 TO myDBF:FIELDCount
				//
				LOCAL currentField AS RddFieldInfo
				fieldInfo := fields[i]:Split( c',' )
				currentField := RddFieldInfo{ fieldInfo[DBS_NAME], fieldInfo[DBS_TYPE], Convert.ToInt32(fieldInfo[DBS_LEN]), Convert.ToInt32(fieldInfo[DBS_DEC]) }
				myDBF:AddField( currentField )
			NEXT
			// Now Check
			// Right number of Fields ?
			MyAssert.Equal(fields:Length, myDBF:FieldCount)
			FOR VAR i := 1 TO myDBF:FIELDCount
				// Right decoding ?
				fieldInfo := fields[i]:Split( c',' )
				MyAssert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_NAME, NIL ) )
				MyAssert.Equal( fieldInfo[DBS_TYPE], myDBF:FieldInfo( i, DBS_TYPE, NIL ) )
				MyAssert.Equal( Convert.ToInt32(fieldInfo[DBS_LEN]), myDBF:FieldInfo( i, DBS_LEN, NIL ) )
				MyAssert.Equal( Convert.ToInt32(fieldInfo[DBS_DEC]), myDBF:FieldInfo( i, DBS_DEC, NIL ) )
				MyAssert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_ALIAS, NIL ) )
			NEXT
			//
			RETURN

		METHOD CheckCreateFields() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,10,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( c';' ) AS STRING[]
			//VAR dbInfo := DbOpenInfo{ "XSharpTest.DBF", "XSharpTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			// Right number of Fields ?
			MyAssert.Equal(fields:Length, myDBF:FieldCount)
			FOR VAR i := 1 TO myDBF:FIELDCount
				// Right decoding ?
				fieldInfo := fields[i]:Split( c',' )
				MyAssert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_NAME, NIL ) )
				MyAssert.Equal( fieldInfo[DBS_TYPE], myDBF:FieldInfo( i, DBS_TYPE, NIL ) )
				MyAssert.Equal( Convert.ToInt32(fieldInfo[DBS_LEN]), myDBF:FieldInfo( i, DBS_LEN, NIL ) )
				MyAssert.Equal( Convert.ToInt32(fieldInfo[DBS_DEC]), myDBF:FieldInfo( i, DBS_DEC, NIL ) )
				MyAssert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_ALIAS, NIL ) )
			NEXT
			//
			RETURN

		METHOD CheckCreateDBF() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( c';' ) AS STRING[]
			VAR dbInfo := DbOpenInfo{ "XMenTest.DBF", "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			MyAssert.Equal( TRUE, myDBF:Create( dbInfo ) )
			//
			myDBF:Close()
			RETURN

		METHOD CheckMemo() AS VOID
			// Now Modify in the same space
			VAR dbInfo := DbOpenInfo{ "CustNtx.DBF", "CustNtx", 1, FALSE, FALSE }
			LOCAL myDBF := DBFDBT{} AS DBFDBT
			//VAR Memos := List<STRING>{}
			// Now, Modify the Memo
			myDBF:Open( dbInfo )
			//
			LOCAL oData AS OBJECT
			oData := myDBF:GetValue( 14 )
            ? oData
			//
			myDBF:Close()

		METHOD CheckCreateAppendDBF() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( c';' ) AS STRING[]
			VAR dbInfo := DbOpenInfo{ "XMenTest.DBF", "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			MyAssert.Equal( TRUE, myDBF:Create( dbInfo ) )
			// Now, Add some Data
			//"ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
			LOCAL datas := "1,Professor Xavier,T;2,Wolverine,T;3,Tornade,F;4,Cyclops,T;5,Diablo,T" AS STRING
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
			myDBF:Close()
			// Now, Verify
			myDBF:Open( dbInfo )
			FOR VAR i := __ARRAYBASE__ TO data:Length - (1-__ARRAYBASE__)
				//
				LOCAL elt := data[i]:Split( c',' ) AS STRING[]
				MyAssert.Equal( Convert.ToInt32(elt[__ARRAYBASE__] ), myDBF:GetValue(1) )
				MyAssert.Equal( elt[__ARRAYBASE__+1], myDBF:GetValue(2) )
				MyAssert.Equal( String.Compare(elt[__ARRAYBASE__+2],"T",TRUE)==0, myDBF:GetValue(3) )
				LOCAL o AS OBJECT
				LOCAL dt := DateTime.MinValue AS DateTime
				o := myDBF:GetValue(4)
				IF ( o IS DateTime )
					dt := (DateTime)o
				ENDIF
				MyAssert.Equal( TRUE, o IS DateTime )
				MyAssert.Equal( DateTime.Now:ToString("yyyyMMdd"), dt:ToString("yyyyMMdd") )
				myDBF:Skip(1)
			NEXT
			//
			myDBF:Close()
			RETURN



	END CLASS

END NAMESPACE
