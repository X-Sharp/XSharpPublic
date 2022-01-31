// TestDBF.prg
// Created by    : fabri
// Creation Date : 4/24/2018 5:21:57 PM
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
	/// The TestDBF class.
	/// </summary>
	CLASS TestDBF



		[Fact, Trait("Dbf", "Open")];
		METHOD OpenDBF() AS VOID
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			Assert.Equal( TRUE, myDBF:Open( dbInfo ) )
			//
			myDBF:Close()
			RETURN

		[Fact, Trait("Dbf", "Open")];
		METHOD OpenDBFErr() AS VOID
			VAR dbInfo := DbOpenInfo{ "noFile.DBF", "noFile", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}

			Assert.Throws( typeof(XSharp.RDD.RddError), {=> myDBF:Open( dbInfo ) })
			//
			myDBF:Close()
			RETURN

		[Fact, Trait("Dbf", "Close")];
		METHOD CloseDBF() AS VOID
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				// Should FAIL as currently no ClearScope, ClearRel, ...
				Assert.Equal( TRUE, myDBF:Close() )
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Fields")];
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
				Assert.Equal(Fields:Length, myDBF:FieldCount)
				FOR VAR i := 1 TO myDBF:FIELDCount
					// Right Name decoding ?
					Assert.Equal( fields[i], myDBF:FieldName( i ) )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Fields")];
		METHOD CheckFieldNames() AS VOID
			VAR fields := <STRING>{ "CUSTNUM", "FIRSTNAME", "LASTNAME","ADDRESS","CITY","STATE","ZIP", "PHONE", "FAX" }
			// CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				// Right number of Fields ?
				Assert.Equal(Fields:Length, myDBF:FieldCount)
				FOR VAR i := 1 TO myDBF:FIELDCount
					// Right Name decoding ?
					Assert.Equal( i, myDBF:FieldIndex( fields[i] ) )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Fields")];
		METHOD CheckFieldInfo() AS VOID
			VAR fieldDefs := "CUSTNUM,N,5,0;FIRSTNAME,C,10,0;LASTNAME,C,10,0;ADDRESS,C,25,0;CITY,C,15,0;STATE,C,2,0;ZIP,C,5,0;PHONE,C,13,0;FAX,C,13,0"
			VAR fields := fieldDefs:Split( ';' )
			//
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				// Right number of Fields ?
				Assert.Equal(fields:Length, myDBF:FieldCount)
				FOR VAR i := 1 TO myDBF:FIELDCount
					// Right decoding ?
					VAR fieldInfo := fields[i]:Split( ',' )
					Assert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_NAME, NIL ) )
					Assert.Equal( fieldInfo[DBS_TYPE], myDBF:FieldInfo( i, DBS_TYPE, NIL ) )
					Assert.Equal( Convert.ToInt32(fieldInfo[DBS_LEN]), myDBF:FieldInfo( i, DBS_LEN, NIL ) )
					Assert.Equal( Convert.ToInt32(fieldInfo[DBS_DEC]), myDBF:FieldInfo( i, DBS_DEC, NIL ) )
					Assert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_ALIAS, NIL ) )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Fields")];
		METHOD CheckFieldValue() AS VOID
			VAR values := <OBJECT>{ 2, "Robert", "Evans","732 Johnson Street","New York","NY","11501", "(212)764-1246", "(212)764-1877" }
			//
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				//
				FOR VAR i := 2 TO myDBF:FIELDCount
					//
					LOCAL tmp AS STRING
					tmp := (STRING)myDBF:GetValue( i )
					Assert.Equal( values[i], tmp:Trim() )
				NEXT
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Append")];
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
				Assert.Equal( nbrBefore+1, myDBF:RecCount )
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "AppendData")];
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
				Assert.Equal(  myDBF:RecCount, nbrBefore+1 )
				// Now, Add some Data
				//"CUSTNUM,N,5,0;FIRSTNAME,C,10,0;LASTNAME,C,10,0;ADDRESS,C,25,0;CITY,C,15,0;STATE,C,2,0;ZIP,C,5,0;PHONE,C,13,0;FAX,C,13,0"
				myDBF:PutValue( 1, 5 )
				myDBF:PutValue( 2, "Fabrice" )
				Assert.Equal( 5, Convert.ToInt32(myDBF:GetValue( 1 ) ))
				Assert.Equal( "Fabrice", Convert.ToString(myDBF:GetValue( 2 )):Trim() )
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Move")];
		METHOD CheckSkip() AS VOID
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
			//
			VAR myDBF := DBF{}
			IF myDBF:Open( dbInfo )
				// Start Pos
				Assert.Equal( 1, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Bof )
				Assert.Equal( FALSE, myDBF:Eof )
				// Move Backward
				myDBF:Skip(-1)
				Assert.Equal( 1, myDBF:RecNo )
				Assert.Equal( TRUE, myDBF:Bof )
				// Move to Top
				myDBF:GoTop()
				Assert.Equal( 1, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Bof )
				// Move to Bottom
				myDBF:GoBottom()
				Assert.Equal( myDBF:RecCount, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Eof )
				// Move beyond bottom
				myDBF:Skip(1)
				Assert.Equal( myDBF:RecCount+1, myDBF:RecNo )
				Assert.Equal( TRUE, myDBF:Eof )
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "DeleteRecall")];
		METHOD CheckDeleteRecall() AS VOID
			VAR dbInfo := DbOpenInfo{ "customer.DBF", "", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			IF myDBF:Open( dbInfo )
				// Move to Bottom
				myDBF:GoBottom()
				Assert.Equal( myDBF:RecCount, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Eof )
				// Delete Record
				myDBF:Delete()
				Assert.Equal( TRUE, myDBF:Deleted )
				// Now, move and Come Back
				// Move to Top
				myDBF:GoTop()
				Assert.Equal( 1, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Bof )
				// Move to Bottom
				myDBF:GoBottom()
				Assert.Equal( myDBF:RecCount, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Eof )
				// Still Deleted ?
				Assert.Equal( TRUE, myDBF:Deleted )
				// Ok, now try to Recall
				myDBF:Recall()
				Assert.Equal( FALSE, myDBF:Deleted )
				// And again...
				// Move to Top
				myDBF:GoTop()
				Assert.Equal( 1, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Bof )
				// Move to Bottom
				myDBF:GoBottom()
				Assert.Equal( myDBF:RecCount, myDBF:RecNo )
				Assert.Equal( FALSE, myDBF:Eof )
				// Still Alive ?
				Assert.Equal( FALSE, myDBF:Deleted )
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Fields")];
		METHOD CheckAddFields() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,10,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
			//VAR dbInfo := DbOpenInfo{ "XSharpTestA.DBF", "XSharpTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			// Let's say Three Fields
			myDBF:SetFieldExtent( fields:Length )
			Assert.Equal( fields:Length, myDBF:FieldCount )
			//
			LOCAL fieldInfo AS STRING[]
			FOR VAR i := 1 TO myDBF:FIELDCount
				//
				LOCAL currentField AS RddFieldInfo
				fieldInfo := fields[i]:Split( ',' )
				currentField := RddFieldInfo{ fieldInfo[DBS_NAME], fieldInfo[DBS_TYPE], Convert.ToInt32(fieldInfo[DBS_LEN]), Convert.ToInt32(fieldInfo[DBS_DEC]) }
				myDBF:AddField( currentField )
			NEXT
			// Now Check
			// Right number of Fields ?
			Assert.Equal(fields:Length, myDBF:FieldCount)
			FOR VAR i := 1 TO myDBF:FIELDCount
				// Right decoding ?
				fieldInfo := fields[i]:Split( ',' )
				Assert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_NAME, NIL ) )
				Assert.Equal( fieldInfo[DBS_TYPE], myDBF:FieldInfo( i, DBS_TYPE, NIL ) )
				Assert.Equal( Convert.ToInt32(fieldInfo[DBS_LEN]), myDBF:FieldInfo( i, DBS_LEN, NIL ) )
				Assert.Equal( Convert.ToInt32(fieldInfo[DBS_DEC]), myDBF:FieldInfo( i, DBS_DEC, NIL ) )
				Assert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_ALIAS, NIL ) )
			NEXT
			//
			RETURN

		[Fact, Trait("Dbf", "Fields")];
		METHOD CheckCreateFields() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,10,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
			//VAR dbInfo := DbOpenInfo{ "XSharpTestB.DBF", "XSharpTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			// Right number of Fields ?
			Assert.Equal(fields:Length, myDBF:FieldCount)
			FOR VAR i := 1 TO myDBF:FIELDCount
				// Right decoding ?
				fieldInfo := fields[i]:Split( ',' )
				Assert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_NAME, NIL ) )
				Assert.Equal( fieldInfo[DBS_TYPE], myDBF:FieldInfo( i, DBS_TYPE, NIL ) )
				Assert.Equal( Convert.ToInt32(fieldInfo[DBS_LEN]), myDBF:FieldInfo( i, DBS_LEN, NIL ) )
				Assert.Equal( Convert.ToInt32(fieldInfo[DBS_DEC]), myDBF:FieldInfo( i, DBS_DEC, NIL ) )
				Assert.Equal( fieldInfo[DBS_NAME], myDBF:FieldInfo( i, DBS_ALIAS, NIL ) )
			NEXT
			//
			RETURN

		[Fact, Trait("Dbf", "Create")];
		METHOD CheckCreateDBF() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
 			VAR dbInfo := DbOpenInfo{ TempFileName(), "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			//
			myDBF:Close()
			RETURN

        [Fact, Trait("Dbf", "CreateAppend")];
        METHOD CheckCreateAppendDBF() AS VOID
		    CheckCreateAppendDBF(TempFileName())

		METHOD CheckCreateAppendDBF(cFileName AS STRING) AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
			VAR dbInfo := DbOpenInfo{ cFileName, "", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			// Now, Add some Data
			//"ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
			LOCAL datas := "5,Diablo,T;2,Wolverine,T;4,Cyclops,T;3,Tornade,F;1,Professor Xavier,T" AS STRING
			//LOCAL datas := "1,Professor Xavier,T;2,Wolverine,T;3,Tornade,F;4,Cyclops,T;5,Diablo,T" AS STRING
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
			myDBF:Close()
			// Now, Verify
			myDBF:Open( dbInfo )
			FOR VAR i := __ARRAYBASE__ TO data:Length - (1-__ARRAYBASE__)
				//
				LOCAL tmp AS STRING
				LOCAL elt := data[i]:Split( ',' ) AS STRING[]
				Assert.Equal( Convert.ToInt32(elt[__ARRAYBASE__] ),  Convert.ToInt32( myDBF:GetValue(1) ))
				tmp := (STRING)myDBF:GetValue(2)
				Assert.Equal( elt[__ARRAYBASE__+1], tmp:Trim() )
				Assert.Equal( String.Compare(elt[__ARRAYBASE__+2],"T",TRUE)==0, myDBF:GetValue(3) )
				LOCAL o AS OBJECT
				LOCAL dt := DateTime.MinValue AS DateTime
				o := myDBF:GetValue(4)
				IF ( o IS DbDate )
                    VAR db := (DbDate) o
					dt := DateTime{ db:Year, db:Month, db:Day}
				ENDIF
				Assert.Equal( DateTime.Now.ToString("yyyyMMdd"), dt:ToString("yyyyMMdd") )
				myDBF:Skip(1)
			NEXT
			//
			myDBF:Close()
			RETURN

		[Fact, Trait("Dbf", "CreateBigAppend")];
		METHOD CheckCreateBigAppendDBF() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
			VAR dbInfo := DbOpenInfo{ TempFileName(), "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			// Now, Add some Data
			LOCAL rnd AS Random
			//
			rnd := Random{ (LONG)DateTime.Now.Ticks }
			//"ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
			// 3000 samples
			FOR VAR i := 1 TO 3000
				//
				myDBF:Append( FALSE )
				myDBF:PutValue( 1,  rnd:NEXT( 1, 5000 ))
				myDBF:PutValue( 2,  LoremIpsum(20) )
				myDBF:PutValue( 3,  rnd:NEXT(0,2) == 1 )
				myDBF:PutValue( 4, DateTime.Now )
			NEXT
			//
			myDBF:Append( FALSE )
			myDBF:PutValue( 1,  rnd:NEXT( 1, 5000 ))
			myDBF:PutValue( 2,  LoremIpsum(20) )
			myDBF:PutValue( 3,  rnd:NEXT(0,2) == 1 )
			myDBF:PutValue( 4, DateTime.Now )
			//myDBF:Close()
			myDBF:Close()
			RETURN

		[Fact, Trait("Dbf", "Zap")];
		METHOD CheckZap() AS VOID
			//
            var fileName := TempFileName()
			SELF:CheckCreateAppendDBF(fileName)
			//
			VAR dbInfo := DbOpenInfo{ fileName, "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			IF myDBF:Open( dbInfo )
				//
				//LOCAL nbrBefore := myDBF:RecCount AS LONG
				//
				myDBF:Zap( )
				//
				Assert.Equal(  0, myDBF:RecCount )
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Zap")];
		METHOD CheckZapAppend() AS VOID
			//
            var file := TempFileName()
			SELF:CheckCreateAppendDBF(file)
			//
			VAR dbInfo := DbOpenInfo{ file, "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			IF myDBF:Open( dbInfo )
				//
				//LOCAL nbrBefore := myDBF:RecCount AS LONG
				//
				myDBF:Zap( )
				//
				Assert.Equal(  0, myDBF:RecCount )
				//
				myDBF:Append( FALSE )
				//
				Assert.Equal(  1, myDBF:RecCount )
				// Now, Add some Data
				// "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
				myDBF:PutValue( 1, 5 )
				myDBF:PutValue( 2, "Fabrice" )
				Assert.Equal( 5, Convert.ToInt32(myDBF:GetValue( 1 ) ))
				Assert.Equal( "Fabrice":PadRight(20,' '), myDBF:GetValue( 2 ))
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "Pack")];
		METHOD CheckPack() AS VOID
			//
            var file := TempFileName()
			SELF:CheckCreateAppendDBF(file)
			//
			VAR dbInfo := DbOpenInfo{ file, "", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBF{} AS DBF
			IF myDBF:Open( dbInfo )
				// Delete the First
				myDBF:GoTop()
				myDBF:Delete()
				// and the Last
				myDbf:GoBottom()
				myDbf:Delete()
				//
				LOCAL nbrBefore := myDBF:RecCount AS LONG
				//
				myDBF:Pack( )
				//
				Assert.Equal( nbrBefore-2, myDBF:RecCount )
				//
				myDBF:Close()
			ENDIF
			RETURN

		[Fact, Trait("Dbf", "CreateWithMemo")];
		METHOD CheckCreateDBFMemo() AS VOID
			LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0;BIOGRAPHY,M,10,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            var cFile := TempFileName()
			VAR dbInfo := DbOpenInfo{cFile , "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBFDBT{} AS DBFDBT
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			//
			myDBF:Close()
			//
            cFile := System.IO.Path.ChangeExtension(cFile, ".DBT")
			LOCAL isFile := System.IO.File.Exists( cFile ) AS LOGIC
			Assert.Equal( TRUE, isFile  )
			RETURN

        PRIVATE METHOD CreateFileHelper(cFilename as STRING, data OUT STRING[], Memos OUT List<String>) AS DbOpenInfo
            LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0;BIOGRAPHY,M,10,0" AS STRING
			LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
			VAR dbInfo := DbOpenInfo{ cFilename, "XMenTest", 1, FALSE, FALSE }
			//
			LOCAL myDBF := DBFDBT{} AS DBFDBT
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
			myDBF:SetFieldExtent( fields:Length )
			myDBF:CreateFields( rddInfo )
			// Now Check
			Assert.Equal( TRUE, myDBF:Create( dbInfo ) )
			// Now, Add some Data
			//"ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0"
			LOCAL datas := "1,Professor Xavier,T;2,Wolverine,T;3,Tornade,F;4,Cyclops,T;5,Diablo,T" AS STRING
			data := datas:Split( ';' )
			Memos := List<STRING>{}
			//
			FOR VAR i := __ARRAYBASE__ TO data:Length - (1-__ARRAYBASE__)
				//
				LOCAL elt := data[i]:Split( ',' ) AS STRING[]
				myDBF:Append( FALSE )
				myDBF:PutValue( 1, Convert.ToInt32(elt[__ARRAYBASE__] ))
				myDBF:PutValue( 2, elt[__ARRAYBASE__+1])
				myDBF:PutValue( 3, String.Compare(elt[__ARRAYBASE__+2],"T",TRUE)==0 )
				myDBF:PutValue( 4, DateTime.Now )
				Memos:Add( LoremIpsum( 600 ) )
				myDBF:PutValue( 5, Memos[ Memos:Count -1 ] )
			NEXT
			myDBF:Close()
            return dbInfo

		[Fact, Trait("Dbf", "CreateAppendDBT")];
		METHOD CheckCreateAppendDBFDBT() AS STRING
            var file := TempFileName()
			VAR dbInfo := CreateFileHelper(file, OUT VAR data, OUT VAR Memos)
			LOCAL myDBF := DBFDBT{} AS DBFDBT

			// Now, Verify
			myDBF:Open( dbInfo )
			FOR VAR i := __ARRAYBASE__ TO data:Length - (1-__ARRAYBASE__)
				//
				LOCAL elt := data[i]:Split( ',' ) AS STRING[]
				Assert.Equal( Convert.ToInt32(elt[__ARRAYBASE__] ), Convert.ToInt32(myDBF:GetValue(1) ))
				LOCAL tmp AS STRING
				tmp := (STRING)myDBF:GetValue(2)
				Assert.Equal( elt[__ARRAYBASE__+1], tmp:Trim() )
				Assert.Equal( String.Compare(elt[__ARRAYBASE__+2],"T",TRUE)==0, myDBF:GetValue(3) )
				LOCAL o AS OBJECT
				LOCAL dt := DateTime.MinValue AS DateTime
				o := myDBF:GetValue(4)
				IF ( o IS DBDate )
                    VAR db := (DbDate) o
					dt := DateTime{db:Year, db:Month, db:Day}
				ENDIF
				Assert.Equal( DateTime.Now.ToString("yyyyMMdd"), dt:ToString("yyyyMMdd") )
				// Now the Memo
				LOCAL temp1 AS STRING
				LOCAL temp2 AS STRING
				temp1 :=  Memos[ i - __ARRAYBASE__ ]
				temp2 :=  (STRING)myDBF:GetValue(5)
				VAR res := String.Compare( temp1, temp2 )
				Assert.Equal( TRUE, res == 0 )
				myDBF:Skip(1)
			NEXT
			//
			myDBF:Close()
			RETURN file


		[Fact, Trait("Dbf", "ModifyDBT")];
		METHOD CheckModifyDBT() AS VOID
			// Create and put some Data
			var cFile := SELF:CheckCreateAppendDBFDBT()
			// Now Modify in the same space
			VAR dbInfo := DbOpenInfo{ cFile, "XMenTest", 1, FALSE, FALSE }
			LOCAL myDBF := DBFDBT{} AS DBFDBT
			VAR Memos := List<STRING>{}
			// Now, Modify the Memo
			myDBF:Open( dbInfo )
			WHILE !myDBF:EoF
				//
				Memos:Add( LoremIpsum( 600 ) )
				myDBF:PutValue( 5, Memos[ Memos:Count -1 ] )
				myDBF:Skip(1)
			ENDDO
			//
			myDBF:Close()
			// And verify
			LOCAL i := __ARRAYBASE__ AS LONG
			myDBF:Open( dbInfo )
			WHILE !myDBF:EoF
				//
				LOCAL temp1 AS STRING
				LOCAL temp2 AS STRING
				temp1 :=  Memos[ i - __ARRAYBASE__ ]
				temp2 :=  (STRING)myDBF:GetValue(5)
				VAR res := String.Compare( temp1, temp2 )
				Assert.Equal( TRUE, res == 0 )
				i++
				myDBF:Skip(1)
			ENDDO
			//
			myDBF:Close()
			RETURN

		[Fact, Trait("Dbf", "ModifyDBT_2")];
		METHOD CheckModifyDBT_2() AS VOID
			// Create and put some Data
			VAR dbInfo := CreateFileHelper(TempFileName(), OUT var _, OUT VAR _)
			// Now Modify in the same space
			LOCAL myDBF := DBFDBT{} AS DBFDBT
			VAR Memos := List<STRING>{}
			// Now, Modify the Memo
			myDBF:Open( dbInfo )
			WHILE !myDBF:EoF
				// Now the new block is bigger
				Memos:Add( LoremIpsum( 1900 ) )
				myDBF:PutValue( 5, Memos[ Memos:Count -1 ] )
				myDBF:Skip(1)
			ENDDO
			//
			myDBF:Close()
			// And verify
			LOCAL i := __ARRAYBASE__ AS LONG
			myDBF:Open( dbInfo )
			WHILE !myDBF:EoF
				//
				LOCAL temp1 AS STRING
				LOCAL temp2 AS STRING
				temp1 :=  Memos[ i - __ARRAYBASE__ ]
				temp2 :=  (STRING)myDBF:GetValue(5)
				VAR res := String.Compare( temp1, temp2 )
				Assert.Equal( TRUE, res == 0 )
				i++
				myDBF:Skip(1)
			ENDDO
			//
			myDBF:Close()
			RETURN

        CLASS TestInputBuffer INHERIT InputBuffer
            PROPERTY Stream         AS System.IO.Stream GET SELF:_stream
            PROPERTY RecordLength   AS LONG GET _recordLength
            PROPERTY HeaderLength   AS LONG GET _headerLength
            PROPERTY Shared         AS LOGIC GET _shared SET _shared := VALUE
            PROPERTY LookAhead      AS INT GET _look_ahead SET _look_ahead := VALUE
            PROPERTY LookBehind     AS INT GET _look_behind SET _look_behind := VALUE
            PROPERTY InBuffer       AS BYTE[] GET _inBuffer
            PROPERTY InBufferOfs    AS LONG GET _inBufferOfs SET _inBufferOfs := VALUE
            PROPERTY InBufferLen    AS LONG GET _inBufferLen SET _inBufferLen := VALUE
            PROPERTY InBufferTick   AS INT GET _inBufferTick SET _inBufferTick := VALUE
            CONSTRUCTOR(stream AS System.IO.Stream, headerLength AS INT, recordLength AS INT, shared AS LOGIC) AS VOID
                SUPER(stream, headerLength, recordLength, shared)
        END CLASS

		[Fact, Trait("Dbf", "InBuffer")];
		METHOD CheckDbfInBuffer() AS VOID
            VAR stream := System.IO.MemoryStream{200101}
            FOR VAR i := 1 TO 200101
                stream:WriteByte((Byte)i)
            NEXT
            stream:Position := 0

            VAR buf := TestInputBuffer{stream, 100, 200, FALSE}
            Assert.Equal( stream, buf:Stream )
            Assert.Equal( 100, buf:HeaderLength )
            Assert.Equal( 200, buf:RecordLength )
            Assert.Equal( FALSE, buf:Shared )
            Assert.Equal( 16384/200 - 16384/200/4, buf:LookAhead )
            Assert.Equal( 16384/200/4 - 1, buf:LookBehind )
            Assert.Equal( 16200, buf:InBuffer:Length )
            Assert.Equal( 0, buf:InBufferOfs )
            Assert.Equal( 0, buf:InBufferLen )

            VAR b := BYTE[]{200}
            Assert.Equal( TRUE, buf:Read(100, b, 200) )
            Assert.Equal( 100, buf:InBufferOfs )
            Assert.Equal( 16200, buf:InBufferLen )
            Assert.Equal( 101, b[1] )

            Assert.Equal( TRUE, buf:Read(300, b, 200) )
            Assert.Equal( 100, buf:InBufferOfs )
            Assert.Equal( 16200, buf:InBufferLen )
            Assert.Equal( 301%256, b[1] )
            Assert.Equal( 500%256, b[200] )

            Assert.Equal( TRUE, buf:Read(100+81*200, b, 200) )
            Assert.Equal( 100+(81-buf:LookBehind)*200, buf:InBufferOfs )
            Assert.Equal( 16200, buf:InBufferLen )
            Assert.Equal( (101+81*200)%256, b[1] )
            Assert.Equal( (300+81*200)%256, b[200] )

            Assert.Equal( TRUE, buf:Read(100+500*200, b, 200) )
            Assert.Equal( 100+(500-buf:LookBehind)*200, buf:InBufferOfs )
            Assert.Equal( 16200, buf:InBufferLen )
            Assert.Equal( (101+500*200)%256, b[1] )
            Assert.Equal( (300+500*200)%256, b[200] )

            Assert.Equal( TRUE, buf:Read(100+200*200, b, 200) )
            Assert.Equal( 100+(200-buf:LookAhead)*200, buf:InBufferOfs )
            Assert.Equal( 16200, buf:InBufferLen )
            Assert.Equal( (101+200*200)%256, b[1] )
            Assert.Equal( (300+200*200)%256, b[200] )

            // Test forward access
            FOR VAR i := 1 TO 200000/200
                Assert.Equal( TRUE, buf:Read(100+(i-1)*200, b, 200) )
                Assert.Equal( (101+(i-1)*200)%256, b[1] )
                Assert.Equal( (300+(i-1)*200)%256, b[200] )
            NEXT

            // Test backward access
            FOR VAR i := 200000/200 DOWNTO 1
                Assert.Equal( TRUE, buf:Read(100+(i-1)*200, b, 200) )
                Assert.Equal( (101+(i-1)*200)%256, b[1] )
                Assert.Equal( (300+(i-1)*200)%256, b[200] )
            NEXT

            // Test random access
            VAR rand := System.Random{}
            FOR VAR n := 1 TO 20000
                VAR i := rand:Next(200000/200)+1
                Assert.Equal( TRUE, buf:Read(100+(i-1)*200, b, 200) )
                Assert.Equal( (101+(i-1)*200)%256, b[1] )
                Assert.Equal( (300+(i-1)*200)%256, b[200] )
            NEXT

            buf:Invalidate()
            Assert.Equal( 0, buf:InBufferOfs )
            Assert.Equal( 0, buf:InBufferLen )

            Assert.Equal( FALSE, buf:Read(100+200000, b, 200) )
            Assert.Equal( 100+(1000-buf:LookBehind)*200, buf:InBufferOfs )
            Assert.Equal( 1+buf:LookBehind*200, buf:InBufferLen )

            Assert.Equal( FALSE, buf:Read(100+200200, b, 200) )
            Assert.Equal( 100+(1001-buf:LookBehind)*200, buf:InBufferOfs )
            Assert.Equal( 0, buf:InBufferLen )

            Assert.Equal( FALSE, buf:Read(100+400200, b, 200) )
            Assert.Equal( 100+(2001-buf:LookBehind)*200, buf:InBufferOfs )
            Assert.Equal( 0, buf:InBufferLen )

			RETURN
        STATIC PRIVATE nCounter AS LONG
        STATIC PRIVATE gate := Object{} as Object
        STATIC METHOD TempFilename() as STRING
            BEGIN LOCK gate
            nCounter += 1
            end lock
            return "TestDBF"+nCounter:ToString()
	END CLASS
END NAMESPACE // XSharp.RDD.Tests

