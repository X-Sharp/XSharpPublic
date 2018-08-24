USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING STATIC XSharp.Core.Functions
BEGIN NAMESPACE XSharp.RDD.App

    FUNCTION Start() AS VOID
        LOCAL myTest := TestDBF{} AS TestDBF
        //
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
        //
        Console.WriteLine("Hello World!")
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
        
        
    STATIC CLASS Assert
        STATIC METHOD Equal( a AS OBJECT, b AS OBJECT ) AS VOID
            RETURN
            END CLASS
            
    CLASS TestDBF
    
        METHOD OpenDBF() AS VOID
            // CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
            VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
            //
            VAR myDBF := DBF{}
            Assert.Equal( TRUE, myDBF:Open( dbInfo ) )
            //
            myDBF:Close()
            RETURN
            
        METHOD OpenDBFErr() AS VOID
            VAR dbInfo := DbOpenInfo{ "noFile.DBF", "noFile", 1, FALSE, FALSE }
            //
            VAR myDBF := DBF{}
            
            Assert.Equal( FALSE, myDBF:Open( dbInfo ) )
            //
            myDBF:Close()
            RETURN
			
        METHOD OpenDBFShowFields() AS VOID
            VAR fields := <STRING>{ "CUSTNUM", "FIRSTNAME", "LASTNAME","ADDRESS","CITY","STATE","ZIP", "PHONE", "FAX" }
            // CUSTNUM,N,5,0	FIRSTNAME,C,10	LASTNAME,C,10	ADDRESS,C,25	CITY,C,15	STATE,C,2	ZIP,C,5	PHONE,C,13	FAX,C,13
            VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
            //
            VAR myDBF := DBF{}
            Assert.Equal( TRUE, myDBF:Open( dbInfo ) )
            //
            Console.WriteLine( "ArrayBase is " + Convert.ToString( __ARRAYBASE__ ) )
            //Assert.Equal( myDBF:FIELDCount, myDBF:_Fields:Length )
            FOR VAR i := 1 TO myDBF:FIELDCount
                VAR f1 := fields[i]
                VAR f2 := myDBF:FieldName( i )
                Console.WriteLine( i + " : " + f1 + " == " + f2 )
            NEXT
            //
            Console.WriteLine( "RecCount : " + myDBF:RecCount )
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
                Assert.Equal( TRUE, myDBF:Close() )
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
                    Console.WriteLine( myDBF:FieldInfo( i, DBS_NAME, NIL ) + " == " + oData:ToString() )
                NEXT
                //
                myDBF:Close()
            ENDIF
            RETURN			
            
        METHOD CheckFields() AS VOID
            VAR fields := <STRING>{ "CUSTNUM", "FIRSTNAME", "LASTNAME","ADDRESS","CITY","STATE","ZIP", "PHONE", "FAX" }
            VAR types :=  <STRING>{ "N", "C", "C","C","C","C","C", "C", "C" }
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
            
        METHOD CheckFieldInfo() AS VOID
            VAR fieldDefs := "CUSTNUM,N,5,0;FIRSTNAME,C,10,0;LASTNAME,C,10,0;ADDRESS,C,25,0;CITY,C,15,0;STATE,C,2,0;ZIP,C,5,0;PHONE,C,13,0;FAX,C,13,0"
            VAR fields := fieldDefs:Split( ';' )
            //
            VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
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
                    Assert.Equal( values[i], oData )
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
                Assert.Equal( nbrBefore+1, myDBF:RecCount )
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
                Assert.Equal(  myDBF:RecCount, nbrBefore+1 )
                // Now, Add some Data
                //"CUSTNUM,N,5,0;FIRSTNAME,C,10,0;LASTNAME,C,10,0;ADDRESS,C,25,0;CITY,C,15,0;STATE,C,2,0;ZIP,C,5,0;PHONE,C,13,0;FAX,C,13,0"
                myDBF:PutValue( 1, 5 )
                myDBF:PutValue( 2, "Fabrice" )
                Assert.Equal( 5, myDBF:GetValue( 1 ) )
                Assert.Equal( "Fabrice", myDBF:GetValue( 2 ) )
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
            
        METHOD CheckDeleteRecall() AS VOID
            VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
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
            
        METHOD CheckAddFields() AS VOID
            LOCAL fieldDefs := "ID,N,5,0;NAME,C,10,0;BIRTHDAY,D,8,0" AS STRING
            LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            VAR dbInfo := DbOpenInfo{ "XSharpTest.DBF", "XSharpTest", 1, FALSE, FALSE }
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
            
        METHOD CheckCreateFields() AS VOID
            LOCAL fieldDefs := "ID,N,5,0;NAME,C,10,0;BIRTHDAY,D,8,0" AS STRING
            LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            VAR dbInfo := DbOpenInfo{ "XSharpTest.DBF", "XSharpTest", 1, FALSE, FALSE }
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
            
        METHOD CheckCreateDBF() AS VOID
            LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
            LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            VAR dbInfo := DbOpenInfo{ "XMenTest.DBF", "XMenTest", 1, FALSE, FALSE }
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
            
        METHOD CheckCreateAppendDBF() AS VOID
            LOCAL fieldDefs := "ID,N,5,0;NAME,C,20,0;MAN,L,1,0;BIRTHDAY,D,8,0" AS STRING
            LOCAL fields := fieldDefs:Split( ';' ) AS STRING[]
            VAR dbInfo := DbOpenInfo{ "XMenTest.DBF", "XMenTest", 1, FALSE, FALSE }
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
            LOCAL datas := "1,Professor Xavier,T;2,Wolverine,T;3,Tornade,F;4,Cyclops,T;5,Diablo,T" AS STRING
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
                LOCAL elt := data[i]:Split( ',' ) AS STRING[]
                Assert.Equal( Convert.ToInt32(elt[__ARRAYBASE__] ), myDBF:GetValue(1) )
                Assert.Equal( elt[__ARRAYBASE__+1], myDBF:GetValue(2) )
                Assert.Equal( String.Compare(elt[__ARRAYBASE__+2],"T",TRUE)==0, myDBF:GetValue(3) )
                LOCAL o AS OBJECT
                LOCAL dt AS DateTime
                o := myDBF:GetValue(4)
                IF ( o IS DateTime )
                    dt := (DateTime)o
                ENDIF
                Assert.Equal( TRUE, o IS DateTime )
                Assert.Equal( DateTime.Now.ToString("yyyyMMdd"), dt:ToString("yyyyMMdd") )
                myDBF:Skip(1)
            NEXT
            //
            myDBF:Close()
            RETURN
            
            
                        
    END CLASS
    
END NAMESPACE
