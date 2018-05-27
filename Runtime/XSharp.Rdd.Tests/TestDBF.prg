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
            Assert.Equal( FALSE, myDBF:Open( dbInfo ) )
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
            
        [Fact, Trait("Dbf", "Fields")];
        METHOD CheckFieldValue() AS VOID
            VAR values := <OBJECT>{ 2.0, "Robert", "Evans","732 Johnson Street","New York","NY","11501", "(212)764-1246", "(212)764-1877" }
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
                Assert.Equal(  myDBF:RecCount, nbrBefore+1 )
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
                Assert.Equal( myDBF:GetValue( 1 ), 5 )
                myDBF:PutValue( 2, "Fabrice" )
                Assert.Equal( myDBF:GetValue( 1 ),"Fabrice" )
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
            
            
    END CLASS
END NAMESPACE // XSharp.RDD.Tests