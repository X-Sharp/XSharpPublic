USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using static XSharp.Core.Functions
BEGIN NAMESPACE XSharp.RDD.App

    FUNCTION Start() AS VOID
        LOCAL myTest := TestDBF{} AS TestDBF
        //
        myTest:OpenDBF()
        myTest:OpenDBFShowFields()
        myTest:CheckFieldInfo()
        myTest:WriteDataValue()
        myTest:CheckSkip()
        myTest:CheckAppend()
        myTest:CheckAppendData()
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
            // Right number of Fields ?
            Assert.Equal(9, myDBF:FieldCount)
            //
            Assert.Equal( TRUE, myDBF:Close() )
            
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
                Console.WriteLine(fields:Length + " / " + myDBF:FieldCount)
                FOR VAR i := 1 TO myDBF:FIELDCount
                    // Right decoding ?
                    VAR fieldInfo := fields[i]:Split( ',' )
                    Console.WriteLine( fieldInfo[DBS_NAME]+ " / " + myDBF:FieldInfo( i, DBS_NAME, NIL ) )
                    Console.WriteLine( fieldInfo[DBS_TYPE]+ " / " + myDBF:FieldInfo( i, DBS_TYPE, NIL ) )
                    Console.WriteLine( fieldInfo[DBS_LEN]+ " / " + myDBF:FieldInfo( i, DBS_LEN, NIL ) )
                    Console.WriteLine( fieldInfo[DBS_DEC]+ " / " + myDBF:FieldInfo( i, DBS_DEC, NIL ) )
                    Console.WriteLine( fieldInfo[DBS_NAME]+ " / " + myDBF:FieldInfo( i, DBS_ALIAS, NIL ) )
                NEXT
                //
                myDBF:Close()
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
            
        METHOD CheckSkip() AS VOID
            VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
            //
            VAR myDBF := DBF{}
            IF myDBF:Open( dbInfo ) 
                //
                Assert.Equal( 1, myDBF:RecNo )
                Assert.Equal( FALSE, myDBF:Bof )
                Assert.Equal( FALSE, myDBF:Eof )
                myDBF:Skip(-1)
                Assert.Equal( 1, myDBF:RecNo )
                Assert.Equal( TRUE, myDBF:Bof )
                myDBF:Close()
            ENDIF

        METHOD CheckAppend() AS VOID
            VAR dbInfo := DbOpenInfo{ "customer.DBF", "customer", 1, FALSE, FALSE }
            //
            VAR myDBF := DBF{}
            IF myDBF:Open( dbInfo ) 
                //
                LOCAL nbrBefore := myDBF:RecCount AS LONG
                //
                myDBF:Append( false )
                //
                Assert.Equal(  myDBF:RecCount, nbrBefore+1 )
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
                Assert.Equal( myDBF:GetValue( 1 ), 5 )
                myDBF:PutValue( 2, "Fabrice" )
                Assert.Equal( myDBF:GetValue( 1 ),"Fabrice" )
                //
                myDBF:Close()
            ENDIF
            RETURN
            
    END CLASS
    
END NAMESPACE
