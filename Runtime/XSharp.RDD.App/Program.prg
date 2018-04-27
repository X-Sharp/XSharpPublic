USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE XSharp.RDD.App
    
    FUNCTION Start() AS VOID
        VAR myTest := TestDBF{}
        //
        myTest:OpenDBF()
        myTest:OpenDBFShowFields()
        myTest:CheckFieldInfo()
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
            
    END CLASS
    
END NAMESPACE
