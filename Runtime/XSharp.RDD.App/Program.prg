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
            
    END CLASS
    
END NAMESPACE
