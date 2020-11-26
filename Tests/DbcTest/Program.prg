USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

#command OPEN DATABASE <(db)>                                            ;
           [<ex: EXCLUSIVE >]                                          ;
             [<sh: SHARED> ]                                             ;
             [<ro: NOUPDATE> ]                                           ;
             [<val: VALIDATE>]                                           ;
         ;
        => XSharp.RDD.Dbc.Open( <(db)>, iif(<.sh.> .or. <.ex.>, !<.ex.>, .T.), <.ro.>, <.val.>)

#command SET DATABASE TO [<(db)>]                                        ;
        => XSharp.RDD.Dbc.Select( <(db)>)

#command CREATE DATABASE <(db)>                                          ;
        => XSharp.RDD.Dbc.Create( <(db)>)


#command ASSERT <expression> [MESSAGE <cMessageText>]  ;
        => __FoxAssert( <expression> , <"expression">, <cMessageText> )

#command SET ASSERTS <x:ON,OFF>          =>  Set(Set.Asserts, <(x)> )
#command SET ASSERTS (<x>)               =>  Set(Set.Asserts, <x> )



#command  SET ALTERNATE <x:ON,OFF,&> =>  Set( Set.Alternate, <(x)> )
#command  SET ALTERNATE (<x>)        =>  Set( Set.Alternate, <x> )
#command  SET ALTERNATE TO           =>  SetAltfile( "" , FALSE)

#command  SET ALTERNATE TO <(file)> [<add: ADDITIVE>] ;
      =>  SetAltfile( <(file)>, <.add.> )

#command  CLOSE ALTERNATE            =>  SetAltfile( "" , FALSE)


#command  SET COLOR TO [<*spec*>]                     =>  SetColor( #<spec> )
#command  SET COLOR TO ( <c> )                        =>  SetColor( <c> )
#command  SET COLOUR TO [<*spec*>]                    =>  SET COLOR TO [<spec>]
#command CLEAR SCREEN                                 =>  Cls(); SetPos(0,0)

BEGIN NAMESPACE ConsoleApplication1

/*
    FUNCTION Start2() AS VOID STRICT
        TRY
        local aTest as __FoxArray
        aTest := FoxArrayCreate(5,3)
        LOCAL nI AS INT
        SET COLOR TO W+/b
        Cls()
        FOR nI = 1 to 15
            aTest[nI] = nI
        NEXT
        SET ALTERNATE TO Test.Txt 
        ? aLen(aTest)
        SET ALTERNATE OFF
        ? "Start"
        SET ALTERNATE ON
        ShowArray(aTest)
        CLOSE ALTERNATE
        ? "Row   (1)", aSubScript(aTest, 3, 1)     // should be 1
        ? "Column(3)", aSubScript(aTest, 3, 2)     // should be 3
        ? "Row   (2)", aSubScript(aTest, 4, 1)     // should be 2
        ? "Column(1)", aSubScript(aTest, 4, 2)     // should be 1
        ? aTest[7]
        ? aTest[5,3]
        ? aTest[3,3]
        aTest:Redim(16)
        ? "Resize to 16 elements"
        ShowArray(aTest)
        aTest:ReDim(3,5)
        ? "Resize to 3 * 5 elements"
        ShowArray(aTest)
        ? "ASize 18 -> 20 "
        ASize(aTest, 18)
        ShowArray(aTest)
        ? aTest[3,3]
        ADel(aTest,2,2)
        ? "Deleted column 2"
        ShowArray(aTest)
        ADel(aTest,2,1)
        ? "Deleted row 2"
        ShowArray(aTest)
        AIns(aTest,2,1)
        ? "Inserted row 2"
        ShowArray(aTest)
        AIns(aTest,2,2)
        ? "Inserted column 2"
        ShowArray(aTest)
        CATCH e as Exception
            ? e:ToString()
        END TRY
        _Wait()
        RETURN
  */      

	FUNCTION Start() AS VOID STRICT 
        TRY
        SET ASSERT OFF
        SET ASSERT ON
        SET ASSERT (.T.)
        SET COLOR TO gr+/br
        CLEAR SCREEN

        //ASSERT 1==0 
        //ASSERT 1==2 MESSAGE "stil not equal"
        CoreDb.RddSetDefault("DBFVFP")
        SetDefault("c:\VFF\Samples\data")
        ? "Opening database SHARED R/W NOVAL"
        CREATE DATABASE C:\Test\Test
        OPEN DATABASE Northwind SHARED  
        SET DATABASE TO 
        ? "database opened"
        SET DATABASE TO Northwind
        //var oTable := oDb:FindTable("Customers")
        //? oTable:ObjectName
        //Dbc.Select("Northwind")
        ? DbGetProp("Customers","Table", "Path")
        ? DbGetProp("Customers.CustomerId","Field", "Comment")
        ? DbGetProp("Customers.CustomerId","Field", "Caption")
        ? DbGetProp("Order_Details_Extended.OrderId","Field", "Caption")
        ? DbGetProp("Orders_qry","View", "SQL")
        ? DbGetProp("Orders_qry","View", "BatchUpdateCount")
        ? DbGetProp("Orders_qry","View", "Tables")
        ? DbGetProp("cursadm","connection","password")
        ? DbSetProp("Customers","Table", "Path", "")
        

        Dbc.Close("Northwind")
        ? "database closed"
        Dbc.Create("C:\test\test")
        CATCH e as Exception
            ? e:ToString()
        END TRY
        Console.ReadLine()
END NAMESPACE









