using XSharp.RDD
using XSharp.RDD.Support


FUNCTION RecordToCustomer( oDBF AS DBF ) AS Customer
    LOCAL oCustomer AS Customer
    // Convert the DBF record to a Customer object
    oCustomer := Customer{}
    FOR VAR i := 1 TO oDBF:FieldCount
        VAR name := oDBF:FieldName(i):ToLower()
        SWITCH name
            CASE "custnum"
                oCustomer:CustNum := Convert.ToInt32( oDBF:GetValue(i) )
            CASE "firstname"
                oCustomer:FirstName := oDBF:GetValue(i):ToString()
            CASE "lastname"
                oCustomer:LastName := oDBF:GetValue(i):ToString()
            CASE "address"
                oCustomer:Address := oDBF:GetValue(i):ToString()
            CASE "city"
                oCustomer:City := oDBF:GetValue(i):ToString()
            CASE "state"
                oCustomer:State := oDBF:GetValue(i):ToString()
            CASE "zip"
                oCustomer:Zip := oDBF:GetValue(i):ToString()
            CASE "fax"
                oCustomer:Fax := oDBF:GetValue(i):ToString()
            CASE "phone"
                oCustomer:Phone := oDBF:GetValue(i):ToString()
        END SWITCH
    NEXT
    oCustomer:Recno := oDbf:Recno
    oCustomer:Deleted := oDbf:Deleted
    //
    RETURN oCustomer


PROCEDURE CustomerToRecord( oCustomer AS Customer, oDBF AS DBF )
    FOR VAR i := 1 TO oDBF:FieldCount
        VAR name := oDBF:FieldName(i):ToLower()
        SWITCH name
            CASE "custnum"
                oDBF:PutValue( i, oCustomer:CustNum )
            CASE "firstname"
                oDBF:PutValue( i, oCustomer:FirstName )
            CASE "lastname"
                oDBF:PutValue( i, oCustomer:LastName )
            CASE "address"
                oDBF:PutValue( i, oCustomer:Address )
            CASE "city"
                oDBF:PutValue( i, oCustomer:City )
            CASE "state"
                oDBF:PutValue( i, oCustomer:State )
            CASE "zip"
                oDBF:PutValue( i, oCustomer:Zip )
            CASE "fax"
                oDBF:PutValue( i, oCustomer:Fax )
            CASE "phone"
                oDBF:PutValue( i, oCustomer:Phone )
        END SWITCH
    NEXT
    //
    RETURN 