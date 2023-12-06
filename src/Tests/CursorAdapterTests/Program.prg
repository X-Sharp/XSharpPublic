USING System.Windows.Forms
USING System.Drawing
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.VFP


FUNCTION Start() AS VOID STRICT
    VAR cConnectionString := "DSN=Northwind;UID=robert;Trusted_Connection=Yes;DATABASE=Northwind;"
    local nHandle as Long
    local oCa as CursorAdapter
    local oCa2 as CursorAdapter
    SqlSetFactory("ODBC")
    nHandle := SqlStringConnect(cConnectionString,FALSE)
    oCa := CursorAdapter{}
    oCa:DataSourceType  := "ODBC"
    oCa:DataSource      := nHandle
    oCa:SelectCmd       := "Select * From Customers"
    oCa:KeyFieldList    := "CustomerId"
    oCa:Alias           := "Customers"
    ? oCa:CursorFill()
    ? oCa:CursorAttach()
    Browse()
    ? oCa:DataSourceType, oCa:DataSource, oCa:SelectCmd
    oCa2 := CursorAdapter{}
    ? oCa2:CursorAttach("Customers")
    DbUseArea()
    DbCloseAll()
    wait


