USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.Data


FUNCTION Start() AS VOID STRICT
    LOCAL oConn AS SqlConnection
    LOCAL oSel AS SqlSelect
    //SetSqlFactory(SqlServerFactory{})
    //oConn := SqlOpenConnection()
    //SetSqlFactory(SqlServerFactory{})
    oConn := SqlConnection{"CURSADM","cursbeheer","cursbeheer"}
    //oConn := SqlConnection{"Server=(local);Database=Cursadm;User Id=cursbeheer;Password=cursbeheer"}
    oConn:DriverConnect()
    ? oConn:ConnectString
    IF ! oConn:Connected
        ? oConn:Connect()
    ENDIF
    ? oConn:Connected
    IF oConn:COnnected
        oSel := SqlSelect{"Select * from Bedrijven",oConn}
        oSel:Execute()
        ? "Count", oSel:NumSuccessfulRows
        VAR cName := oSel:FieldGet(2 )
        oSel:FIELDPUT(2, "ABC")
        oSel:Skip(0)
        oSel:FIELDPUT(2, cName)
        oSel:Skip(0)
        oSel:Append()
        oSel:FIELDPUT(2, "ZZZ")
        oSel:AppendRow()
        oSel:Close()        
        ? oConn:Disconnect()
    ENDIF
    WAIT
    RETURN	
    
    
FUNCTION DumpTable(oSel AS SqlSelect) AS VOID
    ? 
    FOR VAR i := 1 TO oSel:FCount
        ?? oSel:FieldName(i),""
    NEXT
    ? 
    DO WHILE ! oSel:EOF
        FOR VAR i := 1 TO oSel:FCount
            ?? Trim(AsString(oSel:FIELDGET(i))),""
        NEXT
        ?
        oSel:Skip(1)
    ENDDO
    ?
