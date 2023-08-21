USING XSHarp.ADS
using static XSHarp.ADS
using static System.Console
using VO

FUNCTION MyThrow(e as Exception)
    THROW e

CLASS DbLogger IMPLEMENTS IDbNotify
    METHOD Notify(sender AS XSharp.RDD.IRdd, e AS DbNotifyEventArgs) AS VOID
        IF (sender != NULL)
            ?  sender:Alias, sender:Area, '#',sender:Recno, e:Type:ToString(), e:Data
        ELSE
            ? "no area", e:Type:ToString(), e:Data
        ENDIF

END CLASS
PROCEDURE Main AS VOID
    TRY
        LOCAL aStruct := {{"NAME","C",10,0},{"CITY","C",10,0}}
        local b as CODEBLOCK
            b := {||_FIELD->NAME}
    RddSetDefault("AXDBFCDX")
    DbRegisterClient(DbLogger{})
    ? DbCreate("test1",aStruct)
    ? DbUseArea(TRUE, ,"Test1","test",TRUE)
    ? EoF()
    ? Header()
    ? DbInfo(DBI_LASTUPDATE)

    ? OrdCreate("test1","NAME",,b)
    ? OrdCreate("test2","CITY","CITY")
    ? OrdCreate("test3","CITYNAME","CITY+NAME")
    ? DbClearIndex()
    SetDeleted(TRUE)
    ? DbSetIndex("test1")
    ? DbSetIndex("test2")
    ? DbSetIndex("test3")
    VAR aList := OrdList()
    LOCAL index
    FOR index := 1 TO aLen(aList)
        ? index, "CUrrent", OrdSetFocus()
        ? index, aList[index], OrdSetFocus(index), OrdKey()
        ? index, "After change", OrdSetFocus()
    NEXT
    DbAppend(); FieldPut(1, "AAA"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "BBB"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "CCC"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "DDD"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "EEE"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "FFF"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "GGG"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "HHH"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "III"); FieldPut(2, "ABC")
    DbAppend(); FieldPut(1, "JJJ"); FieldPut(2, "ABC")
    DbSetFilter({||!empty(_FIELD->Name)})
    DbGoto(6)
    OrdSetFocus(0)

    DbRLockList()
    ? Recno()
    ? DbCloseArea()
    CATCH e AS Exception
        ? e:ToString()
    END TRY
    WAIT
    RETURN
