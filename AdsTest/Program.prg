USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Start AS VOID STRICT
    LOCAL aStruct AS ARRAY
    TRY
    aStruct := {{"ASCII","C",10,0},{"SET","C",10,0}}
    DBCreate("Test",aStruct)
    DbUseArea(TRUE,,"Test")
    //DbFieldInfo(DBS_ALIAS, FieldPos("SET"), "_SET")
    DBCreateIndex("test","ASCII+SET")
    CATCH e AS Exception
        ? e:Message
    END TRY
    Console.Read()
    RETURN



FUNCTION Start1() AS VOID STRICT
    LOCAL aStruct AS ARRAY
    LOCAL aValues AS ARRAY
    TRY
    RddSetDefault("AXDBFVFP")
    aStruct := {;
        {"ID","N",10,0}, ;
        {"FIRSTNAME","C",20,0},;
        {"LASTNAME","C",30,0},;
        {"NOTES","M",10,0};
        }

    IF ! DbCreate("Test",aStruct)
        ErrorDialog(RuntimeState.LastRDDError)
    ENDIF
    DbUseArea(TRUE,,"Test","TEST")
    aValues := {}
    AADD(aValues,{1,"Robert","van der Hulst","Comment1"})
    AADD(aValues,{2,"Remco","Tijssen","Comment2"})
    AADD(aValues,{3,"Wilco","Burggraaf","Comment3"})
    AADD(aValues,{4,"Kees","Alblas","Comment4"})
    AADD(aValues,{5,"Willem","Kosmeijer","Comment5"})
    FOREACH element AS ARRAY IN aValues
        DbAppend()
        FieldPut(1, element[1])
        FieldPut(2, element[2])
        FieldPut(3, element[3])
        FieldPut(4, element[4])
    NEXT
    Ferase("test1"+IndexExt())
    Ferase("test2"+IndexExt())
    Ferase("test3"+IndexExt())
    DbCreateIndex("Test1","ID")
    DbCreateIndex("Test2","Upper(FIRSTNAME)")
    DbCreateIndex("Test3","Upper(LASTNAME)")
    DbClearIndex()
    OrdListAdd("Test1")
    OrdListAdd("Test2")
    OrdListAdd("Test3")
    FOR VAR nI := 1 TO 3
        OrdSetFocus("test"+Ntrim(nI))
        ? "Order", DbOrderInfo(DBOI_NAME), DbOrderInfo(DBOI_EXPRESSION)
        DbGoTop()
        DO WHILE ! EOF()
            ? FieldGet(1), FieldGet(2), FieldGet(3), FieldGet(4)
            DbSkip(1)
        ENDDO
    NEXT
    DbCloseArea()
    WAIT
    CATCH e AS Exception
        ErrorDialog(e)
    END TRY
	RETURN	
