
PROCEDURE main()
    LOCAL oD := Days():NEW() AS Days
    ? oD:MyDate
    ? oD:MyDate := Today()+3
    ? oD:CDow, oD:Dow
    wait
RETURN


FINAL CLASS Days
PUBLIC:
    VAR ddate AS DATE

    inline METHOD init
        ::ddate := Today()
    RETURN SELF  
    ACCESS METHOD Dow       // declaration WITH Access, implementation without
    METHOD CDow    // declaration without ACCESS/ASSIGN, implementation has these

    inline ACCESS ASSIGN METHOD MyDate(dNewDate) AS DATE   // Inline property with ACCESS/ASSIGN
        IF IsDate(dNewDate) 
            ::ddate := dNewDate
        ENDIF
    RETURN ::ddate
endclass

METHOD Days:Dow AS DWORD
    RETURN DOW(::ddate)
ACCESS ASSIGN METHOD Days:CDow(cArg ) AS STRING
    RETURN CDOW(::ddate)

    
