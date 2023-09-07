#pragma options("lb",on)
PROCEDURE main()
    LOCAL oD := Days():NEW() as days
    ? oD:Date
    ? oD:Date := Today()+3
    ? oD:CDow, oD:Dow
    wait
RETURN


FINAL CLASS Days
EXPORTED:
    VAR Date AS DATE

    inline METHOD init
        ::date := Today()
    RETURN SELF
    ACCESS METHOD Dow     // declaration WITH Access, implementation without
    ACCESS METHOD CDow    // declaration WITH Access, implementation also has Access

    INLINE ACCESS ASSIGN METHOD Date(dNewDate) AS DATE   // Inline property with ACCESS/ASSIGN
        IF IsDate(dNewDate)
            ::date := dNewDate
        ENDIF
    RETURN ::date
endclass

ACCESS METHOD Days:Dow AS DWORD
    RETURN DOW(::date)

ACCESS ASSIGN METHOD Days:CDow(cArg ) AS STRING
    RETURN CDOW(::date)


