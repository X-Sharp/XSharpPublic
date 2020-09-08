USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID STRICT
    TRY
    LOCAL bin1 AS BINARY
    LOCAL bin2 AS BINARY
    LOCAL strtest  IS TEST
    strtest.l1 := TRUE
    bin1 := "Robert"
    bin2 := "Rober"
    ? bin1 == bin2
    ? bin1 > bin2
    ? bin1 >= bin2
    ? bin1 < bin2
    ? bin1 <= bin2
    bin2 := "Robert"
    ? bin1 > bin2
    ? bin1 >= bin2
    ? bin1 < bin2
    ? bin1 <= bin2
    CATCH e as Exception
        ? e:ToString()
    END TRY
    WAIT
	RETURN	


VOSTRUCT TEST
    MEMBER l1 as LOGIC
