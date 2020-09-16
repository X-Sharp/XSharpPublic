USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID STRICT
    TRY
    LOCAL bin1 AS BINARY
    LOCAL bin2 AS BINARY
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


