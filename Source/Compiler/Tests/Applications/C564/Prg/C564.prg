
USING System
USING System.Collections.Generic
USING System.Text



BEGIN NAMESPACE ConsoleApplication5

	FUNCTION Start() AS VOID

    LOCAL oMC AS MyObject
   
    oMC := MyObject{}

    ParamAsClass(oMC) //Works
    ParamAsObject(oMC) //Does not work - Codeblock is null

    FUNCTION ParamAsObject(x AS OBJECT) AS VOID

        x:DoStuff({||TRUE})

    RETURN 

    FUNCTION ParamAsClass(oMyObject AS MyObject) AS VOID

        oMyObject:DoStuff({||TRUE})

    RETURN 

    CLASS MyObject

        METHOD DoStuff(;
        cb1 AS CODEBLOCK;
        ) AS VOID

        ? Eval(cb1)

        RETURN 

    END CLASS

END NAMESPACE

