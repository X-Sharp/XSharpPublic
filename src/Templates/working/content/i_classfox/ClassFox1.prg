

USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE Company.Namespace1

	/// <summary>
    /// The ClassFox class.
    /// </summary>
    DEFINE CLASS ClassFox
    PROTECTED prop1         AS STRING // X# allows types
    HIDDEN hiddenprop1      AS DATE
    // This gets called from the (generated) constructor
    PROCEDURE Init(p1 as STRING, p2 AS DATE)   // X# allows to type the parameters
        prop1 := p1
        hiddenprop1 := p2


    FUNCTION Compare (p1 as STRING, p2 as DATE) AS LOGIC
        ? p1, p2
        RETURN p1 == prop1 AND p2 == hiddenprop1


	ENDDEFINE
END NAMESPACE // Company.Namespace1
