// Class1.prg
// Created by    : robert
// Creation Date : 9/23/2024 9:33:04 AM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE ClassLibrary1

	/// <summary>
    /// The Class1 class.
    /// </summary>
    [ComVisible(TRUE)];
    DEFINE CLASS Class1  AS Custom
    PROTECTED prop1         AS STRING // X# allows types
    HIDDEN hiddenprop1      AS DATE
    // CONSTRUCTOR
    PROCEDURE Init(p1, p2)
        prop1 := p1
        hiddenprop1 := p2
		RETURN


    FUNCTION Compare (p1 as STRING, p2 as DATE) AS LOGIC
        ? p1, p2
        RETURN p1 == prop1 AND p2 == hiddenprop1

	END DEFINE
END NAMESPACE // ClassLibrary1

