// $safeitemrootname$.prg
// Created by    : $username$
// Creation Date : $time$
// Created for   : $registeredorganization$
// WorkStation   : $machinename$


USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text

BEGIN NAMESPACE $rootnamespace$

	/// <summary>
    /// The $safeitemrootname$ class.
    /// </summary>
    DEFINE CLASS $safeitemrootname$
    PROTECTED prop1         AS STRING // X# allows types
    HIDDEN hiddenprop1      AS DATE
    // CONSTRUCTOR
    PROCEDURE Init(p1 as STRING, p2 AS DATE)   // X# allows to type the parameters
        prop1 := p1
        hiddenprop1 := p2


    FUNCTION Compare (p1 as STRING, p2 as DATE) AS LOGIC
        ? p1, p2
        RETURN p1 == prop1 AND p2 == hiddenprop1

    CONSTRUCTOR()
         RETURN

	ENDDEFINE
END NAMESPACE // $rootnamespace$
