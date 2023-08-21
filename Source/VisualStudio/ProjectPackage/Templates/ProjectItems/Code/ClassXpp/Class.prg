// $safeitemrootname$.prg
// Created by    : $username$
// Creation Date : $time$
// Created for   : $registeredorganization$
// WorkStation   : $machinename$


USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text

// X# allows to group class inside namespaces. Namespaces are optional.
// You can also delete the BEGIN NAMESPACE .. END NAMESPACE block

BEGIN NAMESPACE $rootnamespace$

	/// <summary>
    /// The $safeitemrootname$ class.
    /// </summary>
    CLASS $safeitemrootname$
    EXPORTED:
        // Instance variables
        VAR a,b
        // Class variables
        CLASS VAR c,d
        // Declare methods
        METHOD Init
        CLASS METHOD InitClass
            ::a := "foo"
            ::b := Date()
        ENDCLASS
    // Implement constructor
    METHOD $safeitemrootname$:Init()
        ::c := 42
        ::d := TRUE
        RETURN

    // implement class constructor
    CLASS METHOD $safeitemrootname$:InitClass()
        RETURN

END NAMESPACE // $rootnamespace$
