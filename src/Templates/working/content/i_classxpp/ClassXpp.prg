

USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

// X# allows to group class inside namespaces. Namespaces are optional.
// You can also delete the BEGIN NAMESPACE .. END NAMESPACE block

BEGIN NAMESPACE Company.Namespace1

	/// <summary>
    /// The ClassXpp class.
    /// </summary>
    CLASS ClassXpp
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
    METHOD ClassXpp:Init()
        ::c := 42
        ::d := TRUE
        RETURN

    // implement class constructor
    CLASS METHOD ClassXpp:InitClass()
        RETURN

END NAMESPACE // Company.Namespace1
