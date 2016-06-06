using System
using System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)using System.Linq
$endif$using System.Runtime.Serialization
using System.ServiceModel
using System.Text

BEGIN NAMESPACE  $rootnamespace$

	// NOTE: You can use the "Rename" command on the "Refactor" menu to change the class name "$safeitemrootname$" in both code and config file together.
	CLASS $safeitemrootname$ IMPLEMENTS $contractName$
		METHOD DoWork() as VOID
			RETURN
		
	END CLASS
END NAMESPACE
