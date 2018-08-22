using System
using System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)using System.Linq
$endif$using System.Runtime.Serialization
using System.ServiceModel
using System.Text

BEGIN NAMESPACE  $rootnamespace$

	CLASS $safeitemrootname$ IMPLEMENTS $contractName$
		METHOD DoWork() as VOID STRICT
			RETURN

	END CLASS
END NAMESPACE
