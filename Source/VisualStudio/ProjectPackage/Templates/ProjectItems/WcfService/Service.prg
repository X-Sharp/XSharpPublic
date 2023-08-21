USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Runtime.Serialization
USING System.ServiceModel
USING System.Text

BEGIN NAMESPACE  $rootnamespace$

	CLASS $safeitemrootname$ IMPLEMENTS I$safeitemrootname$
		METHOD DoWork() AS VOID STRICT
			RETURN

	END CLASS
END NAMESPACE
