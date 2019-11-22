USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Runtime.Serialization
USING System.ServiceModel
USING System.Text

BEGIN NAMESPACE $rootnamespace$

	[ServiceContract];
	PUBLIC INTERFACE $safeitemrootname$

		[OperationContract];
		METHOD DoWork() AS VOID STRICT

	END INTERFACE
END NAMESPACE
