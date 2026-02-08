USING System
USING System.Collections.Generic
USING System.Linq
USING System.Runtime.Serialization
USING System.ServiceModel
USING System.Text

BEGIN NAMESPACE Company.Namespace1

	[ServiceContract];
	PUBLIC INTERFACE IWCFService1

		[OperationContract];
		METHOD DoWork() AS VOID STRICT

	END INTERFACE
END NAMESPACE
