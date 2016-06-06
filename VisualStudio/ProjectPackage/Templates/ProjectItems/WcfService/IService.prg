using System
using System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)using System.Linq
$endif$using System.Runtime.Serialization
using System.ServiceModel
using System.Text

namespace $rootnamespace$
{
	// NOTE: You can use the "Rename" command on the "Refactor" menu to change the interface name "$safeitemrootname$" in both code and config file together.
	[ServiceContract]
	public interface $safeitemrootname$
	{
		[OperationContract]
		void DoWork()
	}
}
