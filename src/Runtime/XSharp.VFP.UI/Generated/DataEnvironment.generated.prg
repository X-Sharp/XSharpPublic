
// Class DataEnvironment  BaseClass   Dataenvironment  Class  Dataenvironment
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS DataEnvironment IMPLEMENTS IVFPObject, IVFPOwner
		#include "VFPObject.xh"
		#include "VFPContainer.xh"
		METHOD CloseTables AS LOGIC CLIPPER
			RETURN FALSE
		PROPERTY DataSourceType AS STRING AUTO
		METHOD OpenTables  AS LOGIC CLIPPER
			RETURN FALSE
		PROPERTY OpenViews  AS LONG AUTO
	END CLASS
	END NAMESPACE
