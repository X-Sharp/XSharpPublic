


CLASS CustomControl INHERIT Control
	// Custom Control is based on a Panel

    PROPERTY ControlType AS ControlType GET ControlType.Panel

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle, lDataAware) 

	SUPER(oOwner, xID, oPoint, oDimension, __WCCustomControlClass, kStyle, lDataAware)

	RETURN 

END CLASS



#region defines
DEFINE __WCCustomControlClass := "_VOCustomControl"
#endregion
