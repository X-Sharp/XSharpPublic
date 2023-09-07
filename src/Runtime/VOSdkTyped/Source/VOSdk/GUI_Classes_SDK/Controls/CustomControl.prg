//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

[XSharp.Internal.TypesChanged];
CLASS CustomControl INHERIT Control
	// Custom Control is based on a Panel

    PROPERTY ControlType AS ControlType GET ControlType.Panel

constructor(oOwner, xID, oPoint, oDimension, kStyle, lDataAware)

	SUPER(oOwner, xID, oPoint, oDimension, __WCCustomControlClass, kStyle, lDataAware)

	return

END CLASS



#region defines
DEFINE __WCCustomControlClass := "_VOCustomControl"
#endregion
