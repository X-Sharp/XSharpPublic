//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
USING XSharp.RDD.CDX
BEGIN NAMESPACE XSharp.RDD
// Inherits all standard DBF and Memo behavior
// Only adds Order Handling
/// <summary>DBFCDX RDD. For DBF/FPT/CDX.</summary>
CLASS DBFCDX INHERIT DBFFPT
 	INTERNAL _orderbagList AS CdxOrderBagList

	CONSTRUCTOR()
		SUPER()
        _orderbagList := CdxOrderBagList{SELF}
		RETURN
		
	PROPERTY SysName AS STRING GET typeof(DBFCDX):ToString()

    METHOD Open(info AS XSharp.RDD.Support.DbOpenInfo) AS LOGIC
        LOCAL lOk AS LOGIC
        lOk := SUPER:Open(info)
        IF lOk
            VAR cCdxFileName := System.IO.Path.ChangeExtension(info:FileName, ".CDX")
            IF RuntimeState.AutoOpen .AND. file(cCdxFileName)
                info:FileName := cCdxFileName
                info:Extension := ".CDX"
                lOk := _orderBagList:Add(info)
            ENDIF
        ENDIF
        RETURN lOk
            

END CLASS    

END NAMESPACE
