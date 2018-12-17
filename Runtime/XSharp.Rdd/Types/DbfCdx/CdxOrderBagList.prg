//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic


BEGIN NAMESPACE XSharp.RDD.CDX
    /// <summary>
	/// List of OrderBags (an Orderbag = CDX file)
	/// </summary>
    INTERNAL CLASS CdxOrderBagList
        PRIVATE _oRdd as DbfCdx
        PRIVATE _list as List<CdxOrderBag>
        
        CONSTRUCTOR(oRdd as DbfCdx)
            _oRdd := oRdd
            _list := List<CdxOrderBag>{}
            RETURN

        METHOD Add(info AS XSharp.RDD.Support.DbOpenInfo) AS LOGIC
            local oBag as CdxOrderBag
            IF File(info:FileName)
                oBag := CdxOrderBag{_oRdd}
                _list:Add(oBag)
                return oBag:Open(info)
            ENDIF
            RETURN FALSE

        METHOD Remove(cFileName AS STRING) AS LOGIC
            IF File(cFileName)
                cFileName := FPathName()
            ENDIF
            FOREACH oBag as CdxOrderBag in _list
                if String.Compare(oBag:FileName, cFileName, StringComparison.OrdinalIgnoreCase) == 0
                    oBag:Close()
                    _list:Remove(oBag)
                    RETURN TRUE
                endif
            NEXT
            RETURN FALSE

    END CLASS
END NAMESPACE
