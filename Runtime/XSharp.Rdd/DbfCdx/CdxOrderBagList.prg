//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING XSharp.RDD.Support


BEGIN NAMESPACE XSharp.RDD.CDX
    /// <summary>
    /// List of OrderBags (an Orderbag = CDX file)
    /// </summary>
    INTERNAL CLASS CdxOrderBagList
        PRIVATE _oRdd AS DbfCdx
        PRIVATE _list AS List<CdxOrderBag>
        
        INTERNAL PROPERTY CurrentOrder AS CdxTag auto
        
        CONSTRUCTOR(oRdd AS DbfCdx)
            _oRdd := oRdd
            _list := List<CdxOrderBag>{}
            RETURN
            
        METHOD Add(info AS DbOrderInfo) AS LOGIC
            LOCAL oBag AS CdxOrderBag
            IF File(info:BagName)
                oBag := CdxOrderBag{_oRdd}
                _list:Add(oBag)
                RETURN oBag:Open(info)
            ENDIF
            RETURN FALSE
            
        METHOD Create(info AS DBORDERCREATEINFO) AS LOGIC
            // todo
            RETURN FALSE
            
        METHOD Close(orderInfo AS DbOrderInfo) AS LOGIC
            LOCAL oTag AS CdxTag
            oTag := SELF:FindOrder(orderInfo)
            IF oTag != NULL
                VAR bag := oTag:OrderBag
                bag:Close()
                SELF:_list:Remove(bag)
            ENDIF
            RETURN TRUE
            
        METHOD CloseAll() AS LOGIC
            // todo
            RETURN FALSE
            
        METHOD Remove(cFileName AS STRING) AS LOGIC
            IF File(cFileName)
                cFileName := FPathName()
            ENDIF
            FOREACH oBag AS CdxOrderBag IN _list
                IF String.Compare(oBag:FileName, cFileName, StringComparison.OrdinalIgnoreCase) == 0
                    oBag:Close()
                    _list:Remove(oBag)
                    RETURN TRUE
                ENDIF
            NEXT
            RETURN FALSE
            
        PROPERTY IsHot AS LOGIC
            GET
                FOREACH oBag AS CdxOrderBag IN _list
                    IF oBag:IsHot
                        RETURN TRUE
                    ENDIF
                NEXT
                RETURN FALSE
            END GET
        END PROPERTY
        
        METHOD GoCold() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _list
                IF ! oBag:GoCold()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk
            
        METHOD GoHot() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _list
                IF ! oBag:GoHot()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk
            
        METHOD Flush() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _list
                IF ! oBag:Flush()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk
            
        METHOD Rebuild() AS LOGIC
            // Todo
            RETURN FALSE
            
        METHOD SetFocus(orderinfo AS DbOrderInfo) AS LOGIC
            VAR oOrder := FindOrder(orderinfo)
            SELF:CurrentOrder := oOrder
            RETURN TRUE

        METHOD FindOrderByName(cBagName AS STRING, cName AS STRING) AS CdxTag
            FOREACH oBag AS CdxOrderBag IN _list
                IF String.IsNullOrEmpty(cBagName) .OR. ;
                    String.Compare(oBag:Name, cBagName, StringComparison.OrdinalIgnoreCase) == 0 .OR. ;
                    String.Compare(oBag:FileName, cBagName, StringComparison.OrdinalIgnoreCase) == 0
                    FOREACH oTag AS CdxTag IN oBag:Tags
                        IF String.Compare(oTag:OrderName, cName, StringComparison.OrdinalIgnoreCase) == 0
                            RETURN oTag
                        ENDIF
                    NEXT
                ENDIF
            NEXT
            RETURN NULL

        METHOD FindOrder(orderinfo AS DbOrderInfo) AS CdxTag
            IF orderInfo:Order IS STRING name
                RETURN FindOrderByName(orderinfo:BagName, name)
            ELSEIF orderInfo:Order IS LONG number
                IF number > 0
                    FOREACH oBag AS CdxOrderBag IN _list
                        IF number <= oBag:Tags:Count
                            RETURN oBag:Tags[number-1]
                        ELSE
                            number -= oBag:Tags:Count
                        ENDIF
                    NEXT
                ENDIF
            ENDIF
            RETURN NULL
            
        PROPERTY Count AS INT 
            GET
                LOCAL nResult := 0 AS LONG
                FOREACH oBag AS CdxOrderBag IN _list
                    nResult += oBag:Count
                NEXT
                RETURN nResult
            END GET
        END PROPERTY
        
        METHOD OrderPos(oTagToFind AS CdxTag) AS LONG
            LOCAL nPos := 0 AS LONG
            IF oTagToFind != NULL
                FOREACH oBag AS CdxOrderBag IN _List
                    FOREACH oTag AS CdxTag IN oBag:Tags
                        ++nPos
                        IF oTag == oTagToFind
                            RETURN nPos
                        ENDIF
                    NEXT
                NEXT
            ENDIF
            RETURN 0
            
    END CLASS
END NAMESPACE
