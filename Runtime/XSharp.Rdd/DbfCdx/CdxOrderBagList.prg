//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING XSharp.RDD.Support
USING System.io

BEGIN NAMESPACE XSharp.RDD.CDX
    /// <summary>
    /// List of OrderBags (an Orderbag = CDX file)
    /// </summary>
    INTERNAL CLASS CdxOrderBagList
        PRIVATE _oRdd AS DbfCdx
        PRIVATE _bags AS List<CdxOrderBag>
        
        INTERNAL PROPERTY CurrentOrder AS CdxTag AUTO
        
        CONSTRUCTOR(oRdd AS DbfCdx)
            _oRdd := oRdd
            _bags := List<CdxOrderBag>{}
            RETURN
            
        METHOD Add(info AS DbOrderInfo, lStructural := FALSE AS LOGIC) AS LOGIC
            LOCAL oBag AS CdxOrderBag
             IF File(info:BagName)
                info:BagName := FPathName()
                IF SELF:FindOrderBag(info:BagName) == NULL_OBJECT
                    oBag := CdxOrderBag{_oRdd}
                    oBag:Structural := lStructural
                    _bags:Add(oBag)
                    RETURN oBag:Open(info)
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE
            
        METHOD Create(info AS DbOrderCreateInfo) AS LOGIC
                LOCAL oBag AS CdxOrderBag
                //
                TRY
                    IF File(info:BagName)
                        info:BagName := FPathName()
                    ENDIF
                    oBag := SELF:FindOrderBag(info:BagName)
                    IF oBag == NULL_OBJECT
                        // Create new OrderBag
                        oBag := CdxOrderBag{_oRDD}
                        oBag:CreateBag(info:BagName)
                    ENDIF
                    RETURN oBag:OrderCreate(info)
                    
                CATCH e AS Exception
                    System.Diagnostics.Debug.WriteLine(e:Message)
                    RETURN FALSE
                END TRY
            
        METHOD Close(orderInfo AS DbOrderInfo) AS LOGIC
            LOCAL oTag AS CdxTag
            oTag := SELF:FindOrder(orderInfo)
            IF oTag != NULL
                VAR bag := oTag:OrderBag
                bag:Close()
                SELF:_bags:Remove(bag)
            ENDIF
            RETURN TRUE
            
        METHOD CloseAll() AS LOGIC
            LOCAL oStruct := NULL AS CdxOrderBag
            FOREACH oBag AS CdxOrderBag IN _bags
                 IF oBag:Structural
                    oStruct := oBag
                 ELSE
                    oBag:Close()
                ENDIF
            NEXT
            _bags:Clear()
            IF oStruct != NULL
                _bags:Add(oStruct)
            ENDIF
            RETURN TRUE
            
        METHOD Remove(cFileName AS STRING) AS LOGIC
            IF File(cFileName)
                cFileName := FPathName()
            ENDIF
            FOREACH oBag AS CdxOrderBag IN _bags
                IF oBag:MatchesFileName(cFileName)
                    oBag:Close()
                    _bags:Remove(oBag)
                    RETURN TRUE
                ENDIF
            NEXT
            RETURN FALSE
            
        PROPERTY IsHot AS LOGIC
            GET
                FOREACH oBag AS CdxOrderBag IN _bags
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
            FOREACH oBag AS CdxOrderBag IN _bags
                IF ! oBag:GoCold()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk
            
        METHOD GoHot() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _bags
                IF ! oBag:GoHot()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk
            
        METHOD Flush() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _bags
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

        METHOD FindOrderBag(cBagName AS STRING) AS CdxOrderBag
            FOREACH oBag AS CdxOrderBag IN _bags
                IF oBag:MatchesFileName(cBagName)
                    RETURN oBag
                ENDIF
            NEXT
            RETURN NULL

        METHOD FindOrderByName(cBagName AS STRING, cName AS STRING) AS CdxTag
            FOREACH oBag AS CdxOrderBag IN _bags
                IF String.IsNullOrEmpty(cBagName) .OR.  oBag:MatchesFileName(cBagName)
                    VAR oTag := oBag:_FindTagByName(cName)
                    IF oTag != NULL
                        RETURN oTag
                    ENDIF
                ENDIF
            NEXT
            RETURN NULL

        METHOD FindOrder(orderinfo AS DbOrderInfo) AS CdxTag
            IF orderInfo:Order IS STRING name
                RETURN FindOrderByName(orderinfo:BagName, name)
            ELSEIF orderInfo:Order IS LONG number
                IF number > 0
                    FOREACH oBag AS CdxOrderBag IN _bags
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
                FOREACH oBag AS CdxOrderBag IN _bags
                    nResult += oBag:Count
                NEXT
                RETURN nResult
            END GET
        END PROPERTY
        
        METHOD OrderPos(oTagToFind AS CdxTag) AS LONG
            LOCAL nPos := 0 AS LONG
            IF oTagToFind != NULL
                FOREACH oBag AS CdxOrderBag IN _bags
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
