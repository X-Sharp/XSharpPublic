//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING XSharp.RDD.Support
USING System.IO

BEGIN NAMESPACE XSharp.RDD.CDX
    /// <summary>
    /// List of OrderBags (an Orderbag = CDX file)
    /// </summary>
    INTERNAL SEALED CLASS CdxOrderBagList
        PRIVATE _oRdd AS DBFCDX
        PRIVATE _bags AS List<CdxOrderBag>

        INTERNAL PROPERTY CurrentOrder AS CdxTag AUTO
        INTERNAL PROPERTY BagCount AS LONG GET _bags:Count


        INTERNAL PROPERTY First AS CdxOrderBag GET IIF(_bags:Count > 0, _bags[0], NULL)
        INTERNAL CONSTRUCTOR(oRdd AS DBFCDX)
            _oRdd := oRdd
            _bags := List<CdxOrderBag>{}
            RETURN

        INTERNAL METHOD Add(info AS DbOrderInfo, lStructural := FALSE AS LOGIC) AS LOGIC
            LOCAL oBag := NULL AS CdxOrderBag
            LOCAL isOk := FALSE AS LOGIC
            // add Existing order bag to the list.
            // looks for bags through the normal paths
            IF File(info:BagName)
                info:BagName := FPathName()
            ELSE
                // Suggestion from Leonid to handle short path names
                info:BagName := Path.Combine( Path.GetDirectoryName(info:BagName), Path.GetFileName(info:BagName) ) // Short name (MS-DOS 8.3) -> Long Name (if necessary)
            ENDIF
            IF SELF:FindOrderBag(info:BagName) == NULL_OBJECT
                TRY
                    oBag := CdxOrderBag{_oRdd}
                    oBag:Structural := lStructural
                    _bags:Add(oBag)
                    isOk := oBag:Open(info)
                    IF isOk
                        if XSharp.RuntimeState.AutoOrder != 0 .and. self:CurrentOrder == null ;
                            .and. oBag:Tags:Count > 0
                            SELF:CurrentOrder := oBag:Tags[0]
                        ENDIF
                    ELSE
                        _bags:Remove(oBag)
                    ENDIF
                    IF lStructural
                        SELF:_oRdd:Header:TableFlags |= DBFTableFlags.HasStructuralCDX
                    ENDIF
                CATCH e AS Exception
                    XSharp.RuntimeState.LastRddError := e
                    IF oBag != NULL .AND. SELF:_bags:Contains(oBag)
                        SELF:_bags:Remove(oBag)
                    ENDIF
                    isOk := FALSE
                END TRY
            ENDIF
            RETURN isOk
        INTERNAL METHOD _AddBag(oBag AS CdxOrderBag) AS VOID
            _bags:Add(oBag)
            RETURN
        PRIVATE METHOD _CreateBag(info AS DbOrderCreateInfo) AS CdxOrderBag
            // Create new OrderBag on disk
            LOCAL oBag AS CdxOrderBag
            oBag := CdxOrderBag{_oRdd}
            oBag:CreateBag(info:BagName)
            VAR cBag := oBag:FullPath
            VAR cDbf := SELF:_oRdd:FullPath
            info:BagName := oBag:FullPath
            VAR lStructural := String.Compare(Path.GetFileNameWithoutExtension(cDbf), Path.GetFileNameWithoutExtension(cBag),StringComparison.OrdinalIgnoreCase) == 0
            oBag:Structural := lStructural
            _bags:Add(oBag)
            RETURN oBag

        INTERNAL METHOD Create(info AS DbOrderCreateInfo) AS LOGIC
                LOCAL oBag AS CdxOrderBag
                //
                TRY
                    IF String.IsNullOrEmpty(info:BagName)
                        info:BagName := Path.ChangeExtension(_oRdd:FullPath, CdxOrderBag.CDX_EXTENSION)
                    ENDIF
                    IF File(info:BagName)
                        info:BagName := FPathName()
                    ENDIF
                    oBag := SELF:FindOrderBag(info:BagName)
                    IF oBag == NULL_OBJECT
                        // bag does not exist
                        IF String.IsNullOrEmpty(Path.GetExtension(info:BagName))
                            info:BagName := Path.ChangeExtension(info:BagName, CdxOrderBag.CDX_EXTENSION)
                        ENDIF
                        if File(info:BagName)
                            local orderInfo as DbOrderInfo
                            orderInfo := DbOrderInfo{}
                            orderInfo:BagName := FPathName()
                            IF SELF:Add(orderInfo, FALSE)
                                oBag := SELF:FindOrderBag(info:BagName)
                            ENDIF
                        ELSE
                            oBag := SELF:_CreateBag(info)
                        endif
                    ENDIF
                    VAR lOk := oBag:OrderCreate(info)
                    IF lOk
                        LOCAL oI AS DbOrderInfo
                        oI := DbOrderInfo{}
                        oI:BagName := info:BagName
                        oI:Order   := info:Order
                        lOk := SELF:Focus(oI)
                    ENDIF
                    RETURN lOk

                CATCH ex AS Exception
                    SELF:_oRdd:_dbfError(ex, Subcodes.EDB_CREATEINDEX,Gencode.EG_CORRUPTION,  "CdxOrderBagList.Create")
                    RETURN FALSE
                END TRY

        INTERNAL METHOD Close(orderInfo AS DbOrderInfo) AS LOGIC
            // close the bag that matches the orderinfo. Structural indexes are also closed. Is that correct ?
            IF SELF:FindOrder(orderInfo, OUT VAR oTag)
                IF oTag != NULL
                    VAR bag := oTag:OrderBag
                    RETURN SELF:_CloseBag(bag)
                ENDIF
            ENDIF
            RETURN FALSE

        INTERNAL METHOD _CloseBag(oBag AS CdxOrderBag) AS LOGIC
            VAR lOk := oBag:Close()
            // worker method to close bags and remove them from the list
            IF lOk .AND. _bags:Contains(oBag)
                _bags:Remove(oBag)
            ENDIF
            RETURN lOk

        INTERNAL METHOD Delete(orderInfo AS DbOrderInfo, lCloseStructural AS LOGIC) AS LOGIC
            LOCAL oStruct := NULL AS CdxOrderBag
            LOCAL lOk := TRUE AS LOGIC
            SELF:CurrentOrder := NULL
            IF orderInfo:AllTags
                FOREACH oBag AS CdxOrderBag IN _bags
                    IF lCloseStructural
                        oBag:Close()
                    ELSEIF ! oBag:Structural
                        oBag:Close()
                    ELSE
                        oStruct := oBag
                    ENDIF
                NEXT
                _bags:Clear()
                IF oStruct != NULL
                    _bags:Add(oStruct)
                ENDIF
            ELSE
                IF ! String.IsNullOrEmpty(orderInfo:BagName)
                    VAR oBag := SELF:FindOrderBag(orderInfo:BagName)
                    IF oBag != NULL
                        lOk := SELF:_CloseBag(oBag)
                    ENDIF
                ELSE
                    IF SELF:FindOrder(orderInfo, OUT VAR oTag)
                        IF oTag != NULL
                            VAR oBag := oTag:OrderBag
                            lOk :=  SELF:_CloseBag(oBag)
                        ENDIF
                    ENDIF
                ENDIF

            ENDIF
            RETURN lOk

        INTERNAL METHOD Destroy(orderInfo AS DbOrderInfo) AS LOGIC
            // destroy the first bag that matches the orderInfo.
            // when 2 bags exist with the same tag name then the first one will be destroyed.
            IF SELF:FindOrder(orderInfo, OUT VAR oTag)
                IF oTag != NULL
                    VAR bag := oTag:OrderBag
                    RETURN bag:Destroy(oTag)
                ELSE
                    SELF:_oRdd:_dbfError( Subcodes.EDB_ORDDESTROY,Gencode.EG_ARG,   "CdxOrderBagList.Destroy", "Order "+orderInfo:Order:ToString()+" does not exist")
                ENDIF
            ENDIF
            RETURN FALSE

        INTERNAL PROPERTY IsHot AS LOGIC
            GET
                // return TRUE as soon as one bag is hot
                FOREACH oBag AS CdxOrderBag IN _bags
                    IF oBag:IsHot
                        RETURN TRUE
                    ENDIF
                NEXT
                RETURN FALSE
            END GET
        END PROPERTY

        INTERNAL METHOD GoCold() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all even of one fails
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _bags
                IF ! oBag:GoCold()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk

        INTERNAL METHOD GoHot() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all even of one fails
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _bags
                IF ! oBag:GoHot()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk

        INTERNAL METHOD Flush() AS LOGIC
            LOCAL lOk AS LOGIC
            // Process all even of one fails
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _bags
                IF ! oBag:Flush()
                    lOk := FALSE
                ENDIF
            NEXT
            RETURN lOk

        INTERNAL METHOD Rebuild() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := TRUE
            FOREACH oBag AS CdxOrderBag IN _bags
                IF ! oBag:OrderListRebuild()
                    lOk := FALSE
                ENDIF
            NEXT
            SELF:GoCold()
            RETURN lOk

        INTERNAL METHOD Focus(orderinfo AS DbOrderInfo) AS LOGIC
            VAR result := SELF:FindOrder(orderinfo, OUT VAR oOrder)
            SELF:CurrentOrder := oOrder
            RETURN result

        INTERNAL METHOD FindOrderBag(cBagName AS STRING) AS CdxOrderBag
            IF ! String.IsNullOrEmpty(cBagName)
                FOREACH oBag AS CdxOrderBag IN _bags
                    IF oBag:MatchesFileName(cBagName)
                        RETURN oBag
                    ENDIF
                NEXT
            ENDIF
            RETURN NULL

        INTERNAL METHOD FindOrderByName(cBagName AS STRING, cName AS STRING, tag OUT CdxTag) AS LOGIC
            // Return first match even if a tag with the same name exists in other bags
            tag  := NULL_OBJECT
            FOREACH oBag AS CdxOrderBag IN _bags
                IF String.IsNullOrEmpty(cBagName) .OR.  oBag:MatchesFileName(cBagName)
                    VAR oTag := oBag:_FindTagByName(cName)
                    IF oTag != NULL
                        tag := oTag
                        RETURN TRUE
                    ENDIF
                ENDIF
            NEXT
            RETURN FALSE

        INTERNAL METHOD FindOrder(orderinfo AS DbOrderInfo, tag OUT CdxTag) AS LOGIC
            tag := NULL
            IF orderinfo:Order IS STRING VAR name
                RETURN SELF:FindOrderByName(orderinfo:BagName, name, OUT tag)
            ELSEIF orderinfo:Order IS LONG VAR number
                IF number > 0
                    FOREACH oBag AS CdxOrderBag IN _bags
                        IF number <= oBag:Tags:Count
                            tag := oBag:Tags[number-1]
                            RETURN TRUE
                        ELSE
                            number -= oBag:Tags:Count
                        ENDIF
                    NEXT
                ELSEIF number == 0
                    tag := NULL
                    RETURN TRUE
                ENDIF
            ENDIF
            RETURN FALSE

        INTERNAL PROPERTY Count AS INT
            GET
                LOCAL nResult := 0 AS LONG
                FOREACH oBag AS CdxOrderBag IN _bags
                    nResult += oBag:Count
                NEXT
                RETURN nResult
            END GET
        END PROPERTY

        INTERNAL METHOD OrderPos(oTagToFind AS CdxTag) AS LONG
            LOCAL nPos := 0 AS LONG
            // return relative position over all the orderbags
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

         INTERNAL METHOD BagName(nBagPos AS LONG) AS STRING
            IF nBagPos > 0 .AND. nBagPos <= _bags:Count
               RETURN _bags[nBagPos-1]:FullPath
            ENDIF
            RETURN ""
    END CLASS
END NAMESPACE
