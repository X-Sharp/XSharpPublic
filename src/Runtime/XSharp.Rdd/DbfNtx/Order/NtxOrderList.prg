//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.NTX

    INTERNAL SEALED CLASS NtxOrderList
        PRIVATE _focusNtx AS LONG
        PRIVATE _Orders AS List<NtxOrder>
        PRIVATE _oRdd AS DBFNTX
        PRIVATE _currentOrder AS NtxOrder
        INTERNAL PROPERTY Count AS LONG GET _Orders:Count
        INTERNAL PROPERTY Focus AS LONG GET _focusNtx
        INTERNAL PROPERTY CurrentOrder AS NtxOrder GET _currentOrder SET _currentOrder := value
        INTERNAL PROPERTY SELF[index AS LONG ] AS NtxOrder
                GET
                    IF ((index >= 0) .AND. (index < _Orders:Count))
                        RETURN _Orders[index]
                    ENDIF
                    RETURN NULL

                END GET
            END PROPERTY

            INTERNAL CONSTRUCTOR(area AS DBFNTX )
                SELF:_focusNtx := 0
                SELF:_Orders := List<NtxOrder>{}
                SELF:_oRdd := area
                SELF:_currentOrder := NULL


            INTERNAL METHOD Add(oi AS DbOrderInfo , filePath AS STRING ) AS LOGIC
                LOCAL isOk AS LOGIC
                LOCAL ntxIndex := NULL AS NtxOrder
                //
                isOk := FALSE
                VAR cExt := System.IO.Path.GetExtension(filePath)
                IF String.IsNullOrEmpty(cExt)
                    filePath := System.IO.Path.ChangeExtension(filePath, NtxOrder.NTX_EXTENSION)
                ENDIF
                 IF File(filePath)
                    filePath := FPathName()
                    TRY

                        IF SELF:_Orders:Count >= 16
                            SELF:_oRdd:_dbfError(Subcodes.ERDD_NTXLIMIT, Gencode.EG_LIMIT,  filePath)
                            isOk := FALSE
                        ELSE

                            ntxIndex := NtxOrder{SELF:_oRdd, filePath}
                            SELF:_Orders:Add(ntxIndex)
                            isOk := ntxIndex:Open(oi)
                            IF isOk
                                if SELF:_Orders:Count == 1
                                    SELF:_focusNtx      := 1
                                    SELF:_currentOrder  := ntxIndex
                                endif
                                oi:BagName := ntxIndex:FileName
                            ELSE
                                SELF:_Orders:Remove(ntxIndex)
                            ENDIF
                        ENDIF

                    CATCH e AS Exception
                        XSharp.RuntimeState.LastRddError := e
                        isOk := FALSE
                        IF ntxIndex != NULL .AND. SELF:_Orders:Contains(ntxIndex)
                            SELF:_Orders:Remove(ntxIndex)
                        ENDIF
                    END TRY
                    IF !isOk
                        SELF:_focusNtx := 0
                        SELF:_currentOrder := NULL
                    ENDIF
                ENDIF
                RETURN isOk


            INTERNAL METHOD Create(dboci AS DbOrderCreateInfo ) AS LOGIC
                LOCAL ntxIndex AS NtxOrder
                LOCAL isOk AS LOGIC
                //
                TRY
                    ntxIndex := NtxOrder{SELF:_oRdd}
                    isOk := ntxIndex:Create(dboci)
                    IF (!isOk)
                        RETURN isOk
                    ENDIF
                    SELF:_Orders:Add(ntxIndex)
                    SELF:_focusNtx := SELF:_Orders:Count
                    SELF:_currentOrder := ntxIndex
                    ntxIndex:GoTop()
                    RETURN isOk

                CATCH e AS Exception
                    System.Diagnostics.Debug.WriteLine(e:Message)
                    RETURN FALSE
                END TRY

            INTERNAL METHOD Delete( orderInfo AS DbOrderInfo) AS LOGIC
                LOCAL ntxIndex AS NtxOrder
                LOCAL found AS LOGIC
                LOCAL pos AS INT
                //
                IF (SELF:_Orders:Count == 0)
                    RETURN TRUE
                ENDIF
                //
                IF ( orderInfo:AllTags )
                    RETURN SELF:CloseAll()
                ENDIF
                //
                found := FALSE
                pos := SELF:_Orders:Count
                WHILE pos > 0
                    ntxIndex := SELF:_Orders[pos - 1]
                    IF ( ntxIndex:OrderName == orderInfo:BagName )
                        ntxIndex:Flush()
                        ntxIndex:Close()
                        //
                        SELF:_Orders:RemoveAt(pos - 1)
                        found := TRUE
                        EXIT
                    ENDIF
                ENDDO
                IF ( found )
                    IF ( SELF:_Orders:Count > 0 ) .AND. ( ( pos >= SELF:_focusNtx ) .AND. (pos < SELF:_Orders:Count ) )
                        SELF:_focusNtx -= 1
                        SELF:_currentOrder := SELF:_Orders[pos - 1]
                    ELSE
                        SELF:_focusNtx := 0
                        SELF:_currentOrder := NULL
                    ENDIF
                ENDIF
                RETURN TRUE

            INTERNAL METHOD CloseAll() AS LOGIC
                LOCAL ntxIndex AS NtxOrder
                //
                IF (SELF:_Orders:Count == 0)
                    RETURN TRUE
                ENDIF
                //
                WHILE SELF:_Orders:Count > 0
                    ntxIndex := SELF:_Orders[SELF:_Orders:Count - 1]
                    ntxIndex:Flush()
                    ntxIndex:Close()
                    //
                    SELF:_Orders:RemoveAt(SELF:_Orders:Count - 1)
                ENDDO
                SELF:_focusNtx := 0
                SELF:_currentOrder := NULL
                RETURN TRUE


            INTERNAL METHOD SetFocus(oi AS DbOrderInfo ) AS LOGIC
                LOCAL currentOrder AS NtxOrder
                LOCAL isOk AS LOGIC
                //
                currentOrder := SELF:_currentOrder
                IF (currentOrder != NULL)
                    oi:BagName := currentOrder:FileName
                ELSE
                    oi:BagName := NULL
                ENDIF
                SELF:_focusNtx := 0
                SELF:_currentOrder := NULL
                isOk := SELF:FindOrder(oi, out var newOrder)
                IF (isOk)
                    SELF:_focusNtx := SELF:OrderPos(newOrder)
                    isOk := SELF:_oRdd:GoCold()
                    IF currentOrder != NULL_OBJECT
                        currentOrder:SetOffLine()
                    ENDIF
                    IF (isOk)
                        IF SELF:_focusNtx == 0
                            _currentOrder := NULL
                        ELSE
                            SELF:_currentOrder := newOrder
                            SELF:_currentOrder:SetOffLine()
                        ENDIF
                    ENDIF
                ELSE
                    SELF:_oRdd:_dbfError(Subcodes.ERDD_INVALID_ORDER, Gencode.EG_NOORDER,  NULL)
                ENDIF
                RETURN isOk

          METHOD OrderPos(oToFind AS NtxOrder) AS LONG
                LOCAL nPos := 0 AS LONG
                IF oToFind != NULL
                    FOREACH oOrder AS NtxOrder IN _Orders
                        ++nPos
                        IF oToFind == oOrder
                            RETURN nPos
                        ENDIF
                    NEXT
                ENDIF
                RETURN 0
            INTERNAL METHOD Rebuild() AS LOGIC
                LOCAL ordCondInfo AS DbOrderCondInfo
                LOCAL isOk AS LOGIC
                //
                ordCondInfo := SELF:_oRdd:OrderCondInfo
                isOk := SELF:GoCold()
                IF (!isOk)
                    RETURN FALSE
                ENDIF
                //
                FOREACH order AS NtxOrder IN SELF:_Orders
                    isOk := order:Truncate()
                    IF isOk
                        IF !order:Unique .AND. !order:Conditional .AND. !order:Descending .AND. !ordCondInfo:Scoped
                            isOk := order:_CreateIndex()
                        ELSE
                            isOk := order:_CreateUnique(ordCondInfo)
                        ENDIF
                    ENDIF
                NEXT
                IF !isOk
                    isOk := SELF:Flush()
                    IF !isOk
                        RETURN isOk
                    ENDIF
                    isOk := SELF:_oRdd:GoTop()
                ENDIF
                RETURN isOk

            INTERNAL METHOD FindOrder(info AS DbOrderInfo, order OUT NtxOrder) AS LOGIC
                //
                order := NULL
                IF info == null .or. info:IsEmpty
                    order := SELF:CurrentOrder
                    RETURN TRUE
                ENDIF
                //
                IF info:Order is STRING VAR name
                    if SELF:FindOrderByName(name, out order)
                        RETURN TRUE
                    endif
                ELSEIF info:Order is LONG VAR number
                    IF number > 0 .AND. number <= SELF:_Orders:Count
                        order := SELF:_Orders[number - 1]
                        RETURN TRUE
                    ELSEIF number == 0
                        RETURN TRUE
                    ENDIF
                ELSE
                    order := NULL
                ENDIF
            RETURN FALSE


        INTERNAL METHOD Flush() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL i AS LONG
            LOCAL ntxIndex AS NtxOrder
            //
            isOk := TRUE
            FOR i := 0 TO SELF:_Orders:Count-1
                ntxIndex := SELF:_Orders[i]
                isOk := ntxIndex:Commit()
                IF !isOk
                    EXIT
                ENDIF
            NEXT
            RETURN isOk


        INTERNAL METHOD GoCold() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL i AS LONG
            LOCAL ntxIndex AS NtxOrder
            //
            isOk := TRUE
            FOR i := 0 TO SELF:_Orders:Count-1
                ntxIndex := SELF:_Orders[i]
                isOk := ntxIndex:GoCold()
                IF (!isOk)
                    EXIT
                ENDIF
            NEXT
            RETURN isOk


        INTERNAL METHOD GoHot() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL i AS LONG
            LOCAL ntxIndex AS NtxOrder
            //
            isOk := TRUE
            FOR i := 0 TO SELF:_Orders:Count-1
                ntxIndex := SELF:_Orders[i]
                isOk := ntxIndex:GoHot()
                IF (!isOk)
                    EXIT
                ENDIF
            NEXT
            RETURN isOk


        PRIVATE METHOD FindOrderByName(orderName AS STRING, order OUT NtxOrder ) AS LOGIC
            order := NULL
            IF (SELF:_Orders:Count > 0)
                FOREACH var ntxIndex IN SELF:_Orders
                    IF String.Compare(ntxIndex:OrderName, orderName, TRUE) == 0
                        order := ntxIndex
                        RETURN TRUE
                    ENDIF
                NEXT
            ENDIF
            RETURN FALSE


    END CLASS


END NAMESPACE
