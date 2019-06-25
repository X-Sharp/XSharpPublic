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
        INTERNAL PROPERTY CurrentOrder AS NtxOrder GET _currentOrder SET _currentOrder := VALUE
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
                LOCAL ntxIndex AS NtxOrder
                //
                isOk := FALSE
                TRY
                    IF SELF:_Orders:Count >= 16
                        SELF:_oRdd:_dbfError(SubCodes.ERDD_NTXLIMIT, GenCode.EG_LIMIT,  filePath)
                        isOk := FALSE
                    ELSE

                        ntxIndex := NtxOrder{SELF:_oRdd, filePath}
                        SELF:_Orders:Add(ntxIndex)
                        isOk := ntxIndex:Open(oi)
                        IF isOk
                            SELF:_focusNtx := SELF:_Orders:Count
                            SELF:_currentOrder := ntxIndex
                            oi:BagName := ntxIndex:FileName
                        ENDIF
                    ENDIF
                    
                CATCH //Exception
                    isOk := FALSE
                END TRY
                IF !isOk
                    SELF:_focusNtx := 0
                    SELF:_currentOrder := NULL
                ENDIF
                RETURN isOk
                
                
            INTERNAL METHOD Create(dboci AS DBORDERCREATEINFO ) AS LOGIC
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
                        SELF:_focusNtx :=- 1
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
                SELF:_focusNtx := SELF:FindOrder(oi)
                isOk := FALSE
                IF (SELF:_focusNtx >= 0)
                    isOk := SELF:_oRdd:GoCold()
                    IF currentOrder != NULL_OBJECT
                        currentOrder:SetOffLine()
                    ENDIF
                    IF (isOk)
                        IF SELF:_focusNtx == 0
                            _currentOrder := NULL
                        ELSE
                            SELF:_currentOrder := SELF:_Orders[SELF:_focusNtx - 1]
                            SELF:_currentOrder:SetOffLine()
                        ENDIF
                    ENDIF
                ELSE
                    isOk := FALSE
                    SELF:_oRdd:_dbfError(SubCodes.ERDD_INVALID_ORDER, GenCode.EG_NOORDER,  NULL)
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
                LOCAL ordCondInfo AS DBORDERCONDINFO
                LOCAL isOk AS LOGIC
                //
                ordCondInfo := SELF:_oRdd:_OrderCondInfo
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
                
            INTERNAL METHOD FindOrder(info AS DbOrderInfo ) AS LONG
                LOCAL result AS LONG
                LOCAL num AS LONG
                //
                result := -1
                IF info == NULL .OR. info:Order == NULL
                    RETURN SELF:_focusNtx
                ENDIF
                //
                BEGIN SWITCH Type.GetTypeCode(info:Order:GetType())
                CASE TypeCode.String
                    result := SELF:__GetNamePos((STRING)info:Order)
                CASE TypeCode.Int16
                CASE TypeCode.Int32
                CASE TypeCode.Int64
                CASE TypeCode.Single
                CASE TypeCode.Double
                    num := Convert.ToInt32(info:Order)
                    IF ((num >= 0) .AND. (num <= SELF:_Orders:Count))
                        result := num
                    ENDIF
                OTHERWISE
                    result := -1
                END SWITCH
            RETURN result
            
            
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
            
            
        PRIVATE METHOD __GetNamePos(orderName AS STRING ) AS LONG
            LOCAL i AS LONG
            LOCAL ntxIndex AS NtxOrder
            //
            IF (SELF:_Orders:Count > 0)
                FOR i := 0 TO SELF:_Orders:Count-1
                    ntxIndex := SELF:_Orders[i]
                    IF (string.Equals(ntxIndex:OrderName, orderName, StringComparison.OrdinalIgnoreCase))
                        RETURN i + 1
                    ENDIF
                NEXT
            ENDIF
            RETURN 0
            
            
    END CLASS
    
    
END NAMESPACE
