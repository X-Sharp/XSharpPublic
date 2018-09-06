// NtxOrderList.prg
// Created by    : fabri
// Creation Date : 9/4/2018 6:14:12 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD

    INTERNAL CLASS NtxOrderList
        PRIVATE m_iNtxFocus AS LONG
        PRIVATE _Orders AS List<NtxOrder>
        PRIVATE _oRdd AS DBFNTX
        PRIVATE m_CurrentOrder AS NtxOrder
        PUBLIC PROPERTY Count AS LONG GET _Orders:Count
        PUBLIC PROPERTY Focus AS LONG GET m_iNtxFocus
            PUBLIC PROPERTY CurrentOrder AS NtxOrder GET m_CurrentOrder
                PUBLIC PROPERTY SELF[index AS LONG ] AS NtxOrder
                    GET
                        IF ((index >= 0) .AND. (index < _Orders:Count))
                            RETURN _Orders[index]
                        ENDIF
                        RETURN NULL
                        
                    END GET
                END PROPERTY
                
                PUBLIC CONSTRUCTOR(area AS DBFNTX )
                    SELF:m_iNtxFocus := 0
                    SELF:_Orders := List<NtxOrder>{}
                    SELF:_oRdd := area
                    SELF:m_CurrentOrder := NULL
                    
                    
                PUBLIC METHOD ADD(oi AS DBORDERINFO , filePath AS STRING ) AS LOGIC
                    LOCAL flag AS LOGIC
                    LOCAL nOrder AS NtxOrder
                    //
                    flag := FALSE
                    TRY
                        IF ((LONG)SELF:_Orders:Count >= 16L)
                            SELF:_oRdd:_dbfError(SubCodes.ERDD_NTXLIMIT, GenCode.EG_LIMIT,  filePath)
                            flag := FALSE
                        ELSE
                            nOrder := NtxOrder{SELF:_oRdd, filePath}
                            SELF:_Orders:Add(nOrder)
                            flag := nOrder:Open(oi)
                            IF (flag)
                                SELF:m_iNtxFocus := SELF:_Orders:Count
                                SELF:m_CurrentOrder := nOrder
                                oi:BagName := nOrder:FileName
                            ENDIF
                        ENDIF
                        
                    CATCH //Exception
                        flag := FALSE
                    END TRY
                    IF (!flag)
                        SELF:m_iNtxFocus := 0
                        SELF:m_CurrentOrder := NULL
                    ENDIF
                    RETURN flag
                    
                    
                PUBLIC METHOD Create(dboci AS DBORDERCREATEINFO ) AS LOGIC
                    LOCAL nOrder AS NtxOrder
                    LOCAL isOk AS LOGIC
                    //
                    TRY
                        nOrder := NtxOrder{SELF:_oRdd}
                        isOk := nOrder:Create(dboci)
                        IF (!isOk)
                            RETURN isOk
                        ENDIF
                        SELF:_Orders:Add(nOrder)
                        SELF:m_iNtxFocus := SELF:_Orders:Count
                        SELF:m_CurrentOrder := nOrder
                        nOrder:GoTop()
                        RETURN isOk
                        
                    CATCH //Exception
                        RETURN FALSE
                    END TRY
                    
                    
                PUBLIC METHOD Delete(dbordInfo AS DBORDERINFO ) AS LOGIC
                    LOCAL count AS LONG
                    LOCAL nOrder AS NtxOrder
                    //
                    IF (SELF:_Orders:Count == 0)
                        RETURN TRUE
                    ENDIF
                    //Init
                    count := SELF:_Orders:Count
                    WHILE count > 0
                        nOrder := SELF:_Orders[count - 1]
                        nOrder:Flush()
                        nOrder:Close()
                        SELF:_Orders:RemoveAt(count - 1)
                        //Iterators
                        count := SELF:_Orders:Count
                    ENDDO
                    SELF:m_iNtxFocus := 0
                    SELF:m_CurrentOrder := NULL
                    RETURN TRUE
                    
                    
                PUBLIC METHOD SetFocus(oi AS DBORDERINFO ) AS LOGIC
                    LOCAL currentOrder AS NtxOrder
                    LOCAL isOk AS LOGIC
                    //
                    currentOrder := SELF:m_CurrentOrder
                    IF (currentOrder != NULL)
                        oi:BagName := currentOrder:FileName
                    ELSE
                        oi:BagName := NULL
                    ENDIF
                    SELF:m_iNtxFocus := 0
                    SELF:m_CurrentOrder := NULL
                    SELF:m_iNtxFocus := SELF:FindOrder(oi:Order)
                    isOk := FALSE
                    IF (SELF:m_iNtxFocus > 0)
                        isOk := SELF:_oRdd:GoCold()
                        currentOrder:SetOffLine()
                        IF (isOk)
                            SELF:m_CurrentOrder := SELF:_Orders[SELF:m_iNtxFocus - 1]
                            SELF:m_CurrentOrder:SetOffLine()
                        ENDIF
                    ELSE
                        isOk := FALSE
                        SELF:_oRdd:_dbfError(SubCodes.ERDD_INVALID_ORDER, GenCode.EG_NOORDER,  NULL)
                    ENDIF
                    RETURN isOk
                    
                    
                PUBLIC METHOD Rebuild() AS LOGIC
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
                        IF (isOk)
                            IF !order:_Unique .AND. !order:_Conditional .AND. !order:_Descending .AND. !ordCondInfo:Scoped
                                isOk := order:_CreateIndex()
                            ELSE
                                isOk := order:_CreateUnique(ordCondInfo)
                            ENDIF
                        ENDIF
                    NEXT
                    IF (!isOk)
                        isOk := SELF:Flush()
                        IF (!isOk)
                            RETURN isOk
                        ENDIF
                        FOREACH order2 AS NtxOrder IN SELF:_Orders 
                            order2:_Hot := FALSE
                        NEXT
                        isOk := SELF:_oRdd:GoTop()
                    ENDIF
                    RETURN isOk
                    
                    
                PRIVATE METHOD __GetName(iOrder AS LONG , uiType AS DWORD , strValue REF STRING ) AS LOGIC
                    LOCAL result AS LOGIC
                    LOCAL ntxOrder AS NtxOrder
                    //
                    result := TRUE
                    IF ((iOrder > 0) .AND. (iOrder <= SELF:_Orders:Count))
                        ntxOrder := SELF:_Orders[iOrder - 1]
                        BEGIN SWITCH uiType
                        CASE 5
                            strValue := ntxOrder:OrderName
                        CASE 7
                            strValue := ntxOrder:FileName
                        OTHERWISE
                            strValue := NULL
                            result := FALSE
                    END SWITCH
                    ENDIF
                RETURN result
                
                
            PUBLIC METHOD FindOrder(uOrder AS OBJECT ) AS LONG
                LOCAL result AS LONG
                LOCAL num AS LONG
                //
                result := 0
                IF (uOrder == NULL)
                    RETURN SELF:m_iNtxFocus
                ENDIF
                //
                BEGIN SWITCH Type.GetTypeCode(uOrder:GetType())
                CASE TypeCode.String
                    result := SELF:__GetNamePos((STRING)uOrder)
                CASE TypeCode.Int16
                CASE TypeCode.Int32
                CASE TypeCode.Int64
                CASE TypeCode.Single
                CASE TypeCode.Double
                    num := (LONG)uOrder
                    IF ((num > 0) .AND. (num <= SELF:_Orders:Count))
                        result := num
                    ENDIF
                OTHERWISE
                    result := 0
                END SWITCH
            RETURN result
            
            
        PUBLIC METHOD Flush() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL i AS LONG
            LOCAL nOrder AS NtxOrder
            //
            isOk := TRUE
            FOR i := 0 TO SELF:_Orders:Count-1
                nOrder := SELF:_Orders[i]
                isOk := nOrder:Commit()
                IF (!isOk)
                    EXIT
                ENDIF
            NEXT
            RETURN isOk
            
            
        PUBLIC METHOD GoCold() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL i AS LONG
            LOCAL nOrder AS NtxOrder
            //
            isOk := TRUE
            FOR i := 0 TO SELF:_Orders:Count-1
                nOrder := SELF:_Orders[i]
                isOk := nOrder:GoCold()
                IF (!isOk)
                    EXIT
                ENDIF
            NEXT
            RETURN isOk
            
            
        PUBLIC METHOD GoHot() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL i AS LONG
            LOCAL nOrder AS NtxOrder
            //
            isOk := TRUE
            FOR i := 0 TO SELF:_Orders:Count-1
                nOrder := SELF:_Orders[i]
                isOk := nOrder:_KeySave((DWORD)SELF:_oRdd:RecNo)
                IF (!isOk)
                    EXIT
                ENDIF
            NEXT
            RETURN isOk
            
            
        PRIVATE METHOD __GetNamePos(orderName AS STRING ) AS LONG
            LOCAL i AS LONG
            LOCAL nOrder AS NtxOrder
            //
            IF (SELF:_Orders:Count > 0)
                FOR i := 0 TO SELF:_Orders:Count-1
                    nOrder := SELF:_Orders[i]
                    IF (string.Equals(nOrder:OrderName, orderName, StringComparison.OrdinalIgnoreCase))
                        RETURN i + 1
                    ENDIF
                NEXT
            ENDIF
            RETURN 0
            
            
    END CLASS
    
    
END NAMESPACE
