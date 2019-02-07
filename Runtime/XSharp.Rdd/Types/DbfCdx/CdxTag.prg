//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Diagnostics
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.CDX
    [DebuggerDisplay("Tag: {Name}, Key: {KeyExpression}, For: {ForExpression}")];
    INTERNAL CLASS CdxTag
        PROTECT oHeader AS CdxTagHeader
        PROTECT _bag    AS CdxOrderBag
        INTERNAL CONSTRUCTOR (oBag AS CdxOrderBag, nPage AS Int32, buffer AS BYTE[], cName AS STRING)
            SELF:_bag := oBag
            SELF:Name := cName
            SELF:Page := nPage
            SELF:FieldIndex  := 0
            SELF:SingleField := FALSE
            oHeader := CdxTagHeader{oBag, nPage, buffer, SELF:Name}
            SELF:KeyExpression := oHeader:KeyExpression
            SELF:ForExpression := oHeader:ForExpression
            SELF:KeyLength     := oHeader:KeySize
            SELF:Descending    := oHeader:Descending
            SELF:Options       := oHeader:Options
            SELF:Signature     := oHeader:Signature
            

#region Properties
        PROPERTY OrderBag       AS CdxOrderBag GET SELF:_bag
        PROPERTY Name           AS STRING AUTO
        PROPERTY Page           AS Int32 AUTO
        PROPERTY KeyExpression  AS STRING AUTO
        PROPERTY KeyBlock       AS ICodeblock AUTO
        PROPERTY ForExpression  AS STRING AUTO
        PROPERTY ForBlock       AS ICodeblock AUTO
        PROPERTY IsConditional  AS LOGIC GET Options:HasFlag(CdxOptions.HasFor)
        PROPERTY KeyLength      AS INT AUTO
        PROPERTY KeyDecimals    AS INT AUTO
        PROPERTY KeyType        AS INT AUTO
        PROPERTY Custom         AS LOGIC GET Options:HasFlag(CdxOptions.IsCustom)
        PROPERTY Descending     AS LOGIC AUTO
        PROPERTY Unique         AS LOGIC GET Options:HasFlag(CdxOptions.IsUnique)
        PROPERTY Signature      AS BYTE AUTO
        PROPERTY SingleField    AS LOGIC AUTO
        PROPERTY FieldIndex     AS INT AUTO             // 1 based FieldIndex
        PROPERTY Options        AS CdxOptions AUTO
        PROPERTY TopScope       AS OBJECT AUTO
        PROPERTY BottomScope    AS OBJECT AUTO
        PROPERTY HasTopScope    AS LOGIC AUTO
        PROPERTY HasBottomScope AS LOGIC AUTO
        PROPERTY IsHot          AS LOGIC AUTO
        PROPERTY LockOffSet     AS LONG AUTO
        
#endregion

        METHOD GoCold() AS LOGIC
            // Todo
            IF SELF:IsHot
                // Do Something
                NOP
            ENDIF
            RETURN TRUE

        METHOD _GotoRecno(nRec AS LONG) AS LOGIC
            // Todo
            RETURN TRUE
        METHOD _getRecPos(nRec REF LONG) AS LOGIC
            // Todo
            nRec := 0
            RETURN TRUE
        METHOD _CountRecords(nCount REF LONG) AS LOGIC
            // Todo
            nCount := 0
            RETURN TRUE
        METHOD _dump() AS VOID
            RETURN

        METHOD SkipRaw(nCount AS LONG) AS LOGIC
            // Todo
            RETURN TRUE

        METHOD Seek(seekInfo AS DBSEEKINFO ) AS LOGIC
            // Todo
            RETURN TRUE

       METHOD GoTop() AS LOGIC
            // Todo
            RETURN TRUE

       METHOD GoBottom() AS LOGIC
            // Todo
            RETURN TRUE

        METHOD SetOrderScope(itmScope AS OBJECT , uiScope AS DBOrder_Info ) AS LOGIC
            // Todo
            RETURN TRUE
    END CLASS
END NAMESPACE
