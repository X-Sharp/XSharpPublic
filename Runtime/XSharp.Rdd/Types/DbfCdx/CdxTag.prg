//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
using System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX
    [DebuggerDisplay("Tag: {Name}, Key: {KeyExpression}, For: {ForExpression}")];
    INTERNAL CLASS CdxTag
        PROTECT oHeader as CdxTagHeader
        
        INTERNAL CONSTRUCTOR (hFile as IntPtr, nPage as Int32, cName as STRING)
            SELF:Name := cName
            SELF:Page := nPage
            SELF:FieldIndex  := 0
            SELF:SingleField := FALSE
            oHeader := CdxTagHeader{hFile, SELF:Page, SELF:Name}
            oHeader:Read()
            SELF:KeyExpression := oHeader:KeyExpression
            SELF:ForExpression := oHeader:ForExpression
            SELF:KeyLength     := oHeader:KeySize
            SELF:Descending    := oHeader:Descending
            SELF:Options       := oHeader:Options
            SELF:Signature     := oHeader:Signature
            
            

#region Properties
        PROPERTY Name           as STRING AUTO
        PROPERTY Page           as Int32 AUTO
        PROPERTY KeyExpression  as STRING AUTO
        PROPERTY KeyBlock       as ICodeblock AUTO
        PROPERTY ForExpression  as STRING AUTO
        PROPERTY ForBlock       as ICodeblock AUTO
        PROPERTY KeyLength      as INT AUTO
        PROPERTY KeyType        as TypeCode auto
        PROPERTY Custom         AS LOGIC GET Options:HasFlag(CdxOptions.IsCustom)
        PROPERTY Descending     AS LOGIC AUTO
        PROPERTY Unique         AS LOGIC GET Options:HasFlag(CdxOptions.IsUnique)
        PROPERTY Signature      as BYTE AUTO
        PROPERTY SingleField    as LOGIC AUTO
        PROPERTY FieldIndex     AS INT AUTO             // 1 based FieldIndex
        PROPERTY Options        as CdxOptions AUTO
        
#endregion
          
    END CLASS
END NAMESPACE
