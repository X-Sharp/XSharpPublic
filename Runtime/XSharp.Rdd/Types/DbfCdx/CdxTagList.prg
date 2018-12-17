//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxTagList class is a special CdxLeaf. Its NodeAttribute must be Root + Leaf = 3
	/// </summary>
	INTERNAL CLASS CdxTagList INHERIT CdxLeafPage
        PROTECTED _tagList as List<CdxTag>
	    PROTECTED INTERNAL CONSTRUCTOR( fileHandle AS IntPtr, nPage as Int32 , nKeyLen as Int32)
            SUPER(fileHandle, nPage, nKeyLen)
            _tagList := List<CdxTag>{}
		PROTECTED INTERNAL OVERRIDE METHOD Read() AS LOGIC
            LOCAL lOk := SUPER:Read() as LOGIC
            Debug.Assert (SELF:NodeAttribute == CdxNodeAttribute.TagList)
            // Decode the keys
            FOR VAR nI := 0 to SELF:NumKeys-1
                local nRecno    := SELF:GetRecno(nI) as Int32
                local bName     := SELF:GetKey(nI)  as byte[]
                local cName     := System.Text.Encoding.ASCII:GetString( bName, 0, bName:Length) as STRING
                var tag         := CdxTag{SELF:_hFile, nRecno, cName:Trim()}
                _tagList:Add(tag)
            NEXT
            // default sort for tags in an orderbag is on pageno. 
            _tagList:Sort( { tagX, tagY => tagX:Page - tagY:Page} ) 
            RETURN lOk
        PROPERTY Tags as IList<cdxTag> Get _tagList
    END CLASS
END NAMESPACE 
