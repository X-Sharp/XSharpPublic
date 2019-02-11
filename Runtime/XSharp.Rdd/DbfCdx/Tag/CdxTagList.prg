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
        PROTECTED _tagList AS List<CdxTag>

	    PROTECTED INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS Int32)
            SUPER(bag  , nPage , buffer ,nKeyLen)
            _tagList := List<CdxTag>{}

		PROTECTED INTERNAL OVERRIDE METHOD Read() AS LOGIC
            LOCAL lOk := SUPER:Read() AS LOGIC
            Debug.Assert (SELF:NodeAttribute == CdxNodeAttribute.TagList)
            // Decode the keys
            FOR VAR nI := 0 TO SELF:NumKeys-1
                LOCAL nRecno    := SELF:GetRecno(nI) AS Int32
                LOCAL bName     := SELF:GetKey(nI)  AS BYTE[]
                LOCAL cName     := System.Text.Encoding.ASCII:GetString( bName, 0, bName:Length) AS STRING
                LOCAL oPage     AS CdxPage
                oPage           := _bag:GetPage(nRecno, 0)
                VAR tag         := CdxTag{_bag,  nRecno, oPage:Buffer, cName:Trim()}
                _tagList:Add(tag)
            NEXT
            // default sort for tags in an orderbag is on pageno. 
            _tagList:Sort( { tagX, tagY => tagX:Page - tagY:Page} ) 
            RETURN lOk
        PROPERTY Tags AS IList<cdxTag> GET _tagList
    END CLASS
END NAMESPACE 
