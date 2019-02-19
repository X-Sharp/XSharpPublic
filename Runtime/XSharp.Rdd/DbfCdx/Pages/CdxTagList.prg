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

	    PROTECTED INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , page AS CdxPage)
            SUPER(bag  , page)

        INTERNAL METHOD ReadTags() AS List<CdxTag>
            _tagList := List<CdxTag>{}
            Debug.Assert (SELF:NodeAttribute:HasFlag(CdxNodeAttribute.TagList))
            FOR VAR nI := 0 TO SELF:NumKeys-1
                LOCAL nRecno    := SELF:GetRecno(nI) AS Int32
                LOCAL bName     := SELF:GetKey(nI)  AS BYTE[]
                LOCAL cName     := System.Text.Encoding.ASCII:GetString( bName, 0, bName:Length) AS STRING
                LOCAL oPage     AS CdxPage
                oPage           := _bag:GetPage(nRecno, 0,NULL)
                VAR tag         := CdxTag{_bag,  nRecno, oPage:Buffer, cName:Trim()}
                _tagList:Add(tag)
            NEXT
            // default sort for tags in an orderbag is on pageno. 
            _tagList:Sort( { tagX, tagY => tagX:Page - tagY:Page} ) 
            RETURN _tagList
        PROPERTY Tags AS IList<cdxTag> GET _tagList
    END CLASS
END NAMESPACE 
