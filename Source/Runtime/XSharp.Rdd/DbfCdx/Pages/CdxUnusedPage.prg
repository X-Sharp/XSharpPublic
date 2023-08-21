//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD.CDX
	/// <summary>
	/// CdxLeaf page. this class maps the Leaf page from the file in memory
    /// Manipulating the page is implemented in the CdxTag class
	/// </summary>
	INTERNAL SEALED CLASS CdxGeneralPage INHERIT CdxPage 
		
	    INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[])
            SUPER(bag, nPage, buffer)
    END CLASS



END NAMESPACE 

