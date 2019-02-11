//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Text
USING XSharp.Rdd.Support

BEGIN NAMESPACE XSharp.RDD

    INTERNAL ENUM SkipDirection
        MEMBER Backward := -1
        MEMBER Forward := 1
        
    END ENUM

	INTERNAL CLASS SortRecord
		PRIVATE _data AS BYTE[]
		PRIVATE _Recno AS LONG
		
		INTERNAL PROPERTY Data AS BYTE[] GET _data
		
		INTERNAL PROPERTY Recno AS LONG GET _Recno
		
		INTERNAL CONSTRUCTOR(data AS BYTE[] , uiRecno AS LONG )
			SELF:_data  := (BYTE[])data:Clone()
			SELF:_Recno := uiRecno
			
	END CLASS
    INTERNAL ENUM SearchMode
        MEMBER Left
        MEMBER LeftFound
        MEMBER Right
        MEMBER Bottom
        MEMBER Top
        
    END ENUM
    
END NAMESPACE
