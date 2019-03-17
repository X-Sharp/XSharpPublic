//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Text
USING XSharp.Rdd.Support
USING System.Diagnostics
BEGIN NAMESPACE XSharp.RDD

    INTERNAL ENUM SkipDirection
        MEMBER Backward := -1
        MEMBER Forward := 1
        
    END ENUM

	PUBLIC CLASS SortRecord
		PRIVATE _data AS BYTE[]
		PRIVATE _Recno AS LONG
        INTERNAL PROPERTY Duplicate AS LOGIC AUTO
		
		INTERNAL PROPERTY Data AS BYTE[] GET _data
		
		INTERNAL PROPERTY Recno AS LONG GET _Recno
		
		INTERNAL CONSTRUCTOR(data AS BYTE[] , lRecno AS LONG )
			SELF:_data  := (BYTE[])data:Clone()
			SELF:_Recno := lRecno
            SELF:Duplicate := FALSE
			
	END CLASS
    INTERNAL ENUM SearchMode
        MEMBER Left
        MEMBER LeftFound
        MEMBER Right
        MEMBER Bottom
        MEMBER Top
        
    END ENUM

    // Rdd Stack item
    // Keep informations
    [DebuggerDisplay("Page {Page}, Pos {Pos}, Count {Count}")];
    INTERNAL CLASS RddStack
        INTERNAL Page   AS LONG
        INTERNAL Pos    AS WORD
        INTERNAL Count  AS WORD
        
        INTERNAL METHOD Clear() AS VOID
            SELF:Page := 0
            SELF:Count := 0
            SELF:Pos := 0
            
    END CLASS

    INTERNAL CLASS RddKeyData
        EXPORT Recno   AS LONG
        EXPORT Key     AS BYTE[]
        EXPORT ForCond AS LOGIC
        INTERNAL CONSTRUCTOR (nKeyLen AS LONG)
            SELF:ForCond := TRUE
            SELF:Recno   := -1
            SELF:Key     := BYTE[]{nKeyLen}
            RETURN

        INTERNAL METHOD CopyTo(oOther AS RddKeyData) AS VOID
            oOther:ForCond := SELF:ForCond
            oOther:Recno   := SELF:Recno
            IF oOther:Key:Length != SELF:Key:Length
                oOther:Key  := (BYTE[]) SELF:Key:Clone()
            ELSE
                Array.Copy(SELF:Key, oOther:Key, SELF:Key:Length)
            ENDIF
    END CLASS

END NAMESPACE
