//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
BEGIN NAMESPACE XSharp.RDD
/// <summary>DELIM RDD. For reading and writing delimited files.</summary>
CLASS DELIM INHERIT TEXTRDD  
	CONSTRUCTOR
		SUPER()                     

VIRTUAL PROPERTY Driver AS STRING GET "DELIM" 


METHOD _GetLastRec as LONG
    Local dwPos     as DWORD
    local nCount := 0 as LONG
    dwPos := FTell(SELF:_hFile)
    FSeek3(SELF:_hFile, 0, FS_SET)
    DO WHILE ! FEof(self:_hFile)
        nCount++
        FReadLine(SELF:_hFile, 4096)
    ENDDO
    FSeek3(SELF:_hFile, (LONG) dwPos, FS_SET)
    RETURN nCount

END CLASS
END NAMESPACE
