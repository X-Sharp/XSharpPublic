//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
BEGIN NAMESPACE XSharp.RDD
/// <summary>SDF RDD. For reading and writing text files.</summary>
CLASS SDF INHERIT TEXTRDD  

VIRTUAL PROPERTY Driver AS STRING GET "SDF"

    METHOD _GetLastRec as LONG
    Local dwPos as DWORD
    local dwLen as LONG
    local nCount as LONG
    dwPos := FTell(SELF:_hFile)
    dwLen := FSeek3(SELF:_hFile, 0, FS_END)
    nCount := dwLen / SELF:_RecordLength
    FSeek3(SELF:_hFile, (LONG) dwPos, FS_SET)
    RETURN nCount


END CLASS
END NAMESPACE
