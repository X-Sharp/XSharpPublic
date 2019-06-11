//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.IO
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

/// <summary>
/// The AdsRDD class. 
/// </summary>
CLASS XSharp.ADS.AdsError INHERIT RddError
    PRIVATE _Msg AS STRING
    CONSTRUCTOR(msg AS STRING, dwGenCode AS DWORD, dwSubCode AS DWORD, cDriver AS STRING, dwSeverity AS DWORD, strFunction AS STRING, strFile AS STRING)
        SUPER(msg)
        SELF:_msg         := Msg
        SELF:SubCode      := dwSubCode
        SELF:Gencode      := dwGenCode
        SELF:SubSystem    := cDriver
        SELF:Severity     := dwSeverity
        SELF:FuncSym      := strFunction
        SELF:FileName     := strFile
     PROPERTY SubCodeText AS STRING GET _Msg
        
        
END CLASS
