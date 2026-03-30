//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XSharp.RDD
USING System.Diagnostics

/// <include file="XSharp.RDD.Docs.xml" path="doc/AXDBFVFP/*" />
[DebuggerDisplay("AXDBFVFP ({Alias,nq})")];
CLASS XSharp.ADS.AXDBFVFP INHERIT ADSRDD
    /// <include file="XSharp.RDD.Docs.xml" path="doc/AXDBFVFP.ctor/*" />
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_VFP
        SELF:_Driver       := "AXDBFVFP"
        SELF:_MaxKeySize  := 240
END CLASS

/// <include file="XSharp.RDD.Docs.xml" path="doc/AXSQLVFP/*" />
[DebuggerDisplay("AXSQLVFP ({Alias,nq})")];
CLASS XSharp.ADS.AXSQLVFP INHERIT AXSQLRDD
    /// <include file="XSharp.RDD.Docs.xml" path="doc/AXSQLVFP.ctor/*" />
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_VFP
        SELF:_Driver       := "AXSQLVFP"
        SELF:_MaxKeySize  := 240

END CLASS
