//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING XSharp.RDD
USING System.Diagnostics

/// <include file="XSharp.RDD.Docs.xml" path="doc/AXDBFNTX/*" />
[DebuggerDisplay("AXDBFNTX ({Alias,nq})")];
CLASS XSharp.ADS.AXDBFNTX INHERIT ADSRDD
    /// <include file="XSharp.RDD.Docs.xml" path="doc/AXDBFNTX.ctor/*" />
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_NTX
        SELF:_Driver       := "AXDBFNTX"
        SELF:_MaxKeySize  := 256
END CLASS

/// <include file="XSharp.RDD.Docs.xml" path="doc/AXSQLNTX/*" />
[DebuggerDisplay("AXSQLNTX ({Alias,nq})")];
CLASS XSharp.ADS.AXSQLNTX INHERIT AXSQLRDD
    /// <include file="XSharp.RDD.Docs.xml" path="doc/AXSQLNTX.ctor/*" />
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_NTX
        SELF:_Driver       := "AXSQLNTX"
        SELF:_MaxKeySize  := 256
END CLASS
