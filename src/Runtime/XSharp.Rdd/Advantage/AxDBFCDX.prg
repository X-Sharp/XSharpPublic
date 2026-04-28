//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING System.Diagnostics
/// <include file="XSharp.RDD.Docs.xml" path="doc/AXDBFCDX/*" />
[DebuggerDisplay("AXDBFCDX ({Alias,nq})")];
CLASS XSharp.ADS.AXDBFCDX INHERIT ADSRDD
/// <include file="XSharp.RDD.Docs.xml" path="doc/AXDBFCDX.ctor/*" />
CONSTRUCTOR()
	SUPER()
	SELF:_TableType := ACE.ADS_CDX
	SELF:_Driver := "AXDBFCDX"
	SELF:_MaxKeySize := 240
END CLASS

/// <include file="XSharp.RDD.Docs.xml" path="doc/AXSQLCDX/*" />
[DebuggerDisplay("AXSQLCDX ({Alias,nq})")];
CLASS XSharp.ADS.AXSQLCDX INHERIT AXSQLRDD
/// <include file="XSharp.RDD.Docs.xml" path="doc/AXSQLCDX.ctor/*" />
CONSTRUCTOR()
	SUPER()
	SELF:_TableType    := ACE.ADS_CDX
	SELF:_Driver       := "AXSQLCDX"
	SELF:_MaxKeySize  := 240
END CLASS
