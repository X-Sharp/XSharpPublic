//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING XSharp.RDD

/// <summary>Advantage.AXDBFNTX RDD </summary>
CLASS XSharp.ADS.AXDBFNTX INHERIT ADSRDD
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_NTX
        SELF:_Driver       := "AXDBFNTX"
        SELF:_MaxKeySize  := 256
END CLASS

/// <summary>Advantage.AXSQLNTX RDD </summary>
CLASS XSharp.ADS.AXSQLNTX INHERIT AXSQLRDD 
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_NTX
        SELF:_Driver       := "AXSQLNTX"
        SELF:_MaxKeySize  := 256
END CLASS
