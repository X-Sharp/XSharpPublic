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
        SUPER:_TableType    := ACE.ADS_NTX
        SUPER:_Driver       := "Advantage.AXDBFNTX"
        SUPER:_MaxKeySize  := 256
END CLASS

/// <summary>Advantage.AXSQLNTX RDD </summary>
CLASS XSharp.ADS.AXSQLNTX INHERIT AXSQLRDD 
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SUPER:_TableType    := ACE.ADS_NTX
        SUPER:_Driver       := "Advantage.AXSQLNTX"
        SUPER:_MaxKeySize  := 256
END CLASS
