//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD

/// <summary>Advantage.AXDBFCDX RDD </summary>
CLASS XSharp.ADS.AXDBFCDX INHERIT ADSRDD
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SUPER:_TableType := ACE.ADS_CDX
        SUPER:_Driver := "Advantage.AXDBFCDX"
        SUPER:_MaxKeySize := 240
END CLASS

/// <summary>Advantage.AXSQLCDX RDD </summary>
CLASS XSharp.ADS.AXSQLCDX INHERIT AXSQLRDD 
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SUPER:_TableType    := ACE.ADS_CDX
        SUPER:_Driver       := "Advantage.AXSQLCDX"
        SUPER:_MaxKeySize  := 240
END CLASS
