//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING XSharp.RDD


CLASS XSharp.Ads.AXDBFNTX INHERIT ADSRDD
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_NTX
        SUPER:m_strDriver   := "Advantage.AXDBFNTX"
        SUPER:MAX_KEY_SIZE  := 256
    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXDBFNTX):ToString()
END CLASS


CLASS XSharp.Ads.AXSQLNTX INHERIT AXSQLRDD 
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_NTX
        SUPER:m_strDriver := "Advantage.AXSQLNTX"
        SUPER:MAX_KEY_SIZE := 256
    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXSQLNTX):ToString()
END CLASS