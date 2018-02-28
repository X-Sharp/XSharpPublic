//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD


CLASS XSharp.Ads.AXDBFCDX INHERIT ADSRDD
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_CDX
        SUPER:m_strDriver := "Advantage.AXDBFCDX"
        SUPER:MAX_KEY_SIZE := 240
    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXDBFCDX):ToString()
END CLASS

CLASS XSharp.Ads.AXSQLCDX INHERIT AXSQLRDD 
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_CDX
        SUPER:m_strDriver := "Advantage.AXSQLCDX"
        SUPER:MAX_KEY_SIZE := 240
    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXSQLCDX):ToString()
END CLASS
