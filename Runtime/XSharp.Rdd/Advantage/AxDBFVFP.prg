//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING XSharp.RDD


CLASS XSharp.ADS.AXDBFVFP INHERIT ADSRDD
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_VFP
        SUPER:m_strDriver := "Advantage.AXDBFVFP"
        SUPER:MAX_KEY_SIZE := 240
    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXDBFVFP):ToString()

END CLASS

CLASS XSharp.ADS.AXSQLVFP INHERIT AXSQLRDD 
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_VFP
        SUPER:m_strDriver := "Advantage.AXSQLVFP"
        SUPER:MAX_KEY_SIZE := 240
    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXSQLVFP):ToString()

END CLASS