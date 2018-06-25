//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING XSharp.RDD 


CLASS XSharp.ADS.ADSADT INHERIT ADSRDD
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_ADT
        SUPER:m_strDriver := "Advantage.ADSADT"
        SUPER:MAX_KEY_SIZE := ACE.ADS_MAX_KEY_LENGTH
    VIRTUAL PROPERTY SysName AS STRING GET typeof(ADSADT):ToString()
END CLASS

CLASS XSharp.ADS.AXSQLADT INHERIT AXSQLRDD 
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_ADT
        SUPER:m_strDriver := "Advantage.AXSQLADT"
        SUPER:MAX_KEY_SIZE := ACE.ADS_MAX_KEY_LENGTH
    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXSQLADT):ToString()
END CLASS
