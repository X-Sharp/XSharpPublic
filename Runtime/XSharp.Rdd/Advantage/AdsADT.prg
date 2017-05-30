//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING XSharp.RDD
USING AdvantageClientEngine

CLASS XSharp.RDD.ADSADT INHERIT ADSRDD
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_ADT
        SUPER:m_strDriver := "Advantage.ADSADT"
        SUPER:MAX_KEY_SIZE := 4082

END CLASS

CLASS XSharp.RDD.AXSQLADT INHERIT AXSQLRDD 
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_ADT
        SUPER:m_strDriver := "Advantage.AXSQLADT"
        SUPER:MAX_KEY_SIZE := 4082

END CLASS
