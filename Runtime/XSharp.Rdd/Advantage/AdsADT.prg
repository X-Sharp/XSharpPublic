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
        SUPER:_TableType := ACE.ADS_ADT
        SUPER:_Driver := "Advantage.ADSADT"
        SUPER:_MaxKeySize := ACE.ADS_MAX_KEY_LENGTH
    VIRTUAL PROPERTY SysName AS STRING GET "ADSADT"
END CLASS

CLASS XSharp.ADS.AXSQLADT INHERIT AXSQLRDD 
    CONSTRUCTOR()
        SUPER()
        SUPER:_TableType := ACE.ADS_ADT
        SUPER:_Driver := "Advantage.AXSQLADT"
        SUPER:_MaxKeySize := ACE.ADS_MAX_KEY_LENGTH
    VIRTUAL PROPERTY SysName AS STRING GET "AXSQLADT"
END CLASS
