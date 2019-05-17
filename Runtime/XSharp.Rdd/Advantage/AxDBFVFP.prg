//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING XSharp.RDD


/// <summary>Advantage.AXDBFVFP RDD </summary>
CLASS XSharp.ADS.AXDBFVFP INHERIT ADSRDD
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_VFP
        SELF:_Driver       := "AXDBFVFP"
        SELF:_MaxKeySize  := 240
END CLASS

/// <summary>Advantage.AXSQLVFP RDD </summary>
CLASS XSharp.ADS.AXSQLVFP INHERIT AXSQLRDD 
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SELF:_TableType    := ACE.ADS_VFP
        SELF:_Driver       := "AXSQLVFP"
        SELF:_MaxKeySize  := 240

END CLASS
