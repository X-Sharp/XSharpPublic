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
        SUPER:m_usTableType := ACE.ADS_VFP
        SUPER:m_strDriver := "Advantage.AXDBFVFP"
        SUPER:MAX_KEY_SIZE := 240
	/// <inheritdoc />
		VIRTUAL PROPERTY SysName AS STRING GET m_strDriver
END CLASS

/// <summary>Advantage.AXSQLVFP RDD </summary>
CLASS XSharp.ADS.AXSQLVFP INHERIT AXSQLRDD 
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_VFP
        SUPER:m_strDriver := "Advantage.AXSQLVFP"
        SUPER:MAX_KEY_SIZE := 240
	/// <inheritdoc />
		VIRTUAL PROPERTY SysName AS STRING GET m_strDriver

END CLASS