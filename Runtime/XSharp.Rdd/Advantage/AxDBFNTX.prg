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
        SUPER:m_usTableType := ACE.ADS_NTX
        SUPER:m_strDriver   := "Advantage.AXDBFNTX"
        SUPER:MAX_KEY_SIZE  := 256
		/// <inheritdoc />
		VIRTUAL PROPERTY SysName AS STRING GET m_strDriver
END CLASS

/// <summary>Advantage.AXSQLNTX RDD </summary>
CLASS XSharp.ADS.AXSQLNTX INHERIT AXSQLRDD 
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_NTX
        SUPER:m_strDriver := "Advantage.AXSQLNTX"
        SUPER:MAX_KEY_SIZE := 256
		/// <inheritdoc />
		VIRTUAL PROPERTY SysName AS STRING GET m_strDriver
END CLASS