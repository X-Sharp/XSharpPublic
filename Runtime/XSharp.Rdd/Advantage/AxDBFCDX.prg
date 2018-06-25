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
        SUPER:m_usTableType := ACE.ADS_CDX
        SUPER:m_strDriver := "Advantage.AXDBFCDX"
        SUPER:MAX_KEY_SIZE := 240
	/// <inheritdoc />
    VIRTUAL PROPERTY SysName AS STRING GET m_strDriver
END CLASS

/// <summary>Advantage.AXSQLCDX RDD </summary>
CLASS XSharp.ADS.AXSQLCDX INHERIT AXSQLRDD 
	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SUPER:m_usTableType := ACE.ADS_CDX
        SUPER:m_strDriver := "Advantage.AXSQLCDX"
        SUPER:MAX_KEY_SIZE := 240
	/// <inheritdoc />
    VIRTUAL PROPERTY SysName AS STRING GET m_strDriver
END CLASS
