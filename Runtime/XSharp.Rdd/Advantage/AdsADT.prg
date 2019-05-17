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
	SELF:_TableType := ACE.ADS_ADT
	SELF:_Driver := "ADSADT"
	SELF:_MaxKeySize := ACE.ADS_MAX_KEY_LENGTH
END CLASS

CLASS XSharp.ADS.AXSQLADT INHERIT AXSQLRDD 
CONSTRUCTOR()
	SUPER()
	SELF:_TableType := ACE.ADS_ADT
	SELF:_Driver := "AXSQLADT"
	SELF:_MaxKeySize := ACE.ADS_MAX_KEY_LENGTH
END CLASS
