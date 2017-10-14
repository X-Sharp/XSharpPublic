//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp
	
	STATIC INTERNAL CLASS Constants
		
		INTERNAL CONST Company := "XSharpBV"													AS STRING 			
		INTERNAL CONST Product := "XSharp"														AS STRING 
		INTERNAL CONST Version := "1.1.0.0"														AS STRING 
		INTERNAL CONST Copyright := "Copyright © XSharp BV 2015-2017"					AS STRING 
		
		INTERNAL CONST RegistryKey := "Software\" + Company + "\" + Product			AS STRING 
		INTERNAL CONST RegistryKey64 := "Software\WOW6432Node\" + Company + "\" + Product AS STRING 
		INTERNAL CONST RegistryValue := "XSharpPath"											AS STRING 
	END CLASS
	
END NAMESPACE
