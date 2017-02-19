//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp

    STATIC INTERNAL CLASS Constants
   
        internal const Company := "XSharpBV"													AS string 			
        internal const Product := "XSharp"														AS string 
        internal const Version := "0.2.9.0"														AS string 
        internal const Copyright := "Copyright © XSharp BV 2015-2017"							AS string 

        internal const RegistryKey := "Software\" + Company + "\" + Product						AS string 
        internal const RegistryKey64 := "Software\WOW6432Node\" + Company + "\" + Product		AS string 
        internal const RegistryValue := "XSharpPath"											AS string 
    END CLASS

END NAMESPACE
