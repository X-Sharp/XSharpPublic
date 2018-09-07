//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>VO Compatible type to allow name/value pairs for Ole Automation parameters</summary>
CLASS XSharp.NamedArg
   PROPERTY ArgName AS STRING AUTO
   PROPERTY VALUE   AS USUAL AUTO
   
   CONSTRUCTOR( symArgName AS STRING, uValue AS USUAL )
      SELF:ArgName	:= symArgName
      SELF:Value	:= uValue
      RETURN
      
   METHOD ToString() AS STRING
      RETURN  "NamedArg{ " + ArgName + ": " + SELF:Value:ToString() + " }"
   
END CLASS
