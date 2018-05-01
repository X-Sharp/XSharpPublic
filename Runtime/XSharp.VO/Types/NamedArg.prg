//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

CLASS XSharp.NamedArg
   PROPERTY ArgName AS STRING AUTO
   PROPERTY Value   AS USUAL AUTO
   
   CONSTRUCTOR( symArgName AS STRING, uValue AS USUAL )
      SELF:ArgName	:= symArgName
      SELF:Value	:= uValue
      RETURN
      
   METHOD ToString() AS STRING
      RETURN  "NamedArg{ " + ArgName + ": " + SELF:Value:ToString() + " }"
   
END CLASS
