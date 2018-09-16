//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>VO Compatible type to allow name/value pairs for Ole Automation parameters</summary>
CLASS XSharp.NamedArg
    /// <summary>Argument name.</summary>
   PROPERTY ArgName AS STRING AUTO
    /// <summary>Argument value.</summary>
   PROPERTY @@Value AS USUAL AUTO
   /// <inheritdoc />
   CONSTRUCTOR( symArgName AS STRING, uValue AS USUAL )
      SELF:ArgName	:= symArgName
      SELF:Value	:= uValue
      RETURN
   /// <inheritdoc />   
   METHOD ToString() AS STRING
      RETURN  "NamedArg{ " + ArgName + ": " + SELF:Value:ToString() + " }"
   
END CLASS
