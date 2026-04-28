//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <include file="XSharp.VO.Docs.xml" path="doc/NamedArg/*" />
CLASS XSharp.NamedArg
   /// <include file="XSharp.VO.Docs.xml" path="doc/NamedArg.ArgName/*" />
   PROPERTY ArgName AS STRING AUTO
   /// <include file="XSharp.VO.Docs.xml" path="doc/NamedArg.Value/*" />
   PROPERTY @@Value AS USUAL AUTO
   /// <include file="XSharp.VO.Docs.xml" path="doc/NamedArg.ctor/*" />
   CONSTRUCTOR( symArgName AS STRING, uValue AS USUAL )
      SELF:ArgName	:= symArgName
      SELF:Value	:= uValue
      RETURN
   /// <inheritdoc />   
   OVERRIDE METHOD ToString() AS STRING
      RETURN  "NamedArg{ " + ArgName + ": " + SELF:Value:ToString() + " }"
   
END CLASS
