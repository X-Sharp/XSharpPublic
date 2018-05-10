//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices
using System.Runtime.CompilerServices
using System.Runtime.Remoting
using System.Reflection
using System.Diagnostics
using System.Runtime.InteropServices.ComTypes

/// <summary>VO Compatible class to create an OLEAutObject from a file on disk.</summary>	
[DebuggerDisplay( "Type= {__ComObject}, File={cFileName}", Type := "OleAutoObjectFromFile" )];
CLASS XSharp.OleAutoObjectFromFile INHERIT OleAutoObject
   PROTECT _cFileName 	AS STRING
   
   CONSTRUCTOR(cFile as STRING)
      local oDoc  as Object
      local type  as System.Type
      local oObj  as MarshalByRefObject
      
      _cFileName := cFile
      try
         oDoc    := Marshal.BindToMoniker(cFile)
         oObj    := (MarshalByRefObject) oDoc
         type    := oObj:GetType()
         
         super(oDoc, type)
      end try
   
      RETURN

   PROPERTY cFileName AS STRING GET SELF:_cFileName
      
END CLASS




