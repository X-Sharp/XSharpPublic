//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING System.Runtime.Remoting
USING System.Reflection
USING System.Diagnostics
USING System.Runtime.InteropServices.ComTypes

/// <summary>VO Compatible class to create an OLEAutObject from a file on disk.</summary>	
[DebuggerDisplay( "Type= {__ComObject}, File={cFileName}", Type := "OleAutoObjectFromFile" )];
CLASS XSharp.OleAutoObjectFromFile INHERIT OleAutoObject
   PROTECT _cFileName 	AS STRING
   
   CONSTRUCTOR(cFile AS STRING)
      LOCAL oDoc  AS OBJECT
      LOCAL type  AS System.Type
      LOCAL oObj  AS MarshalByRefObject
      
      _cFileName := cFile
      TRY
         oDoc    := Marshal.BindToMoniker(cFile)
         oObj    := (MarshalByRefObject) oDoc
         type    := oObj:GetType()
         
         SUPER(oDoc, type)
      END TRY
   
      RETURN

   PROPERTY cFileName AS STRING GET SELF:_cFileName
      
END CLASS




