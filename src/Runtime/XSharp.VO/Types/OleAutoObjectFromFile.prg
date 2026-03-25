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

/// <include file="XSharp.VO.Docs.xml" path="doc/OleAutoObjectFromFile/*" />
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
      CATCH as Exception
         NOP

      END TRY

      RETURN

   PROPERTY cFileName AS STRING GET SELF:_cFileName

END CLASS




