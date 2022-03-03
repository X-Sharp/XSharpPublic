//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING System.Linq

BEGIN NAMESPACE XSharpModel
   [DebuggerDisplay("{ToString(),nq}")];
   CLASS XTypeSignature INHERIT XBaseSignature
      PROPERTY Interfaces               AS List<STRING>  AUTO
      PROPERTY BaseType                 AS STRING        AUTO
      PROPERTY InterfaceList            AS STRING        GET ToList(SELF:Interfaces)

      CONSTRUCTOR(cBaseType AS STRING)
         SUPER()
         SELF:Interfaces                 := List<STRING>{}
         SELF:BaseType                   := cBaseType
      METHOD ClearInterfaces() AS VOID
          SELF:Interfaces                 := List<STRING>{}
      METHOD AddInterface(sInterface AS STRING) AS VOID
         SELF:Interfaces:Add(sInterface)
         RETURN

      METHOD ToString() AS STRING
         var result := Super:ToString()
         if !String.IsNullOrEmpty(SELF:BaseType)
            result += i" INHERIT {BaseType} "
         ENDIF
         IF SELF:Interfaces:Count > 0
            result += "IMPLEMENTS "+SELF:InterfaceList
         endif
         return result

   END CLASS
END NAMESPACE
