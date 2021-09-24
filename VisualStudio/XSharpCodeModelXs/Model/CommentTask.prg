//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics
#pragma options("vo3", off)
BEGIN NAMESPACE XSharpModel
   [DebuggerDisplay("{Line}:{Column} {Comment,nq}")];
   SEALED CLASS XCommentTask
      PROPERTY File     AS XFile AUTO
      PROPERTY Line     AS LONG AUTO
      PROPERTY Column   AS LONG AUTO
      PROPERTY Priority AS LONG AUTO
      PROPERTY Comment  AS STRING AUTO
         
   END CLASS
      
   [DebuggerDisplay("{Text,nq}:{Priority}")];
   SEALED CLASS XCommentToken
      PROPERTY Text     AS STRING AUTO GET PRIVATE SET
      PROPERTY Priority AS LONG   AUTO GET PRIVATE SET
         
      CONSTRUCTOR (cText AS STRING, nPriority AS LONG)
         SELF:Text     := cText
         SELF:Priority := nPriority
   END CLASS
      
END NAMESPACE   
