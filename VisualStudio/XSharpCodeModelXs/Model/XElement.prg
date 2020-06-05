//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
USING System.Collections.Generic
USING LanguageService.SyntaxTree

BEGIN NAMESPACE XSharpModel
     // an entity in the source code
    [DebuggerDisplay("{Kind}, {Name,nq}")];
    CLASS XElement 
      PROPERTY Kind        AS Kind         AUTO GET INTERNAL SET
      PROPERTY Name        AS STRING       AUTO GET INTERNAL SET
      PROPERTY Attributes  AS Modifiers    AUTO GET INTERNAL SET
      PROPERTY Modifiers   AS Modifiers    GET _AND(Attributes, ~Modifiers.VisibilityMask)
      PROPERTY Visibility  AS Modifiers    GET _AND(Attributes, Modifiers.VisibilityMask)
      PROPERTY ModVis      AS STRING       
      GET 
            IF SELF:Attributes == Modifiers.None
               RETURN ""      // prevent returning "none"
            ENDIF
            return SELF:Attributes:ToDisplayString()+" "
      END GET
      
      END PROPERTY
      PROPERTY Glyph                   AS LONG     GET Self:Kind:GetGlyph(self:Visibility)
      PROPERTY ModifiersKeyword			AS STRING   GET SELF:Modifiers:ToDisplayString()
      PROPERTY VisibilityKeyword			AS STRING   GET SELF:Visibility:ToDisplayString()
      PROPERTY KindKeyword				   AS STRING   GET SELF:Kind:ToDisplayString()


      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes as Modifiers)
         SELF:Name       := name
         SELF:Kind       := kind
         SELF:Attributes := attributes
         if SELF:Visibility == Modifiers.None
            attributes |= Modifiers.Public
         endif
         



    END CLASS
END NAMESPACE      