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
    ABSTRACT CLASS XSymbol IMPLEMENTS IXSymbol
      PROPERTY Kind        AS Kind         AUTO
      PROPERTY Name        AS STRING       AUTO
      PROPERTY Attributes  AS Modifiers    AUTO
      PROPERTY Modifiers   AS Modifiers    GET _AND(Attributes, ~Modifiers.VisibilityMask)
      PROPERTY Visibility  AS Modifiers    GET _AND(Attributes, Modifiers.VisibilityMask)
      PROPERTY ModVis      AS STRING
      GET
            IF SELF:Attributes == Modifiers.None
               RETURN ""      // prevent returning "none"
            ENDIF
            RETURN SELF:Attributes:ToDisplayString():Replace(",","")+" "
      END GET

      END PROPERTY
      PROPERTY Glyph                   AS LONG     GET Self:Kind:GetGlyph(self:Visibility)
      PROPERTY ModifiersKeyword		   AS STRING   GET SELF:Modifiers:ToDisplayString()
      PROPERTY VisibilityKeyword	   AS STRING   GET SELF:Visibility:ToDisplayString()
      PROPERTY KindKeyword			   AS STRING   GET SELF:Kind:ToDisplayString()
      PROPERTY TypeName                AS STRING   AUTO
      PROPERTY Parent                  AS IXSymbol AUTO
      PROPERTY Description             AS STRING AUTO
      ABSTRACT PROPERTY Prototype      AS STRING GET
      ABSTRACT PROPERTY Location       AS STRING GET
      PROPERTY IsStatic                AS LOGIC AUTO
      PROPERTY IsArray                 AS LOGIC AUTO
      PROPERTY IsPublic                AS LOGIC GET SELF:Visibility >= Modifiers.Public
      PROPERTY IsExternalVisible       AS LOGIC GET SELF:Visibility >= Modifiers.Protected .or. SELF:Visibility == Modifiers.ProtectedInternal
      PROPERTY Namespace               AS STRING AUTO
      PROPERTY FullName                AS STRING GET Name
      PROPERTY ResolvedType            AS IXTypeSymbol AUTO
      PROPERTY ElementType AS STRING
            GET
                IF SELF:TypeName:EndsWith("[]")
                    RETURN SELF:TypeName:Substring(0, SELF:TypeName:Length -2)
                ENDIF
                 var index := SELF:TypeName:IndexOf("<")
                IF index > 0
                    var result := SELF:TypeName:Substring(index+1)
                    result := result:Substring(0, result.Length-1)
                    return result
                ENDIF
                RETURN SELF:TypeName
            END GET
      END PROPERTY
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes as Modifiers)
         SELF:Name       := name
         SELF:Kind       := kind
         SELF:Attributes := attributes
         if SELF:Visibility == Modifiers.None
            attributes |= Modifiers.Public
         endif
      METHOD ForceComplete() AS VOID
            RETURN
    END CLASS
END NAMESPACE


