//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel

   [DebuggerDisplay("{ToString(),nq}")];
   CLASS XMemberDefinition INHERIT XEntityDefinition IMPLEMENTS IXMember
      // Fields
      PRIVATE _signature    AS XMemberSignature 
      PROPERTY InitExit     AS STRING AUTO
      PROPERTY SubType      AS Kind AUTO
      PROPERTY DeclaringType  AS STRING AUTO
      PROPERTY ReturnType   AS STRING GET TypeName SET TypeName := value
      PROPERTY SourceCode   AS STRING AUTO      
      #region constructors
      
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, ;
            span AS TextRange, position AS TextInterval, returnType AS STRING, isStatic := FALSE AS LOGIC)
         SUPER(name, kind, attributes, span, position)
         SELF:Parent       := NULL
         SELF:ReturnType   := returnType
         SELF:IsStatic     := isStatic
         SELF:_signature   := XMemberSignature{}
         
      CONSTRUCTOR(sig as XMemberSignature, kind AS Kind, attributes AS Modifiers,  ;
            span AS TextRange, position AS TextInterval, isStatic := FALSE AS LOGIC)
         SUPER(sig:Id, kind, attributes, span, position)
         SELF:Parent       := NULL
         SELF:ReturnType   := sig:DataType
         SELF:IsStatic     := isStatic
         SELF:_signature   := sig
         FOREACH var par in sig:Parameters
            par:Parent := SELF
         NEXT         
         #endregion
      
      
      METHOD AddParameters( list AS IList<IXVariable>) AS VOID
         IF list != NULL
            FOREACH VAR par IN list
               SELF:AddParameter(par)
            NEXT
         ENDIF
         RETURN
         
      METHOD AddParameter(oVar AS IXVariable) AS VOID
         oVar:Parent := SELF
         IF oVar IS XVariable VAR xVar
            xVar:File   := SELF:File
         ENDIF
         _signature:Parameters:Add(oVar)
         oVar:Parent := SELF
         RETURN
         
         
      #region Properties. Some are implemented as Extension methods, others forwarded to the signature
      PROPERTY Description AS STRING GET SELF:GetDescription()
 		PROPERTY FullName AS STRING GET SELF:GetFullName()
      
      PROPERTY HasParameters     AS LOGIC GET _signature:HasParameters
      PROPERTY ParameterCount    AS INT   GET _signature:ParameterCount
      
      PROPERTY ParameterList      AS STRING GET _signature:ParameterList
      
     PROPERTY ComboParameterList AS STRING
         GET
            VAR parameters := ""
            FOREACH variable AS IXVariable IN SELF:Parameters
               IF (parameters:Length > 0)
                  parameters := parameters + ", "
               ENDIF
               VAR cType := variable:ShortTypeName
               IF variable:IsTyped .AND. variable:ParamType != ParamType.As
                  parameters += variable:ParamTypeDesc + cType
               ELSE
                  parameters += cType
               ENDIF
            NEXT
            RETURN parameters
         END GET
      END PROPERTY
      PROPERTY Parameters         AS IList<IXVariable> GET _signature:Parameters:ToArray()
      
      PROPERTY Signature         AS XMemberSignature  GET _signature SET _signature := @@value
      PROPERTY CallingConvention AS CallingConvention GET _signature:CallingConvention SET _signature:CallingConvention := @@value
      
      
      PROPERTY Prototype      AS STRING GET SELF:GetProtoType()
      
      PROPERTY ComboPrototype AS STRING 
         GET
            VAR vars := ""
            VAR desc := ""
            IF SELF:Kind:HasParameters()
               IF ( SELF:Kind == Kind.@@Constructor )
                  vars := "{" + SELF:ComboParameterList + "}"
               ELSE
                  vars := "(" + SELF:ComboParameterList + ")"
               ENDIF 
            ENDIF
            IF ( SELF:Kind == Kind.@@Constructor )
               desc := SELF:Parent:Name + vars
            ELSE
               desc := SELF:Name + vars
            ENDIF
            IF SELF:Kind:HasReturnType()
               desc := desc +  XLiterals.AsKeyWord + SELF:TypeName
            ENDIF
            RETURN desc
         END GET
      END PROPERTY
         
      PROPERTY ParentType     AS IXType   GET SELF:Parent ASTYPE IXType
      PROPERTY IsExtension    AS LOGIC    GET _signature:IsExtension
      PROPERTY XMLSignature   AS STRING GET SELF:GetXmlSignature()
      PROPERTY OriginalTypeName  AS STRING               GET SELF:TypeName
      PROPERTY TypeParameters as IList<STRING>           GET SELF:_signature:TypeParameters:ToArray()
      PROPERTY TypeParametersList AS STRING              GET SELF:_signature:TypeParametersList
      PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()
      PROPERTY TypeParameterConstraintsList AS STRING    GET SELF:_signature:TypeParameterConstraintsList
      PROPERTY Location       AS STRING GET SELF:File:FullPath
      PROPERTY Glyph                   AS LONG     
         GET 
            VAR glyph := SUPER:Glyph
            IF SELF:Name:EndsWith(XLiterals.XppDeclaration)
               glyph := glyph - (glyph % 6) + ImageListOverlay.ImageListOverlayArrow
            ENDIF
            RETURN glyph
         END GET
      END PROPERTY
         
      METHOD ToString() AS STRING
         VAR result := i"{Kind} {Name}"
         if SELF:_signature != NULL .and. SELF:_signature:TypeParameters:Count > 0
            result += self:_signature:ToString()
         ENDIF
         RETURN result

      #endregion
   END CLASS
   
END NAMESPACE

