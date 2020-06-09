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

   [DebuggerDisplay("{Kind}, {Name,nq}")];
   CLASS XMemberDefinition INHERIT XEntityDefinition IMPLEMENTS IXMember
      // Fields
      PRIVATE _signature    AS XMemberSignature 
      PROPERTY InitExit     AS STRING AUTO
      PROPERTY SubType      AS Kind AUTO
      PROPERTY DeclaringType  AS STRING AUTO
      
      #region constructors
      
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, ;
            span AS TextRange, position AS TextInterval, returnType AS STRING, isStatic := FALSE AS LOGIC)
         SUPER(name, kind, attributes, span, position)
         SELF:Parent       := NULL
         SELF:TypeName     := returnType
         SELF:IsStatic     := isStatic
         SELF:_signature   := XMemberSignature{}
         
      CONSTRUCTOR(sig as XMemberSignature, kind AS Kind, attributes AS Modifiers,  ;
            span AS TextRange, position AS TextInterval, isStatic := FALSE AS LOGIC)
         SUPER(sig:Id, kind, attributes, span, position)
         SELF:Parent       := NULL
         SELF:TypeName     := sig:DataType
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
      
      PROPERTY ComboParameterList AS STRING	GET _signature:ComboParameterList
      PROPERTY Parameters         AS IList<IXVariable> GET _signature:Parameters:ToArray()
      
      PROPERTY Signature         AS XMemberSignature  GET _signature SET _signature := @@value
      PROPERTY CallingConvention AS CallingConvention GET _signature:CallingConvention SET _signature:CallingConvention := @@value
      
      
      PROPERTY Prototype      AS STRING GET SELF:GetProtoType()
      
      PROPERTY ComboPrototype AS STRING GET SELF:GetComboProtoType()
         
      PROPERTY ParentType     AS IXType   GET SELF:Parent ASTYPE IXType
      PROPERTY IsExtension    AS LOGIC    GET _signature:IsExtension
      PROPERTY XMLSignature   AS STRING GET SELF:GetXmlSignature()
      PROPERTY OriginalTypeName  AS STRING GET SELF:TypeName
         
      #endregion
   END CLASS
   
END NAMESPACE

