//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Diagnostics
USING System
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING Mono.Cecil
BEGIN NAMESPACE XSharpModel
   
   [DebuggerDisplay("{Kind}, {Name,nq}")];
   CLASS XRefElement IMPLEMENTS IXElement
      #region Simple Properties
      PROPERTY Assembly AS XAssembly               AUTO
      PROPERTY FullName AS STRING                  GET SELF:Name
      PROPERTY Kind AS Kind                        AUTO GET PROTECTED SET
      PROPERTY Name AS STRING                      AUTO GET PRIVATE SET
      PROPERTY IsStatic AS LOGIC                   AUTO GET PROTECTED SET
      PROPERTY NameSpace  AS STRING                AUTO
      
      PROPERTY Parent AS IXElement                 AUTO
      PROPERTY ParentName AS STRING                GET SELF:Parent?:FullName
      PROPERTY Prototype AS STRING                 GET SELF:Name
      PROPERTY ComboPrototype AS STRING            GET SELF:Name
      PROPERTY IsArray AS LOGIC                    AUTO
      PROPERTY Attributes AS STRING                AUTO
      PROPERTY SingleLine        AS LOGIC          AUTO
      PROPERTY Value             AS STRING         AUTO 
      PROPERTY Modifiers         AS Modifiers      AUTO GET INTERNAL SET
      PROPERTY Visibility        AS Modifiers      AUTO GET INTERNAL SET
      
      #endregion

      PRIVATE STATIC nullUsings   as IList<String>

      
      // Methods
      STATIC CONSTRUCTOR()
         nullUsings := <String>{}

         RETURN
         
         
      STATIC METHOD GetVisibility (attributes AS TypeAttributes) as Modifiers
         attributes := _AND(attributes, TypeAttributes.VisibilityMask)
         SWITCH attributes
         CASE TypeAttributes.Public
            return Modifiers.Public
         CASE TypeAttributes.NotPublic
            return Modifiers.Private
         CASE TypeAttributes.NestedFamily
            return Modifiers.Protected
         CASE TypeAttributes.NestedAssembly
            return Modifiers.Internal
         CASE TypeAttributes.NestedFamANDAssem
         CASE TypeAttributes.NestedFamORAssem
            return Modifiers.ProtectedInternal
         END SWITCH
         RETURN Modifiers.Public
        
     STATIC METHOD GetVisibility (attributes AS MethodAttributes) as Modifiers
         attributes := _AND(attributes, MethodAttributes.MemberAccessMask)
         SWITCH attributes
         CASE MethodAttributes.Private    // 1
            return Modifiers.Private
         CASE MethodAttributes.Assembly  // 3
            return Modifiers.Internal
         CASE MethodAttributes.Family     // 4
            return Modifiers.Protected
         CASE MethodAttributes.FamORAssem // 5
         CASE MethodAttributes.FamANDAssem // 5
            return Modifiers.ProtectedInternal
         CASE MethodAttributes.Public     // 6
            return Modifiers.Public
         END SWITCH
         RETURN Modifiers.Public

   STATIC METHOD GetVisibility (attributes AS FieldAttributes) as Modifiers
         attributes := _AND(attributes, FieldAttributes.FieldAccessMask)
         SWITCH attributes
         CASE FieldAttributes.Private    // 1
            return Modifiers.Private
         CASE FieldAttributes.Assembly  // 3
            return Modifiers.Internal
         CASE FieldAttributes.Family     // 4
            return Modifiers.Protected
         CASE FieldAttributes.FamORAssem // 5
         CASE FieldAttributes.FamANDAssem // 5
            return Modifiers.ProtectedInternal
         CASE FieldAttributes.Public     // 6
            return Modifiers.Public
         END SWITCH
         RETURN Modifiers.Public

      STATIC METHOD GetModifiers (attributes AS TypeAttributes) as Modifiers
         var modifiers := Modifiers.None
         IF attributes:HasFlag(TypeAttributes.Abstract)
            modifiers |= Modifiers.Abstract
         ENDIF
         IF attributes:HasFlag(TypeAttributes.Sealed)
            modifiers |= Modifiers.Sealed
         ENDIF
         IF attributes:HasFlag(TypeAttributes.Import)
            modifiers |= Modifiers.External
         ENDIF
         return modifiers
         
     STATIC METHOD GetModifiers (attributes AS MethodAttributes) as Modifiers
         var modifiers := Modifiers.None
         IF attributes:HasFlag(MethodAttributes.Abstract)
            modifiers |= Modifiers.Abstract
         ENDIF
         IF attributes:HasFlag(MethodAttributes.Final)
            modifiers |= Modifiers.Sealed
         ENDIF
         IF attributes:HasFlag(MethodAttributes.PInvokeImpl)
            modifiers |= Modifiers.External
         ENDIF
         IF attributes:HasFlag(MethodAttributes.UnmanagedExport)
            modifiers |= Modifiers.External
         ENDIF
         IF attributes:HasFlag(MethodAttributes.Virtual)
            modifiers |= Modifiers.Virtual
         ENDIF
         IF attributes:HasFlag(MethodAttributes.NewSlot)
            modifiers |= Modifiers.New
         ENDIF
         IF attributes:HasFlag(MethodAttributes.Static)
            modifiers |= Modifiers.Static
         ENDIF         
         return modifiers
         
     STATIC METHOD GetModifiers (attributes AS FieldAttributes) as Modifiers
         var modifiers := Modifiers.None
         IF attributes:HasFlag(FieldAttributes.InitOnly)
            modifiers |= Modifiers.InitOnly
         ENDIF
         IF attributes:HasFlag(FieldAttributes.Literal)
            modifiers |= Modifiers.Const
         ENDIF
         IF attributes:HasFlag(FieldAttributes.Static)
            modifiers |= Modifiers.Static
         ENDIF         
         return modifiers

      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS MethodAttributes, asm as XAssembly)
         SUPER()
         SELF:Name         := name
         SELF:Kind         := kind
         SELF:Visibility   := GetVisibility(attributes)
         SELF:Modifiers    := GetModifiers(attributes)
         SELF:IsStatic     := FALSE
         SELF:IsArray      := FALSE
         SELF:Assembly     := asm

         
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS TypeAttributes, asm as XAssembly)
         SUPER()
         SELF:Name := name
         SELF:Kind := kind
         SELF:Visibility   := GetVisibility(attributes)
         SELF:Modifiers    := GetModifiers(attributes)
         SELF:IsStatic     := FALSE
         SELF:IsArray      := FALSE
         SELF:Assembly     := asm

      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS FieldAttributes, asm as XAssembly)
         SUPER()
         SELF:Name := name
         SELF:Kind := kind
         SELF:Visibility   := GetVisibility(attributes)
         SELF:Modifiers    := GetModifiers(attributes)
         SELF:IsStatic     := FALSE
         SELF:IsArray      := FALSE
         SELF:Assembly     := asm

      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS PropertyAttributes, asm as XAssembly)
         SUPER()
         SELF:Name := name
         SELF:Kind := kind
         SELF:Visibility   := Modifiers.None //GetVisibility(attributes)
         SELF:Modifiers    := Modifiers.None//GetModifiers(attributes)
         SELF:IsStatic     := FALSE
         SELF:IsArray      := FALSE
         SELF:Assembly     := asm


         
      METHOD ForceComplete() AS VOID
         RETURN
         
         
         // Properties
         
      PROPERTY ModVis AS STRING 
         GET
            VAR strModVis := ""
            IF SELF:Modifiers != Modifiers.None
               strModVis := SELF:ModifiersKeyword
            ENDIF
            strModVis += VisibilityKeyword
            return strModVis
         END GET
      END PROPERTY
      PROPERTY Description AS STRING
         GET
            RETURN SELF:ModVis + KindKeyword +  SELF:Prototype
         END GET
      END PROPERTY
      
      
      
      PROPERTY Glyph                   AS LONG     GET Self:Kind:GetGlyph(self:Visibility)
      PROPERTY ModifiersKeyword			AS STRING   GET SELF:Modifiers:ToDisplayString()
      PROPERTY VisibilityKeyword			AS STRING   GET SELF:Visibility:ToDisplayString()
      PROPERTY KindKeyword				   AS STRING   GET SELF:Kind:ToDisplayString()
      PROPERTY IsTyped                 AS LOGIC    GET TRUE
      PROPERTY TypeName                AS STRING   AUTO
      PROPERTY FileUsings              AS IList<String> GET nullUsings
      
   END CLASS
   
END NAMESPACE 


