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

BEGIN NAMESPACE XSharpModel
   
   [DebuggerDisplay("{Kind}, {Name,nq}")];
   CLASS XElement IMPLEMENTS IXElement
      #region Simple Properties
      PROPERTY File AS XFile                       AUTO 
      PROPERTY FullName AS STRING                  GET SELF:Name
      PROPERTY Kind AS Kind                        AUTO GET PROTECTED SET
      PROPERTY Name AS STRING                      AUTO GET PRIVATE SET
      PROPERTY IsStatic AS LOGIC                   AUTO GET PROTECTED SET
      PROPERTY FileUsings AS IList<STRING>         GET IIF(SELF:File != NULL, SELF:File:Usings, <STRING>{})
      PROPERTY NameSpace  AS STRING                AUTO
      
      PROPERTY Parent AS IXElement                 AUTO
      PROPERTY ParentName AS STRING                GET SELF:Parent?:FullName
      PROPERTY Prototype AS STRING                 GET SELF:Name
      PROPERTY ComboPrototype AS STRING            GET SELF:Name
      PROPERTY Range AS TextRange                  AUTO
      PROPERTY Interval AS TextInterval            AUTO
      PROPERTY Dialect AS XSharpDialect            AUTO      
      PROPERTY IsArray AS LOGIC                    AUTO
      PROPERTY Attributes AS STRING                AUTO
      PROPERTY SingleLine        AS LOGIC          AUTO
      PROPERTY Value             AS STRING         AUTO 
      PROPERTY Modifiers         AS Modifiers      AUTO GET INTERNAL SET
      
      PRIVATE _typeName        AS STRING
      
      #endregion
      PROPERTY Visibility  AS Modifiers AUTO GET INTERNAL SET
      CONST PUBLIC GlobalName := "(Global Scope)" AS STRING
      STATIC INITONLY PUBLIC VarType := "$VAR$" AS STRING
      STATIC INITONLY PUBLIC UsualType := "USUAL" AS STRING
      STATIC INITONLY PUBLIC NoType := "$NOTYPE$" AS STRING
      
      // Methods
      CONSTRUCTOR()
         RETURN
         
         
      CONSTRUCTOR(name AS STRING, kind AS Kind)
         SUPER()
         // For keywords
         SELF:Name       := name
         SELF:Kind       := kind
         SELF:Visibility := Modifiers.Public
         
         
      CONSTRUCTOR(name AS STRING, kind AS Kind, modifiers AS Modifiers, visibility AS Modifiers, range AS TextRange, interval AS TextInterval)
         SUPER()
         SELF:Name := name
         SELF:Kind := kind
         SELF:Modifiers := modifiers
         SELF:Visibility := visibility
         SELF:Range := range
         SELF:Interval := interval
         SELF:IsStatic := FALSE
         SELF:IsArray := FALSE
         SELF:Dialect := XSharpDialect.Core
         
         
      METHOD ForceComplete() AS VOID
         LOCAL parentName AS STRING
         LOCAL thisName AS STRING
         LOCAL tmp AS XType
         
         IF SELF:Parent == NULL .AND. ! String.IsNullOrEmpty(SELF:ParentName)
            
            parentName := SELF:ParentName
            thisName := SELF:FullName
            IF parentName:IndexOf(".") == -1 .AND. thisName:IndexOf(".") > 0
               
               parentName := thisName:Substring(0, (thisName:LastIndexOf(".") + 1)) + parentName
            ENDIF
            IF SELF:File != NULL .AND. parentName != "System.Object"
               
               tmp := SELF:File:Project:Lookup(parentName, TRUE)
               IF tmp != NULL
                  
                  SELF:Parent := tmp
                  // Ensure whole tree is resolved.
                  SELF:Parent:ForceComplete()
               ENDIF
            ENDIF
         ENDIF
         
         
      METHOD OpenEditor() AS VOID
         IF SELF:File?:Project?:ProjectNode != NULL
            SELF:File:Project:ProjectNode:OpenElement(SELF:File:SourcePath, SELF:Range:StartLine+1, (SELF:Range:StartColumn ))
         ENDIF
         #region Complexer properties		
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
               RETURN SELF:ModVis + " "+KindKeyword +  " "+SELF:Prototype
            END GET
         END PROPERTY
         
         
         PROPERTY Glyph                   AS LONG     GET Self:Kind:GetGlyph(self:Visibility)
         PROPERTY ModifiersKeyword			AS STRING   GET SELF:Modifiers:ToDisplayString()
         PROPERTY VisibilityKeyword			AS STRING   GET SELF:Visibility:ToDisplayString()
         PROPERTY KindKeyword				   AS STRING   GET SELF:Kind:ToDisplayString()
         
         PROPERTY IsTyped                  AS LOGIC GET !String.IsNullOrEmpty(_typeName)
         PROPERTY TypeName AS STRING
            GET
               IF IsTyped
                  RETURN SELF:_typeName
               ELSE
                  RETURN UsualType
               ENDIF
            END GET
            SET
               SELF:_typeName := VALUE
            END SET
         END PROPERTY
         
         #endregion
   END CLASS
   
END NAMESPACE 


