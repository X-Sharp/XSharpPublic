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
   CLASS XEntityReference INHERIT XElement IMPLEMENTS IXEntity
      #region Simple Properties
      PROPERTY Assembly AS XAssembly               AUTO
      PROPERTY FullName AS STRING                  GET SELF:Name
      PROPERTY Namespace  AS STRING                AUTO
      
      PROPERTY Prototype AS STRING                 GET SELF:Name
      PROPERTY IsArray  AS LOGIC                   AUTO GET PROTECTED SET 
      PROPERTY IsStatic  AS LOGIC                  GET FALSE
      PROPERTY CustomAttributes AS STRING           AUTO
      PROPERTY SingleLine        AS LOGIC          AUTO
      PROPERTY Value             AS STRING         AUTO 
      PROPERTY OriginalTypeName  AS STRING         AUTO
      
      #endregion

      PRIVATE STATIC nullUsings   as IList<String>

      
      // Methods
      STATIC CONSTRUCTOR()
         nullUsings := <String>{}
         RETURN
   
  
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, asm AS XAssembly)
         SUPER(name, kind, attributes)
         SELF:Assembly     := asm
         
      METHOD ForceComplete() AS VOID
         RETURN
         
         // Properties
         
      PROPERTY Description AS STRING
         GET
            RETURN SELF:ModVis + KindKeyword +  SELF:Prototype
         END GET
      END PROPERTY
      
      PROPERTY IsTyped                 AS LOGIC    GET TRUE
      PROPERTY TypeName                AS STRING   AUTO
      PROPERTY FileUsings              AS IList<String> GET nullUsings
      
   END CLASS
   
END NAMESPACE 


