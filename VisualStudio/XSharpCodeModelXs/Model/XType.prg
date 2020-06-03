//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING LanguageService.CodeAnalysis.Text
USING LanguageService.CodeAnalysis.XSharp
USING System.Diagnostics

BEGIN NAMESPACE XSharpModel
   /// <summary>
      /// Model for Namespace, Class, Interface, Structure, Enum
   /// </summary>
   [DebuggerDisplay("{Kind}, {Name,nq}")];
   CLASS XType INHERIT XElement IMPLEMENTS IXType
      PRIVATE _isPartial      AS LOGIC
      PRIVATE _members        AS List<XTypeMember>
      PRIVATE _children       AS List<XType>
      PRIVATE _signature      AS XTypeSignature
     
      
      CONSTRUCTOR(name AS STRING, kind AS Kind, modifiers AS Modifiers, visibility AS Modifiers, ;
         span AS TextRange, position AS TextInterval, oFile AS XFile)
         SUPER(name, kind, modifiers, visibility, span, position)
         SELF:_members     := List<XTypeMember>{}
         SELF:_children    := List<XType>{}
         SELF:_signature   := XTypeSignature{"System.Object"}
         SELF:NameSpace    := ""
         IF modifiers:HasFlag(Modifiers.Static)
            SELF:IsStatic := TRUE
         ENDIF
         IF modifiers:HasFlag(Modifiers.Partial)
            SELF:_isPartial := TRUE
         ENDIF
         SELF:File := oFile
         
         /// <summary>
            /// Duplicate the current Object, so we have the same properties in another object
            /// </summary>
         /// <returns></returns>
      CONSTRUCTOR( oOther AS XType)
         SELF(oOther:Name, oOther:Kind, oOther:Modifiers, oOther:Visibility, oOther:Range, oOther:Interval, oOther:File)
         SELF:Parent    := oOther:Parent
         SELF:BaseType  := oOther:BaseType
         SELF:IsPartial := oOther:IsPartial
         SELF:IsStatic  := oOther:IsStatic
         SELF:NameSpace := oOther:NameSpace
         SELF:AddMembers(oOther:XMembers)
         RETURN
         
         
         
      METHOD AddChild(oChild AS XType) AS VOID
         BEGIN LOCK SELF:_children
            SELF:_children:Add(oChild)
            oChild:Parent := SELF
         END LOCK
         
         
      METHOD AddMember(oMember AS XTypeMember) AS VOID
         BEGIN LOCK SELF:_members
            SELF:_members:Add(oMember)
            oMember:Parent := SELF
         END LOCK
         
      METHOD AddMembers(members AS IEnumerable<XTypeMember>) AS VOID
         BEGIN LOCK SELF:_members
            SELF:_members:AddRange(members)
            FOREACH VAR oMember IN members
               oMember:Parent := SELF
            NEXT
         END LOCK
       
         
      METHOD AddInterface(sInterface AS STRING) AS VOID
         SELF:_signature:AddInterface(sInterface)
         
      METHOD AddTypeParameter(name AS STRING) AS VOID
         SELF:_signature:TypeParameters:Add(name)
         
      METHOD AddConstraints(name AS STRING) AS VOID
         SELF:_signature:TypeParameterContraints:Add(name)
         
         
      PROPERTY Interfaces  AS IList<STRING> GET _signature:Interfaces:ToArray()
      PROPERTY TypeParameters as IList<STRING> GET SELF:_signature:TypeParameters:ToArray()
      PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()

      
      PROPERTY Members AS IList<IXTypeMember>  
         GET
            BEGIN LOCK SELF:_members
               RETURN SELF:_members:ToArray()
            END LOCK
         END GET
      END PROPERTY
      
      PROPERTY XMembers AS IList<XTypeMember>
         GET
            BEGIN LOCK SELF:_members
               RETURN SELF:_members:ToArray()
            END LOCK
         END GET
      END PROPERTY
      
      METHOD GetMember(elementName AS STRING) AS IList<XTypeMember>
         VAR tempMembers := List<XTypeMember>{}
         FOREACH x AS XTypeMember IN SELF:Members
            IF nameEquals(x:Name, elementName)
               tempMembers:Add(x)
            ENDIF
         NEXT
         RETURN tempMembers;
         
      PRIVATE METHOD nameEquals(name AS STRING, compareWith AS STRING) AS LOGIC
         RETURN String.Compare(name, compareWith, StringComparison.OrdinalIgnoreCase) == 0
      
      PROPERTY FullName  AS STRING   GET SELF:GetFullName()
      PROPERTY IsGeneric as LOGIC   GET SELF:TypeName:EndsWith(">")
      
      /// <summary>
         /// Merge two XType Objects : Used to create the resulting  XType from 2 or more partial classes
         /// </summary>
      /// <param name="otherType"></param>
      METHOD Merge(otherType AS XType) AS XType
         LOCAL clone AS XType
         clone := XType{SELF}
         VAR otherFile := otherType:File:FullPath:ToLower()
         VAR thisFile  := SELF:File:FullPath:ToLower()
         IF otherFile != thisFile  
            SELF:IsPartial := TRUE
            IF otherType != NULL
               clone:AddMembers(otherType:XMembers)
               IF clone:Parent == NULL .AND. otherType:Parent != NULL
                  clone:Parent := otherType:Parent
               ELSE
                  IF clone:BaseType == NULL .AND. otherType:BaseType != NULL
                     clone:BaseType := otherType:BaseType
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         IF String.IsNullOrEmpty(clone:BaseType)
            clone:BaseType := "System.Object"
         ENDIF
         
         RETURN clone
         
         
         
         /// <summary>
            /// If this XType is a Partial type, return a Copy of it, merged with all other informations
            /// coming from other files.
         /// </summary>
         
      PROPERTY Clone AS XType
         GET
            IF SELF:IsPartial .AND. SELF:File != NULL
               RETURN SUPER:File:Project:Lookup(SELF:FullName, TRUE)
            ENDIF
            RETURN SELF
         END GET
      END PROPERTY
      
      PROPERTY Description AS STRING GET SELF:GetDescription()
      
      PROPERTY IsPartial AS LOGIC GET SELF:_isPartial SET SELF:_isPartial := VALUE
      PROPERTY IsNested  AS LOGIC GET SELF:Parent IS XType
      
      PROPERTY BaseType AS STRING GET SELF:_signature:BaseType SET SELF:_signature:BaseType := @@value

      
      
      STATIC METHOD CreateGlobalType(xfile AS XFile) AS XType
         VAR globalType := XType{GlobalName, Kind.Class, Modifiers.None, Modifiers.Public, TextRange{0, 0, 0, 0}, TextInterval{}, xfile}
         globalType:IsPartial:=TRUE
         globalType:IsStatic:=TRUE
         RETURN globalType
      
      STATIC METHOD IsGlobalType(type AS IXElement) AS LOGIC
         RETURN type != NULL .AND. type is IXType .and. type:Name == XType.GlobalName
      
      PROPERTY Children   AS IList<IXType> 
         GET 
            BEGIN LOCK SELF:_children
               return SELF:_children:ToArray()
            END LOCK
         END GET
      END PROPERTY
      
      PROPERTY XChildren   AS IList<XType>  
         GET 
            BEGIN LOCK SELF:_children
               return SELF:_children:ToArray()
            END LOCK
         END GET
      END PROPERTY
      
   END CLASS
   
END NAMESPACE

