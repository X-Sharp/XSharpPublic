//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil

BEGIN NAMESPACE XSharpModel
   /// <summary>
      /// Model for Namespace, Class, Interface, Structure, Enum
   /// </summary>
   [DebuggerDisplay("{Kind}, {Name,nq}")];
   CLASS XTypeRef INHERIT XRefElement IMPLEMENTS IXType
      PRIVATE _members        AS List<XTypeRefMember>
      PRIVATE _children       AS List<XTypeRef>
      PRIVATE _signature      AS XTypeSignature
      PRIVATE _typeDef        as TypeDefinition
      PRIVATE _hasMembers:= FALSE  AS LOGIC
      
      
      PRIVATE STATIC METHOD GetKind(typedef as TypeDefinition) AS Kind
         DO CASE
         CASE typedef:IsClass
            RETURN Kind.Class
         CASE typedef:IsEnum
            RETURN Kind.Enum
         CASE typedef:IsInterface
            RETURN Kind.Interface
         case typedef:BaseType:FullName == "System.MulticastDelegate"
            RETURN Kind.Delegate
         ENDCASE
         RETURN Kind.Structure
      
      
      CONSTRUCTOR(typedef as TypeDefinition, asm as XAssembly)
         SUPER(typedef:Name, GetKind(typedef), typedef:Attributes, asm)  
         SELF:_typeDef     := typedef
         SELF:_members     := List<XTypeRefMember>{}
         SELF:_children    := List<XTypeRef>{}
         SELF:_signature   := XTypeSignature{"System.Object"}
         SELF:NameSpace    := typedef:Namespace
         IF typedef:BaseType != NULL
            SELF:BaseType     := typedef:BaseType:FullName
         ELSE
            SELF:BaseType     := ""
         ENDIF
         
         
       METHOD AddChild(oChild AS XTypeRef) AS VOID
         BEGIN LOCK SELF:_children
            SELF:_children:Add(oChild)
            oChild:Parent := SELF
         END LOCK
         
         
      METHOD AddMember(oMember AS XTypeRefMember) AS VOID
         BEGIN LOCK SELF:_members
            SELF:_members:Add(oMember)
            oMember:Parent := SELF
         END LOCK
         
      METHOD AddMembers(members AS IEnumerable<XTypeRefMember>) AS VOID
         BEGIN LOCK SELF:_members
            SELF:_members:AddRange(members)
            FOREACH VAR oMember IN members
               oMember:Parent := SELF
            NEXT
         END LOCK
         
         
      PRIVATE METHOD _LoadMembers() AS VOID
         IF ! SELF:_hasMembers
               VAR aMembers := List<XTypeRefMember>{}
               FOREACH var md in _typeDef:Methods
                  aMembers:Add(XTypeRefMember{md,SELF:Assembly})
               NEXT
               FOREACH var pd in _typeDef:Properties
                  aMembers:Add(XTypeRefMember{pd,SELF:Assembly})
               NEXT
               FOREACH var fd in _typeDef:Fields
                  aMembers:Add(XTypeRefMember{fd,SELF:Assembly})
               NEXT
               FOREACH var ed in _typeDef:Events
                  aMembers:Add(XTypeRefMember{ed,SELF:Assembly})
               NEXT
               //            _typeDef:GenericParameters
              BEGIN LOCK SELF:_members
                  SELF:_members:Clear()
                  SELF:_members:AddRange(aMembers)
              END LOCK
              SELF:_hasMembers := TRUE
         ENDIF
         RETURN
      
      PROPERTY Members AS IList<IXTypeMember>  
         GET
            SELF:_LoadMembers()
            BEGIN LOCK SELF:_members
               RETURN SELF:_members:ToArray()
            END LOCK
         END GET
      END PROPERTY
      
      PROPERTY XMembers AS IList<XTypeRefMember>
         GET
            SELF:_LoadMembers()
            BEGIN LOCK SELF:_members
               RETURN SELF:_members:ToArray()
            END LOCK
         END GET
      END PROPERTY
      
      METHOD GetMember(elementName AS STRING) AS IList<XTypeRefMember>
         VAR tempMembers := List<XTypeRefMember>{}
         FOREACH x AS XTypeRefMember IN SELF:Members
            IF nameEquals(x:Name, elementName)
               tempMembers:Add(x)
            ENDIF
         NEXT
         RETURN tempMembers;
         
      PRIVATE METHOD nameEquals(name AS STRING, compareWith AS STRING) AS LOGIC
         RETURN String.Compare(name, compareWith, StringComparison.OrdinalIgnoreCase) == 0
      
      PROPERTY FullName AS STRING   GET SELF:GetFullName()
      
      PROPERTY Description AS STRING GET SELF:GetDescription()
      
      PROPERTY IsNested  AS LOGIC GET SELF:Parent IS XTypeRef
      PROPERTY IsGeneric as LOGIC   GET SELF:FullName != NULL .and. SELF:FullName:EndsWith(">")         
      
      PROPERTY Children   AS IList<IXType> 
         GET 
            BEGIN LOCK SELF:_children
               return SELF:_children:ToArray()
            END LOCK
         END GET
      END PROPERTY
      
      PROPERTY XChildren   AS IList<XTypeRef>  
         GET 
            BEGIN LOCK SELF:_children
               return SELF:_children:ToArray()
            END LOCK
         END GET
      END PROPERTY

      PROPERTY Interfaces  AS IList<STRING> GET _signature:Interfaces:ToArray()      
         
      PROPERTY BaseType AS STRING GET SELF:_signature:BaseType SET SELF:_signature:BaseType := @@value

      METHOD AddInterface(sInterface AS STRING) AS VOID
         SELF:_signature:AddInterface(sInterface)

      METHOD AddTypeParameter(name AS STRING) AS VOID
         SELF:_signature:AddTypeParameter(name)
         
      METHOD AddConstraints(name AS STRING) AS VOID
         SELF:_signature:AddConstraints(name)

      PROPERTY TypeParameters as IList<STRING> GET SELF:_signature:TypeParameters:ToArray()
      PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()

      
   END CLASS
   
END NAMESPACE

