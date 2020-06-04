//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Linq
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil

BEGIN NAMESPACE XSharpModel
   /// <summary>
      /// Model for Namespace, Class, Interface, Structure, Enum
   /// </summary>
   [DebuggerDisplay("{Kind} {FullName,nq}")];
   CLASS XTypeReference INHERIT XEntityReference IMPLEMENTS IXType
      PRIVATE _baseType       AS XTypeReference
      PRIVATE _members        AS IList<XMemberReference>
      PRIVATE _children       AS IList<XTypeReference>
      PRIVATE _signature      AS XTypeSignature
      PRIVATE _typeDef        as TypeDefinition
      PRIVATE _initialized   := FALSE  AS LOGIC

     CONSTRUCTOR(typedef as TypeDefinition, asm as XAssembly)
         SUPER(typedef:Name, GetKind(typedef), ConvertAttributes(typedef:Attributes), asm)  
         SELF:_typeDef     := typedef
         SELF:_members     := List<XMemberReference>{}
         SELF:_children    := List<XTypeReference>{}
         SELF:_signature   := XTypeSignature{"System.Object"}
         SELF:Namespace    := typedef:Namespace
         IF typedef:BaseType != NULL
            SELF:BaseType     := typedef:BaseType:FullName
         ELSE
            SELF:BaseType     := ""
         ENDIF


      
      PRIVATE STATIC METHOD GetKind(typedef as TypeDefinition) AS Kind
         IF typedef:BaseType != NULL
            SWITCH typedef:BaseType:FullName 
            CASE "System.ValueType"
               RETURN Kind.Structure
            CASE "System.MulticastDelegate"
               RETURN Kind.Delegate
            END SWITCH
         ENDIF
         DO CASE
         CASE typedef:IsEnum
            RETURN Kind.Enum
         CASE typedef:IsClass
            RETURN Kind.Class
         CASE typedef:IsInterface
            RETURN Kind.Interface
         ENDCASE
         RETURN Kind.Structure
      
      
      STATIC METHOD ConvertAttributes (attributes AS TypeAttributes) as Modifiers
         var modifiers := Modifiers.None
         var visattributes := _AND(attributes, TypeAttributes.VisibilityMask)
         SWITCH visattributes
         CASE TypeAttributes.Public
            modifiers :=  Modifiers.Public
         CASE TypeAttributes.NotPublic
            modifiers := Modifiers.Private
         CASE TypeAttributes.NestedFamily
            modifiers := Modifiers.Protected
         CASE TypeAttributes.NestedAssembly
            modifiers := Modifiers.Internal
         CASE TypeAttributes.NestedFamANDAssem
         CASE TypeAttributes.NestedFamORAssem
            modifiers := Modifiers.ProtectedInternal
         OTHERWISE
            modifiers := Modifiers.Public
         END SWITCH
                 
         IF attributes:HasFlag(TypeAttributes.Abstract | TypeAttributes.Sealed)
            modifiers |= Modifiers.Static
         ELSEIF attributes:HasFlag(TypeAttributes.Abstract)
            modifiers |= Modifiers.Abstract
         ELSEIF attributes:HasFlag(TypeAttributes.Sealed)
            modifiers |= Modifiers.Sealed
         ENDIF
         IF attributes:HasFlag(TypeAttributes.Import)
            modifiers |= Modifiers.External
         ENDIF
         return modifiers         
              
          
      INTERNAL METHOD Resolve() AS VOID
         IF ! SELF:_initialized
               VAR aMembers := List<XMemberReference>{}
             
               IF _typeDef:HasMethods
                  FOREACH var md in _typeDef:Methods
                     // filter 
                     var kind := Kind.Method
                     if md:Attributes:HasFlag(MethodAttributes.SpecialName)
                        var name := md:Name
                        if name:StartsWith("set_")
                           loop
                        elseif name:StartsWith("get_")
                           loop
                        elseif name:StartsWith("op_")
                           kind := Kind.Operator
                        elseif name:StartsWith(".ctor")
                           kind := Kind.Constructor
                        endif
                     endif
                     var xmember := XMethodReference{md,SELF:Assembly}
                     if xmember:Kind == Kind.Method
                        xmember:Kind := kind
                     endif
                     aMembers:Add(xmember)
                  NEXT
               ENDIF
               IF _typeDef:HasProperties
                  FOREACH var pd in _typeDef:Properties
                     aMembers:Add(XPropertyReference{pd,SELF:Assembly})
                  NEXT
               ENDIF
               IF _typeDef:HasFields
                  FOREACH var fd in _typeDef:Fields
                     aMembers:Add(XFieldReference{fd,SELF:Assembly})
                  NEXT
               ENDIF
               IF _typeDef:HasEvents
                  FOREACH var ed in _typeDef:Events
                     aMembers:Add(XEventReference{ed,SELF:Assembly})
                  NEXT
               ENDIF
               
              // Get methods from parent class(es), recursively
              if SELF:Assembly != NULL .and. ! String.IsNullOrEmpty(SELF:BaseType)
                  _baseType := SystemTypeController.FindType(SELF:BaseType, SELF:Assembly:FullName)
                  if _baseType != NULL
                     _baseType:Resolve()
                     var basemembers := _baseType:XMembers:Where( { m => m.Kind != Kind.Constructor })
                     aMembers:AddRange( basemembers )
                  ENDIF
               ENDIF
              BEGIN LOCK SELF
                  SELF:_members := aMembers:ToArray()
              END LOCK
              // nested children ?
              IF _typeDef:HasNestedTypes
                  var aChildren := List<XTypeReference>{}
                  FOREACH var child in _typeDef:NestedTypes
                     aChildren:Add(XTypeReference{child,SELF:Assembly})
                  NEXT
                  BEGIN LOCK SELF
                     SELF:_children := aChildren:ToArray()
                  END LOCK
               ENDIF
               IF _typeDef:HasInterfaces
                  FOREACH VAR interface in _typeDef:Interfaces
                     SELF:_signature:AddInterface(interface:InterfaceType:FullName)
                  NEXT
               ENDIF
               
              SELF:_initialized := TRUE
         ENDIF
         RETURN
      
      PROPERTY Members AS IList<IXMember>  
         GET
            SELF:Resolve()
            RETURN (IList<IXMember>) SELF:_members
         END GET
      END PROPERTY
      
      PROPERTY XMembers AS IList<XMemberReference>
         GET
            SELF:Resolve()
            RETURN SELF:_members
         END GET
      END PROPERTY
      
      METHOD GetMember(elementName AS STRING) AS IList<XMemberReference>
         VAR foundMembers := List<XMemberReference>{}
         FOREACH x AS XMemberReference IN SELF:XMembers
            IF nameEquals(x:Name, elementName)
               foundMembers:Add(x)
            ENDIF
         NEXT
         RETURN foundMembers
         
      PRIVATE METHOD nameEquals(name AS STRING, compareWith AS STRING) AS LOGIC
         RETURN String.Compare(name, compareWith, StringComparison.OrdinalIgnoreCase) == 0
      
      PROPERTY FullName AS STRING   GET SELF:GetFullName()
      
      PROPERTY Description AS STRING GET SELF:GetDescription()
      
      PROPERTY IsNested  AS LOGIC GET SELF:Parent IS XTypeReference
      PROPERTY IsGeneric as LOGIC GET _typeDef:HasGenericParameters
      PROPERTY IsStatic  AS LOGIC GET _typeDef:Attributes:HasFlag(TypeAttributes.Abstract |TypeAttributes.Sealed)
      PROPERTY Children   AS IList<IXType> 
         GET 
            return (IList<IXType> ) SELF:_children
         END GET
      END PROPERTY
      
      PROPERTY XChildren   AS IList<XTypeReference>  
         GET 
            BEGIN LOCK SELF
               return SELF:_children
            END LOCK
         END GET
      END PROPERTY

      PROPERTY Interfaces  AS IList<STRING> GET _signature:Interfaces:ToArray()      
         
      PROPERTY BaseType AS STRING GET SELF:_signature:BaseType SET SELF:_signature:BaseType := @@value


      METHOD AddTypeParameter(name AS STRING) AS VOID
         SELF:_signature:AddTypeParameter(name)
         
      METHOD AddConstraints(name AS STRING) AS VOID
         SELF:_signature:AddConstraints(name)

      PROPERTY TypeParameters as IList<STRING> GET SELF:_signature:TypeParameters:ToArray()
      PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()
      PROPERTY XMLSignature   AS STRING GET SELF:GetXmlSignature()
      
   END CLASS
   
END NAMESPACE

