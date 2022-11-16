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
    [DebuggerDisplay("{ToString(),nq}")];
    CLASS XPETypeSymbol INHERIT XPESymbol IMPLEMENTS IXTypeSymbol
        PRIVATE _baseType       AS XPETypeSymbol
        PRIVATE _members        AS IList<XPEMemberSymbol>
        PRIVATE _allmembers     AS IList<XPEMemberSymbol>
        PRIVATE _children       AS IList<XPETypeSymbol>
        PRIVATE _signature      AS XTypeSignature
        PRIVATE _typeDef        AS TypeDefinition
        PRIVATE _initialized   := FALSE  AS LOGIC
        PROPERTY ShortName      AS STRING       AUTO GET INTERNAL SET
        PROPERTY TypeDef        AS TypeDefinition GET _typeDef
        PROPERTY GenericName    AS STRING AUTO
        PROPERTY TickedName     AS STRING GET _typeDef:Name
        PROPERTY IsSpecialName  AS LOGIC GET _typeDef:IsSpecialName
        CONSTRUCTOR(typedef as TypeDefinition, asm as XAssembly)
            SUPER(typedef:Name, GetKind(typedef), ConvertAttributes(typedef:Attributes), asm)
            SELF:_typeDef        := typedef
            SELF:_members        := XPEMemberSymbol[]{0}
            SELF:_allmembers     := XPEMemberSymbol[]{0}
            SELF:_children       := List<XPETypeSymbol>{}
            SELF:_signature      := XTypeSignature{"System.Object"}
            SELF:Namespace       := typedef:Namespace
            IF SELF:Namespace:Length == 0 .AND. typedef:DeclaringType != NULL
                // nested type !
                SELF:Namespace    := typedef:DeclaringType:FullName
            ENDIF
            IF typedef:BaseType != NULL
                SELF:BaseTypeName     := RemoveGenericParameters(typedef:BaseType:FullName)
            ELSE
                SELF:BaseTypeName     := ""
            ENDIF
            SELF:OriginalTypeName := typedef:FullName
            SELF:_custatts := typedef:CustomAttributes
            IF typedef:HasGenericParameters
                VAR cName          := SELF:Name
                VAR nsGeneric := FALSE
                VAR pos := cName:IndexOf('`')
                IF pos == -1
                    cName := SELF:Namespace
                    nsGeneric := TRUE
                    pos := cName:IndexOf('`')
                ENDIF
                IF pos > 0
                    cName := cName:Substring(0,pos)
                    SELF:ShortName := cName
                    SELF:_signature:ReadGenericParameters(typedef:GenericParameters)
                    cName += _signature:GetTypeParameterNames()
                    IF nsGeneric
                        SELF:Namespace := cName
                    ELSE
                        SELF:Name := cName
                    ENDIF
                ENDIF
            ENDIF

        INTERNAL METHOD AddChild(child as XPETypeSymbol) AS VOID
        SELF:_children:Add(child)

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
        RETURN modifiers


        INTERNAL STATIC METHOD AddMembers(aMembers AS  List<XPEMemberSymbol>, typedef AS TypeDefinition, parent AS XPETypeSymbol) AS VOID

            IF typedef:HasMethods
                FOREACH md as MethodDefinition IN typedef:Methods
                    // filter
                    IF md:IsPrivate .OR. md:IsAssembly
                        LOOP
                    ENDIF
                    VAR kind := Kind.Method
                    if md:IsGetter .or. md:IsSetter
                        loop
                    endif
                    if md:IsConstructor
                         kind := Kind.Constructor
                     ELSE
                        VAR name := md:Name
                        IF name:StartsWith("op_")
                            kind := Kind.Operator
                        ENDIF
                    ENDIF
                    VAR xmember := XPEMethodSymbol{md,parent:Assembly}
                    IF xmember:Kind == Kind.Method      // this could have changed from Method to Function
                        xmember:Kind := kind
                    ENDIF
                    aMembers:Add(xmember)
                    xmember:Parent := parent
                NEXT
            ENDIF
            IF typedef:HasProperties
                FOREACH VAR pd IN typedef:Properties
                    IF pd:GetMethod != NULL .AND. pd:GetMethod:IsPrivate
                        LOOP
                    ENDIF
                    VAR xprop := XPEPropertySymbol{pd,parent:Assembly}
                    aMembers:Add(xprop)
                    xprop:Parent := parent
                NEXT
            ENDIF
            IF typedef:HasFields
                FOREACH VAR fd IN typedef:Fields
                    IF fd:IsPrivate
                        LOOP
                    ENDIF
                    VAR xField := XPEFieldSymbol{fd,parent:Assembly}
                    aMembers:Add(xField)
                    xField:Parent := parent
                NEXT
            ENDIF
            IF typedef:HasEvents
                FOREACH VAR ed IN typedef:Events
                    IF ed:AddMethod != NULL .AND. ed:AddMethod:IsPrivate
                        LOOP
                    ENDIF
                    VAR xEvent := XPEEventSymbol{ed,parent:Assembly}
                    aMembers:Add(xEvent)
                    xEvent:Parent := parent

                NEXT
        ENDIF

        INTERNAL METHOD Resolve() AS VOID
            IF !SELF:Assembly:Loaded
                RETURN
            ENDIF
            IF ! SELF:_initialized .AND. SELF:_typeDef != NULL
                VAR aMembers := List<XPEMemberSymbol>{}
                LOCAL hasbasemembers as LOGIC
                AddMembers(aMembers, SELF:_typeDef, SELF)
                BEGIN LOCK SELF
                    // now add to
                    SELF:_members := aMembers:ToArray()
                END LOCK
                // Get methods from parent class(es), recursively
                if SELF:Assembly != NULL .and. ! String.IsNullOrEmpty(SELF:BaseTypeName)
                    _baseType := SystemTypeController.FindType(SELF:BaseTypeName, SELF:Assembly:FullName)
                    if _baseType != NULL
                        // do not inherit private members from the base class
                        // and also no constructors
                        VAR basemembers := _baseType:XPEMembers:Where( { m => m:IsMethodVisibleInSubclass()})
                        hasbasemembers := basemembers:Count() > 0
                        aMembers:AddRange( basemembers )
                    ENDIF
                ENDIF
                BEGIN LOCK SELF
                    // now add to
                    SELF:_allmembers := aMembers:ToArray()
                END LOCK

                // nested children ?
                IF _typeDef:HasNestedTypes
                    var aChildren := List<XPETypeSymbol>{}
                    FOREACH var child in _typeDef:NestedTypes
                        aChildren:Add(XPETypeSymbol{child,SELF:Assembly})
                    NEXT
                    BEGIN LOCK SELF
                        SELF:_children := aChildren
                    END LOCK
                ENDIF
                IF _typeDef:HasInterfaces
                    VAR isCoClass := FALSE
                    IF _typeDef:HasCustomAttributes
                        isCoClass := _typeDef:CustomAttributes:FirstOrDefault( { x => x:AttributeType:FullName == "System.Runtime.InteropServices.CoClassAttribute"}) != NULL
                    ENDIF
                    SELF:_signature:ClearInterfaces()
                    FOREACH VAR @@interface IN _typeDef:Interfaces
                        VAR ifname := @@interface:InterfaceType:FullName
                        // cecil returns System.Collections.Generic.IList`1<T> for the FullName
                        VAR LtPos := ifname:IndexOf(c'<')
                        IF  LtPos > 0 .AND. ifname:Contains(c'`')
                            ifname := ifname:Substring(0, LtPos)
                        ENDIF
                        SELF:_signature:AddInterface(ifname)
                        VAR mod := _typeDef:Module
                        // see if we can find the type behind the interface in our assembly
                        // and if we do then update the member list
                        // this is especially needed with AdoDb where the Connection interface
                        // has no members but inherits all its members from 2 other interfaces
                        VAR ifType := mod:GetType(ifname)
                        IF ifType != NULL .AND. isCoClass
                            AddMembers(aMembers, ifType, SELF)
                        ENDIF

                    NEXT
                ENDIF
                if hasbasemembers  .or. String.IsNullOrEmpty(SELF:BaseTypeName)
                    SELF:_initialized := TRUE
                endif
            ENDIF
        RETURN



        METHOD GetMembers(elementName AS STRING) AS IList<IXMemberSymbol>
            VAR tempMembers := List<IXMemberSymbol>{}
            IF elementName:StartsWith("@@")
                elementName := elementName:Substring(2)
            ENDIF
            SELF:Resolve()
            IF ! String.IsNullOrEmpty(elementName)
                tempMembers:AddRange(SELF:_allmembers:Where ( {m => m.Name.StartsWith(elementName, StringComparison.OrdinalIgnoreCase) }))
            ELSE
                tempMembers:AddRange(SELF:_allmembers)
            ENDIF
            RETURN tempMembers

        METHOD GetMembers(elementName AS STRING, lExact AS LOGIC) AS IList<IXMemberSymbol>
            IF elementName:StartsWith("@@")
                elementName := elementName:Substring(2)
            ENDIF
            IF lExact
                SELF:Resolve()
                var result := List<IXMemberSymbol>{}
                result:AddRange(SELF:_allmembers:Where ( {m => m.Name.Equals(elementName, StringComparison.OrdinalIgnoreCase) }))
                RETURN result
            ELSE
                RETURN SELF:GetMembers(elementName)
            ENDIF

        PROPERTY AllMembers AS IList<IXMemberSymbol>
            GET
                SELF:Resolve()
                VAR members := List<IXMemberSymbol>{}
                members:AddRange(SELF:_allmembers)
                RETURN members
            END GET
        END PROPERTY
        PROPERTY XPEMembers AS IList<XPEMemberSymbol>
            GET
                SELF:Resolve()
                RETURN SELF:_allmembers:ToArray()
            END GET
        END PROPERTY
        PROPERTY Members AS IList<IXMemberSymbol>
            GET
                SELF:Resolve()
                VAR members := List<IXMemberSymbol>{}
                members:AddRange(SELF:_members)
                RETURN members
            END GET
        END PROPERTY

        PROPERTY XMembers AS IList<XPEMemberSymbol>
            GET
                SELF:Resolve()
                RETURN SELF:_members:ToArray()
            END GET
        END PROPERTY

        PROPERTY FullName  AS STRING
            GET
                IF SELF:IsGeneric .and. String.IsNullOrEmpty(SELF:GenericName)
                    SELF:GenericName :=  SELF:_GetGenericName()
                ENDIF
                RETURN SELF:GetFullName()
            END GET
        END PROPERTY
        PROPERTY FullTickedName  AS STRING
            GET
                IF SELF:IsGeneric
                    IF String.IsNullOrEmpty(SELF:Namespace)
                        RETURN SELF:TickedName
                    ENDIF
                    RETURN SELF:Namespace + "." + SELF:TickedName
                ENDIF
                RETURN SELF:FullName
            END GET
        END PROPERTY

        PROPERTY Description AS STRING GET SELF:GetDescription()
        PROPERTY IsFunctionsClass as LOGIC GET SELF:Assembly != NULL .and. SELF:FullName == SELF:Assembly:GlobalClassName
        PROPERTY IsNested  AS LOGIC GET SELF:Parent IS XPETypeSymbol
        PROPERTY IsGeneric AS LOGIC GET _typeDef:HasGenericParameters
        PROPERTY IsStatic  AS LOGIC GET _typeDef:Attributes:HasFlag(TypeAttributes.Abstract |TypeAttributes.Sealed)
        PROPERTY Location  AS STRING GET SELF:Assembly:DisplayName

        PROPERTY Children   AS IList<IXTypeSymbol>
            GET
                var children := List<IXTypeSymbol>{}
                children:AddRange(SELF:_children)
                return children
            END GET
        END PROPERTY

        PROPERTY XChildren   AS IList<XPETypeSymbol>
            GET
                BEGIN LOCK SELF
                    return SELF:_children:ToArray()
                END LOCK
            END GET
        END PROPERTY

        PROPERTY Interfaces  AS IList<STRING>
            GET
                SELF:Resolve()
                RETURN _signature:Interfaces:ToArray()
            END GET
        END PROPERTY

        PROPERTY BaseType AS IXTypeSymbol GET SELF:_baseType

        PROPERTY BaseTypeName AS STRING GET SELF:_signature:BaseType SET SELF:_signature:BaseType := @@value


        METHOD AddTypeParameter(name AS STRING) AS VOID
            SELF:_signature:AddTypeParameter(name)

        METHOD AddConstraints(name AS STRING) AS VOID
            SELF:_signature:AddConstraints(name)

        PROPERTY TypeParameters as IList<STRING> GET SELF:_signature:TypeParameters:ToArray()
        PROPERTY TypeParameterList as STRING GET SELF:_signature:TypeParameterList
        PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()

        PROPERTY XMLSignature   AS STRING GET SELF:GetXmlSignature()

        METHOD ToString() AS STRING
            var result := i"{Kind} {Name}"
            RETURN result

    END CLASS
    CLASS XPEArrayTypeSymbol INHERIT XPETypeSymbol
        PRIVATE _elementName     AS STRING
        PRIVATE _ctors           AS IList<IXMemberSymbol>
        CONSTRUCTOR (aType as XPETypeSymbol, elementType as STRING)
            SUPER(aType:TypeDef,aType:Assembly)
            SELF:_elementName := elementType
            SELF:IsArray := TRUE

        PROPERTY TypeName  AS STRING
            GET
                RETURN SELF:_elementName+"[]"
            END GET
        END PROPERTY

        PROPERTY FullName  AS STRING
            GET
                RETURN SELF:_elementName+"[]"
            END GET
        END PROPERTY

        METHOD SetConstructors(ctors as IList<IXMemberSymbol>) AS VOID
            _ctors := ctors

        METHOD GetConstructors() AS IList<IXMemberSymbol>
            if (_ctors == null)
                var temp := SUPER:Members:Where ( {x => x:Name == "CreateInstance" }):ToArray()
                var ctors := List<IXMemberSymbol>{}
                foreach var m in temp

                    var newmethod := (XPEMethodSymbol) m:Clone()
                    // remove first parameter
                    var pars := List<IXParameterSymbol>{}
                    pars:AddRange(m:Parameters)
                    pars:RemoveAt(0)
                    newmethod:Kind := Kind.Constructor
                    newmethod:Attributes := _XOR(newmethod:Attributes, Modifiers.Static)
                    newmethod:Name := ".ctor"
                    newmethod:DeclaringType := SELF:FullName
                    newmethod:Parent := SELF
                    newmethod:Signature:DataType := SELF:TypeName
                    newmethod:Signature:Parameters := pars:ToList()

                    ctors.Add(newmethod)
                next
                _ctors := ctors:ToArray()
            endif
            return _ctors
        METHOD ToString() AS STRING
            var result := i"{Kind} {TypeName}"
            RETURN result
    END CLASS

END NAMESPACE

