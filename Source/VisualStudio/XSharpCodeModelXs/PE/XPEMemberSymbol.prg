//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil
using XSharp.Settings
BEGIN NAMESPACE XSharpModel

[DebuggerDisplay("{ToString(),nq}")];
ABSTRACT CLASS XPEMemberSymbol     INHERIT XPESymbol IMPLEMENTS IXMemberSymbol
    // Fields
    PROTECTED  _signature    AS XMemberSignature
    PROTECTED  _resolved    AS LOGIC
    PROTECTED  _generic     AS LOGIC
    PROPERTY  SubType      AS Kind AUTO
    PROPERTY  DeclaringType  AS STRING AUTO
    PROPERTY  DeclaringTypeSym as XPETypeSymbol AUTO
    PROPERTY  Signature     AS XMemberSignature  GET _signature INTERNAL SET _signature := value
    PROPERTY  IsGeneric    AS LOGIC GET _generic
    ABSTRACT PROPERTY  ClassGenText      AS STRING GET

#region constructors

    CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, asm AS XAssembly)
        SUPER(name, kind, attributes,  asm)
        SELF:_signature      := XMemberSignature{}
        SELF:_resolved       := FALSE
        SELF:_custatts       := NULL
        SELF:_generic        := FALSE
        RETURN


#endregion

    PROTECTED INTERNAL VIRTUAL METHOD Resolve() AS VOID
        IF SELF:_custatts != NULL
            FOREACH VAR custatt IN SELF:_custatts
                SWITCH custatt:AttributeType:FullName
                CASE "System.Diagnostics.DebuggerBrowsableAttribute"
                    VAR cvalue := custatt:ConstructorArguments[0]
                    IF cvalue:Type:FullName == typeof(DebuggerBrowsableState):FullName
                        VAR state := (DebuggerBrowsableState) cvalue:Value
                        IF state == DebuggerBrowsableState.Never
                            // hide these
                            SELF:Attributes := _OR(SELF:Modifiers, Modifiers.Internal)
                        ENDIF
                    ENDIF
                END SWITCH
            NEXT
        ENDIF
        SELF:_custatts := NULL
        RETURN

    PRIVATE METHOD DoResolve() AS VOID
        IF ! self:_resolved
            SELF:Resolve()
            self:_resolved := TRUE
        ENDIF

    METHOD AddParameters( oAttr as Mono.Cecil.CustomAttribute) AS VOID
        var arg    := oAttr:ConstructorArguments[0]
        var type   := arg:Type
        if type:FullName == "System.String[]"
            var args := (Mono.Cecil.CustomAttributeArgument[]) arg:Value
            FOREACH VAR argName in args
                var parRef := XPEParameterSymbol{self, argName:Value:ToString(), "USUAL"}
                parRef:ParamType := ParamType.As
                parRef:OriginalTypeName := KnownTypes.XSharpUsual
                SELF:_signature:Parameters:Add(parRef)
            NEXT
        endif


    METHOD AddParameters (aPars as Mono.Collections.Generic.Collection<ParameterDefinition>) AS VOID
        SELF:_signature:Parameters:Clear()
        FOREACH var oPar in aPars
            var name  := oPar:Name
            var index := oPar:Index
            var type  := oPar:ParameterType:GetXSharpTypeName()
            var defValue := oPar:Constant
            var custatt := ""
            LOCAL parType as ParamType
            IF oPar:IsOut .and. oPar:IsIn
                parType := ParamType.Ref
            ELSEIF oPar:IsOut
                parType := ParamType.Out
            ELSEIF oPar:IsIn
                parType := ParamType.As
            ELSE
                parType := ParamType.As
            ENDIF
            if oPar:HasCustomAttributes
                defValue := SELF:DecodeCustomAttributes(oPar:CustomAttributes, oPar:ParameterType)
            ENDIF
            var parRef := XPEParameterSymbol{self, name, type}
            parRef:OriginalTypeName := RemoveGenericParameters(oPar:ParameterType:FullName)
            if defValue != NULL
                parRef:Value := defValue:ToString()
            ENDIF
            IF parRef:OriginalTypeName:Contains("&") .AND. parType == ParamType.As
                parType := ParamType.Ref
            ENDIF
            parRef:ParamType := parType
            IF parRef:OriginalTypeName:Contains("`")
                VAR count := SELF:TypeParameters:Count
                parRef:TypeName := parRef:OriginalTypeName:Replace("`"+count:ToString(),"")
            ENDIF
            SELF:_signature:Parameters:Add(parRef)
        NEXT
        RETURN

    METHOD DecodeCustomAttributes( attributes as Mono.Collections.Generic.Collection<CustomAttribute>, oType as TypeReference) AS STRING
        local result as STRING
        local done   as LOGIC
        FOREACH var attr in attributes
            SWITCH attr:AttributeType:FullName
            CASE KnownTypes.XSharpDefaultParam
            CASE KnownTypes.VulcanDefaultParam
                var arg1     := attr:ConstructorArguments[0]
                IF arg1:Value is CustomAttributeArgument VAR arg
                    arg1 := arg
                ENDIF
                var arg2 := (Int32) attr:ConstructorArguments[1]:Value
                switch arg2
                case 1   // NIL
                    result := "NIL"
                case 2   // Arg1 is date in ticks
                    var ticks := (Int64)  arg1:Value
                    var dt    := DateTime{ticks}
                    if dt == DateTime.MinValue
                        result := KnownTypes.NullDate
                    else
                        result    := dt:ToString("yyyy.MM.dd")
                    endif
                case 3   // Arg1 is Symbol , when NULL then NULL_SYMBOL
                    var sym := (STRING)  arg1:Value
                    if (sym == NULL)
                        result := KnownTypes.NullSymbol
                    else
                        result := "#" + sym:ToString()
                    endif
                case 4   // Arg1 is PSZ, when NULL then NULL_SYMBOL
                    var psz1 := (STRING)  arg1:Value
                    if (psz1 == NULL)
                        result := KnownTypes.NullPsz
                    else
                        result := psz1:ToString()
                    endif
                case 5   // Arg1 is NULL_PTR
                    VAR p := IntPtr{ (INT64) arg1:Value}
                    if (p == IntPtr.Zero)
                        result := KnownTypes.NullPtr
                    ELSE
                        result := "0x"+p:ToString("X")
                    ENDIF

                case 0   // Normal .Net value
                    var obj := arg1:Value
                    if (obj != null)
                        result := obj:ToString()
                        if arg1:Type:FullName == KnownTypes.SystemString
                            result := e"\""+result+e"\""
                        endif
                    else
                        switch oType:FullName
                        case KnownTypes.SystemString
                            return KnownTypes.NullString
                        otherwise
                            return KnownTypes.NullObject
                        end switch
                    endif
                end switch
                done := TRUE
            END SWITCH
            if (done)
                EXIT
            ENDIF

        NEXT
        return result


    METHOD AddTypeParameters(aPars AS Mono.Collections.Generic.Collection<GenericParameter>) AS VOID
        SELF:_signature:TypeParameters:Clear()
        FOREACH typeParam AS Mono.Cecil.GenericParameter IN aPars
            SELF:_signature:TypeParameters:Add(typeParam:Name)
        NEXT
        RETURN

        //
#region Properties
    PROPERTY Description AS STRING GET SELF:GetDescription()


    PROPERTY FullName AS STRING GET SELF:GetFullName()

    PROPERTY IsStatic AS LOGIC GET SELF:Modifiers:HasFlag(Modifiers.Static)
    PROPERTY Location AS STRING GET SELF:Assembly:DisplayName

    PROPERTY HasParameters     AS LOGIC
        GET
            SELF:DoResolve()
            RETURN _signature:HasParameters
        END GET
    END PROPERTY

    PROPERTY ParameterCount    AS INT
        GET
            SELF:DoResolve()
            RETURN _signature:ParameterCount
        END GET
    END PROPERTY

    PROPERTY ParameterList     AS STRING
        GET
            SELF:DoResolve()
            RETURN _signature:ParameterList
        END GET
    END PROPERTY
    PROPERTY TickedName as STRING                      GET SELF:GetTickedName()
    PROPERTY TypeParameters as IList<STRING>           GET SELF:_signature:TypeParameters:ToArray()
    PROPERTY TypeParameterList AS STRING               GET SELF:_signature:TypeParameterList
    PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()
    PROPERTY TypeParameterConstraintsList AS STRING    GET SELF:_signature:TypeParameterConstraintsList


    PROPERTY Parameters         AS IList<IXParameterSymbol>
        GET
            SELF:DoResolve()
            RETURN _signature:Parameters:ToArray()
        END GET
    END PROPERTY

    PROPERTY CallingConvention AS CallingConvention GET _signature:CallingConvention SET _signature:CallingConvention := @@value

    PROPERTY ParentType     AS IXTypeSymbol GET SELF:Parent ASTYPE IXTypeSymbol
    PROPERTY Prototype      AS STRING
        GET
            SELF:DoResolve()
            RETURN SELF:GetProtoType()
        END GET
    END PROPERTY

    PROPERTY IsExtension    AS LOGIC  GET _signature:IsExtension
    PROPERTY XMLSignature   AS STRING
        GET
            SELF:DoResolve()
            RETURN SELF:GetXmlSignature()
        END GET
    END PROPERTY

    METHOD ToString() AS STRING
        var result := i"{Kind} {Name}"
        if SELF:_signature != NULL .and. SELF:_signature:TypeParameters:Count > 0
            result += self:_signature:ToString()
        ENDIF
        RETURN result

    METHOD Clone() AS IXMemberSymbol
        SELF:Resolve()
        var clone := (XPEMemberSymbol) SELF:MemberwiseClone()
        clone:_signature := SELF:_signature:Clone()
        RETURN (IXMemberSymbol) clone

    METHOD WithName(newName as STRING) AS IXMemberSymbol
        var clone := (XPEMemberSymbol) SELF:MemberwiseClone()
        clone:Name := newName
        clone:_signature := SELF:_signature:Clone()
        RETURN (IXMemberSymbol) clone

    METHOD Detach() AS VOID
        RETURN

#endregion
END CLASS

END NAMESPACE

