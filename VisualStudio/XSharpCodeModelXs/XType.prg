//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using EnvDTE
using LanguageService.CodeAnalysis.Text
using System.Diagnostics
using System.Collections.Immutable

BEGIN NAMESPACE XSharpModel
    [DebuggerDisplay("{FullName,nq}")];
    CLASS XType INHERIT XElement
        // Fields
        PRIVATE _isPartial AS Logic
        PRIVATE _members AS List<XTypeMember>
        PRIVATE _nameSpace AS string
        PRIVATE _parentName AS string
        CONST PUBLIC GlobalName := "(Global Scope)" AS string

        // Methods
         CONSTRUCTOR(name AS string, kind AS Kind, modifiers AS Modifiers, visibility AS Modifiers, span AS TextRange, position AS TextInterval)
			SUPER(name, kind, modifiers, visibility, span, position)
            //
            SELF:_members := List<XTypeMember>{}
            SELF:_parentName := "System.Object"
            SELF:_nameSpace := ""
            IF modifiers:HasFlag(Modifiers.Static)
                SUPER:_isStatic := TRUE
            ENDIF
            IF modifiers:HasFlag(Modifiers.Partial)
                //
                SELF:_isPartial := TRUE
            ENDIF

        METHOD AddMember(oMember AS XTypeMember) AS void
            BEGIN LOCK SELF:_members
                SELF:_members:Add(oMember)
            END LOCK

        METHOD AddMembers(members AS IEnumerable<XTypeMember>) AS void
            BEGIN LOCK SELF:_members
                SELF:_members:AddRange(members)
            END LOCK

        STATIC METHOD CreateGlobalType(xfile AS XFile) AS XType
            //
            VAR oType := XType{GlobalName, Kind.Class, Modifiers.None, Modifiers.Public, TextRange{1, 1, 1, 1}, TextInterval{}} 
			oType:IsPartial:=true
			oType:IsStatic:=TRUE
			oType:File:=xfile
			return oType

        METHOD Duplicate() AS XType
            VAR type := XType{super:Name, super:Kind, super:Modifiers, super:Visibility, super:Range, super:Interval} 
            type:AddMembers(SELF:Members)
            RETURN type

        METHOD GetMember(elementName AS string) AS IImmutableList<XTypeMember>
            VAR list := List<XTypeMember>{} 
            FOREACH oMember AS XTypeMember IN SELF:Members
				if nameEquals(oMember:Name, elementName) 
					list:Add(oMember)
				endif
            NEXT
            RETURN ImmutableArray.ToImmutableArray<XTypeMember>(list)

        STATIC METHOD IsGlobalType(type AS XType) AS Logic
            //
            RETURN (type:Name == "(Global Scope)")

        METHOD Merge(otherType AS XType) AS XType
            LOCAL type AS XType
            //
            type := SELF:Duplicate()
            IF ((String.Compare(otherType:@@File:FullPath, SUPER:@@File:FullPath, System.StringComparison.OrdinalIgnoreCase) != 0) .OR. (SUPER:Range:StartLine != otherType:Range:StartLine))
                //
                SELF:IsPartial := TRUE
                IF (otherType != null)
                    //
                    type:AddMembers(otherType:Members)
                    IF ((type:Parent == null) .AND. (otherType:Parent != null))
                        //
                        type:Parent := otherType:Parent
                    ELSE
                        //
                        IF ((type:ParentName == null) .AND. (otherType:ParentName != null))
                            //
                            type:ParentName := otherType:ParentName
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            RETURN type


		property NameSpace as string get _namespace Set _namespace := value
        PRIVATE METHOD nameEquals(name AS string, compareWith AS string) AS Logic
            //
            RETURN (name:ToLower():CompareTo(compareWith:ToLower()) == 0)


        // Properties
        PROPERTY Clone AS XType
            GET
                IF (SELF:IsPartial)
                    RETURN SUPER:File:Project:LookupFullName(SELF:FullName, TRUE)
                ENDIF
                RETURN SELF
            END GET
        END PROPERTY

        VIRTUAL PROPERTY Description AS string
            GET
                //
                var str := ""
                IF (SUPER:Kind == Kind.Class)
                    //
                    IF (SUPER:Modifiers != Modifiers.None)
                        //
                        str := String.Concat(str, SUPER:Modifiers:ToString(), " ")
                    ENDIF
                    str := String.Concat(str, SUPER:Visibility:ToString(), " ")
                ENDIF
                var str2 := str
                IF (SUPER:Kind == Kind.Keyword)
                    //
                    RETURN String.Concat(SUPER:Name, " ", SUPER:Kind:ToString())
                ENDIF
                RETURN String.Concat(String.Concat(str2, SUPER:Kind:ToString(), " "), SELF:Prototype)
            END GET
        END PROPERTY

        VIRTUAL PROPERTY FullName AS string
            GET
                IF (! String.IsNullOrEmpty(SELF:_nameSpace))
                    RETURN String.Concat(SELF:NameSpace, ".", SUPER:Name)
                ENDIF
                RETURN SUPER:Name
            END GET
        END PROPERTY

        PROPERTY IsPartial AS Logic
            GET
                //
                RETURN SELF:_isPartial
            END GET
            SET
                //
                SELF:_isPartial := value
            END SET
        END PROPERTY

        PROPERTY IsType AS Logic
            GET
                //
                SWITCH SUPER:Kind
                CASE Kind.Enum 
				CASE Kind.VOStruct 
				CASE Kind.Union 
				CASE Kind.Class 
				CASE Kind.Structure 
				CASE Kind.Interface 
                    //
                    RETURN TRUE
                END SWITCH
                RETURN FALSE
            END GET
        END PROPERTY

        PROPERTY Members AS IImmutableList<XTypeMember>
            GET
                //
                BEGIN LOCK SELF:_members
                    //
                    RETURN ImmutableArray.ToImmutableArray<XTypeMember>(SELF:_members)
                END LOCK
            END GET
        END PROPERTY

        

        VIRTUAL PROPERTY ParentName AS string
            GET
                IF (SUPER:Parent != null)
                    RETURN SUPER:Parent:Name
                ENDIF
                IF (SELF:_parentName != null)
                    RETURN SELF:_parentName
                ENDIF
                RETURN null
            END GET
            SET
                IF (SUPER:Parent != null)
                    THROW System.Exception{"Cannot set ParentName if Parent is not null"}
                ENDIF
                SELF:_parentName := value
            END SET
        END PROPERTY


    END CLASS

END NAMESPACE 

