//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Diagnostics
USING XSharpModel
BEGIN NAMESPACE XSharpModel
    [DebuggerDisplay("{Prototype,nq}")];
    CLASS XTypeMember INHERIT XElement
        // Fields
        PRIVATE _locals AS List<XVariable>
        PRIVATE _parameters AS List<XVariable>
        PRIVATE _typeName AS string

        // Methods
         CONSTRUCTOR(name AS string, kind AS Kind, modifiers AS Modifiers, visibility AS Modifiers, span AS TextRange, position AS TextInterval, isStatic AS Logic)
		 
			SUPER(name, kind, modifiers, visibility, span, position)
            //
            SELF:Parent := null
            SELF:_parameters := List<XVariable>{}
            SELF:_locals := List<XVariable>{}
            SELF:_typeName := ""
            SUPER:_isStatic := isStatic

         CONSTRUCTOR(name AS string, kind AS Kind, modifiers AS Modifiers, visibility AS Modifiers, span AS TextRange, position AS TextInterval, typeName AS string, isStatic AS Logic)
        SELF(name, kind, modifiers, visibility, span, position, isStatic)
            //
            SELF:_typeName := typeName

        METHOD Namesake() AS List<XTypeMember>
            VAR list := List<XTypeMember>{}
            IF (SELF:Parent != null)
                FOREACH VAR @@member IN ((XType) SELF:Parent):Members
                    IF String.Compare(@@member:FullName, SELF:FullName, TRUE) == 0 .AND. String.Compare(@@member:Prototype, SELF:Prototype, TRUE) > 0
                        //// 
                        list:Add(@@member)
                    ENDIF
                NEXT
            ENDIF
            RETURN list
//
        // Properties
        VIRTUAL PROPERTY Description AS string
            GET
                //
                var str := ""
                IF (SUPER:Modifiers != Modifiers.None)
                    //
                    str := String.Concat(str, SUPER:Modifiers:ToString(), " ")
                ENDIF
                var str2 := String.Concat(str, SUPER:Visibility:ToString(), " ")
                IF (SUPER:Kind != Kind.Field)
                    //
                    str2 := String.Concat(str2, ElementExtensions.DisplayName(SUPER:Kind), " ")
                    IF (SUPER:Kind == Kind.VODefine)
                        //
                        RETURN String.Concat(str2, SUPER:Name, SELF:Suffix)
                    ENDIF
                ENDIF
                RETURN String.Concat(str2, SELF:Prototype)
            END GET
        END PROPERTY

        VIRTUAL PROPERTY FullName AS string
            GET
                //
                IF (SELF:Parent != null)
                    //
                    RETURN String.Concat(SELF:Parent:FullName, ".", SUPER:Name)
                ENDIF
                RETURN SUPER:Name
            END GET
        END PROPERTY

        PROPERTY HasParameters AS Logic
            GET
                //
                RETURN (ElementExtensions.HasParameters(SUPER:Kind) .AND. (SELF:Parameters:Count > 0))
            END GET
        END PROPERTY

        PROPERTY IsArray AS Logic AUTO 

        PROPERTY Locals AS List<XVariable>
            GET
                //
                RETURN SELF:_locals
            END GET
        END PROPERTY

        NEW PROPERTY Parent AS XTYPE
            GET
                //
                RETURN (XType) SUPER:parent
            END GET
            SET
                //
                SUPER:parent := value
            END SET
        END PROPERTY
	    PROPERTY ParameterList AS string
            GET
                //
                var str := ""
                FOREACH variable AS XVariable IN SELF:Parameters
                    //
                    IF (str:Length > 1)
                        //
                        str := String.Concat(str, ", ")
                    ENDIF
                    str := String.Concat(str, variable:Name, " as ", variable:TypeName)
                NEXT
                RETURN str
            END GET
        END PROPERTY

        PROPERTY Parameters AS List<XVariable>
            GET
                //
                RETURN SELF:_parameters
            END GET
        END PROPERTY


        VIRTUAL PROPERTY Prototype AS string
            GET
                //
                var str := ""
                IF SELF:Kind:HasParameters()
                    //
                    str := String.Concat("(", SELF:ParameterList, ")")
                ENDIF
                var str2 := String.Concat(SUPER:Name, str)
                IF SELF:Kind:HasReturnType()
                    //
                    str2 := String.Concat(str2, " AS ", SELF:TypeName)
                ENDIF
                RETURN str2
            END GET
        END PROPERTY

        PROPERTY Suffix AS string AUTO 

        PROPERTY TypeName AS string
            GET
                //
                RETURN SELF:_typeName
            END GET
        END PROPERTY


    END CLASS

END NAMESPACE 

