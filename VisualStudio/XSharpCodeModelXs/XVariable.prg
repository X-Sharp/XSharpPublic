
using XSharpModel
using System.Diagnostics
BEGIN NAMESPACE XSharpModel
    [DebuggerDisplay("{Prototype,nq}")];
    CLASS XVariable INHERIT XElement
        // Fields
        PRIVATE _isParameter AS Logic
        PRIVATE _typeName AS string
        STATIC INITONLY PUBLIC VarType := "$VAR$" AS string

        // Methods
         CONSTRUCTOR(parent AS XElement, name AS string, kind AS Kind, visibility AS Modifiers, span AS TextRange, position AS TextInterval, typeName AS string,  isParameter := FALSE AS Logic)
        SUPER(name, kind, Modifiers.None, visibility, span, position)
            //
            IF (String.IsNullOrEmpty(typeName))
                //
                typeName := "USUAL"
            ENDIF
            SELF:_typeName := typeName
            SELF:_isParameter := isParameter
            SUPER:Parent := parent


        // Properties
        VIRTUAL PROPERTY Description AS string
            GET
                //
				local str as string
                IF (SELF:_isParameter)
                    //
                    str := "PARAMETER "
                ELSE
                    //
                    str := "LOCAL "
                ENDIF
                var textArray1 := <string>{str, SELF:Prototype, " as ", SELF:TypeName, IIF(SELF:IsArray,"[]","")}
                RETURN String.Concat(textArray1)
            END GET
        END PROPERTY

        PROPERTY IsArray AS Logic AUTO 

        VIRTUAL PROPERTY Prototype AS string
            GET
                //
                RETURN SUPER:Name
            END GET
        END PROPERTY

        PROPERTY TypeName AS string
            GET
                //
                RETURN SELF:_typeName
            END GET
            SET
                //
                IF (String.IsNullOrEmpty(value))
                    //
                    value := "USUAL"
                ENDIF
                SELF:_typeName := value
            END SET
        END PROPERTY


    END CLASS

END NAMESPACE 

