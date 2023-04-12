//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Reflection
using System.Linq
using XSharp.RT
#pragma options("az", on)

/// <summary>
/// This class returns the ClasssObject for classes created at compile time
/// This object allows to access static members and methods late bound
/// Such as <code>Example():Fieldname</code>
/// </summary>
class XSharp.XPP.StaticClassObject implements ILateBound
    hidden type as System.Type

    CONSTRUCTOR(t AS System.Type)
        SELF:type := t

    /// <summary>
    /// Create a new instance of the class.
    /// </summary>
    /// <returns>new object</returns>
    /// <remarks>The compiler compiled <code>SomeClass():New(....)</code> directly into <code>SomeClass{....}</code> </remarks>
    METHOD New() AS OBJECT CLIPPER
        // This is normally not called. The compiler
        // converts Foo():New(...)
        // to Foo{....}
        RETURN _CreateInstance(type:FullName, _Args())

    /// <summary>
    /// Late bound access to class/static vars
    /// </summary>
    /// <param name="cName"></param>
    /// <returns></returns>
    METHOD NoIvarGet(cName AS STRING) AS USUAL
        VAR mem := OOPHelpers.GetMember(type, cName)
        IF mem != NULL
            IF mem IS FieldInfo VAR fld .AND. fld:IsStatic
                RETURN fld:GetValue(NULL)
            ENDIF
            IF mem IS PropertyInfo VAR  prop .AND. prop:CanRead .AND. prop:GetMethod:IsStatic
                RETURN prop:GetValue(NULL)
            ENDIF
        ENDIF
        FOREACH fld AS FieldInfo IN  type:GetFields()
            IF fld:IsStatic .AND. String.Compare(fld:Name, cName, TRUE) == 0
                OOPHelpers.AddMember(type, cName, fld)
                RETURN fld:GetValue(NULL)
            ENDIF
        NEXT
        FOREACH prop AS PropertyInfo IN  type:GetProperties()
            IF prop:CanRead .AND. prop:GetMethod:IsStatic .AND. String.Compare(prop:Name, cName, TRUE) == 0
                RETURN prop:GetValue(NULL)
            ENDIF
        NEXT
        VAR oError := Error.VOError( EG_NOVARMETHOD, type:Name, NAMEOF(cName), 2, <OBJECT>{type:Name, cName} )
        oError:Description := oError:Message+" '"+cName+"'"
        THROW oError

    /// <summary>
    /// Late bound assign for class/static vars
    /// </summary>
    /// <param name="cName"></param>
    /// <param name="uValue"></param>
    /// <returns></returns>
    METHOD NoIvarPut(cName AS STRING, uValue AS USUAL) AS VOID
        LOCAL oValue AS OBJECT
        VAR mem := OOPHelpers.GetMember(type, cName)
        IF mem != NULL
            IF mem IS FieldInfo VAR fld .AND. fld:IsStatic
                oValue := OOPHelpers.ValueConvert(uValue, fld:FieldType)
                fld:SetValue(NULL, oValue)
                RETURN
            ENDIF
            IF mem IS PropertyInfo VAR  prop .AND. prop:CanWrite .AND. prop:SetMethod:IsStatic
                oValue := OOPHelpers.ValueConvert(uValue, prop:PropertyType)
                prop:SetValue(NULL, (OBJECT) uValue)
                RETURN
            ENDIF
        ENDIF
        FOREACH fld AS FieldInfo IN  type:GetFields()
            IF fld:IsStatic .AND. String.Compare(fld:Name, cName, TRUE) == 0
                oValue := OOPHelpers.ValueConvert(uValue, fld:FieldType)
                fld:SetValue(NULL, oValue)
                OOPHelpers.AddMember(type, cName, fld)
                RETURN
            ENDIF
        NEXT
        FOREACH prop AS PropertyInfo IN  type:GetProperties()
            IF prop:CanWrite .AND. prop:SetMethod:IsStatic .AND. String.Compare(prop:Name, cName, TRUE) == 0
                oValue := OOPHelpers.ValueConvert(uValue, prop:PropertyType)
                prop:SetValue(NULL, (OBJECT) uValue)
                OOPHelpers.AddMember(type, cName, prop)
                RETURN
            ENDIF
        NEXT
        VAR oError := Error.VOError( EG_NOVARMETHOD, type:Name, NAMEOF(cName), 2, <OBJECT>{type:Name, cName, uValue} )
        oError:Description := oError:Message+" '"+cName+"'"
        THROW oError


    /// <summary>
    /// Late bound calls for Static/Class methods.
    /// </summary>
    /// <returns>Result of Method Call</returns>
    METHOD NoMethod() AS USUAL CLIPPER
        // Lookup class method and call it.
        VAR cMethod := XSharp.RT.Functions.NoMethod()
        VAR uArgs := _Args()
        VAR overloads := OOPHelpers.FindOverloads(type, cMethod, FALSE):ToArray()
        VAR mi  := OOPHelpers.FindBestOverLoad<MethodInfo>(overloads, cMethod, uArgs)
        IF mi != NULL
            IF OOPHelpers.SendHelper(NULL, mi, uArgs, OUT VAR result)
                RETURN result
            ENDIF
        ENDIF
        VAR oError := Error.VOError( EG_NOMETHOD, __FUNCTION__, NAMEOF(cMethod), 2, <OBJECT>{NULL, cMethod, uArgs} )
        THROW oError

END CLASS
