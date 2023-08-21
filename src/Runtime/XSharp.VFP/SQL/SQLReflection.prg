//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that the VFP Docs speak about "Statement Handles" for connections and also 
// about "statement handles" for statements. These are returned for example by SqlPrepare
// In this code we speak about "connection handles" and "statement handles".



USING System
USING System.Reflection



INTERNAL STATIC CLASS XSharp.VFP.SQLReflection
    #region Find Metadata
    STATIC METHOD GetMemberInType(oType AS System.Type,cMemberName AS STRING, memberType AS MemberTypes) AS MemberInfo
        VAR aMembers := oType:GetMember(cMemberName)
        FOREACH VAR oMember IN aMembers
            IF oMember:MemberType == memberType 
                RETURN oMember
            ENDIF
        NEXT
        VAR oMembers := oType:GetMembers(BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.FlattenHierarchy)
        FOREACH VAR oMember IN oMembers
            IF oMember:MemberType == memberType .AND. String.Compare(oMember:Name, cMemberName, TRUE) == 0
                RETURN oMember
            ENDIF
        NEXT
        RETURN NULL

    STATIC METHOD GetMember(oObject AS OBJECT, cMemberName AS STRING, memberType AS MemberTypes) AS MemberInfo
        IF oObject == NULL
            RETURN NULL
        ENDIF
        VAR oType    := oObject:GetType()
        VAR oMember  := GetMemberInType(oType, cMemberName, memberType)
        IF oMember != NULL
            RETURN oMember
        ENDIF
        oType := oType:UnderlyingSystemType
        oMember  := GetMemberInType(oType, cMemberName, memberType)
        RETURN oMember        
        

    STATIC METHOD  GetProperty(oObject AS OBJECT, cName AS STRING) AS PropertyInfo
        VAR oProp := GetMember(oObject, cName, MemberTypes.Property)
        IF oProp != NULL
            RETURN (PropertyInfo) oProp
        ENDIF
        RETURN NULL
        
    STATIC METHOD GetMethod(oObject AS OBJECT, cName AS STRING) AS MethodInfo
        VAR oMethod := GetMember(oObject, cName, MemberTypes.Method)
        IF oMethod != NULL
            RETURN (MethodInfo) oMethod
        ENDIF
        RETURN NULL

   STATIC METHOD GetField(oObject AS OBJECT, cName AS STRING) AS FieldInfo
        VAR oField := GetMember(oObject, cName, MemberTypes.Field)
        IF oField != NULL
            RETURN (FieldInfo) oField
        ENDIF
        RETURN NULL
   #endregion
    #region Get/Set methods
    STATIC METHOD GetFieldValue(oObject AS OBJECT, cName AS STRING, oValue OUT OBJECT) AS LOGIC
        oValue := NULL
        VAR oField := GetField(oObject, cName)
        IF oField != NULL
            oValue := oField:GetValue(oObject)
        ENDIF
        RETURN oField != NULL        

    STATIC METHOD GetPropertyValue(oObject AS OBJECT, cName AS STRING, oValue OUT OBJECT) AS LOGIC
        oValue := NULL
        VAR oProp := GetProperty(oObject, cName)
        IF oProp != NULL
            oValue := oProp:GetValue(oObject)
        ENDIF
        RETURN oProp != NULL        

    STATIC METHOD SetPropertyValue(oObject AS OBJECT, cName AS STRING, oValue AS OBJECT) AS LOGIC
        VAR oProp := GetProperty(oObject, cName)
        IF oProp != NULL .AND. oProp:CanWrite
            oProp:SetValue(oObject, oValue, NULL)
        ENDIF
        RETURN oProp != NULL .AND. oProp:CanWrite

    STATIC METHOD SetFieldValue(oObject AS OBJECT, cName AS STRING, oValue AS OBJECT) AS LOGIC
        VAR oField := GetField(oObject, cName)
        IF oField != NULL .AND. ! oField:IsInitOnly .AND. ! oField:IsLiteral
            oField:SetValue(oObject, oValue)
        ENDIF
        RETURN oField != NULL .AND. ! oField:IsInitOnly .AND. ! oField:IsLiteral

    STATIC METHOD InvokeMethod(oObject AS OBJECT, cName AS STRING, oValue OUT OBJECT, aParams PARAMS OBJECT[] ) AS LOGIC 
        oValue := NULL
        VAR oMethod := GetMethod(oObject, cName)
        IF oMethod != NULL 
            oValue := oMethod:Invoke(oObject, aParams)
        ENDIF
        RETURN oMethod != NULL
    #endregion
END CLASS

