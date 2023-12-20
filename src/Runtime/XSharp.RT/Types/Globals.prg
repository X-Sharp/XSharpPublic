//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.Collections.Concurrent
USING System.Collections.Generic

/// <summary>
/// Support class to support runtime access to globals declared in the loaded X# assemblies
/// </summary>
CLASS XSharp.Globals
    PRIVATE STATIC Cache AS ConcurrentDictionary<Assembly, ConcurrentDictionary<STRING, FieldInfo> >
    STATIC CONSTRUCTOR()
        Cache := ConcurrentDictionary<Assembly, ConcurrentDictionary<STRING, FieldInfo> >{}

    PRIVATE STATIC METHOD LoadAssemblies() AS LOGIC
        LOCAL newAsm AS LOGIC
        newAsm := FALSE
        FOREACH asm AS Assembly IN  AppDomain.CurrentDomain:GetAssemblies()
            IF Cache:ContainsKey(asm)
                LOOP
            ENDIF
            newAsm := TRUE
            VAR aDict := ConcurrentDictionary<STRING, FieldInfo>{StringComparer.OrdinalIgnoreCase}
            VAR att := TYPEOF( XSharp.Internal.ClassLibraryAttribute )
            IF asm:IsDefined(  att, FALSE )
                LOCAL cFunctionClass AS STRING
                FOREACH VAR attribute IN asm:GetCustomAttributes(att,FALSE)
                    VAR cla := (XSharp.Internal.ClassLibraryAttribute) attribute
                    IF !String.IsNullOrEmpty(cla:GlobalClassName)
                        cFunctionClass := cla:GlobalClassName
                        LOCAL oType := asm:GetType(cFunctionClass,FALSE, TRUE) AS System.Type
                        IF oType != NULL
                            LOCAL aFields := oType:GetFields() AS FieldInfo[]
                            FOREACH oFld AS FieldInfo IN aFields
                                IF oFld:IsStatic .AND. ;
                                        ! oFld:Attributes:HasFlag(FieldAttributes.Literal) .AND. ;          // DEFINES
                                        ! oFld:Attributes:HasFlag(FieldAttributes.InitOnly) .AND. ;         // DEFINES
                                        oFld:IsPublic
                                    aDict:TryAdd(oFld:Name, oFld)
                                ENDIF
                            NEXT
                        ENDIF
                    ENDIF
                NEXT
            ENDIF
            // when no fields then add the assembly with a NULL dictionary
            IF aDict:Count > 0
                Cache:TryAdd(asm, aDict)
            ELSE
                Cache:TryAdd(asm, NULL)
            ENDIF
        NEXT
        RETURN newAsm
    /// <summary>
    /// Find the Global Field with a name
    /// </summary>
    /// <param name="cVarName">Name of the global to find</param>
    /// <returns></returns>
    STATIC METHOD Find(cVarName AS STRING) AS FieldInfo
        IF Cache:Count > 0
            FOREACH VAR pair IN Cache
                IF pair:Value != NULL .AND. pair:Value:TryGetValue(cVarName, OUT VAR res)
                    RETURN res
                ENDIF
            NEXT
        ENDIF
        // Load Assemblies and try again when there are new assemblies
        IF LoadAssemblies()
            FOREACH VAR pair IN Cache
                IF pair:Value != NULL .AND. pair:Value:TryGetValue(cVarName, OUT VAR res)
                    RETURN res
                ENDIF
            NEXT
        ENDIF
        RETURN NULL_OBJECT
    /// <summary>
    /// Return a list of all Global Fields. This is used in the Globals Debugger Window
    /// </summary>
    /// <returns>List of Globals</returns>
    STATIC METHOD GetAllGlobals() AS IList<FieldInfo>
        LoadAssemblies()
        VAR result := List<FieldInfo>{}
        FOREACH VAR pair IN Cache
            IF pair:Value != NULL
                result:AddRange(pair:Value:Values)
            ENDIF
        NEXT
        RETURN result
    /// <summary>
    /// Return the value of a Global
    /// </summary>
    /// <param name="cVarName">Name of the global to find</param>
    /// <param name="uValue">Value</param>
    /// <returns>TRUE when a global with the name exists, FALSE when it does not exist</returns>
    STATIC METHOD Get(cVarName AS STRING, uValue OUT USUAL) AS LOGIC
        uValue := NIL
        VAR oFld := Globals.Find(cVarName)
        IF (oFld != NULL_OBJECT)
            TRY
                LOCAL oValue AS OBJECT
                oValue := oFld:GetValue(NULL)
                uValue := oValue
                RETURN TRUE
            CATCH
                RETURN FALSE
            END TRY
        ENDIF
        RETURN FALSE
    /// <summary>
    /// Assign a value to a Global
    /// </summary>
    /// <param name="cVarName">Name of the global to assign</param>
    /// <param name="uValue">New value</param>
    /// <returns>TRUE when a global with the name exists, and the value was successfully assigned. FALSE when it does not exist or when there was an error assigning the value.,</returns>
    STATIC METHOD Put(cVarName AS STRING, uValue AS USUAL) AS LOGIC
        VAR oFld := Globals.Find(cVarName)
        IF (oFld != NULL_OBJECT)
            TRY
                LOCAL oValue AS OBJECT
                oValue := uValue
                oValue := OOPHelpers.ValueConvert(oValue, oFld:FieldType)
                oFld:SetValue(NULL, oValue)
                RETURN TRUE
            CATCH
                RETURN FALSE
            END TRY
        ENDIF
        RETURN FALSE
END CLASS
