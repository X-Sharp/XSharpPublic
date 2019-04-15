//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Reflection
USING System.Reflection.Emit
USING XSharp.XPP
/// <summary>Create a class dynamically.</summary>
/// <returns>The class object.</returns>
FUNCTION ClassCreate(cClassName AS STRING, aSuperClasses:= NULL_ARRAY AS ARRAY, aMember:= NULL_ARRAY AS ARRAY, aMethod:= NULL_ARRAY AS ARRAY) AS USUAL
    // in XPP the superclass can all be classes
    // we support one superclass and interfaces
    // the members array contains sub arrays with per field a name and attribute
    // The attribute contains a visibility aspect (CLASS_HIDDEN ,CLASS_PROTECTED, CLASS_EXPORTED )
    // as well as assignment rights( VAR_ASSIGN_HIDDEN, VAR_ASSIGN_PROTECTED,VAR_ASSIGN_EXPORTED)
    // and types of instance vars (VAR_INSTANCE, VAR_CLASS , VAR_CLASS_SHARED)
    // The methods array contains for each method a name, attribute and codeblock
    // The attribute contains a visibility aspect (CLASS_HIDDEN ,CLASS_PROTECTED, CLASS_EXPORTED )
    // and types of method (METHOD_INSTANCE, METHOD_CLASS )
    // and assignment property (METHOD_ACCESS, METHOD_ASSIGN)
    // INIT method should be mapped to the constructor, with clipper calling convention
    /*
      // Return class object for database in the current work area 
      // and create it if necessary. 

      FUNCTION DbRecord() 
      oClass := ClassObject( Alias() ) 
      IF oClass <> NIL 
         RETURN oClass                 // Class already exists 
      ENDIF 
      nAttr   := CLASS_EXPORTED + VAR_INSTANCE 
      aIVar   := AEval( DbStruct(), {|a| a:={a[1], nAttr} } ,,, .T.) 
      nAttr   := CLASS_EXPORTED + METHOD_INSTANCE 
      aMethod := {{ "INIT" , nAttr, {|self| GetRecord(self) } }, ; 
                  { "GET"  , nAttr, {|self| GetRecord(self) } }, ; 
                  { "PUT"  , nAttr, {|self| PutRecord(self) } }  } 
 

      // Method with parameter according to obj:skip( n ) 
      bSkip   := {|self,n| DbSkip(n), ::get() } 
      AAdd( aMethod, { "SKIP" , nAttr, bSkip } ) 
      RETURN ClassCreate( Alias(),, aIVar, aMethod )

      STATIC FUNCTION GetRecord( oRecord ) 
        AEval( DbStruct(), {|a,i| oRecord:&(a[1]) := FieldGet(i) } ) 
        RETURN oRecord

    STATIC FUNCTION PutRecord( oRecord ) 
      LOCAL lLocked := RLock() 
      IF lLocked 
         AEval( DbStruct(), {|a,i| FieldPut(i, oRecord:&(a[1])) } ) 
         DbUnlock() 
      ENDIF 
      RETURN lLocked 

    */
   RETURN NULL

/// <summary>Retrieves the class object of class. </summary>
/// <param name="cClassName">The name of the class whose class object should be returned. <param>
FUNCTION ClassObject(cClassName AS STRING) AS ClassObject
    LOCAL oResult AS ClassObject
    oResult := ClassObject.FindClass(cClassName, FALSE)
    RETURN oResult

/// <summary>Remove the class object of class. </summary>
/// <param name="cName">The name of the class whose class object should be deleted.<param>
/// <returns>The return value is .T. (true) when the class object is removed from memory, otherwise it is .F. (false). </returns>
FUNCTION ClassDestroy(cName AS STRING) AS LOGIC
    LOCAL oObject AS ClassObject
    oObject := ClassObject(cName)
    IF oObject != NULL
        RETURN ClassDestroy(oObject)
    ENDIF
    RETURN FALSE

/// <summary>Remove the class object of class. </summary>
/// <param name="oObject">The name of the class whose class object should be deleted.<param>
/// <returns>The return value is .T. (true) when the class object is removed from memory, otherwise it is .F. (false). </returns>
FUNCTION ClassDestroy(oObject AS ClassObject) AS LOGIC
    RETURN ClassObject.DeleteClass(oObject)

CLASS XSharp.XPP.ClassObject

    STATIC Classes   AS Dictionary<STRING, ClassObject>
    STATIC Graveyard AS Dictionary<STRING, ClassObject>

    STATIC CONSTRUCTOR
        Classes    := Dictionary<STRING, ClassObject>{StringComparer.OrdinalIgnoreCase}
        Graveyard  := Dictionary<STRING, ClassObject>{StringComparer.OrdinalIgnoreCase}

    STATIC METHOD FindClass(cClassName AS STRING, lIncludeDeleted AS LOGIC) AS ClassObject
        IF Classes:ContainsKey(cClassName)
            RETURN Classes[cClassName]
        ENDIF
        IF lIncludeDeleted .AND. Graveyard:ContainsKey(cClassName)
            RETURN Graveyard[cClassName]
        ENDIF
        RETURN NULL

    STATIC METHOD DeleteClass(classObject AS ClassObject) AS LOGIC
        LOCAL lOk := FALSE AS LOGIC
        IF Classes:ContainsKey(classObject:Name)
            lOk := TRUE
            IF ! Graveyard:ContainsKey(classObject:Name)
                GraveYard:Add(classObject:Name, classObject)
            ENDIF
        ENDIF
        RETURN lOk
    
        

    PROTECTED _type AS Type
    PROTECTED _name AS STRING
    PROPERTY Name AS STRING GET _name

    CONSTRUCTOR(oType AS Type, cName AS STRING)
        _type := oType
        _name := cName

    METHOD New(aParams PARAMS USUAL[]) AS OBJECT
        LOCAL oRes AS OBJECT
        LOCAL oParams AS OBJECT[]
        IF aParams:Length > 0
            oParams := _UsualArrayToObjectArray(aParams)
        ELSE
            oParams := OBJECT[]{ 0 }
        ENDIF
        oRes := Activator.CreateInstance(_type, oParams)
        RETURN oRes

    //STATIC DynAssembly AS Assembly
    //STATIC DynModule   AS Module

    INTERNAL METHOD CreateType() AS TypeBuilder
        RETURN NULL

    INTERNAL METHOD CreateField(tb AS TypeBuilder, cName AS STRING, nVis AS LONG) AS FieldBuilder
        RETURN NULL
    INTERNAL METHOD CreateMethod(tb AS TypeBuilder, aParams AS ARRAY, cName AS STRING, nVis AS LONG, bBody AS CODEBLOCK) AS MethodBuilder
        RETURN NULL
    INTERNAL METHOD CreateConstructor(tb AS TypeBuilder, aParams AS ARRAY, nVis AS LONG, bBody AS CODEBLOCK) AS ConstructorBuilder
        RETURN NULL


END CLASS


/*
using System;
using System.Reflection;
using System.Reflection.Emit;

namespace TypeBuilderNamespace
{
    public static class MyTypeBuilder
    {
        public static void CreateNewObject()
        {
            var myType = CompileResultType();
            var myObject = Activator.CreateInstance(myType);
        }
        public static Type CompileResultType()
        {
            TypeBuilder tb = GetTypeBuilder();
            ConstructorBuilder constructor = tb.DefineDefaultConstructor(MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.RTSpecialName);

            // NOTE: assuming your list contains Field objects with fields FieldName(string) and FieldType(Type)
            foreach (var field in yourListOfFields)
                CreateProperty(tb, field.FieldName, field.FieldType);

            Type objectType = tb.CreateType();
            return objectType;
        }

        private static TypeBuilder GetTypeBuilder()
        {
            var typeSignature = "MyDynamicType";
            var an = new AssemblyName(typeSignature);
            AssemblyBuilder assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(an, AssemblyBuilderAccess.Run);
            ModuleBuilder moduleBuilder = assemblyBuilder.DefineDynamicModule("MainModule");
            TypeBuilder tb = moduleBuilder.DefineType(typeSignature,
                    TypeAttributes.Public |
                    TypeAttributes.Class |
                    TypeAttributes.AutoClass |
                    TypeAttributes.AnsiClass |
                    TypeAttributes.BeforeFieldInit |
                    TypeAttributes.AutoLayout,
                    null);
            return tb;
        }

        private static void CreateProperty(TypeBuilder tb, string propertyName, Type propertyType)
        {
            FieldBuilder fieldBuilder = tb.DefineField("_" + propertyName, propertyType, FieldAttributes.Private);

            PropertyBuilder propertyBuilder = tb.DefineProperty(propertyName, PropertyAttributes.HasDefault, propertyType, null);
            MethodBuilder getPropMthdBldr = tb.DefineMethod("get_" + propertyName, MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig, propertyType, Type.EmptyTypes);
            ILGenerator getIl = getPropMthdBldr.GetILGenerator();

            getIl.Emit(OpCodes.Ldarg_0);
            getIl.Emit(OpCodes.Ldfld, fieldBuilder);
            getIl.Emit(OpCodes.Ret);

            MethodBuilder setPropMthdBldr =
                tb.DefineMethod("set_" + propertyName,
                  MethodAttributes.Public |
                  MethodAttributes.SpecialName |
                  MethodAttributes.HideBySig,
                  null, new[] { propertyType });

            ILGenerator setIl = setPropMthdBldr.GetILGenerator();
            Label modifyProperty = setIl.DefineLabel();
            Label exitSet = setIl.DefineLabel();

            setIl.MarkLabel(modifyProperty);
            setIl.Emit(OpCodes.Ldarg_0);
            setIl.Emit(OpCodes.Ldarg_1);
            setIl.Emit(OpCodes.Stfld, fieldBuilder);

            setIl.Emit(OpCodes.Nop);
            setIl.MarkLabel(exitSet);
            setIl.Emit(OpCodes.Ret);

            propertyBuilder.SetGetMethod(getPropMthdBldr);
            propertyBuilder.SetSetMethod(setPropMthdBldr);
        }
    }
}
// using DynamicObject
public class DynamicClass : DynamicObject
{
    private Dictionary<string, KeyValuePair<Type, object>> _fields;

    public DynamicClass(List<Field> fields)
    {
        _fields = new Dictionary<string, KeyValuePair<Type, object>>();
        fields.ForEach(x => _fields.Add(x.FieldName,
            new KeyValuePair<Type, object>(x.FieldType, null)));
    }

    public override bool TrySetMember(SetMemberBinder binder, object value)
    {
        if (_fields.ContainsKey(binder.Name))
        {
            var type = _fields[binder.Name].Key;
            if (value.GetType() == type)
            {
                _fields[binder.Name] = new KeyValuePair<Type, object>(type, value);
                return true;
            }
            else throw new Exception("Value " + value + " is not of type " + type.Name);
        }
        return false;
    }

    public override bool TryGetMember(GetMemberBinder binder, out object result)
    {
        result = _fields[binder.Name].Value;
        return true;
    }
}

*/
