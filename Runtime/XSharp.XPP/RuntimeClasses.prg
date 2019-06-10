// RuntimeClasses.prg
// Created by    : robert
// Creation Date : 4/30/2019 9:29:24 AM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Reflection
USING System.Reflection.Emit
USING XSharp.XPP
USING System.Diagnostics


/// <summary>The worker class to help create classes at runtime.</summary>
[DebuggerDisplay("RuntimeClass {Name}")];
CLASS XSharp.XPP.ClassObject 
   #region Helper classes
    [DebuggerDisplay("FieldDescriptor {Name}")];
    INTERNAL CLASS FieldDescriptor
        INTERNAL PROPERTY Name       AS STRING AUTO
        INTERNAL PROPERTY Type       AS System.Type AUTO
        INTERNAL PROPERTY Attributes AS FieldAttributes AUTO
    END CLASS

    [DebuggerDisplay("MethodDescriptor {Name}")];
    INTERNAL CLASS MethodDescriptor
        INTERNAL PROPERTY Name       AS STRING AUTO
        INTERNAL PROPERTY Type       AS System.Type AUTO
        INTERNAL PROPERTY Attributes AS MethodAttributes AUTO
        INTERNAL PROPERTY Block      AS CODEBLOCK AUTO
        INTERNAL PROPERTY Varname    AS STRING AUTO
        INTERNAL PROPERTY Getter     AS LOGIC AUTO
        INTERNAL PROPERTY Setter     AS LOGIC AUTO
    END CLASS

    [DebuggerDisplay("ClassDescriptor {Name}")];
    INTERNAL CLASS ClassDescriptor
        INTERNAL PROPERTY Name       AS STRING AUTO
        INTERNAL PROPERTY SuperClass AS STRING AUTO
        INTERNAL PROPERTY Interfaces AS STRING[] AUTO
        INTERNAL PROPERTY Fields     AS FieldDescriptor[] AUTO
        INTERNAL PROPERTY Methods    AS MethodDescriptor[] AUTO
        INTERNAL PROPERTY Properties AS MethodDescriptor[] AUTO
        INTERNAL PROPERTY HasInit    AS LOGIC AUTO
        INTERNAL METHOD GetUniqueHashCode AS INT
            LOCAL nCode AS INT
            nCode := SELF:Name:GetHashCode() + SELF:SuperClass:GetHashCode()
            BEGIN UNCHECKED
                FOREACH VAR fld IN Fields
                    nCode += fld:Name:GetHashCode() + fld:Attributes:GetHashCode()
                NEXT
                FOREACH VAR meth IN Methods
                    nCode += meth:Name:GetHashCode() + meth:Attributes:GetHashCode()
                NEXT
                FOREACH VAR prop IN Properties
                    nCode += prop:Name:GetHashCode() + prop:Attributes:GetHashCode()
                NEXT
            END UNCHECKED
            RETURN nCode


        INTERNAL METHOD GetPropertyGetBlock(cName AS STRING) AS CODEBLOCK
            FOREACH oMethod AS MethodDescriptor IN SELF:Properties
                IF string.Compare(oMethod:Name, cName, TRUE) == 0 .AND. oMethod:Getter
                    RETURN oMethod:Block
                ENDIF
            NEXT
            RETURN NULL_CODEBLOCK

        INTERNAL METHOD GetPropertySetBlock(cName AS STRING) AS CODEBLOCK
            FOREACH oMethod AS MethodDescriptor IN SELF:Properties
                IF string.Compare(oMethod:Name, cName, TRUE) == 0 .AND. oMethod:Setter
                    RETURN oMethod:Block
                ENDIF
            NEXT
            RETURN NULL_CODEBLOCK


        INTERNAL METHOD GetMethodBlock(cName AS STRING) AS CODEBLOCK
            FOREACH oMethod AS MethodDescriptor IN SELF:Methods
                IF string.Compare(oMethod:Name, cName, TRUE) == 0
                    RETURN oMethod:Block
                ENDIF
            NEXT
            RETURN NULL_CODEBLOCK

        INTERNAL CONSTRUCTOR (cName AS STRING)
            Name := cName
    
    END CLASS
    #endregion
    #region Fields and Properties
    PRIVATE _type AS Type
    PRIVATE _name AS STRING
    PRIVATE _desc AS ClassDescriptor
    PROPERTY Name AS STRING GET _name
    INTERNAL PROPERTY Descriptor AS  ClassDescriptor GET _desc
    PROPERTY Type  AS System.Type GET _type
    #endregion
    PRIVATE CONSTRUCTOR(oType AS Type, cName AS STRING, oDesc  AS ClassDescriptor)
        _type := oType
        _name := cName
        _desc := oDesc

    METHOD New() AS OBJECT CLIPPER
        LOCAL oRes AS OBJECT
        oRes := Activator.CreateInstance(_type)
        IF SELF:Descriptor:HasInit
            LOCAL oBlock := SELF:Descriptor:GetMethodBlock("INIT") AS CODEBLOCK
            // The pseudo function _ARGS() returns the Clipper arguments array
            EvalBlock(oBlock, oRes, _ARGS())
        ENDIF
        RETURN oRes

    #region Class Helper methods
    PRIVATE STATIC Classes     AS Dictionary<STRING, ClassObject>
    PRIVATE STATIC OldClasses  AS Dictionary<INT, ClassObject>
    
    STATIC CONSTRUCTOR
        Classes    := Dictionary<STRING, ClassObject>{StringComparer.OrdinalIgnoreCase}
        OldClasses := Dictionary<INT, ClassObject>{}
	
    STATIC METHOD FindClass(cClassName AS STRING, lIncludeDeleted AS LOGIC) AS ClassObject
        IF Classes:ContainsKey(cClassName)
            RETURN Classes[cClassName]
        ENDIF
        RETURN NULL

    STATIC METHOD DeleteClass(classObject AS ClassObject) AS LOGIC
        LOCAL lOk := FALSE AS LOGIC
        IF Classes:ContainsKey(classObject:Name)
            lOk := TRUE
            VAR nCode := classObject:Descriptor:GetUniqueHashCode()
            IF ! OldClasses:ContainsKey(nCode)
                OldClasses:Add(nCode, classObject)
            ENDIF
            Classes:Remove(classObject:Name)
        ENDIF
        RETURN lOk

    STATIC METHOD GetClassObject(oObject AS OBJECT) AS ClassObject
        LOCAL oType AS System.Type
        IF oObject == NULL
            RETURN NULL_OBJECT
        ENDIF
        oType := oObject:GetType()
        FOREACH VAR element IN Classes
            IF element:Value:Type == oType
                RETURN element:Value
            ENDIF
        NEXT
        RETURN NULL_OBJECT

    STATIC METHOD  IsInstanceofRuntimeClass(oObject AS OBJECT) AS LOGIC
        RETURN GetClassObject(oObject) != NULL_OBJECT

    OVERRIDE METHOD HasIVar(cVar AS STRING) AS LOGIC
        LOCAL oClass AS ClassObject
        oClass := GetClassObject(SELF)
        IF oClass != NULL
            IF oClass:Descriptor:GetPropertyGetBlock(cVar) != NULL .OR. ;
                oClass:Descriptor:GetPropertySetBlock(cVar) != NULL
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN IVarGetInfo(SELF, cVar) != 0

    INTERNAL STATIC METHOD CallIVarGet(oObject AS OBJECT, cName AS STRING) AS USUAL
        LOCAL oClass AS ClassObject
        oClass := GetClassObject(oObject)
        IF oClass != NULL_OBJECT
            VAR oDesc := oClass:Descriptor
            LOCAL oBlock AS CODEBLOCK
            oBlock := oDesc:GetPropertyGetBlock(cName)
            IF oBlock != NULL_CODEBLOCK
                RETURN EvalBlock(oBlock, oObject, NULL)
            ENDIF
        ENDIF
        VAR oerror := Error.VOError( EG_NOVAR, __ENTITY__, nameof(cName), 2, <OBJECT>{oObject, cName} )
        oError:Description  := oError:Message + " '"+cName+"'"
        THROW oError

    INTERNAL STATIC METHOD CallIVarPut(oObject AS OBJECT, cName AS STRING, uValue AS USUAL) AS USUAL
        LOCAL oClass AS ClassObject
        oClass := GetClassObject(oObject)
        IF oClass != NULL_OBJECT
            VAR oDesc := oClass:Descriptor
            LOCAL oBlock AS CODEBLOCK
            oBlock := oDesc:GetPropertySetBlock(cName)
            IF oBlock != NULL_CODEBLOCK
                RETURN EvalBlock(oBlock, oObject, uValue)
            ENDIF
        ENDIF
        VAR oerror := Error.VOError( EG_NOVAR, __ENTITY__, nameof(cName), 2, <OBJECT>{oObject, cName, uValue} )
        oError:Description  := oError:Message + " '"+cName+"'"
        THROW oError


    INTERNAL STATIC METHOD CallMethod(oObject AS OBJECT, cName AS STRING, uParams AS USUAL[]) AS USUAL
        LOCAL oClass AS ClassObject
        oClass := GetClassObject(oObject)
        IF oClass != NULL_OBJECT
            VAR oDesc := oClass:Descriptor
            LOCAL oBlock AS CODEBLOCK
            oBlock := oDesc:GetMethodBlock(cName)
            IF oBlock != NULL_CODEBLOCK
                RETURN EvalBlock(oBlock, oObject, uParams)
            ENDIF
        ENDIF
        VAR oerror := Error.VOError( EG_NOMETHOD, __ENTITY__, nameof(cName), 2, <OBJECT>{oObject, cName, uParams} )
        oError:Description  := oError:Message + " '"+cName+"'"
        THROW oError

    INTERNAL STATIC METHOD EvalBlock(oBlock AS CODEBLOCK, oObject AS OBJECT, uParams PARAMS USUAL[]) AS USUAL
        LOCAL uNewParams AS USUAL[]
        IF uParams != NULL
            uNewParams := USUAL[]{uParams:Length+1}
        ELSE
            uNewParams := USUAL[]{1}
        ENDIF
        uNewParams[1] := oObject
        IF uParams != NULL .AND. uParams:Length > 0
            System.Array.Copy( uParams, 0, uNewParams, 1, uParams:Length )
        ENDIF
        RETURN eval(oBlock, uNewParams)


    INTERNAL STATIC an := NULL AS AssemblyName
    INTERNAL STATIC ab := NULL AS AssemblyBuilder
    INTERNAL STATIC mb := NULL AS ModuleBuilder
    INTERNAL STATIC METHOD GetDynamicModule() AS ModuleBuilder
        IF an == NULL .OR. ab == NULL .OR. mb == NULL
            an  := AssemblyName{"XSharp.XPP.DynamicClasses"}
            ab  := AppDomain.CurrentDomain:DefineDynamicAssembly(an, AssemblyBuilderAccess.Run)
            mb  := ab:DefineDynamicModule("MainModule")
        ENDIF
        RETURN mb


    INTERNAL STATIC METHOD CreateType(oDesc AS ClassDescriptor) AS TypeBuilder
        LOCAL tb AS TypeBuilder
        LOCAL ta AS TypeAttributes
        LOCAL parent := NULL AS System.Type
        LOCAL mb := NULL AS ModuleBuilder
        mb := GetDynamicModule()
        ta  := TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.AutoClass | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | TypeAttributes.AutoLayout
        IF ! String.IsNullOrEmpty(oDesc:Superclass)
            parent := XSharp.RT.Functions.FindClass(oDesc:Superclass)
            IF parent == NULL_OBJECT
                VAR oError := Error.VOError( EG_NOCLASS, __FUNCTION__, "SuperClass", 1,  <OBJECT>{oDesc:Superclass}  )
                oError:Description := oError:Message+" '"+oDesc:Superclass+"'"
                THROW oError
            ELSE
                IF !typeof(XSharp.XPP.Abstract):IsAssignableFrom(parent)
                    VAR oError := Error.VOError( EG_NOCLASS, __FUNCTION__, "SuperClass", 1,  <OBJECT>{oDesc:Superclass}  )
                    oError:Description :=" Class '"+oDesc:Superclass+"' must inherit from XSharp.XPP.Abstract"
                    THROW oError

                ENDIF
            ENDIF
        ELSE
            parent := typeof(XSharp.XPP.Abstract)
        ENDIF

        VAR ctor := typeof(DebuggerDisplayAttribute).GetConstructor(<Type> { typeof(STRING) })
        VAR arguments   := <OBJECT>{ oDesc:Name }
        VAR debuggerDisplay := CustomAttributeBuilder{ctor, arguments}
        
        LOCAL suffix := 0 AS INT
        DO WHILE TRUE
            VAR ns := IIF(suffix == 0, "XppDynamic", "XppDynamic"+ suffix:ToString())
            VAR name := ns+"."+oDesc:name
            TRY
                VAR existing := mb:FindTypes(FullNameTypeFilter, name)
                IF existing:Length > 0
                    suffix += 1
                    LOOP
                ENDIF
                tb  := mb:DefineType(name,ta, parent)
                EXIT
            CATCH
                // this should not happen since we are checking already
                suffix += 1
            END TRY
        ENDDO
        tb:SetCustomAttribute(debuggerDisplay)
        
        RETURN tb

    /// Delegate to filter types in the DynamicAssembly
    INTERNAL STATIC METHOD FullNameTypeFilter(t AS type, oParam AS OBJECT) AS LOGIC
        RETURN t:FullName == (STRING) oParam

    INTERNAL STATIC METHOD DecodeFieldAttributes(nAttrib AS LONG) AS FieldAttributes
        LOCAL attribute AS FieldAttributes
        IF _AND(nAttrib, CLASS_EXPORTED ) == CLASS_EXPORTED 
            attribute := FieldAttributes.Public
        ELSEIF _AND(nAttrib, CLASS_PROTECTED) == CLASS_PROTECTED
            attribute := FieldAttributes.Family 
        ELSEIF _AND(nAttrib, CLASS_HIDDEN) == CLASS_HIDDEN
            attribute := FieldAttributes.Private
        ELSE
            attribute := FieldAttributes.Public
        ENDIF
        IF _AND(nAttrib, VAR_CLASS) == VAR_CLASS
            attribute |= FieldAttributes.Static
        ELSEIF _AND(nAttrib, VAR_CLASS_SHARED) == VAR_CLASS_SHARED
            attribute |= FieldAttributes.Static
        ELSEIF _AND(nAttrib, VAR_INSTANCE) == VAR_INSTANCE
            // Instance attribute does not exist
            NOP
        ELSE
            NOP
        ENDIF
        /*
        IF _AND(nAttrib, VAR_ASSIGN_HIDDEN) == VAR_ASSIGN_HIDDEN
            NOP
        ELSEIF _AND(nAttrib, VAR_ASSIGN_PROTECTED) == VAR_ASSIGN_PROTECTED
            NOP
        ELSEIF _AND(nAttrib, VAR_ASSIGN_EXPORTED) == VAR_ASSIGN_EXPORTED
            NOP
        ELSE
            NOP
        ENDIF
        */  
        RETURN attribute

    INTERNAL STATIC METHOD DecodeMethodAttributes(nAttrib AS LONG) AS MethodAttributes
       LOCAL attribute AS MethodAttributes
        IF _AND(nAttrib, CLASS_EXPORTED ) == CLASS_EXPORTED 
            attribute := MethodAttributes.Public
        ELSEIF _AND(nAttrib, CLASS_PROTECTED) == CLASS_PROTECTED
            attribute := MethodAttributes.Family 
        ELSEIF _AND(nAttrib, CLASS_HIDDEN) == CLASS_HIDDEN
            attribute := MethodAttributes.Private
        ELSE
            attribute := MethodAttributes.Public
        ENDIF
        IF _AND(nAttrib, METHOD_CLASS) == METHOD_CLASS
            attribute |= MethodAttributes.Static
        ELSEIF _AND(nAttrib, METHOD_INSTANCE) == METHOD_INSTANCE
            // Instance attribute does not exist
            NOP
        ELSE
            NOP
        ENDIF
       
        RETURN attribute  
    #endregion
 
         

    // create the class by converting the parameters to a class descriptor
    INTERNAL STATIC METHOD CreateClassDescriptor(cClassName AS STRING, aSuperClasses:= NULL_ARRAY AS ARRAY, aMember:= NULL_ARRAY AS ARRAY, aMethod:= NULL_ARRAY AS ARRAY) AS ClassDescriptor
        LOCAL aFields  := List<FieldDescriptor>{} AS List<FieldDescriptor>
        LOCAL aMethods := List<MethodDescriptor>{} AS List<MethodDescriptor>
        LOCAL aProperties := List<MethodDescriptor>{} AS List<MethodDescriptor>
        LOCAL oSuper    AS OBJECT
        LOCAL oSuperType AS System.Type
        LOCAL aInterfaces := List<STRING>{} AS List<STRING>
        LOCAL cSuperClass := "" AS STRING
        IF String.IsNullOrEmpty(cClassName)
            THROW Error.ArgumentError("ClassCreate", nameof(cClassName),1, <OBJECT>{cClassName})
        ENDIF
        IF aSuperClasses != NULL_ARRAY
            FOR VAR nI := 1 TO alen(aSuperClasses)
                LOCAL uElement AS USUAL
                uElement := aSuperClasses[nI]
                IF IsString(uElement)
                    oSuper := XSharp.XPP.ClassObject.FindClass((STRING) uElement,FALSE)
                    IF oSuper == NULL
                        oSuper := XSharp.RT.Functions.FindClass((STRING) uElement)
                    ENDIF
                ELSEIF IsSymbol(uElement)
                    oSuper := XSharp.XPP.ClassObject.FindClass((SYMBOL) uElement,FALSE)
                ELSEIF IsObject(uElement)
                    oSuper := uElement
                ELSE
                    THROW Error.ArgumentError("ClassCreate", nameof(aSuperClasses),2, <OBJECT>{aSuperClasses})
                ENDIF
                IF oSuper != NULL
                    
                    IF oSuper IS XSharp.XPP.ClassObject VAR rtClass
                        cSuperClass := rtClass:Name
                    ELSE
                        IF oSuper IS System.Type
                            oSuperType := oSuper
                        ELSE
                            oSuperType := oSuper:GetType()
                        ENDIF
                        IF oSuperType:IsInterface
                            aInterFaces:Add(oSuperType:FullName)
                        ELSE
                            cSuperClass := oSuperType:FullName
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
        ENDIF
        IF aMember != NULL_ARRAY
            // check to see if each member is a 2 dimensional array, where the sub array has 2 or more 
            FOR VAR nI := 1 TO alen(aMember)
                LOCAL uElement := aMember[nI] AS USUAL
                LOCAL lError := FALSE AS LOGIC
                IF IsArray(uElement) .AND. aLen(uElement) >= 2
                    LOCAL aElement := uElement AS ARRAY
                    IF IsString(aElement[1]) .AND. IsNumeric(aElement[2])
                        LOCAL oField AS FieldDescriptor
                        oField := FieldDescriptor{}
                        oField:Name  := aElement[1]
                        oField:Attributes := DecodeFieldAttributes(aElement[2])
                        oField:Type  := typeof(USUAL)
                        aFields:Add(oField)
                    ELSE
                        lError := TRUE
                    ENDIF
                ELSE
                    lError := TRUE
                ENDIF
                IF lError
                    THROW Error.ArgumentError("ClassCreate", nameof(aMember),3, <OBJECT>{aMember})
                ENDIF
            NEXT
        ENDIF
        LOCAL hasInit := FALSE AS LOGIC
        IF aMethod != NULL_ARRAY
            // check to see if each method is a 2 dimensional array, where the sub array has 3 or 4 members
            FOR VAR nI := 1 TO alen(aMethod)
                LOCAL uElement := aMethod[nI] AS USUAL
                LOCAL lError := FALSE AS LOGIC
                IF IsArray(uElement) .AND. aLen(uElement) >= 3
                    LOCAL aElement := uElement AS ARRAY
                    IF IsString(aElement[1]) .AND. IsNumeric(aElement[2]) .AND. IsCodeBlock(aElement[3])
                        LOCAL oMethod AS MethodDescriptor
                        oMethod := MethodDescriptor{}
                        oMethod:Name        := aElement[1]
                        oMethod:Attributes  := DecodeMethodAttributes(aElement[2])
                        oMethod:Type        := typeof(USUAL)
                        oMethod:Block       := aElement[3]
                        oMethod:Getter := _AND(aElement[2], METHOD_ACCESS) == METHOD_ACCESS
                        oMethod:Setter := _AND(aElement[2], METHOD_ASSIGN) == METHOD_ASSIGN 
                        IF aLen(aElement) > 3 .AND. IsString(aElement[4])
                            oMethod:VarName := aElement[4]
                        ENDIF
                        IF oMethod:Getter .OR. oMethod:Setter
                            aProperties:Add(oMethod)
                        ELSE
                            aMethods:Add(oMethod)
                        ENDIF
                        IF string.Compare(oMethod:Name, "INIT",TRUE) == 0
                            hasInit := TRUE
                        ENDIF
                    ELSE
                        lError := TRUE
                    ENDIF
                ELSE
                    lError := TRUE
                ENDIF
                IF lError
                    THROW Error.ArgumentError("ClassCreate", nameof(aMethod),3, <OBJECT>{aMethod})
                ENDIF
            NEXT
        ENDIF
        VAR oClassDesc := ClassDescriptor{cClassName}
        oClassDesc:Fields     := aFields:ToArray()
        oClassDesc:Methods    := aMethods:ToArray()
        oClassDesc:Properties := aProperties:ToArray()
        oClassDesc:SuperClass := cSuperClass
        oClassDesc:Interfaces := aInterfaces:ToArray()
        oClassDesc:HasInit    := hasInit
        RETURN oClassDesc

    // Use the class descriptor to create the class
    STATIC INTERNAL METHOD ImplementClass(oDesc AS ClassDescriptor) AS ClassObject
        LOCAL cName AS STRING
        LOCAL oResult AS ClassObject

        cName := oDesc:Name
        IF ClassObject.Classes:ContainsKey(cName)
            THROW Error{"A Class definition for "+cName+" already exists with another structure"}
        ENDIF
        // fetch the type from the old classes list when it exists
        LOCAL nCode := oDesc:GetUniqueHashCode() AS LONG
        IF OldClasses:ContainsKey(nCode)
            LOCAL oldClass := OldClasses[nCode] AS ClassObject
            IF oldClass:Descriptor:Name == oDesc:Name
                Classes:Add(cName, oldClass)
                RETURN oldClass
            ENDIF
        ENDIF


        LOCAL oTb AS TypeBuilder
        oTb := CreateType(oDesc)
        FOREACH VAR oFld IN oDesc:Fields
            oTb:DefineField(oFld:Name, oFld:Type, oFld:Attributes)
        NEXT
        VAR type := oTb:CreateType()
        oResult := ClassObject{type, oDesc:Name, oDesc}
        Classes:Add(oDesc:Name, oResult)
        RETURN oResult
END CLASS



/// <summary>Retrieves the class object of class. </summary>
/// <param name="cClassName">The name of the class whose class object should be returned. </param>
/// <returns>The function returns the class object of the class with the name &lt;cClassName&gt;. The return value is NULL_OBJECT when the class does not exist. </returns>
FUNCTION ClassObject(cClassName AS STRING) AS ClassObject
    LOCAL oResult AS ClassObject
    oResult := ClassObject.FindClass(cClassName, FALSE)
    RETURN oResult


/// <summary>Remove the class object of class. </summary>
/// <param name="uObject">The name of the class whose class object should be deleted, or the class object.</param>
/// <returns>The return value is .T. (true) when the class object is removed from memory, otherwise it is .F. (false). </returns>
/// <remarks>The function ClassDestroy() removes the class object of a dynamically created class from main memory.
/// Dynamic classes are created during runtime by the ClassCreate() function. They are unknown at compile time.
/// Therefore, they do not have a class function and are represented at runtime of a program only by a class object. <br/>
/// When a program uses a dynamic class, the corresponding class object should be removed from main memory when the class
/// is no longer needed. Otherwise, the class object remains in memory and can be retrieved by the ClassObject() function at any time.<br/>
/// <b>Note</b>In X# classes are never really freed from memory. The .Net framework does not allow that. ClassDestroy() does remove the class
/// from the list of active classes. If you recreate the same class later with the same structure then the class definition from the previous
/// defintion is reused.</remarks>
FUNCTION ClassDestroy(uObject) AS LOGIC CLIPPER
    LOCAL oObject AS OBJECT
    IF IsObject(uObject)
        oObject := uObject
        IF oObject IS ClassObject VAR rtClass
            RETURN ClassObject.DeleteClass(rtClass)
        ENDIF
    ELSEIF IsString(uObject)
        oObject := ClassObject((STRING) uObject)
        IF oObject != NULL
            RETURN ClassDestroy(oObject)
        ENDIF
    ENDIF
    RETURN FALSE

/// <summary>Create a class dynamically.</summary>
/// <returns>The class object.</returns>
FUNCTION ClassCreate(cClassName , aSuperClasses , aMember , aMethod ) AS USUAL CLIPPER
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
    LOCAL oClass AS XSharp.XPP.ClassObject.ClassDescriptor
    EnforceType(REF cClassName, STRING)
    EnForceType(REF aSuperClasses, ARRAY)
    EnForceType(REF aMember, ARRAY)
    EnForceType(REF aMethod, ARRAY)
    oClass := ClassObject.CreateClassDescriptor(cClassName, aSuperClasses, aMember, aMethod)
    RETURN ClassObject.ImplementClass(oClass)
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




/*
#include "class.ch"

PROCEDURE Main

  LOCAL oRecSet

  USE Customer EXCL NEW

  oRecSet := RecSet():New()

  ? oRecSet:LastName
  oRecSet:LastName := "Otto"
  ? oRecSet:LastName

  oRecSet:GoTo( 3 )
  ? oRecSet:FirstName
  ? oRecSet:LastName

  wait

  oRecSet:Close()

RETURN


CLASS RecSet

  EXPORTED:
    VAR Alias, FieldNames
    CLASS METHOD New
    METHOD Init, Close
    INLINE METHOD FieldGet( nFieldPos )
    RETURN (::Alias)->( FieldGet( nFieldPos ) )
    INLINE METHOD FieldPut( nFieldPos, xValue )
    RETURN (::Alias)->( FieldPut( nFieldPos, xValue ) )
    INLINE ACCESS METHOD Struct()
    RETURN (::Alias)->( dbStruct() )
    INLINE ACCESS METHOD RecNo()
    RETURN (::Alias)->( RecNo() )
    INLINE ACCESS METHOD Bof
    RETURN (::Alias)->( Bof() )
    INLINE ACCESS METHOD Eof
    RETURN (::Alias)->( Eof() )
    INLINE METHOD Skip( n )
    RETURN (::Alias)->( dbSkip( n ) )
    INLINE METHOD GoTo( n )
    RETURN (::Alias)->( dbGoTo( n ) )
    INLINE METHOD GoTop
    RETURN (::Alias)->( dbGoTop() )
    INLINE METHOD GoBottom()
    RETURN (::Alias)->( dbGoBottom() )

    // weitere Methoden wie Seek, Append, Replace, Delete ...

ENDCLASS

METHOD RecSet:init()
RETURN self

METHOD RecSet:Close()
  dbSelectArea( ::Alias )
  dbCloseArea()
  ClassDestroy( ::className() )
RETURN self

// - Class-Methode der RecSet-Klasse
// - mittels dieser Methode wird dynamisch eine neue Klasse, abgeleitet von der RecSet-Klasse,
//   also von sich selbst, erzeugt
// - anschl. wird das Object der neuen Klasse erzeugt und die Init()-Methode aufgerufen 
// - die Methode gibt das Object der Klasse zurueck, mit dem gearbeitet wird
// - dieser Methode habe ich den Namen ::New() gegeben, um das Erzeugen einer statischen Klasse
//   nachzubilden (sie koennte genauso gut auch ::ClassCreate() heissen)
CLASS METHOD RecSet:New()

  LOCAL n, nMax
  LOCAL aMethod, cBlock, cName, nType
  LOCAL aFieldNames
  LOCAL cClassName := "RecSet", oClass

  cClassName := GetDynamicClassName( cClassName )

  nType := CLASS_EXPORTED + METHOD_INSTANCE + ;
           METHOD_ACCESS  + METHOD_ASSIGN

  aFieldNames := FieldNames()

  nMax:= Len( aFieldNames )

  aMethod := Array( nMax )

  FOR n := 1 TO nMax

    cName := aFieldNames[n]

    cBlock := "{ |o,x| iif( x == NIL," + ;
                       "o:FieldGet(" + Var2Char(n) + ")," + ;
                       "o:FieldPut(" + Var2Char(n) + ",x) ) }"

    aMethod[n] := { cName, nType, &(cBlock), cName }

  NEXT

  // Klasse wird dynamisch erzeugt
  oClass := ClassCreate( cClassName, { Self }, {}, aMethod )

  oClass := oClass:New()

  oClass:Alias := Alias()
  oClass:FieldNames := AClone( aFieldNames )

RETURN oClass


// - Namen fuer dynamische Klassen ermitteln, so dass keine doppelten Namen bei
//   erneuter Erzeugung und somit ein Laufzeitfehler entsteht, indem ein Zaehler an den Namen
//   angehaengt wird
FUNCTION GetDynamicClassName( cClassName )

  LOCAL nCounter := 0
  LOCAL cNewClassName := cClassName
  LOCAL oClass := ClassObject( cNewClassName )

  while oClass != NIL
    nCounter++
    cNewClassName := cClassName + AllTrim( Str( nCounter ) )
    oClass := ClassObject( cNewClassName )
  enddo

RETURN cNewClassName


// Feldnamen der aktuellen DB in ein Array schreiben
FUNCTION FieldNames()
  LOCAL n, nTo := FCount()
  LOCAL aFields := Array( nTo )
  FOR n := 1 TO nTo
    aFields[n] := FieldName( n )
  NEXT 
RETURN aFields

*/


