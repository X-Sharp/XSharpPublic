//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Diagnostics
USING System.Reflection
USING System.Runtime.Remoting
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices

/// <summary>VO Compatible OLE Automation class</summary>
[DebuggerDisplay( "Type= {__ComObject}", Type := "OleAutoObject" )];
CLASS XSharp.OleAutoObject
	PROTECTED oComObject AS OBJECT
	PROTECTED lOk        AS LOGIC
	PROTECTED _liFuncs   AS LONG
	PROTECTED _liVars    AS LONG
	PROTECTED oType      AS System.Type	
	PROTECTED lDateTimeAsDate   AS LOGIC
	
	
	#region Constructors
	PRIVATE CONSTRUCTOR
		_liFuncs                := _liVars := -1 
		lOk                     := FALSE
		oComObject              := NULL
		oType                   := NULL 
		SELF:lDateTimeAsDate    := OleDateTimeAsDate()
		RETURN
			
	CONSTRUCTOR(cProgId AS STRING)
		SELF()
		oComObject        := OleCreateObject(cProgId)
		lOk := oComObject != NULL
		IF lOk
			oType := oComObject:GetType()
		ENDIF
			
		RETURN
			
	CONSTRUCTOR(cProgId AS STRING, fRotCheck AS LOGIC)
		SELF()
		IF fRotCheck
			oComObject := OleGetObject(cProgId)
		ENDIF
		IF oComObject == NULL
			oComObject := OleCreateObject(cProgId)
		ENDIF
		lOk := oComObject != NULL
		IF lOk
			oType := oComObject:GetType()
		ENDIF
		RETURN
			
			
	// Builds an OleAutoObject on Any OBJECT (including another AutoObject)
	CONSTRUCTOR(oObject AS OBJECT)
		SELF()
		oComObject :=  OleUnWrapObject(oObject)
		lOk := oComObject != NULL
		IF lOk
			oType := oComObject:GetType()
		ENDIF
			
		RETURN
			
		// Builds an OleAutoObject on Any OBJECT (including another AutoObject). Type already known
	CONSTRUCTOR(oObject AS OBJECT, _type AS System.Type)
		SELF()
		oComObject	:=  OleUnWrapObject(oObject)
		oType		:= _Type
		lOk			:= oComObject != NULL
		RETURN
			
	#endregion
		
	#region No..() EntryPoints
			
	// ? oObject:Property
	METHOD NoIVarGet(cName ) AS USUAL CLIPPER
		LOCAL oRet AS OBJECT
		oRet := OleAutoObject.__OleIvarGet(oComObject,oType, cName, NULL)
		RETURN OleAutoObject.OleWrapObject(oRet, lDateTimeAsDate)
			
		// oObject:Property := Value
	METHOD NoIvarPut(cName, uValue) AS VOID CLIPPER
		OleAutoObject.__OleIVarPut(oComObject, oType, cName , uValue, NULL)
		RETURN 
			
	METHOD NoMethod( ) AS USUAL CLIPPER
		LOCAL cName AS STRING
		LOCAL args  AS USUAL[]
		LOCAL nArg, nArgs  AS INT
		LOCAL oRet  AS OBJECT
		nArgs := PCOUNT()
		IF (nArgs >= 1)
			cName := _GetMParam(1)
			args  := USUAL[]{nArgs-1}
			FOR nArg := 2 TO nArgs
				args[nArg-1] :=  _GetMParam(nArg)
			NEXT
		ELSE
			// Throw Exception
			THROW Error.VOError(EG_SEND_MISSINGARG, "OleAutoObject:NoMethod","Send",0,NULL )
		ENDIF
		oRet := OleAutoObject.OleSend(oComObject, oType, cName, args)
			
		RETURN OleAutoObject.OleWrapObject(oRet, lDateTimeAsDate)
			
	// ? oObject:Property[dims]
	// The compiler needs to be changed to look for this method
	METHOD NoIVarGetCollection(cName AS STRING, dims AS USUAL[]) AS USUAL
		LOCAL oRet AS OBJECT
		oRet := OleAutoObject.OleIvarGet(oComObject, cName, dims)
		RETURN OleAutoObject.OleWrapObject(oRet, lDateTimeAsDate)
			
			
	// ? oObject:Property[dims] := Value
	// The compiler needs to be changed to look for this method
	METHOD NoIVarPutCollection(cName AS STRING, uValue AS USUAL, dims AS USUAL[]) AS VOID  
		OleAutoObject.OleIVarPut(oComObject, cName, uValue , dims )
		RETURN  
	#endregion
		
	#region VO Compatibility Accesses
		
	ACCESS dwFuncs  AS LONG
		IF (lOk)
			LOCAL t AS System.Type
			IF (_liFuncs < 0)
				t := oComObject:GetType()
				_liFuncs := t:GetMethods():Length
			ENDIF
			RETURN _liFuncs
		ENDIF
		RETURN 0
			
	ASSIGN dwFuncs(dwNew AS LONG) 
		SELF:_liFuncs := dwNew
		RETURN
			
	ACCESS dwVars   AS LONG
		IF (lOk)
			LOCAL t AS System.Type
			IF (_liVars < 0)
				t := oComObject:GetType()
				_liVars := t:GetProperties():Length
			ENDIF
			RETURN _liVars
		ENDIF
		RETURN 0
			
	ACCESS fInit    AS LOGIC
		RETURN lOk
			
	#endregion
		
	#region	INTERNAL Properties
			
	// Access to OBJECT inside AutoObject
	ACCESS __ComObject AS OBJECT
		RETURN SELF:oComObject
			
	#endregion
		
	#region STATIC Methods
	STATIC METHOD OleCreateObject(cProgId AS STRING) AS OBJECT
		LOCAL oComObject := NULL AS OBJECT
		LOCAL objecttype AS System.Type
		TRY
			objecttype        := System.Type.GetTypeFromProgID(cProgId, FALSE)
			IF objecttype != NULL
				oComObject        := System.Activator.CreateInstance (objecttype)
			ENDIF
		CATCH AS Exception
			// We catch the exception but do not process it
		END TRY
		RETURN oComObject
			
			
	STATIC METHOD  OleGetObject(cProgId AS STRING) AS OBJECT
		LOCAL oComObject AS OBJECT
		TRY
			oComObject := Marshal.GetActiveObject(cProgId)
		CATCH AS Exception
			oComObject := NULL
		END TRY
		RETURN oComObject
			
	#region Wrapping Objects
	STATIC METHOD  OleWrapObject(oObject AS OBJECT,lDateTimeAsDate AS LOGIC) AS USUAL
		LOCAL t  AS System.Type 
		LOCAL tc  AS System.TypeCode
		LOCAL oDt AS OleDateTime
		LOCAL uResult := NIL AS USUAL
		IF (oObject != NULL)
			t := oObject:GetType()
			tc := Type.GetTypeCode(t)
			IF tc == TypeCode.DateTime
				IF (lDateTimeAsDate)
					LOCAL oD AS System.DateTime
					oD := (System.DateTime)oObject
					uResult := XSharp.__VODate{oD}
				ELSE
					oDt := (DateTime) oObject
					uResult := oDT
				ENDIF
			ELSE
				IF t:IsCOMObject
					uResult := OleAutoObject{oObject,t}
				ELSE
					uResult := oObject
				ENDIF
			ENDIF
		ENDIF 
		RETURN uResult
				
	STATIC METHOD  OleUnWrapObject(oObject AS OBJECT) AS OBJECT
		LOCAL oAuto AS OleAutoObject
		IF oObject != NULL
			IF oObject IS OleAutoObject
				oAuto := (OleAutoObject) oObject
				oObject := oAuto:__ComObject
			ENDIF
		ENDIF
		RETURN oObject
				
	#endregion
			
	#region OBJECT Properties
			
	STATIC METHOD  OleIvarGet(oObject AS OBJECT,strName AS STRING) AS OBJECT
		RETURN OleAutoObject.__OleIvarGet(oObject, oObject:GetType(), strName, NULL)
				
	STATIC METHOD  OleIvarGet(oObject AS OBJECT, symName AS STRING, dims  AS USUAL[]) AS OBJECT
		RETURN OleAutoObject.__OleIvarGet(oObject, oObject:GetType(), symName, dims)
				
	STATIC METHOD  OleIvarGet(oObject AS OBJECT, symName AS STRING, index  AS USUAL) AS OBJECT
		LOCAL dims AS USUAL[]
		dims := USUAL[]{1}
		dims[1] := index
		RETURN OleAutoObject.__OleIvarGet(oObject, oObject:GetType(), symName, dims)
				
				
	STATIC METHOD  OleIVarPut(oObject AS OBJECT, symName AS STRING, uValue AS USUAL) AS VOID
		OleAutoObject.__OleIVarPut(oObject, oObject:GetType(), symName, uValue, NULL)
		RETURN
				
	STATIC METHOD  OleIVarPut(oObject AS OBJECT, symName AS STRING, uValue AS USUAL, dims AS USUAL[]) AS VOID
		OleAutoObject.__OleIVarPut(oObject, oObject:GetType(), symName, uValue, dims)
		RETURN
				
	STATIC METHOD  OleIVarPut(oObject AS OBJECT, symName AS STRING, uValue AS USUAL, index AS USUAL) AS VOID
		LOCAL dims AS USUAL[]
		dims := USUAL[]{1}
		dims[1] := index
		OleAutoObject.__OleIVarPut(oObject, oObject:GetType(), symName, uValue, dims)
		RETURN
				
				
	#endregion
			
	#region Worker Functions
			
	STATIC METHOD  __OleIvarGet(oComObject AS OBJECT, oType AS System.Type, cName AS STRING, dims AS USUAL[]) AS OBJECT
		LOCAL fi       AS FieldInfo
		LOCAL oRet     AS OBJECT
		LOCAL t        AS System.Type
		LOCAL lIndexed AS LOGIC
		LOCAL cMethod  AS STRING
		LOCAL bf AS BindingFlags
				
		cMethod := __ENTITY__
		IF oComObject == NULL
			THROW Error.NullArgumentError( cMethod, "oComObject", 1 )
		ENDIF
		lIndexed := (dims != NULL)
		t  := oType
		oComObject := OleUnWrapObject( oComObject )
		bf := BindingFlags.FlattenHierarchy | BindingFlags.GetField | ;
		BindingFlags.IgnoreCase | BindingFlags.GetProperty |  BindingFlags.Instance | BindingFlags.Public ;
				
		fi :=t:GetField( cName, bf )
				
		IF fi != NULL
			IF ! fi:IsPublic
				THROW Error.VOError( EG_NOVARMETHOD,  cMethod, "cMethod", 2,  <OBJECT>{cName } )
			ELSE
				oRet := fi:GetValue( oComObject )
			ENDIF   
		ELSE
			IF lIndexed
							
				LOCAL oArgs AS OBJECT[]
				LOCAL i     AS INT
				oArgs := OBJECT[]{dims:Length }
				FOR i := 1 UPTO dims:Length
					oArgs[i] :=  OleUnWrapObject(dims[i])
				NEXT
				oRet := oType:InvokeMember(cName,bf ,NULL, oComObject,oArgs)
			ELSE
				oRet := oType:InvokeMember(cName, bf,NULL, oComObject, NULL)
			ENDIF
		ENDIF
		RETURN oRet
				
		#endregion
			
#region Putting OBJECT Properties
			
STATIC METHOD  __OleIVarPut(oComObject AS OBJECT, oType AS System.Type, cName AS STRING, uValue AS USUAL, dims AS USUAL[]) AS VOID
	LOCAL t        AS System.Type
	LOCAL fi       AS FieldInfo
	LOCAL lIndexed AS LOGIC
	LOCAL cMethod  AS STRING
	cMethod := __ENTITY__
				
	IF oComObject == NULL
		THROW Error.NullArgumentError( cMethod, NAMEOF(oComObject), 1 )
	ENDIF
				
	// Get the inner OBJECT from OleAutoObjects
	// For both the Target and the value
	oComObject  := OleUnWrapObject( oComObject)
	uValue      := OleUnWrapObject( uValue )
				
	t := oType
				
	lIndexed := (dims != NULL)
				
	fi := t:GetField( cName, BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase  )
				
	IF fi != NULL
		fi:SetValue( oComObject, uValue )
	ELSE
		LOCAL bf      AS BindingFlags
		LOCAL oArgs   AS OBJECT[]
		bf := BindingFlags.FlattenHierarchy | BindingFlags.SetField | ;
		BindingFlags.IgnoreCase | BindingFlags.SetProperty
		IF lIndexed
			LOCAL i     AS INT
			oArgs := OBJECT[]{dims:Length+1 }
			FOR i := 1 UPTO dims:Length
				oArgs[i+1] := OleUnWrapObject(dims[i])
			NEXT
		ELSE
			oArgs := OBJECT[]{1 }
		ENDIF
		oArgs[1] := uValue
		TRY
			oType:InvokeMember(cName, bf,NULL, oComObject, oArgs)
			CATCH e AS Exception
			THROW e
		END TRY
					
	ENDIF
				
	RETURN 
				
	#endregion
		
STATIC METHOD  OleSend(oComObject AS OBJECT, oType AS System.Type, cName AS STRING, args AS USUAL[]) AS OBJECT
	LOCAL mi       AS MethodInfo
	LOCAL pi       AS ParameterInfo[]
	LOCAL t        AS Type
	LOCAL cMethod  AS STRING
	LOCAL x        AS INT
	LOCAL bf       AS BindingFlags
			
	LOCAL retval := NULL AS OBJECT
	cMethod  := __ENTITY__
			
	IF oComObject == NULL
		THROW Error.NullArgumentError( __ENTITY__, NAMEOF(oComObject), 1 )
	ENDIF
	t  :=oType
	oComObject := OleUnWrapObject(oComObject)
	bf :=  BindingFlags.Instance | BindingFlags.Public | BindingFlags.FlattenHierarchy| BindingFlags.InvokeMethod |  BindingFlags.IgnoreCase
			
	TRY
		mi := t:GetMethod( cName, bf)
		IF mi == NULL
			mi := t:GetMethod( cName)
		ENDIF
				
		IF mi == NULL
			// Ok we know nothing, just try to Invoke the method
			TRY
				
				VAR oArgs1 := OBJECT[] {args:Length} // Array of arguments for call to Invoke
				FOR x := 1 UPTO args:Length
					oArgs1[x] := OleUnWrapObject(args[x])
				NEXT
				retval := t:InvokeMember(cName, bf, NULL, oComObject, oArgs1)
			CATCH AS ArgumentException
				THROW Error.VOError(EG_ARG,  cMethod,"args", 4, <OBJECT>{args} )
			CATCH AS TargetException
				THROW Error.VOError(EG_ARG,  cMethod, "args", 4, <OBJECT>{args})
			CATCH AS MissingMethodException
				THROW Error.VOError(EG_NOMETHOD,  cMethod, "cName", 2,  <OBJECT>{cName}  )
			CATCH AS Exception
				THROW Error.VOError( EG_ARG , cMethod, "args", 4,  <OBJECT>{args}  )
			END TRY
		ELSE
			pi := mi:GetParameters()
			IF pi:Length == 1 .AND. mi:IsDefined( TYPEOF( XSharp.Internal.ClipperCallingConventionAttribute ), FALSE )
				retval := mi:Invoke(oComObject, <OBJECT>{args})
			ELSEIF pi:Length == 0
				retval := mi:Invoke(oComObject, NULL )
			ELSE                                 
				LOCAL nArgs          AS INT   // # of arguments passed in
				LOCAL pArg           AS USUAL PTR   // This is used to derefence arguments
				LOCAL nLastUnNamed   AS INT  // Last unnamed argument passed in
				LOCAL lFound         AS LOGIC // Was the Named Arg Found 
						
				VAR nDefArgs := pi:Length			// # of arguments defined for method
				VAR oArgs := OBJECT[]{ nDefArgs }	// Array of arguments for call to Invoke
				IF args != NULL
					nArgs := args:Length
					nArgs := Math.Min( nArgs, nDefArgs )
				ELSE
					nArgs := 0
				ENDIF
				nLastUnNamed := nArgs
				FOR x := 1 UPTO nArgs
					VAR oArg := OleUnWrapObject(args[x])
							
					IF IsPtr(oArg)
						// By Reference ?
						pArg     := (USUAL) oArg
						oArgs[x] := pArg[1]
								
					ELSEIF oArg IS NamedArg
						// Named Argument ?   
						nLastUnNamed:= Math.Min( x - 1, nLastUnNamed )
						VAR oNamedArg   := (NamedArg) oArg
						VAR cArgName    := oNamedArg:ArgName
						lFound      := FALSE
						FOR VAR y := 1 TO  nDefArgs
							VAR paramInfo := pi[y]
							IF String.Compare( paramInfo:Name, cArgName, StringComparison.OrdinalIgnoreCase ) == 0 
								oArgs[y] := oNamedArg:Value                        
								lFound   := TRUE
								EXIT
							ENDIF
						NEXT
						IF ! lFound
							THROW Error.VOError(EG_ARG,  cMethod, cArgName, (DWORD) x, <OBJECT>{oNamedArg} )
						ENDIF
								
					ELSE
						// Normal argument
						oArgs[x] := oArg
					ENDIF
				NEXT
				// Fill in missing method parameter values
				// nLastUnNamed has # of last unnamed argument
				FOR x :=nLastUnNamed+1 TO nDefArgs
					IF oArgs[x] == NULL
						// When no value, fill in the default
						VAR paramInfo := pi[x]
						oArgs[x] := paramInfo:DefaultValue
					ENDIF
				NEXT
				LOCAL lOk AS LOGIC
				// Try MethodInfo:Invoke first. 
				TRY
					retval := mi:Invoke(oComObject, oArgs)
					lOk := TRUE
					// return Out parameters 
					FOR x := 1 TO nArgs
						VAR paramInfo := pi[x]
						IF paramInfo:Attributes:HasFlag( ParameterAttributes.Out) 
							IF IsPtr(args[x])
								pArg := args[x]
								pArg[1] := oArgs[x]
							ENDIF
						ENDIF
					NEXT 
				CATCH AS Exception
					lOk := FALSE              
				END TRY
				// If that fails we try t:InvokeMember. You never know if that will work...
				IF ! lOk
					TRY
						retval := t:InvokeMember(cName, bf, NULL, oComObject, oArgs)
						lOk    := TRUE
					CATCH AS ArgumentException
						THROW Error.VOError(EG_ARG,  cMethod, cMethod, 1, <OBJECT>{args})
						lOk := FALSE                                  
					CATCH AS TargetException
						THROW Error.VOError(EG_ARG,  cMethod, cMethod, 1, <OBJECT>{args} )
						lOk := FALSE                                  
					CATCH AS MissingMethodException
						THROW Error.VOError(EG_NOMETHOD,  cMethod, "cName", 2, <OBJECT>{cName}  )
						lOk := FALSE                                  
					CATCH AS Exception
						lOk := FALSE              
					END TRY
				ENDIF
				IF ! lOk
					THROW Error.VOError(EG_ARG,  cMethod, cMethod, 1, <OBJECT>{args}  )
				ENDIF   
			ENDIF
		ENDIF
	CATCH AS AmbiguousMatchException
		THROW Error.VOError( EG_AMBIGUOUSMETHOD,  cMethod, "cMethod", 2, <OBJECT>{cMethod} )
	END TRY  
	RETURN retval   
	#endregion
	
END CLASS

