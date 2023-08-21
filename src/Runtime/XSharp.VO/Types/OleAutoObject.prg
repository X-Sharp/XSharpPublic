//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Diagnostics
using System.Reflection
using System.Runtime.Remoting
using System.Runtime.InteropServices
using System.Runtime.CompilerServices
using XSharp.Internal
using System.Collections.Generic
/// <summary>VO Compatible OLE Automation class</summary>
[AllowLateBinding];
[DebuggerDisplay( "Type= {__ComObject}", Type := "OleAutoObject" )];
CLASS XSharp.OleAutoObject IMPLEMENTS IDynamicProperties, ILateBound
	protected oComObject as object
	protected lOk        as logic
	protected _liFuncs   as long
	protected _liVars    as long
	protected oType      as System.Type
	protected lDateTimeAsDate   as logic


	#region Constructors
	internal constructor
		_liFuncs                := _liVars := -1
		lOk                     := false
		oComObject              := null
		oType                   := null
		self:lDateTimeAsDate    := OleDateTimeAsDate()
		return

    /// <summary>Construct an OleAutoObject</summary>
	constructor(cProgId as string)
		self()
		oComObject        := OleCreateObject(cProgId)
		lOk := oComObject != null
		if lOk
			oType := oComObject:GetType()
		endif
		return

    /// <summary>Construct an OleAutoObject</summary>
	constructor(cProgId as string, fRotCheck as logic)
		self()
		if fRotCheck
			oComObject := OleGetObject(cProgId)
		endif
		if oComObject == null
			oComObject := OleCreateObject(cProgId)
		endif
		lOk := oComObject != null
		if lOk
			oType := oComObject:GetType()
		endif
		return


	// Builds an OleAutoObject on Any OBJECT (including another AutoObject)
    /// <summary>Construct an OleAutoObject</summary>
	constructor(oObject as object)
		self()
		oComObject :=  OleUnWrapObject(oObject)
		lOk := oComObject != null
		if lOk
			oType := oComObject:GetType()
		endif
		return

		// Builds an OleAutoObject on Any OBJECT (including another AutoObject). Type already known
    /// <summary>Construct an OleAutoObject</summary>
	constructor(oObject as object, _type as System.Type)
		self()
		oComObject	:=  OleUnWrapObject(oObject)
		oType		:= _type
		lOk			:= oComObject != null
		return

	#endregion

	#region No..() EntryPoints

	    /// <exclude />
    private method GetOurFieldInfo(cName as string, result out usual) as logic
        switch cName:ToLower()
        case "finit"
            result := self:lOk
            return true
        case "dwfuncs"
            result := self:_liFuncs
            return true
        case "dwVars"
            result := self:_liVars
            return true
        otherwise
            result := nil
            return false
        end switch

    private method SetOurFieldInfo(cName as string, newvalue as usual) as logic
        switch cName:ToLower()
        case "dwfuncs"
            self:_liFuncs := newvalue
            return true
        end switch
        return false

	/// <exclude />
    // ? oObject:Property
    VIRTUAL METHOD NoIvarGet(cName AS STRING ) AS USUAL
        IF SELF:GetOurFieldInfo(cName, OUT VAR result)
            return result
        endif
		VAR oRet := OleAutoObject.__OleIvarGet(oComObject,oType, cName, NULL)
		return OleAutoObject.OleWrapObject(oRet, lDateTimeAsDate)

		// oObject:Property := Value
	    /// <exclude />
	VIRTUAL METHOD NoIvarPut(cName AS STRING, uValue AS USUAL) AS VOID
        if self:SetOurFieldInfo(cName, uValue)
            return
        endif
		OleAutoObject.__OleIVarPut(oComObject, oType, cName , uValue, null)
		return

    virtual method GetPropertyNames() as string[]
        var props := oType:GetProperties()
        var names := List<string>{}
        foreach var prop in props
            names:Add(prop:Name)
        next
        return names:ToArray()

	    /// <exclude />
	method NoMethod( ) as usual clipper
		local cName as string
		local args  as usual[]
		local nArg, nArgs  as int
		local oRet  as object
		nArgs := PCOUNT()
		cName := RuntimeState.NoMethod
        if RuntimeState.Dialect == XSharpDialect.Vulcan
		    args  := usual[]{nArgs-1}
		    for nArg := 2 to nArgs
			    args[nArg-1] :=  _GetMParam(nArg)
            next
        else
		    args  := usual[]{nArgs}
		    for nArg := 1 to nArgs
			    args[nArg] :=  _GetMParam(nArg)
            next
        endif
		oRet := OleAutoObject.OleSend(oComObject, oType, cName, args)
		return OleAutoObject.OleWrapObject(oRet, lDateTimeAsDate)

	// ? oObject:Property[dims]
	// The compiler needs to be changed to look for this method
	    /// <exclude />
	method NoIVarGetCollection(cName as string, dims as usual[]) as usual
		local oRet as object
		oRet := OleAutoObject.OleIvarGet(oComObject, cName, dims)
		return OleAutoObject.OleWrapObject(oRet, lDateTimeAsDate)


	// ? oObject:Property[dims] := Value
	// The compiler needs to be changed to look for this method
	    /// <exclude />
	method NoIVarPutCollection(cName as string, uValue as usual, dims as usual[]) as void
		OleAutoObject.OleIVarPut(oComObject, cName, uValue , dims )
		return
	#endregion

	#region VO Compatibility Accesses
	    /// <exclude />

	access dwFuncs  as long
		if (lOk)
			local t as System.Type
			if (_liFuncs < 0)
				t := oComObject:GetType()
				_liFuncs := t:GetMethods():Length
			endif
			return _liFuncs
		endif
		return 0

	assign dwFuncs(liValue as long)
		self:_liFuncs := liValue
		return

	    /// <exclude />
	access dwVars   as long
		if (lOk)
			local t as System.Type
			if (_liVars < 0)
				t := oComObject:GetType()
				_liVars := t:GetProperties():Length
			endif
			return _liVars
		endif
		return 0

	    /// <exclude />
	access fInit    as logic
		return lOk

	#endregion

	#region	INTERNAL Properties

	// Access to OBJECT inside AutoObject
    /// <exclude />

    access __ComObject as object
		return self:oComObject

	#endregion

	#region STATIC Methods
	    /// <exclude />
	static method OleCreateObject(cProgId as string) as object
		local oComObject := null as object
		local objecttype as System.Type
		try
			objecttype        := System.Type.GetTypeFromProgID(cProgId, false)
			if objecttype != null
				oComObject        := System.Activator.CreateInstance (objecttype)
			endif
		catch as Exception
			// We catch the exception but do not process it
                NOP
		end try
		return oComObject


    /// <exclude />
	static method  OleGetObject(cProgId as string) as object
		local oComObject as object
		try
			oComObject := Marshal.GetActiveObject(cProgId)
		catch as Exception
			oComObject := null
		end try
		return oComObject

	#region Wrapping Objects
    /// <exclude />
    static method  OleWrapObject(oObject as object,lDateTimeAsDate as logic) as usual
		local t  as System.Type
		local tc  as System.TypeCode
		local oDt as OleDateTime
		local uResult := nil as usual
		if (oObject != null)
			t := oObject:GetType()
			tc := Type.GetTypeCode(t)
			if tc == TypeCode.DateTime
				if (lDateTimeAsDate)
					local oD as System.DateTime
					oD := (System.DateTime)oObject
					uResult := XSharp.__Date{oD}
				else
					oDt := (DateTime) oObject
					uResult := oDt
				endif
			else
				if t:IsCOMObject
					uResult := OleAutoObject{oObject,t}
				else
					uResult := oObject
				endif
			endif
		endif
		return uResult

    /// <exclude />
	static method  OleUnWrapObject(oObject as object) as object
		local oAuto as OleAutoObject
		if oObject != null
			if oObject is OleAutoObject
				oAuto := (OleAutoObject) oObject
				oObject := oAuto:__ComObject
			endif
		endif
		return oObject

	#endregion

	#region OBJECT Properties

    /// <exclude />
	static method  OleIvarGet(oObject as object,strName as string) as object
		return OleAutoObject.__OleIvarGet(oObject, oObject:GetType(), strName, null)

    /// <exclude />
	static method  OleIvarGet(oObject as object, symName as string, dims  as usual[]) as object
		return OleAutoObject.__OleIvarGet(oObject, oObject:GetType(), symName, dims)

    /// <exclude />
	static method  OleIvarGet(oObject as object, symName as string, index  as usual) as object
		local dims as usual[]
		dims := usual[]{1}
		dims[1] := index
		return OleAutoObject.__OleIvarGet(oObject, oObject:GetType(), symName, dims)


    /// <exclude />
	static method  OleIVarPut(oObject as object, symName as string, uValue as usual) as void
		OleAutoObject.__OleIVarPut(oObject, oObject:GetType(), symName, uValue, null)
		return

    /// <exclude />
	static method  OleIVarPut(oObject as object, symName as string, uValue as usual, dims as usual[]) as void
		OleAutoObject.__OleIVarPut(oObject, oObject:GetType(), symName, uValue, dims)
		return

    /// <exclude />
	static method  OleIVarPut(oObject as object, symName as string, uValue as usual, index as usual) as void
		local dims as usual[]
		dims := usual[]{1}
		dims[1] := index
		OleAutoObject.__OleIVarPut(oObject, oObject:GetType(), symName, uValue, dims)
		return


	#endregion

	#region Worker Functions

    /// <exclude />
	static method  __OleIvarGet(oComObject as object, oType as System.Type, cName as string, dims as usual[]) as object
		local fi       as FieldInfo
		local oRet     as object
		local t        as System.Type
		local lIndexed as logic
		local cMethod  as string
		local bf as BindingFlags

		cMethod := __entity__
		if oComObject == null
			throw Error.NullArgumentError( cMethod, "oComObject", 1 )
		endif
		lIndexed := (dims != null)
		t  := oType
		oComObject := OleUnWrapObject( oComObject )
		bf := BindingFlags.FlattenHierarchy | BindingFlags.GetField | ;
		BindingFlags.IgnoreCase | BindingFlags.GetProperty |  BindingFlags.Instance | BindingFlags.Public ;

		fi :=t:GetField( cName, bf )

		if fi != null
			if ! fi:IsPublic
				throw Error.VOError( EG_NOVARMETHOD,  cMethod, "cMethod", 2,  <object>{cName } )
			else
				oRet := fi:GetValue( oComObject )
			endif
		else
			if lIndexed

				local oArgs as object[]
				local i     as int
				oArgs := object[]{dims:Length }
				for i := 1 upto dims:Length
					oArgs[i] :=  OleUnWrapObject(dims[i])
				next
				oRet := oType:InvokeMember(cName,bf ,null, oComObject,oArgs)
			else
				oRet := oType:InvokeMember(cName, bf,null, oComObject, null)
			endif
		endif
		return oRet

		#endregion

#region Putting OBJECT Properties

    /// <exclude />
static method  __OleIVarPut(oComObject as object, oType as System.Type, cName as string, uValue as usual, dims as usual[]) as void
	local t        as System.Type
	local fi       as FieldInfo
	local lIndexed as logic
	local cMethod  as string
	cMethod := __entity__

	if oComObject == null
		throw Error.NullArgumentError( cMethod, NAMEOF(oComObject), 1 )
	endif

	// Get the inner OBJECT from OleAutoObjects
	// For both the Target and the value
	oComObject  := OleUnWrapObject( oComObject)
	uValue      := OleUnWrapObject( uValue )

	t := oType

	lIndexed := (dims != null)

	fi := t:GetField( cName, BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase  )

	if fi != null
		fi:SetValue( oComObject, uValue )
	else
		local bf      as BindingFlags
		local oArgs   as object[]
		bf := BindingFlags.FlattenHierarchy | BindingFlags.SetField | ;
		BindingFlags.IgnoreCase | BindingFlags.SetProperty
		if lIndexed
			local i     as int
			oArgs := object[]{dims:Length+1 }
			for i := 1 upto dims:Length
				oArgs[i+1] := OleUnWrapObject(dims[i])
			next
		else
			oArgs := object[]{1 }
		endif
		oArgs[1] := uValue
		try
			oType:InvokeMember(cName, bf,null, oComObject, oArgs)
			catch e as Exception
			throw e
		end try

	endif

	return

	#endregion
    /// <exclude />

static method  OleSend(oComObject as object, oType as System.Type, cName as string, args as usual[]) as object
	local mi       as MethodInfo
	local pi       as ParameterInfo[]
	local t        as Type
	local cMethod  as string
	local x        as int
	local bf       as BindingFlags

	local retval := null as object
	cMethod  := __entity__

	if oComObject == null
		throw Error.NullArgumentError( __entity__, NAMEOF(oComObject), 1 )
	endif
	t  :=oType
	oComObject := OleUnWrapObject(oComObject)
	bf :=  BindingFlags.Instance | BindingFlags.Public | BindingFlags.FlattenHierarchy| BindingFlags.InvokeMethod |  BindingFlags.IgnoreCase

	try
		mi := t:GetMethod( cName, bf)
		if mi == null
			mi := t:GetMethod( cName)
		endif

		if mi == null
			// Ok we know nothing, just try to Invoke the method
			try

				var oArgs1 := object[] {args:Length} // Array of arguments for call to Invoke
				for x := 1 upto args:Length
					oArgs1[x] := OleUnWrapObject(args[x])
				next
				retval := t:InvokeMember(cName, bf, null, oComObject, oArgs1)
			catch e as ArgumentException
            if e:InnerException is WrappedException
                throw e:InnerException
            endif
				throw Error.VOError(EG_ARG,  cName,"args", 4, <object>{args} )
			catch e as TargetException
            if e:InnerException is WrappedException
                throw e:InnerException
            endif
				throw Error.VOError(EG_ARG,  cName, "args", 4, <object>{args})
			catch e as COMException
				throw Error{e}
         catch e as TargetInvocationException
            // this always has the original exception as inner exception
            if e:InnerException is WrappedException
                throw e:InnerException
            endif
            var er := Error{e:GetInnerException()}
            var arguments := List<object>{}{cName}
            arguments:AddRange(_UsualArrayToObjectArray(args))
            er:Args := arguments:ToArray()
            throw er
        catch as Exception
				throw Error.VOError(EG_NOMETHOD,  cMethod, "cName", 2,  <object>{cName}  )
			end try
		else
			pi := mi:GetParameters()
			if pi:Length == 1 .and. mi:IsDefined( TYPEOF( XSharp.Internal.ClipperCallingConventionAttribute ), false )
				retval := mi:Invoke(oComObject, <object>{args})
			elseif pi:Length == 0
				retval := mi:Invoke(oComObject, null )
			else
				local nArgs          as int   // # of arguments passed in
				local pArg           as usual ptr   // This is used to derefence arguments
				local nLastUnNamed   as int  // Last unnamed argument passed in
				local lFound         as logic // Was the Named Arg Found

				var nDefArgs := pi:Length			// # of arguments defined for method
				var oArgs := object[]{ nDefArgs }	// Array of arguments for call to Invoke
				if args != null
					nArgs := args:Length
					nArgs := Math.Min( nArgs, nDefArgs )
				else
					nArgs := 0
				endif
				nLastUnNamed := nArgs
				for x := 1 upto nArgs
					var oArg := OleUnWrapObject(args[x])

					if IsPtr(oArg)
						// By Reference ?
						pArg     := usual{oArg}
						oArgs[x] := pArg[1]

					elseif oArg is NamedArg
						// Named Argument ?
						nLastUnNamed:= Math.Min( x - 1, nLastUnNamed )
						var oNamedArg   := (NamedArg) oArg
						var cArgName    := oNamedArg:ArgName
						lFound      := false
						for var y := 1 to  nDefArgs
							var paramInfo := pi[y]
							if String.Compare( paramInfo:Name, cArgName, StringComparison.OrdinalIgnoreCase ) == 0
								oArgs[y] := oNamedArg:Value
								lFound   := true
								exit
							endif
						next
						if ! lFound
							throw Error.VOError(EG_ARG,  cMethod, cArgName, (dword) x, <object>{oNamedArg} )
						endif

					else
						// Normal argument
						oArgs[x] := oArg
					endif
				next
				// Fill in missing method parameter values
				// nLastUnNamed has # of last unnamed argument
				for x :=nLastUnNamed+1 to nDefArgs
					if oArgs[x] == null
						// When no value, fill in the default
						var paramInfo := pi[x]
						oArgs[x] := paramInfo:DefaultValue
					endif
				next
				local lOk as logic
				// Try MethodInfo:Invoke first.
				try
					retval := mi:Invoke(oComObject, oArgs)
					lOk := true
					// return Out parameters
					for x := 1 to nArgs
						var paramInfo := pi[x]
						if paramInfo:Attributes:HasFlag( ParameterAttributes.Out)
							if IsPtr(args[x])
								pArg := args[x]
								pArg[1] := oArgs[x]
							endif
						endif
					next
				catch as Exception
					lOk := false
				end try
				// If that fails we try t:InvokeMember. You never know if that will work...
				if ! lOk
					try
						retval := t:InvokeMember(cName, bf, null, oComObject, oArgs)
						lOk    := true
					catch as ArgumentException
						throw Error.VOError(EG_ARG,  cMethod, cMethod, 1, <object>{args})
						//lOk := FALSE
					catch as TargetException
						throw Error.VOError(EG_ARG,  cMethod, cMethod, 1, <object>{args} )
						//lOk := FALSE
					catch as MissingMethodException
						throw Error.VOError(EG_NOMETHOD,  cMethod, "cName", 2, <object>{cName}  )
						//lOk := FALSE
					catch as Exception
						lOk := false
					end try
				endif
				if ! lOk
					throw Error.VOError(EG_ARG,  cMethod, cMethod, 1, <object>{args}  )
				endif
			endif
		endif
	catch as AmbiguousMatchException
		throw Error.VOError( EG_AMBIGUOUSMETHOD,  cMethod, "cMethod", 2, <object>{cMethod} )
	end try
	return retval
	#endregion

end class

