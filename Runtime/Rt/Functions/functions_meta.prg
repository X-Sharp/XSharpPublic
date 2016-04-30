//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Get the class hierarchy of a class.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION ClassTreeClass(symClassName AS SYMBOL) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Concatenate two symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConcatAtom(s1 AS SYMBOL,s2 AS SYMBOL) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Concatenate three symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConcatAtom3(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <param name="s4"></param>
	/// <param name="s5"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConcatAtom5(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL,s4 AS SYMBOL,s5 AS SYMBOL) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="symClass"></param>
	/// <param name="symMeth"></param>
	/// <param name="nType"></param>
	/// <param name="pFunc"></param>
	/// <param name="nArgs"></param>
	/// <returns>
	/// </returns>
	FUNCTION DeclareMethod(symClass AS SYMBOL,symMeth AS SYMBOL,nType AS DWORD,pFunc AS PTR,nArgs AS DWORD) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return a set-get code block for a field that is identified by a symbol.
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldBlockSym(symVar AS SYMBOL) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// Get the contents of a field that is identified by a work area alias and the field name.
	/// </summary>
	/// <param name="symAlias"></param>
	/// <param name="symField"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldGetAlias(symAlias AS SYMBOL,symField AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Retrieve the contents of a field that is identified by its symbolic name.
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldGetSym(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return the position of a field that is identified by a symbol.
	/// </summary>
	/// <param name="sField"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldPosSym(sField AS SYMBOL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Set the value of a field identified by its work area alias and field name.
	/// </summary>
	/// <param name="symAlias"></param>
	/// <param name="symField"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldPutAlias(symAlias AS SYMBOL,symField AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Set the value of a field that is identified by its symbolic name.
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldPutSym(symVar AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return a set-get code block for a field, specified as a symbol, in a specified work area.
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="nArea"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldWBlockSym(symVar AS SYMBOL,nArea AS DWORD) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// Return the number of local arguments that a function with the CLIPPER calling convention is expecting.
	/// </summary>
	/// <param name="symFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION FParamCount(symFunc AS SYMBOL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="symFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION FunctionSym2Ptr(symFunc AS SYMBOL) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION GSG9(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Determine if a class exists.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsClass(symClassName AS SYMBOL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if one class is a subclass of another class.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symSuperClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsClassOf(symClassName AS SYMBOL,symSuperClassName AS SYMBOL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Check whether a particular method can be sent to a class.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsMethodClass(symClassName AS SYMBOL,symMethodName AS SYMBOL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Store all instance variables of a class into an array.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION IvarListClass(symClassName AS SYMBOL) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Obtain a set-get code block for a given memory variable.
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarBlockSym(symVar AS SYMBOL) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarGetSym(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION MemVarPutSym(symVar AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Create a class list in the form of an array for the specified class.
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION MethodListClass(symClassName AS SYMBOL) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Return the number of arguments that a method is expecting.
	/// </summary>
	/// <param name="symClass"></param>
	/// <param name="symMethod"></param>
	/// <returns>
	/// </returns>
	FUNCTION MParamCount(symClass AS SYMBOL,symMethod AS SYMBOL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return a multidimensional array of all object-oriented programming symbols that constitute the class of an object.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION OOPTreeClass(s AS SYMBOL) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Convert a symbol to a string.
	/// </summary>
	/// <param name="sym"></param>
	/// <returns>
	/// </returns>
	FUNCTION Symbol2String(sym AS SYMBOL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysCompAtom(s1 AS SYMBOL,s2 AS SYMBOL) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="symClass"></param>
	/// <returns>
	/// </returns>
	FUNCTION UnDeclareClass(symClass AS SYMBOL) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION VarGetSym(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION VarPutSym(symVar AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	#endregion
end namespace