//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic


/// <summary>
/// Perform an assignment to a variable whose name is stored in a specified string.
/// </summary>
/// <param name="cExp"></param>
/// <param name="xValue"></param>
/// <returns>
/// </returns>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION MAssign(cExp AS STRING,xValue AS USUAL) AS USUAL
    RETURN MemVarPutSym(cExp, xValue)
    
    
/// <summary>
/// Return a set-get code block for a given memory variable.
/// </summary>
/// <param name="cVar"></param>
/// <returns>
/// </returns>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION MemVarBlock(cVar AS STRING) AS OBJECT
    RETURN {| uValue| IIF (uValue == NIL, MemVarGet(cVar), MemVarPut(cVar, uValue))} 
    
    
/// <summary>
/// Return the contents of a memory variable.
/// </summary>
/// <param name="cVar">The name of the memory variable.</param>
/// <returns>The value of the memory variable. When there is no memory variable with that name then a runtime error is thrown.</returns>
/// <seealso cref='M:XSharp.RT.Functions.VarGet(System.String)' >VarGet</seealso>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION MemVarGet(cVar AS STRING) AS USUAL
    RETURN XSharp.MemVar.Get(cVar)
    
    
    
/// <summary>
/// Assign a value to a memory variable of a given name.
/// </summary>
/// <param name="cVar">The name of the variable you want to create. </param>
/// <param name="uValue">The value to assign to the variable. </param>
/// <returns>The value assigned to the memvar.</returns>
/// <remarks>
/// If a memory variable with that name does not exits, a new memory variable is created.  Therefore, MemVarPut(), like VarPut() can be used to create undeclared memory variables.  It should be used instead of a macro.
/// </remarks>
/// <seealso cref='M:XSharp.RT.Functions.VarPut(System.String,XSharp.__Usual)' >VarPut</seealso>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION MemVarPut(cVar AS STRING,uValue AS USUAL) AS USUAL
    RETURN XSharp.MemVar.Put(cVar, uValue) 
    


/// <summary>
/// Release a memory variable
/// </summary>
/// <param name="symVar">The name of the variable you want to release. </param>
/// <remarks>
/// The value of this variable will be set to NIL. The variable is NOT deleted.  
/// </remarks>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION MemVarRelease(symVar AS STRING) AS VOID 
	XSharp.MemVar.Release(symVar)
	RETURN

    
/// <summary>Return the contents of a field or a memory variable.</summary>
/// <param name="cVar">The name of the field or memory variable.</param>
/// <returns>The value of the field or memory variable. When there is no field and also no memory variable then a runtime error is thrown.</returns>
/// <remarks>
/// This function is used instead of a macro when the name of the field or memory variable is in a string. 
/// </remarks>
/// <seealso cref='M:XSharp.RT.Functions.MemVarGet(System.String)' >MemVarGet</seealso>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION VarGet(cVar AS STRING) AS USUAL
    RETURN __VarGet(cVar)
    
/// <summary>
/// Assign a value to a field or a memory variable of a given name.
/// </summary>
/// <param name="cVar"> The name of the variable or field you want to Assign to </param>
/// <param name="uValue">The value to assign to the variable</param>
/// <returns>
/// </returns>
/// <remarks>
/// If a field or memory variable with the specified name does not exist, then a new a memory variable is created.
/// This function like MemVarPut(), can be used to create undeclared memory variables.  It should be used instead of a macro.
/// </remarks>
/// <seealso cref='M:XSharp.RT.Functions.MemVarPut(System.String,XSharp.__Usual)' >MemVarPut</seealso>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION VarPut(cVar AS STRING,uValue AS USUAL) AS USUAL
    RETURN __VarPut(cVar, uValue)
    

/// <inheritdoc cref='M:XSharp.RT.Functions.VarGet(System.String)' />
/// <param name="symVar">The name of the variable .</param>
FUNCTION VarGetSym(symVar AS SYMBOL) AS USUAL
    RETURN __VarGet(symVar)
    
/// <inheritdoc cref='M:XSharp.RT.Functions.VarPut(System.String,XSharp.__Usual)' />
/// <param name="symVar">The name of the variable .</param>
FUNCTION VarPutSym(symVar AS SYMBOL,uValue AS USUAL) AS USUAL
    RETURN __VarPut(symVar, uValue)
    
/// <inheritdoc cref='M:XSharp.RT.Functions.MemVarBlock(System.String)' />
/// <param name="symVar">The name of the variable .</param>
FUNCTION MemVarBlockSym(symVar AS SYMBOL) AS OBJECT
    RETURN {| uValue| IIF (uValue == NIL, MemVarGetSym(symVar), MemVarPutSym(symVar, uValue))} 
    
/// <inheritdoc cref='M:XSharp.RT.Functions.MemVarGet(System.String)' />
/// <param name="symVar">The name of the variable .</param>
FUNCTION MemVarGetSym(symVar AS SYMBOL) AS USUAL 
    RETURN XSharp.MemVar.Get(symVar)
    
/// <inheritdoc cref='M:XSharp.RT.Functions.MemVarPut(System.String,XSharp.__Usual)' /> 
/// <param name="symVar">The name of the variable you want to assign to.</param>
FUNCTION MemVarPutSym(symVar AS SYMBOL, uValue AS USUAL) AS USUAL  
    RETURN XSharp.MemVar.Put(symVar, uValue) 


/// <summary>
/// Clear all memory variables (all public variables and the private variables of the current thread)
/// </summary>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION _MClear() AS VOID STRICT  
	XSharp.MemVar.ClearAll()
	RETURN

/// <summary>
/// Release one or more memory variables variables. 
/// </summary>
/// <param name="var1">Variable 1</param>
/// <param name="var2">Variable 2</param>
/// <param name="var3">Variable 3</param>
/// <param name="var4">Variable 4</param>
/// <param name="varn">Variable n</param>
/// <remarks>
/// The variables are not removed but their values are replaced with NIL.
/// </remarks>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION _MxRelease (var1, var2, var3, var4, varn) AS VOID CLIPPER	
	LOCAL nCount AS LONG
	LOCAL name AS USUAL
	nCount := PCount()
	FOR VAR i := 1 TO nCount
		name := _GetFParam(i)
		IF name:IsString
			MemVarRelease(name)
		ELSEIF name:IsSymbol			
			MemVarRelease(name)
		ELSE  
			// throw argument error
		ENDIF
	NEXT
	RETURN 

/// <summary>
/// Release variables that match a certain wildcard pattern
/// </summary>
/// <param name="cMask">The wildcard pattern to use when releasing the memvars. May contain * and ? characters.</param>
/// <param name="lMatch">Indicates if the variables that need to be released should match (TRUE) or NOT match (FALSE) the pattern.</param>
/// <remarks>
/// The variables are not removed but their values are replaced with NIL.
/// </remarks>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION _MRelease(cMask AS STRING, lMatch AS LOGIC)	AS VOID
	LOCAL cName AS STRING
	// Case INsensitive comparison. Symbols are all in UPPER case
	cMask := Upper(cMask)                                        
	cName := _PrivateFirst()
	DO WHILE cName != NULL
		IF _Like(cMask, cName) == lMatch
			MemVarPut(cName, NIL)
		ENDIF
		cName := _PrivateNext()
	ENDDO
	RETURN   



/// <summary>
/// Enumerate private variables
/// </summary>
FUNCTION _PrivateFirst(lCurrentOnly := FALSE AS LOGIC) AS STRING 
	RETURN XSharp.MemVar.PrivatesFirst(lCurrentOnly)


/// <summary>
/// Enumerate private variables
/// </summary>
FUNCTION _PrivateNext() AS STRING STRICT
	RETURN XSharp.MemVar.PrivatesNext() 


/// <summary>
/// Enumerate public variables
/// </summary>
FUNCTION _PublicFirst() AS STRING STRICT 
	RETURN XSharp.MemVar.PublicsFirst()


/// <summary>
/// Enumerate public variables
/// </summary>
FUNCTION _PublicNext() AS STRING STRICT
	RETURN XSharp.MemVar.PublicsNext()
	
/// <summary>
/// Count private variables
/// </summary>

FUNCTION _PrivateCount(lCurrentOnly := FALSE AS LOGIC) AS INT      
	RETURN XSharp.MemVar.PrivatesCount(lCurrentOnly)

/// <summary>
/// Count public variables
/// </summary>

FUNCTION _PublicCount() AS INT STRICT    
	RETURN XSharp.MemVar.PublicsCount()

/// <summary>
/// Enumerate private variables
/// </summary>
FUNCTION _PrivateEnum(lCurrentOnly := FALSE AS LOGIC) AS IEnumerator<STRING>
	RETURN XSharp.MemVar.PrivatesEnum(lCurrentOnly)

/// <summary>
/// Enumerate public variables
/// </summary>
FUNCTION _PublicEnum AS IEnumerator<STRING>
	RETURN XSharp.MemVar.PublicsEnum()

