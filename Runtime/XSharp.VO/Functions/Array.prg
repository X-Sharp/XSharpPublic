//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


INTERNAL STATIC CLASS ArrayHelpers

	STATIC METHOD aScan<T>(aTarget AS __ArrayBase<T>, element  AS T , nStart AS LONG, nCount AS LONG) AS DWORD WHERE T IS NEW()
		LOCAL nItem AS LONG
		LOCAL nLen  AS LONG
		nLen := (INT) aTarget:Length
		FOR nItem := nStart TO nLen
			IF object.Equals(aTarget[ nItem], element)
				RETURN (DWORD) nItem
			ENDIF
			nCount -= 1
			IF nCount == 0
				EXIT
			ENDIF
		NEXT
		RETURN 0

	STATIC METHOD aScan<T>(aTarget AS __ArrayBase<T>, bAction AS @@Func<T, LOGIC> , nStart AS LONG, nCount AS LONG) AS DWORD WHERE T IS NEW()
		LOCAL nItem AS LONG
		LOCAL nLen  AS LONG
		nLen := (INT) aTarget:Length
		FOR nItem := nStart TO nLen
			VAR oElement := aTarget[ nItem]
			IF bAction(oElement)
				RETURN  (DWORD) nItem
			ENDIF
			nCount -= 1
			IF nCount == 0
				EXIT
			ENDIF
		NEXT
		RETURN 0


	STATIC METHOD aScan( aTarget AS USUAL, x AS USUAL, uStart AS USUAL, uCount AS USUAL, lExact AS LOGIC ) AS DWORD
		LOCAL nSize		AS DWORD
		IF ! ArrayHelpers.ValidateArrayParams(REF aTarget, REF uStart, REF uCount, OUT nSize)
			RETURN 0
		ENDIF
		LOCAL a := aTarget AS ARRAY
		LOCAL nStart := uStart	AS DWORD
		LOCAL nCount := uCount	AS DWORD
		LOCAL nRet := 0			AS DWORD
		LOCAL i					AS DWORD
		LOCAL cb				AS CODEBLOCK
		IF IsCodeBlock( x )
			cb := x
		ELSE
			IF lExact
				cb   := {|element| element == x } 
			ELSE
				cb   := {|element| element = x } 
			ENDIF
		ENDIF
		IF nStart < nSize
			FOR i := nStart UPTO nSize
				IF nCount > 0
					IF Eval( cb, a[i] )
						nRet := i
						EXIT
					ENDIF
					nCount--
				ELSE
					EXIT
				ENDIF
			NEXT
		ELSE
			FOR i := nStart DOWNTO nSize
				IF nCount > 0
					IF Eval( cb, a[i] )
						nRet := i
						EXIT
					ENDIF
					nCount--
				ELSE
					EXIT
				ENDIF
			NEXT
		ENDIF
		
		RETURN nRet

	STATIC METHOD AscanBin(cFuncName AS STRING, a AS ARRAY, seekVal AS USUAL, lExact AS LOGIC ) AS DWORD
		LOCAL dwLow        AS DWORD
		LOCAL dwHigh       AS DWORD
		LOCAL x            AS DWORD
		LOCAL dwRet        AS DWORD
		LOCAL iSave        AS DWORD
		LOCAL lIsCodeBlock AS LOGIC
		LOCAL iCBRet       AS USUAL 
		LOCAL iComp        AS INT
		LOCAL cb			  AS CODEBLOCK
		
		dwLow  := 1
		dwHigh := (DWORD) ALen(a)
		iSave := dwRet := 0
		iComp := 0
		lIsCodeblock := IsCodeBlock( seekVal )
		IF lIsCodeBlock
			cb := seekVal
		ELSEIF lExact
			cb := {|element| IIF( element == seekVal , 0, IIF( seekVal < element, -1, 1 ) ) }
		ELSE
			cb := {|element| IIF( element = seekVal , 0, IIF( seekVal < element, -1, 1 ) ) }
		ENDIF
		
		DO WHILE dwLow <= dwHigh
			x := dwLow + ( ( dwHigh - dwLow ) / 2 )
			
			IF iSave == x
				IF dwHigh - dwLow == 1
					x++
					dwLow++
				ELSE
					EXIT
				ENDIF
			ENDIF
			
			iSave := x
			IF lIsCodeBlock      
				iCBRet := Eval( cb, a[x] )
				IF ! IsNumeric( iCBRet )
					THROW Error.DataTypeError(cFuncName, "Return value of CodeBlock", 0)
				ELSE
					iComp := iCBRet
				ENDIF
			ELSE
				iComp := Eval( cb, a[x] )
			ENDIF
			IF iComp < 0
				dwHigh := x
			ELSEIF iComp > 0
				dwLow := x
			ELSE
				dwRet := x
				x--
				
				DO WHILE dwLow <= x
				
					IF lIsCodeBlock      
						iCBRet := Eval( cb, a[x] )
						IF ! IsNumeric( iCBRet )
							THROW Error.DataTypeError(cFuncName, "Return value of CodeBlock", 0)
						ELSE
							iComp := iCBRet
						ENDIF
					ELSE
						iComp := Eval( cb, a[x] )
					ENDIF
					
					IF iComp != 0
						EXIT
					ENDIF
					
					dwRet := x
					x--
				ENDDO
				dwLow := dwHigh + 1
			ENDIF
		ENDDO
		
		RETURN dwRet
		
		
		
	STATIC METHOD Aeval(aArray AS ARRAY, cbBlock AS ICODEBLOCK, nStart AS DWORD, nCount AS DWORD, bUpdateArray AS CONST LOGIC, bPassIndex AS CONST LOGIC )  AS ARRAY
		LOCAL elements := ALen(aArray) AS DWORD
		LOCAL last   AS DWORD
		LOCAL result AS USUAL
		LOCAL x      AS DWORD
		
		IF nStart == 0
			THROW  Error.ArgumentError( __ENTITY__, "nStart", 3, __CavoStr( VOErrors.ArgCannotBeZero ), { nStart } )
		ENDIF
		
		IF nCount != 0
		
			IF nStart < 0
				nStart := elements + 1 + nStart
			ENDIF
			
			IF nCount > 0
			
				last := Math.Min( nStart + ( nCount - 1 ), elements )
				
				FOR x := nStart UPTO last
					IF ( bPassIndex )
						result := Eval( cbBlock, aArray[x], x )
					ELSE
						result := Eval( cbBlock, aArray[x] )
					ENDIF
					
					IF ( bUpdateArray )
						aArray[x] := result
					ENDIF
				NEXT
			ELSE
			
				last := Math.Max( 1, nStart + nCount + 1 )
				
				FOR x := nStart DOWNTO last
					IF ( bPassIndex )
						result := Eval( cbBlock, aArray[x], x )
					ELSE
						result := Eval( cbBlock, aArray[x] )
					ENDIF
					
					IF ( bUpdateArray )
						aArray[x] := result
					ENDIF
				NEXT
			ENDIF
		ENDIF
		
		RETURN aArray
		
	STATIC METHOD AEvalCheckArgs(aArray AS ARRAY, cb AS ICodeBlock, iStart REF USUAL, iCount REF USUAL,cFuncName AS STRING) AS LOGIC
		IF aArray == NULL_ARRAY
			THROW Error.NullArgumentError( cFuncName, NAMEOF(aArray),1)
		ENDIF
		IF cb == NULL
			THROW Error.NullArgumentError( cFuncName, NAMEOF(cb),2)
		ENDIF
		DEFAULT( REF iStart, 1)
		DEFAULT( REF iCount, ALen(aArray))
		IF ! IsNumeric(iStart)
			THROW Error.ArgumentError( cFuncName, NAMEOF(iStart), 3 , <OBJECT>{iStart})
		ENDIF
		IF ! IsNumeric(iCount)
			THROW Error.ArgumentError( cFuncName, NAMEOF(iCount), 4 , <OBJECT>{iCount})
		ENDIF
		RETURN TRUE

	STATIC METHOD ValidateArrayParams(aTarget REF USUAL, nStart REF USUAL,nCount REF USUAL, nSize OUT DWORD) AS LOGIC
		nSize := 0
		IF IsArray( aTarget )
			nSize := ALen( (ARRAY) aTarget )
			IF nSize == 0
				RETURN FALSE
			ENDIF
		ELSE
			RETURN FALSE
		ENDIF
		IF ! IsNumeric( nCount )
			nCount := nSize
		ENDIF
		IF ! IsNumeric( nStart )
			IF nCount < 0
				nStart := nSize
			ELSE
				nStart := 1
			ENDIF
		ELSE
			IF nStart > nSize
				RETURN FALSE
			ENDIF
		ENDIF
		
		IF nCount < 0
			nSize  := 1
			nCount := -nCount
		ENDIF
		RETURN TRUE

	END CLASS
		
		
/// <summary>
/// Create an uninitialized, one-dimensional Array.
/// </summary>
/// <param name="dwDim">The number of elements in the new Array.</param>
/// <returns>
/// An uninitialized of the given length.
/// </returns>
FUNCTION ArrayCreate(dwDim AS DWORD) AS ARRAY 
	RETURN __Array{dwDim}
	
	
/// <summary>
/// Create an uninitialized, one-dimensional Array.
/// </summary>
/// <param name="dwDim">The number of elements in the new Array.</param>
/// <returns>
/// An uninitialized of the given length.
/// </returns>
FUNCTION ArrayCreate<T>(dwDim AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	RETURN __ArrayBase<T>{dwDim}
	
	
/// <summary>
/// Create an initialized Array.
/// </summary>
/// <param name="dwDim"></param>
/// <param name="ptrBuff"></param>
/// <returns>
/// </returns>
FUNCTION ArrayInit(dwDim AS DWORD, avalues REF USUAL[]) AS ARRAY 
	LOCAL aTemp AS ARRAY
	LOCAL x AS DWORD
	
	IF dwDim > (DWORD) aValues:Length
		THROW Error.ArgumentError( __ENTITY__, NAMEOF(dwDim), "Element too big")
	ENDIF
	
	aTemp := ArrayNew(aValues:Length)
	FOR x := 1 UPTO aValues:Length
		aTemp [x] := aValues[x] 
	NEXT
	RETURN aTemp
	
/// <summary>
/// Add a new element to the end of an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
FUNCTION AAdd(a AS ARRAY,x AS USUAL) AS USUAL
	RETURN AAdd<USUAL>(a, x)
	
/// <summary>
/// Add a new element to the end of an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
FUNCTION AAdd<T>(a AS __ArrayBase<T>,x AS T) AS T WHERE T IS NEW()
	a:Add(x)
	RETURN x 	
	
/// <summary>
/// Duplicate an array .
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION AClone(a AS ARRAY) AS ARRAY
	IF a == NULL_ARRAY
		RETURN a
	END IF
	RETURN (ARRAY) a:Clone()
	
/// <summary>
/// Duplicate a multidimensional Array.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION AClone<T>(a AS __ArrayBase<T>) AS __ArrayBase<T> WHERE T IS NEW()
	IF a == NULL
		RETURN a
	END IF
	RETURN  a:Clone()
	
	
/// <summary>
/// Duplicate an Array without its subArrays.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION ACloneShallow(a AS ARRAY) AS ARRAY
	IF a == NULL_ARRAY
		RETURN a
	END IF
	RETURN (ARRAY) a:CloneShallow()
	
/// <summary>
/// Duplicate a multidimensional Array.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION ACloneShallow<T>(a AS __ArrayBase<T>) AS __ArrayBase<T> WHERE T IS NEW()
	IF a == NULL
		RETURN a
	END IF
	RETURN a:Clone()
	
/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ADel(a AS ARRAY,dwEl AS DWORD) AS ARRAY
	a:Delete((INT) dwEl)  
	RETURN a
	
/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ADel<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	a:Delete((INT) dwEl)  
	RETURN a
	
/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ATrueDel(a AS ARRAY,dwEl AS DWORD) AS ARRAY
	a:RemoveAt((INT) dwEl)  
	RETURN a
	
/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ATrueDel<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	a:RemoveAt((INT) dwEl)  
	RETURN a
	
/// <summary>Calculate the # of dimensions in an array</summary>
/// <param name="a"></param>
/// <returns>Number of dimensions in an array</returns>
FUNCTION ADim(a AS ARRAY) AS DWORD
	LOCAL dwDims AS DWORD
	dwDims := 1
	DO WHILE a != NULL_ARRAY
		IF ALen(a) > 0 
			IF IsArray(a[1])
				dwDims += 1
				a := a[1]
			ELSE
				EXIT
			ENDIF
		ELSE
			EXIT
		ENDIF
	ENDDO
	RETURN dwDims
	
/// <summary>Calculate a string that represents the dimensions in an array</summary>
/// <param name="a"></param>
/// <returns>String that displays the dimensions in an array</returns>
FUNCTION ADimPic(a AS ARRAY) AS STRING
	RETURN repl("[]", aDim(a))
	
	
/// <summary>
/// Insert an element into an Array and assign it a NIL value.
/// </summary>
/// <param name="a">The array into which the element will be inserted.</param>
/// <param name="dwEl">The position at which the element will be inserted.</param>
/// <returns>A reference to the original array</returns>
FUNCTION AIns(a AS ARRAY,dwEl AS DWORD) AS ARRAY 
	a:Insert((INT) dwEl) 
	RETURN a
	
/// <summary>
/// Insert an element into an Array and assign it a NIL value.
/// </summary>
/// <param name="a">The array into which the element will be inserted.</param>
/// <param name="dwEl">The position at which the element will be inserted.</param>
/// <returns>A reference to the original array</returns>
FUNCTION AIns<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	a:Insert((INT) dwEl) 
	RETURN a
	
/// <summary>
/// Return the number of elements in an Array.
/// </summary>
/// <param name="a">The array to count.</param>
/// <returns>The number of elements in the array.  If the array is empty, ALen() returns 0.
/// </returns>
FUNCTION ALen<T>(a AS __ArrayBase<T>) AS DWORD WHERE T IS NEW()
	IF a != NULL
		RETURN a:Length
	ELSE
		RETURN 0
	ENDIF
	
/// <summary>
/// Return the number of elements in an Array.
/// </summary>
/// <param name="a">The array to count.</param>
/// <returns>The number of elements in the array.  If the array is empty, ALen() returns 0.
/// </returns>
FUNCTION ALen(a AS ARRAY) AS DWORD 
	IF a != NULL
		RETURN a:Length
	ELSE
		RETURN 0
	ENDIF
	
/// <summary>
/// Removes write protection from an entire Array.
/// </summary>
/// <param name="a">The array to deprotect.</param>
/// <returns>TRUE if the array was successfully deprotected; otherwise, FALSE.
/// </returns>
FUNCTION ArrayDeProtect<T>(a AS __ArrayBase<T>) AS LOGIC WHERE T IS NEW()
	RETURN a:Lock(FALSE)
	
/// <summary>
/// Removes write protection from an entire Array.
/// </summary>
/// <param name="a">The array to deprotect.</param>
/// <returns>TRUE if the array was successfully deprotected; otherwise, FALSE.
/// </returns>
FUNCTION ArrayDeProtect(a AS ARRAY) AS LOGIC 
	RETURN a:Lock(FALSE)
	
/// <summary>
/// Read an Array element.
/// </summary>
/// <param name="a">The array to read.</param>
/// <param name="dwEl">The number of the element to read.</param>
/// <returns>The value held by the element.
/// </returns>
	
FUNCTION ArrayGet(a AS ARRAY,dwEl AS DWORD) AS USUAL
	RETURN a:__GetElement( (INT) dwEl-1)
	
/// <summary>
/// Read an Array element.
/// </summary>
/// <param name="a">The array to read.</param>
/// <param name="dwEl">The number of the element to read.</param>
/// <returns>The value held by the element.
/// </returns>
	
FUNCTION ArrayGet<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS T WHERE T IS NEW()
	RETURN a:__GetElement( (INT) dwEl-1)
	
/// <summary>
/// Protect an Array from change in all functions except the one in which it was declared.
/// </summary>
/// <param name="a">The array to protect.</param>
/// <returns>TRUE if the array was successfully protected; otherwise, FALSE.
/// </returns>
FUNCTION ArrayProtect(a AS ARRAY) AS LOGIC
	RETURN a:Lock(TRUE)
	
/// <summary>
/// Protect an Array from change in all functions except the one in which it was declared.
/// </summary>
/// <param name="a">The array to protect.</param>
/// <returns>TRUE if the array was successfully protected; otherwise, FALSE.
/// </returns>
FUNCTION ArrayProtect<T>(a AS __ArrayBase<T>) AS LOGIC WHERE T IS NEW()
	RETURN a:Lock(TRUE)	
	
/// <summary>
/// Write a value to an Array element.
/// </summary>
/// <param name="a">The array to write to.</param>
/// <param name="dwEl">The number of the array element to receive the value.</param>
/// <param name="u">The value to write to the array element.</param>
/// <returns>The value assigned.
/// </returns>
FUNCTION ArrayPut<T>(a AS __ArrayBase<T>,dwEl AS DWORD,u AS T) AS T WHERE T IS NEW()
	a:__SetElement(u, (INT)dwEl -1)
	RETURN u
	
/// <summary>
/// Write a value to an Array element.
/// </summary>
/// <param name="a">The array to write to.</param>
/// <param name="dwEl">The number of the array element to receive the value.</param>
/// <param name="u">The value to write to the array element.</param>
/// <returns>The value assigned.
/// </returns>
FUNCTION ArrayPut(a AS ARRAY,dwEl AS DWORD,u AS USUAL) AS USUAL
	a:__SetElement(u, (INT)dwEl -1)
	RETURN u
	
/// <summary>
/// Store an Array to a buffer.
/// </summary>
/// <param name="a">The array to be stored to a buffer.</param>
/// <param name="Buff">A pointer to the buffer.</param>
/// <param name="dwLen">The length of the buffer</param>
/// <returns>The number of values stored to the buffer.</returns>
FUNCTION ArrayStore(a AS ARRAY,Buff AS USUAL PTR,dwLen AS DWORD) AS DWORD
	LOCAL i AS DWORD
	LOCAL nLen AS DWORD
	nLen := ALen(a)
	dwLen := Math.Min(dwLen, nLen)
	FOR i := 1 TO dwLen
		buff[i] := a[i]
	NEXT
	RETURN dwLen
	
/// <summary>
/// Store an Array to a buffer.
/// </summary>
/// <param name="a">The array to be stored to a buffer.</param>
/// <param name="Buff">A pointer to the buffer.</param>
/// <param name="dwLen">The length of the buffer</param>
/// <returns>The number of values stored to the buffer.</returns>
FUNCTION ArrayStore<T>(a AS __ArrayBase<T>,Buff AS T PTR,dwLen AS DWORD) AS DWORD WHERE T IS NEW()
	LOCAL i AS DWORD
	LOCAL nLen AS DWORD
	nLen := ALen(a)
	dwLen := Math.Min(dwLen, nLen)
	FOR i := 1 TO dwLen
		buff[i] := a[i]
	NEXT
	RETURN dwLen
	
/// <summary>
/// Replace an Array element with a new value and return the old value.
/// </summary>
/// <param name="a">The array whose element will be replaced with a new value.</param>
/// <param name="dwEl">The number of the element to be replaced.</param>
/// <param name="u">The new value.</param>
/// <returns>The original value that was replaced by u.</returns>
FUNCTION ArraySwap(a AS ARRAY,dwEl AS DWORD,u AS USUAL) AS USUAL
	RETURN a:Swap((INT) dwEl, u)
	
/// <summary>
/// Replace an Array element with a new value and return the old value.
/// </summary>
/// <param name="a">The array whose element will be replaced with a new value.</param>
/// <param name="dwEl">The number of the element to be replaced.</param>
/// <param name="u">The new value.</param>
/// <returns>The original value that was replaced by u.</returns>
FUNCTION ArraySwap<T>(a AS __ArrayBase<T>,dwEl AS DWORD,u AS T) AS T  WHERE T IS NEW()
	RETURN a:Swap((INT) dwEl, u)
	
/// <summary>
/// Scan an array until a value is found or a code block returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="uSearch">The number of the element to be replaced.</param>
/// <param name="nStart">The number of the element to be replaced.</param>
/// <param name="nCount">The new value.</param>
/// <returns>If uSearch is a code block, AScan() returns the position of the first element for which the code block returns TRUE.  Otherwise, AScan() returns the position of the first matching element.  AScan() returns 0 if no match is found.</returns>
FUNCTION Ascan(aTarget AS ARRAY, uSearch AS USUAL,nStart AS LONG,nCount AS LONG) AS DWORD 
	RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, nCount, SetExact()) 

/// <summary>
/// Scan an array until a value is found or a code block returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="uSearch">The number of the element to be replaced.</param>
/// <param name="nStart">The number of the element to be replaced.</param>
/// <returns>If uSearch is a code block, AScan() returns the position of the first element for which the code block returns TRUE.  Otherwise, AScan() returns the position of the first matching element.  AScan() returns 0 if no match is found.</returns>
FUNCTION Ascan(aTarget AS ARRAY, uSearch AS USUAL,nStart AS LONG) AS DWORD 
	RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, ALen(aTarget), SetExact()) 


/// <summary>
/// Scan an array until a value is found or a code block returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="uSearch">The number of the element to be replaced.</param>
/// <returns>If uSearch is a code block, AScan() returns the position of the first element for which the code block returns TRUE.  Otherwise, AScan() returns the position of the first matching element.  AScan() returns 0 if no match is found.</returns>
FUNCTION Ascan(aTarget AS ARRAY, uSearch AS USUAL) AS DWORD 
	RETURN ArrayHelpers.Ascan( aTarget, uSearch, 1, ALen(aTarget), SetExact()) 
	

/// <summary>
/// Scan an array until an exact match with a value is found or a code block returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="uSearch">The number of the element to be replaced.</param>
/// <param name="nStart">The number of the element to be replaced.</param>
/// <param name="nCount">The new value.</param>
/// <returns>If uSearch is a code block, AScan() returns the position of the first element for which the code block returns TRUE.  Otherwise, AScan() returns the position of the first matching element.  AScan() returns 0 if no match is found.</returns>
FUNCTION AScanExact( aTarget AS ARRAY, uSearch AS USUAL, nStart AS INT, nCount AS INT) AS DWORD 
	RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, nCount, TRUE )

/// <summary>
/// Scan an array until an exact match with a value is found or a code block returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="uSearch">The number of the element to be replaced.</param>
/// <param name="nStart">The number of the element to be replaced.</param>
/// <returns>If uSearch is a code block, AScan() returns the position of the first element for which the code block returns TRUE.  Otherwise, AScan() returns the position of the first matching element.  AScan() returns 0 if no match is found.</returns>
FUNCTION AScanExact( aTarget AS ARRAY, uSearch AS USUAL, nStart AS INT) AS DWORD 
	RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, ALen(aTarget), TRUE )


/// <summary>
/// Scan an array until an exact match with a value is found or a code block returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="uSearch">The number of the element to be replaced.</param>
/// <param name="nStart">The number of the element to be replaced.</param>
/// <returns>If uSearch is a code block, AScan() returns the position of the first element for which the code block returns TRUE.  Otherwise, AScan() returns the position of the first matching element.  AScan() returns 0 if no match is found.</returns>
FUNCTION AScanExact( aTarget AS ARRAY, uSearch AS USUAL) AS DWORD 
	RETURN ArrayHelpers.Ascan( aTarget, uSearch, 1, ALen(aTarget), TRUE )

	
/// <summary>
/// Scan a sorted Array until a value is found or a code block returns 0.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
FUNCTION AScanBin(a AS ARRAY,x AS USUAL) AS DWORD
	RETURN ArrayHelpers.AScanBin( "AscanBin" , a, x, FALSE )
	
/// <summary>
/// Scan a sorted Array until there is an exact match or a code block returns 0.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
FUNCTION AScanBinExact(a AS ARRAY,x AS USUAL) AS DWORD
	RETURN ArrayHelpers.AScanBin( "AscanBin" , a, x, TRUE )


/// <summary>
/// Scan an array until value is found.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="act">The lambda expression to use for looking up the correct element.</param>
/// <returns>AScan() returns the position of the first element for which the expression returns TRUE.</returns>
FUNCTION Ascan<T>(aTarget AS __ArrayBase<T>, element AS T) AS DWORD WHERE T IS NEW()
	RETURN ArrayHelpers.Ascan( aTarget, element,1, (INT) aTarget:Length) 

/// <summary>
/// Scan an array until an expression returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="act">The lambda expression to use for looking up the correct element.</param>
/// <returns>AScan() returns the position of the first element for which the expression returns TRUE.</returns>
FUNCTION Ascan<T>(aTarget AS __ArrayBase<T>, act AS @@Func<T,LOGIC>) AS DWORD WHERE T IS NEW()
	RETURN ArrayHelpers.Ascan( aTarget, act,1, (INT) aTarget:Length) 


/// <summary>
/// Scan an array until value is found.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="act">The lambda expression to use for looking up the correct element.</param>
/// <param name="nStart">The starting element.  Negative values are not supported for the typed arrays.</param>
/// <returns>AScan() returns the position of the first element for which the expression returns TRUE.</returns>
FUNCTION Ascan<T>(aTarget AS __ArrayBase<T>, element AS T, nStart AS LONG) AS DWORD WHERE T IS NEW()
	RETURN ArrayHelpers.Ascan( aTarget, element, nStart, (INT) aTarget:Length- nStart +1) 


/// <summary>
/// Scan an array until an expression returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="act">The lambda expression to use for looking up the correct element.</param>
/// <param name="nStart">The starting element.  Negative values are not supported for the typed arrays.</param>
/// <returns>AScan() returns the position of the first element for which the expression returns TRUE.</returns>
FUNCTION Ascan<T>(aTarget AS __ArrayBase<T>, act AS @@Func<T,LOGIC>, nStart AS LONG) AS DWORD WHERE T IS NEW()
	RETURN ArrayHelpers.Ascan( aTarget, act, nStart, (INT) aTarget:Length - nStart +1) 



/// <summary>
/// Scan an array until value is found.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="act">The lambda expression to use for looking up the correct element.</param>
/// <param name="nStart">The starting element.  Negative values are not supported for the typed arrays.</param>
/// <param name="nCount">The number of elements to process from nStart.</param>
/// <returns>AScan() returns the position of the first element for which the expression returns TRUE.</returns>
FUNCTION Ascan<T>(aTarget AS __ArrayBase<T>, element AS T, nStart AS LONG, nCount AS LONG) AS DWORD WHERE T IS NEW()
	RETURN ArrayHelpers.Ascan( aTarget, element, nStart, nCount) 

/// <summary>
/// Scan an array until an expression returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="act">The lambda expression to use for looking up the correct element.</param>
/// <param name="nStart">The starting element.  Negative values are not supported for the typed arrays.</param>
/// <param name="nCount">The number of elements to process from nStart.</param>
/// <returns>AScan() returns the position of the first element for which the expression returns TRUE.</returns>
FUNCTION Ascan<T>(aTarget AS __ArrayBase<T>, act AS @@Func<T,LOGIC>, nStart AS LONG, nCount AS LONG) AS DWORD WHERE T IS NEW()
	RETURN ArrayHelpers.Ascan( aTarget, act, nStart, nCount) 

	
/// <summary>
/// Grow or shrink an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="dwDim"></param>
/// <returns>
/// </returns>
FUNCTION ASize(a AS ARRAY,dwDim AS DWORD) AS ARRAY
	a:Resize((INT) dwDim) 
	RETURN a  
	
/// <summary>
/// Grow or shrink an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="dwDim"></param>
/// <returns>
/// </returns>
FUNCTION ASize<T>(a AS __ArrayBase<T>,dwDim AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	a:Resize((INT) dwDim) 
	RETURN a  
	
	
/// <summary>
/// Return the highest numbered element of an Array.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION ATail(a AS ARRAY) AS USUAL
	RETURN a:Tail()
	
/// <summary>
/// Return the highest numbered element of an Array.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION ATail<T>(a AS __ArrayBase<T>) AS T WHERE T IS NEW()
	RETURN a:Tail()
	
	
	
/// <summary>
/// Copy elements from one Array to another.
/// </summary>
/// <param name="uSource"></param>
/// <param name="uTarget"></param>
/// <param name="nStart"></param>
/// <param name="nCount"></param>
/// <param name="nStartDest"></param>
/// <returns>
/// </returns>
FUNCTION ACopy(uSource ,uTarget ,nStart ,nCount ,nStartDest ) AS ARRAY CLIPPER
	LOCAL aSource  AS ARRAY
	LOCAL aTarget  AS ARRAY
	LOCAL sourceLen  AS DWORD
	LOCAL start AS DWORD
	LOCAL count AS DWORD
	IF IsArray( uSource )
       aSource := uSource
    ELSE
      THROW Error.ArgumentError( __ENTITY__, NAMEOF(uSource), 1, <OBJECT>{ uSource } )
    ENDIF
   
     IF IsArray( uTarget )
        aTarget := uTarget
     ELSE
        THROW Error.ArgumentError( __ENTITY__, NAMEOF(uTarget), 2, <OBJECT>{ uTarget } )
     ENDIF
	 start := 1
	 sourceLen  := ALen(aSource)
	 IF pCount() > 2
		IF IsNumeric(nStart)
			start := nStart
		ELSE
			THROW Error.ArgumentError( __ENTITY__, NAMEOF(nStart), 3, <OBJECT>{ nStart } )
		ENDIF
		IF start < 0
			start := sourceLen
		ELSE
			start := Math.Min(start, sourceLen)
		ENDIF
	 ENDIF
	 IF pCount() > 3
		IF IsNumeric(nCount)
			count := nCount
		ELSE
			THROW Error.ArgumentError( __ENTITY__, NAMEOF(nCount), 4, <OBJECT>{ nCount } )
		ENDIF
		IF count > 0
			sourceLen := Math.Min(sourceLen, start+count-1)
		ELSE
			sourceLen := Math.Max(1, start+count-1)
		ENDIF
	 ENDIF
	 LOCAL offSet		:= 1 AS DWORD
	 LOCAL targetLen	:= ALen(aTarget) AS DWORD
	 IF pCount() > 4
		IF IsNumeric(nStartDest)
			offSet := nStartDest
			offSet := Math.Min( offSet, targetLen )
            offSet := Math.Max( 1, offSet )
		ELSE
			THROW Error.ArgumentError( __ENTITY__, NAMEOF(nStartDest), 5, <OBJECT>{ nStartDest } )
		ENDIF
	 ENDIF
	 XSharp.__Array.Copy(aSource, aTarget, start, sourceLen, offSet, targetLen)

	 RETURN aTarget   
	
	
/// <summary>
/// Fill Array elements with a specified value.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="Start"></param>
/// <param name="Stop"></param>
/// <returns>
/// </returns>
FUNCTION AFill(a AS ARRAY,fill := NIL AS USUAL, start := NIL AS USUAL, count := NIL AS USUAL) AS ARRAY 
	// The behavior of AFill() in VO is different than what is descibed in the help file
	// - if start <= 0 throws an error
	// - if start == NIL, then start becomes 1
	// - if count < 0 then it does nothing, unless start == nil, in which case start becomes 1 and count becomes 1, too (yeah, crap!)
	// - if count == nil, then it fills from start to lenght of array
	
	// warning, with the current definition of the function, it is not possible for the user to omit the start param
	LOCAL nLen := ALen( a ) AS DWORD
	IF nLen > 0

		LOCAL lStartWasNil := FALSE AS LOGIC
		IF start == NIL
			start := 1
			lStartWasNil := TRUE
		ENDIF

		IF start > nLen .OR. start <= 0
			THROW Error.BoundError( "AFill", "start", 3, <OBJECT>{ start } )
		ENDIF
		IF count == NIL
			count := (INT)nLen - start + 1
		ELSEIF count > 0
			// VO does not throw an error if count is longer than the array
			IF start + count - 1 > nLen
				count := (INT)nLen - start + 1
			END IF
		ELSE
			IF lStartWasNil
				count := 1
			ELSE
				RETURN a
			END IF
		END IF

		FOR LOCAL x := start AS INT UPTO start + count - 1
			a[(DWORD) x] := fill
		NEXT
	ENDIF
	RETURN a
	
	
	
/// <summary>
/// Create an empty Array.
/// </summary>
/// <returns>
/// </returns>
FUNCTION ArrayBuild() AS ARRAY
	RETURN ARRAY{}
	
/// <summary>
/// Create an uninitialized Array with a single dimension and no elements.
/// </summary>
/// <returns>
/// </returns>
FUNCTION ArrayNew() AS ARRAY
	RETURN ArrayNew(0)
	
/// <summary>
/// Create an uninitialized Array with the specified number of elements and dimensions.
/// </summary>
/// <param name="nDim"></param>
/// <returns>
/// </returns>
FUNCTION ArrayNew(aDims PARAMS INT[]) AS ARRAY
	RETURN __Array.ArrayCreate(aDims)


/// <summary>
/// Create an uninitialized Array with the specified number of elements and dimensions.
/// </summary>
/// <param name="nDim"></param>
/// <returns>
/// </returns>
FUNCTION ArrayNew(aDims PARAMS DWORD[]) AS ARRAY
	LOCAL aDimInt AS INT[]
	LOCAL i AS INT
	aDimInt := INT[]{aDims:Length}
	FOR i := 1 TO aDims:Length
		aDimInt[i] := (INT) aDims[i]
	NEXT
	RETURN __Array.ArrayCreate(aDimInt)



/// <summary>
/// Create an uninitialized Array with the specified number of elements and dimensions.
/// </summary>
/// <param name="nDim"></param>
/// <returns>
/// </returns>
FUNCTION ArrayNew<T>(nSize AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	RETURN __ArrayBase<T>{nSize}
	
	
/// <summary>
/// Create an uninitialized Array with the specified number of elements and dimensions.
/// </summary>
/// <param name="nDim"></param>
/// <returns>
/// </returns>
FUNCTION ArrayNew<T>(nSize AS INT) AS __ArrayBase<T> WHERE T IS NEW()
	RETURN __ArrayBase<T>{(DWORD) nSize}
	

/// <summary>
/// To create an Array and fill its elements with a default value.
/// </summary>
/// <param name="x"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
FUNCTION AReplicate(x AS USUAL,nCount AS DWORD) AS ARRAY
	VAR a:= __Array{nCount} 
	LOCAL i AS DWORD
	FOR i := 1 TO nCount
		a[i] := x
	NEXT
	RETURN a
	
	
/// <summary>
/// Sort an Array.
/// </summary>
/// <param name="uArray"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION ASort(aArray AS ARRAY, startIndex := NIL AS USUAL,nCount := NIL AS USUAL,cbOrder := NIL AS USUAL) AS ARRAY 
	LOCAL nLen AS DWORD
	DEFAULT( REF startIndex, 1 )
	
	nLen := ALen(aArray) 
	IF nLen == 0 // Let it execute if nLen == 1, maybe the codeblock is important to be executed in this case for some (user) reason
		RETURN aArray
	END IF

	EnforceNumeric( startIndex )
	DEFAULT( REF nCount, nLen - startIndex + 1 )
	EnforceNumeric( nCount )
	
	// Note: ASort() in VO accepts arguments out of bounds and translates them this way:
	IF startIndex <= 0
		startIndex := 1
	ELSEIF startIndex > nLen
		RETURN aArray
	END IF
	
	IF nCount <= 0 .OR. startIndex + nCount - 1 > nLen
		nCount := nLen - startIndex + 1
	ENDIF
	
/*	IF startIndex < 1 .or. startIndex > nLen
		THROW Error.ArgumentError( __ENTITY__, NAMEOF(startIndex), 2, <OBJECT>{ startIndex } )
	ENDIF 
	IF nCount + startIndex > ALen((ARRAY)aArray)+1
		THROW Error.ArgumentError( __ENTITY__, NAMEOF(nCount), 3, <OBJECT>{ nCount } )
	ENDIF */
	
	
	IF cbOrder != NIL && ( ( ! cbOrder:IsCodeBlock ) || ((CODEBLOCK)cbOrder):PCount() != 2 )
		THROW Error.ArgumentError( __ENTITY__, "cbOrder", 4, <OBJECT>{ cbOrder } )
	ENDIF
	

	IF cbOrder == NIL
		aArray:Sort( startIndex, nCount, NULL ) // this uses __Usual.ICompareTo()
	ELSE
		aArray:Sort( startIndex, nCount, ArraySortComparer{ cbOrder } )
	ENDIF   
	
	RETURN aArray
	
	// This wraps a codeblock and provides an IComparer implementation so
	// we can use ArrayList:Sort() with a codeblock.

INTERNAL STRUCTURE ArraySortComparer  IMPLEMENTS System.Collections.Generic.IComparer<USUAL> 

	PRIVATE _cb AS ICodeBlock
	
	CONSTRUCTOR( cb AS ICodeBlock)
		_cb := cb
		RETURN
		
	METHOD Compare( x AS USUAL, y AS USUAL ) AS INT
		IF x == y
			RETURN 0
		ENDIF
		LOCAL u AS USUAL
		u := _cb:EvalBlock( x, y )
		RETURN IIF (  (LOGIC) u , -1, 1 ) 
	RETURN 0
		
END STRUCTURE
 
INTERNAL STRUCTURE ArraySortComparer<T, U>  IMPLEMENTS System.Collections.Generic.IComparer<T> WHERE T IS NEW()

	PRIVATE _cb AS @@Func<T,T,LOGIC>
	
	CONSTRUCTOR( cb AS @@Func<T,T,LOGIC> )
		_cb := cb
		RETURN
		
	METHOD Compare( x AS T, y AS T ) AS INT
		IF x:Equals( y )  
			RETURN 0
		ENDIF
		LOCAL u AS LOGIC
		u := _cb( x, y )
		RETURN IIF (  u , -1, 1 ) 
	RETURN 0
		
END STRUCTURE

/// <summary>
/// Sort an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION ASort<T>(aArray AS __ArrayBase<T> ,startIndex AS INT,nCount AS INT,cbOrder AS @@Func<T,T,LOGIC>) AS __ArrayBase<T> WHERE T IS NEW()

	aArray:Sort( startIndex, nCount, ArraySortComparer<T, LOGIC> { cbOrder } )
	
	RETURN aArray


/// <summary>
/// Sort an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION ASort<T>(aArray AS __ArrayBase<T> ,cbOrder AS @@Func<T,T,LOGIC>) AS __ArrayBase<T> WHERE T IS NEW()

	aArray:Sort( ArraySortComparer<T, LOGIC> { cbOrder } )
	
	RETURN aArray


/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION AEval<T>(aArray AS __ArrayBase<T>, cb AS Action<T>) AS __ArrayBase<T> WHERE T IS NEW()
	RETURN AEval(aArray, cb, 1, ALen(aArray) )


/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <returns>
/// </returns>
FUNCTION AEval<T>(aArray AS __ArrayBase<T>, cb AS Action<T>,iStart AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	RETURN AEval(aArray, cb, iStart, ALen(aArray) - iStart +1)
	

/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEval<T>(aArray AS __ArrayBase<T>, cb AS Action<T>,iStart AS DWORD,iCount AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	LOCAL nX AS DWORD
	LOCAL nEnd AS DWORD
	nEnd := iStart + iCount -1
	FOR nX := iStart TO nEnd
		cb(aArray[ nX])
	NEXT
	RETURN aArray



/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION AEval(aArray AS ARRAY,cb AS ICodeBlock ) AS USUAL 
	LOCAL iStart AS USUAL
	LOCAL iCount AS USUAL
	iStart := NIL
	iCount := NIL
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF iStart, REF iCount, "AEval")
	RETURN ArrayHelpers.AEval( aArray, cb, iStart, iCount, FALSE, FALSE )

/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <returns>
/// </returns>
FUNCTION AEval(aArray AS ARRAY,cb AS ICODEBLOCK ,iStart AS INT ) AS USUAL 
	LOCAL uCount AS USUAL
	LOCAL uStart AS USUAL
	uCount := NIL
	uStart  := iStart
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEval")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart, uCount, FALSE, FALSE )

/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEval(aArray AS ARRAY,cb AS ICodeBlock ,iStart AS INT ,iCount AS INT) AS USUAL 
	LOCAL uCount AS USUAL
	LOCAL uStart	 AS USUAL
	uCount := iCount
	uStart  := iStart
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEval")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart, uCount , FALSE, FALSE )


/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION AEvalA(aArray AS ARRAY ,cb AS ICodeBlock) AS ARRAY
	LOCAL uCount AS USUAL
	LOCAL uStart	 AS USUAL
	uCount := NIL
	uStart  := NIL
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEvalA")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart,uCount , TRUE, FALSE )
	

/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <returns>
/// </returns>
FUNCTION AEvalA(aArray AS ARRAY ,cb AS ICodeBlock, iStart AS INT ) AS ARRAY
	LOCAL uCount AS USUAL
	LOCAL uStart	 AS USUAL
	uCount := NIL
	uStart  := iStart
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEvalA")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart,uCount , TRUE, FALSE )
	
/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEvalA(aArray AS ARRAY ,cb AS ICodeBlock, iStart AS INT ,iCount AS INT) AS ARRAY
	LOCAL uCount AS USUAL
	LOCAL uStart	 AS USUAL
	uCount := iCount
	uStart  := iStart
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEvalA")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart,uCount , TRUE, FALSE )


/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION AEvalA<T>(aArray AS __ArrayBase<T>, cb AS @@Func<T,T>) AS __ArrayBase<T> WHERE T IS NEW()
	RETURN AEvalA(aArray, cb, 1, ALen(aArray))


/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <returns>
/// </returns>
FUNCTION AEvalA<T>(aArray AS __ArrayBase<T>, cb AS @@Func<T,T>,iStart AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	RETURN AEvalA(aArray, cb, iStart, ALen(aArray) - iStart +1)

	

/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEvalA<T>(aArray AS __ArrayBase<T>, cb AS @@Func<T,T>,iStart AS DWORD,iCount AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	LOCAL nX AS DWORD
	LOCAL nEnd AS DWORD
	nEnd := iStart + iCount -1
	FOR nX := iStart TO nEnd
		aArray[ nX] := cb(aArray[ nX])
	NEXT
	RETURN aArray

	
/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEvalOld(aArray AS ARRAY ,cb AS ICodeBlock, iStart AS INT ,iCount AS INT) AS ARRAY
	LOCAL uCount	AS USUAL
	LOCAL uStart	 AS USUAL
	uCount := iCount
	uStart  := iStart
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEvalOld")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart,uCount , FALSE, TRUE)


/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEvalOld(aArray AS ARRAY ,cb AS ICodeBlock, iStart AS INT ) AS ARRAY
	LOCAL uCount	AS USUAL
	LOCAL uStart	 AS USUAL
	uCount := NIL
	uStart  := iStart
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEvalOld")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart,uCount , FALSE, TRUE) 


/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="aArray"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEvalOld(aArray AS ARRAY ,cb AS ICodeBlock) AS ARRAY
	LOCAL uCount	AS USUAL
	LOCAL uStart	 AS USUAL
	uCount := NIL
	uStart := NIL
	ArrayHelpers.AEvalCheckArgs(aArray, cb, REF uStart, REF uCount, "AEvalOld")
	RETURN ArrayHelpers.AEval( aArray, cb, uStart,uCount , FALSE, TRUE)


