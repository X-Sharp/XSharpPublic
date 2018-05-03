//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

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
FUNCTION ArrayCreate<T>(dwDim AS DWORD) AS __ArrayBase<T> WHERE T is NEW()
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
		THROW Error.ArgumentError( "ArrayInit", NAMEOF(dwDim), "Element too big")
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
FUNCTION AAdd<T>(a AS __ArrayBase<T>,x AS T) AS T where T is New()
	a:Add(x)
	RETURN x 	

/// <summary>
/// Duplicate a multidimensional Array.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION AClone(a AS ARRAY) AS ARRAY
	RETURN (ARRAY) a:Clone()

/// <summary>
/// Duplicate a multidimensional Array.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION AClone<T>(a AS __ArrayBase<T>) AS __ArrayBase<T> where T is New()
	RETURN  a:Clone()

		
/// <summary>
/// Duplicate an Array without its subArrays.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION ACloneShallow(a AS ARRAY) AS ARRAY
	RETURN (ARRAY) a:CloneShallow()

/// <summary>
/// Duplicate a multidimensional Array.
/// </summary>
/// <param name="a"></param>
/// <returns>
/// </returns>
FUNCTION ACloneShallow<T>(a AS __ArrayBase<T>) AS __ArrayBase<T> where T is New()
	RETURN a:Clone()
	
/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ADel(a AS ARRAY,dwEl AS DWORD) AS ARRAY
	a:Delete(dwEl)  
	RETURN a

/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ADel<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS __ArrayBase<T> where T is New()
	a:Delete(dwEl)  
	RETURN a
	
/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ATrueDel(a AS ARRAY,dwEl AS DWORD) AS ARRAY
	a:RemoveAt(dwEl)  
	RETURN a
	
/// <summary>
/// Delete an Array element.
/// </summary>
/// <param name="a"></param>
/// <param name="dwEl"></param>
/// <returns>
/// </returns>
FUNCTION ATrueDel<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS __ArrayBase<T> where T is New()
	a:RemoveAt(dwEl)  
	RETURN a
	
/// <summary>Calculate the # of dimensions in an array</summary>
/// <param name="a"></param>
/// <returns>Number of dimensions in an array</returns>
FUNCTION ADim(a AS ARRAY) AS DWORD
	LOCAL dwDims AS DWORD
	dwDims := 1
	DO WHILE a != NULL_ARRAY
		IF Alen(a) > 0 
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
FUNCTION AIns(a AS Array,dwEl AS DWORD) AS Array 
	a:Insert(dwEl) 
	RETURN a

/// <summary>
/// Insert an element into an Array and assign it a NIL value.
/// </summary>
/// <param name="a">The array into which the element will be inserted.</param>
/// <param name="dwEl">The position at which the element will be inserted.</param>
/// <returns>A reference to the original array</returns>
FUNCTION AIns<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	a:Insert(dwEl) 
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
FUNCTION ALen(a AS Array) AS DWORD 
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
FUNCTION ArrayDeProtect(a AS Array) AS LOGIC 
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
	
FUNCTION ArrayGet<T>(a AS __ArrayBase<T>,dwEl AS DWORD) AS T Where T is New()
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
FUNCTION ArrayProtect<T>(a AS __ArrayBase<T>) AS LOGIC Where T is New()
	RETURN a:Lock(TRUE)	

/// <summary>
/// Write a value to an Array element.
/// </summary>
/// <param name="a">The array to write to.</param>
/// <param name="dwEl">The number of the array element to receive the value.</param>
/// <param name="u">The value to write to the array element.</param>
/// <returns>The value assigned.
/// </returns>
FUNCTION ArrayPut<T>(a AS __ArrayBase<T>,dwEl AS DWORD,u AS T) AS T Where T is New()
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
	nLen := Alen(a)
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
	nLen := Alen(a)
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
/// <returns>The original value that was replaced by <uNewValue>.</returns>
FUNCTION ArraySwap(a AS ARRAY,dwEl AS DWORD,u AS USUAL) AS USUAL
	RETURN a:Swap(dwEl, u)

/// <summary>
/// Replace an Array element with a new value and return the old value.
/// </summary>
/// <param name="a">The array whose element will be replaced with a new value.</param>
/// <param name="dwEl">The number of the element to be replaced.</param>
/// <param name="u">The new value.</param>
/// <returns>The original value that was replaced by <uNewValue>.</returns>
FUNCTION ArraySwap<T>(a AS __ArrayBase<T>,dwEl AS DWORD,u AS T) AS T  WHERE T IS NEW()
	RETURN a:Swap(dwEl, u)
	
/// <summary>
/// Scan an array until a value is found or a code block returns TRUE.
/// </summary>
/// <param name="aTarget">The array whose element will be replaced with a new value.</param>
/// <param name="uSearch">The number of the element to be replaced.</param>
/// <param name="nStart">The number of the element to be replaced.</param>
/// <param name="nCount">The new value.</param>
/// <returns>If <uSearch> is a code block, AScan() returns the position of the first element for which the code block returns TRUE.  Otherwise, AScan() returns the position of the first matching element.  AScan() returns 0 if no match is found.</returns>
FUNCTION Ascan(aTarget, uSearch,nStart,nCount) AS DWORD CLIPPER
	// Validate arguments
	LOCAL nSize AS DWORD
	IF IsArray( aTarget )
		nSize := ALen( (ARRAY) aTarget )
		IF nSize == 0
			RETURN 0
		ENDIF
	ELSE
		RETURN 0
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
			RETURN 0
		ENDIF
	ENDIF
	
	IF nCount < 0
		nSize  := 1
		nCount := -nCount
	ENDIF
	
	RETURN __aScanWorker( aTarget, uSearch, nStart, nCount, SetExact(), nSize ) 
	
FUNCTION AScanExact( aTarget, uSearch, nStart, nCount ) AS DWORD CLIPPER
	LOCAL nSize AS DWORD
	
	IF IsArray( aTarget )
		nSize := ALen( (ARRAY)aTarget )
		IF nSize == 0
			RETURN 0
		ENDIF
	ELSE
		RETURN 0
	ENDIF
	
	IF ! IsNumeric( nCount )
		nCount := nSize
	ENDIF
	
	IF ! IsNumeric( nStart )
		IF nCount < 0
			nStart := nSize
			nSize  := 1
			nCount := -nCount
		ELSE
			nStart := 1
		ENDIF
	ENDIF
	
	RETURN __aScanWorker( aTarget, uSearch, nStart, nCount, TRUE, nSize )
	
INTERNAL FUNCTION __aScanWorker( a AS ARRAY, x AS USUAL, nStart AS DWORD, nCount AS DWORD, lExact AS LOGIC, nSize AS DWORD ) AS DWORD
	LOCAL nRet := 0	AS DWORD
	LOCAL cb		   AS CODEBLOCK
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
		FOR VAR i := nStart UPTO nSize
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
		FOR VAR i := nStart DOWNTO nSize
			IF nCount > 0
				IF Eval( x, a[i] )
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
	
/// <summary>
/// Scan a sorted Array until a value is found or a code block returns 0.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
FUNCTION AScanBin(a AS ARRAY,x AS USUAL) AS DWORD
	RETURN __aScanBinWorker( a, x, FALSE )
	
/// <summary>
/// Scan a sorted Array until there is an exact match or a code block returns 0.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
FUNCTION AScanBinExact(a AS ARRAY,x AS USUAL) AS DWORD
	RETURN __aScanBinWorker( a, x, TRUE )
	
INTERNAL FUNCTION __aScanBinWorker( a AS ARRAY, seekVal AS USUAL, lExact AS LOGIC ) AS DWORD
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
	dwHigh := (DWORD) Alen(a)
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
				THROW Error.DataTypeError("ASCANBIN", "Return value of CodeBlock", 0)
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
						THROW Error.DataTypeError("ASCANBIN", "Return value of CodeBlock", 0)
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

/// <summary>
/// Grow or shrink an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="dwDim"></param>
/// <returns>
/// </returns>
FUNCTION ASize(a AS ARRAY,dwDim AS DWORD) AS ARRAY
	a:Resize(dwDim) 
	RETURN a  

/// <summary>
/// Grow or shrink an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="dwDim"></param>
/// <returns>
/// </returns>
FUNCTION ASize<T>(a AS __ArrayBase<T>,dwDim AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	a:Resize(dwDim) 
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
/// <param name="a"></param>
/// <param name="aDest"></param>
/// <param name="nStart"></param>
/// <param name="nCount"></param>
/// <param name="nStartDest"></param>
/// <returns>
/// </returns>
FUNCTION ACopy(a ,aDest ,nStart ,nCount ,nStartDest ) AS ARRAY CLIPPER
/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   


/// <summary>
/// Fill Array elements with a specified value.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
FUNCTION AFill(a AS ARRAY,fill AS USUAL) AS ARRAY 
	RETURN AFill(a, fill, 1, (int) aLen(a))
	
	
/// <summary>
/// Fill Array elements with a specified value.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="Start"></param>
/// <returns>
/// </returns>
FUNCTION AFill(a AS ARRAY,fill AS USUAL,Start AS LONG) AS ARRAY 
	RETURN AFill(a, fill, Start, (LONG) aLen(a))

/// <summary>
/// Fill Array elements with a specified value.
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="Start"></param>
/// <param name="Stop"></param>
/// <returns>
/// </returns>
FUNCTION AFill(a AS ARRAY,fill AS USUAL, start AS LONG, Stop AS LONG) AS ARRAY 
	/*
   LOCAL nLen := ALen( a ) AS DWORD
   LOCAL x			AS DWORD
   LOCAL dwStart	AS DWORD
   LOCAL dwEnd		as DWORD
   IF nLen > 0
		IF start > nLen 
			BREAK Error.BoundError( "AFill", "start", 3, <OBJECT>{ start } )
		ENDIF
		IF stop > 0
			uiStop := Math.Min( uiStop, uiStart + stop - 1 )
		ELSE
			uiStop := Math.Max( 1, uiStart + stop - 1 )
		ENDIF   
      
		IF uiStart < uiStop
			 FOR x := uiStart UPTO uiStop
				a[x] := fill
			 NEXT
		ELSE
			 FOR x := uiStop DOWNTO uiStart
				a[x] := fill
			 NEXT
		ENDIF
   ENDIF
   */
   RETURN a

	
	
/// <summary>
/// Create an empty Array.
/// </summary>
/// <returns>
/// </returns>
FUNCTION ArrayBuild() AS ARRAY
	RETURN ARRAY{}
	
/// <summary>
/// Create an uninitialized Array with the specified number of elements and dimensions.
/// </summary>
/// <param name="nDim"></param>
/// <returns>
/// </returns>
FUNCTION ArrayNew(nDim PARAMS INT[]) AS ARRAY
	RETURN __Array.ArrayCreate(nDim)
	
	
/// <summary>
/// To create an Array and fill its elements with a default value.
/// </summary>
/// <param name="x"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
FUNCTION AReplicate<T>(x AS USUAL,nCount AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
	VAR a:=__ArrayBase<T>{nCount} 
	//Todo
	//Array.ArrayFill(a,x)
	RETURN a
	
	
	
	
/// <summary>
/// Sort an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION ASort(aArray ,startIndex ,nCount ,cbOrder ) AS ARRAY CLIPPER

   Default( REF startIndex, 1 )

   IF ! aArray:IsArray
      THROW Error.ArgumentError( "ASort", nameof(aArray), 1, <OBJECT>{ aArray } )
   ENDIF
   LOCAL nLen := Alen((ARRAY)aArray) as DWORD
   Default( REF nCount, nLen - startIndex + 1 )

   if startIndex < 1 .or. startIndex > nLen
		THROW Error.ArgumentError( "ASort", nameof(startIndex), 2, <OBJECT>{ startIndex } )
   endif 
   IF nCount + startIndex > Alen((ARRAY)aArray)+1
		THROW Error.ArgumentError( "ASort", nameof(nCount), 3, <OBJECT>{ nCount } )
   endif 
   
   IF cbOrder != NIL && ( ( ! cbOrder:IsCodeBlock ) || ((CODEBLOCK)cbOrder):PCount() != 2 )
      THROW Error.ArgumentError( "ASort", "cbOrder", 4, <OBJECT>{ cbOrder } )
   ENDIF
   
   EnforceNumeric( startIndex )
   EnforceNumeric( nCount )
   
   IF cbOrder == NIL
      ((ARRAY)aArray):Sort( startIndex, nCount, NULL ) // this uses __Usual.ICompareTo()
   ELSE
      ((ARRAY)aArray):Sort( startIndex, nCount, ArraySortComparer{ cbOrder } )
   ENDIF   
   
   RETURN aArray
   
// This wraps a codeblock and provides an IComparer implementation so
// we can use ArrayList:Sort() with a codeblock.
INTERNAL STRUCTURE ArraySortComparer IMPLEMENTS System.Collections.Generic.IComparer<USUAL>

   PRIVATE _cb AS ICodeblock

   CONSTRUCTOR( cb AS ICodeblock )
      _cb := cb
      RETURN
      
   METHOD Compare( x AS USUAL, y AS USUAL ) AS INT
	  IF x == y
		RETURN 0
	  ENDIF
	  VAR u := _cb:EvalBlock( x, y )
	  IF IsLogic(u)
		RETURN IIF ( (LOGIC) u , -1, 1 ) 
	  endif
      RETURN 0
      
 END STRUCTURE
         
	
/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
FUNCTION _ArrayGetPoly(a AS USUAL,n1 AS USUAL) AS USUAL
/// THROW NotImplementedException{}
RETURN		 NIL   
	
/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
FUNCTION _ArrayGetCollection(a AS USUAL,n1 AS USUAL) AS USUAL
/// THROW NotImplementedException{}
RETURN		 NIL   
	
/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
FUNCTION _ArrayPutCollection(a AS USUAL,x AS USUAL,n1 AS USUAL) AS USUAL
/// THROW NotImplementedException{}
RETURN		 NIL   
	
	
/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
FUNCTION _ArrayPutPoly(a AS USUAL,x AS USUAL,n1 AS USUAL) AS USUAL
/// THROW NotImplementedException{}
RETURN		 NIL  
	
	
/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEval(a,cb ,iStart ,iCount ) AS USUAL CLIPPER
/// THROW NotImplementedException{}
	RETURN NIL   
	
/// <summary>
/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
/// </summary>
/// <param name="a"></param>
/// <param name="cb"></param>
/// <param name="iStart"></param>
/// <param name="iCount"></param>
/// <returns>
/// </returns>
FUNCTION AEvalA(a ,cb ,iStart ,iCount ) AS USUAL CLIPPER
/// THROW NotImplementedException{}
	RETURN NIL   
	
/// <summary>
/// Execute a code block for each element in an Array.
/// </summary>
/// <param name="c"></param>
/// <param name="cod"></param>
/// <param name="nStart"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
FUNCTION AEvalOld(c ,cod ,nStart ,nCount ) AS USUAL CLIPPER
/// THROW NotImplementedException{}
RETURN NIL   


