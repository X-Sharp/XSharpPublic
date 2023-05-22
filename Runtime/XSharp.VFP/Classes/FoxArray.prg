//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections
USING System.Collections.Generic
USING System.Linq
USING System.Diagnostics
USING System.Reflection
USING System.Text
USING XSharp
BEGIN NAMESPACE XSharp
    /// <summary>Internal type that implements the FoxPro Compatible ARRAY type.<br/>
    /// This type has methods and properties that normally are never directly called from user code.
    /// </summary>
    /// <remarks>FoxPro arrays can be one dimensional or two dimensional.
	/// Two dimensional arrays are implemented as a projection
    /// on top of a one dimensional array. <br/>
    /// So a two dimensional array of 4 x 3 elements will be implemented as an array of 12 elements.
    /// The elements 1-3 represent row 1, 4-6 row 2 etc. </remarks>
    /// <seealso cref='IIndexer' />
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
    //[DebuggerTypeProxy(TYPEOF(ArrayDebugView))];
    [DebuggerDisplay("{DebuggerString(),nq}")] ;
    PUBLIC SEALED CLASS __FoxArray INHERIT __Array IMPLEMENTS IIndexer
        [DebuggerBrowsable(DebuggerBrowsableState.Never)];
        PRIVATE _nCols := 0 AS DWORD
        [DebuggerBrowsable(DebuggerBrowsableState.Never)];
        PROPERTY MultiDimensional AS LOGIC GET _nCols > 0
        PROPERTY Columns          AS LONG GET (LONG) _nCols
        PROPERTY Rows             AS LONG
            GET
                IF SELF:MultiDimensional
                    RETURN _internalList:Count / (LONG) _nCols
                ELSE
                    RETURN _internalList:Count
                ENDIF
            END GET
        END PROPERTY
         /// <inheritdoc />
        CONSTRUCTOR()
            SUPER()

            /// <inheritdoc />
        CONSTRUCTOR(capacity AS DWORD)
            SUPER(capacity, TRUE)


        CONSTRUCTOR(nRows AS DWORD, nCols AS DWORD)
            SELF:_nCols := nCols

            SUPER( IIF(SELF:MultiDimensional, nRows *nCols, nRows ), TRUE)

            /// <inheritdoc />
        CONSTRUCTOR( elements AS OBJECT[] )
            SUPER(elements)

        PRIVATE CONSTRUCTOR( elements AS USUAL[])
            SUPER(elements)

        INTERNAL METHOD __Fill(uValue AS USUAL) AS VOID
            FOR VAR nElement := 0 TO SELF:Length-1
               SELF:_internalList[nElement] := uValue
            NEXT
            RETURN

        INTERNAL METHOD __ArrayAssignError(uValue as USUAL, nRow as INT, nCol as INT, lSizeError := FALSE AS LOGIC) AS VOID
            LOCAL err     := Error.ArgumentError(ProcName(),"value", "") as Error
            err:Stack     := ErrorStack(1)
            err:Arg := NULL

            if lSizeError .and. uValue IS __Array VAR aValue
                err:Description := i": Row {nRow} has an incorrect size "+aValue:ElementsToString()
                err:Arg    := i"[{nRow}]"
                err:Args   := <OBJECT>{aValue:ElementsToString()}
            ELSEIF nCol > 0
                 err:Args      := NULL
                IF uValue IS __Array VAR aValue2
                    err:Description := i": Element [{nRow}, {nCol}] is an array " + aValue2:ElementsToString()
                ELSE
                    NOP // should not happen
                ENDIF
            ELSE
                if uValue IS __Array VAR aValue2
                    err:Description := i": Row [{nRow}] is an array: "+aValue2:ElementsToString()
                    err:Arg         := i"[{nRow}]"
                    err:Args        := <OBJECT>{aValue2:ElementsToString()}
                ELSE
                    err:Description := i": Row [{nRow}] is NOT an array"
                    err:Arg         := i"[{nRow}]"
                    err:Args        := <OBJECT>{uValue}
                ENDIF
            endif
            err:Description += " (You can only assign a one dimensional or a symmetrical two dimensional array to a FoxPro Compatible Array)"
            THROW err

        INTERNAL METHOD __AssignFrom(aValue as __Array) AS VOID
            IF aValue:Length == 0
                SELF:_internalList:Clear()
                RETURN
            ENDIF
            LOCAL uValue as USUAL
            uValue := aValue[1]
            IF IsArray(uValue)
                SELF:__AssignFrom2DimArray(aValue)
                RETURN
            ENDIF
            // Inspect to see if each element is NOT an array
            FOR VAR nRow := 2 to aValue:Length
                VAR element := aValue[nRow]
                IF element IS __Array VAR aElement
                    SELF:__ArrayAssignError(element, nRow, 0, FALSE)
                ENDIF
            NEXT
            SELF:_nCols := 0
            SELF:_internalList:Clear()
            SELF:_internalList:AddRange(aValue:_internalList)

        METHOD __AssignFrom2DimArray(aValue as __Array) AS VOID
            LOCAL aRow := aValue[1] as ARRAY
            LOCAL nRowSize := aRow:Length as DWORD
            FOR VAR nRow := 2 to aValue:Length
                VAR element := aValue[nRow]
                IF element IS __Array VAR aElement
                    IF  aElement:Length == nRowSize
                        FOR var nCol := 1 to aElement:Length
                            var uChild := aElement[nCol]
                            IF uChild IS __Array
                                SELF:__ArrayAssignError(aValue, nRow, nCol, FALSE)
                            ENDIF
                        NEXT
                    ELSE
                        SELF:__ArrayAssignError(element, nRow, 0, TRUE)
                    ENDIF
                ELSE
                    SELF:__ArrayAssignError(element, nRow, 0, FALSE)
                ENDIF
            NEXT
            // There is no simple way to assign all the values
            // So do it the slow way
            SELF:ReDim(aValue:Length, nRowSize)
            FOR var i := 1 to aValue:Length
                FOR var j := 1 to nRowSize
                    var uValue := aValue[i,j]
                    SELF:__SetElement(uValue, i-1, j-1)
                NEXT
            NEXT

        INTERNAL METHOD __GetDimensionError(nDimensionsAsked AS LONG) AS Error
            VAR err         := Error.BoundError(ProcName(2),"indices", 1)
            IF SELF:MultiDimensional
               err:Description := i"Bound error: You cannot access a 2 dimensional array with {nDimensionsAsked} indices"
            ELSE
               err:Description := i"Bound error: You cannot access a single dimensional array with {nDimensionsAsked} indices"
            ENDIF
            err:Stack   := ErrorStack(1)
            RETURN err

        INTERNAL STATIC METHOD __NestedArrayError() AS Error
            VAR err     := Error.ArgumentError(ProcName(2),"value", "You cannot assign an array to an element inside a FoxPro Compatible Array")
            err:Stack   := ErrorStack(1)
            RETURN err

        PUBLIC OVERRIDE METHOD __GetElement(index AS INT, index2 AS INT) AS USUAL
             VAR item := SELF:__GetIndex(index, index2)
             SUPER:__CheckArrayElement(SELF, item, nameof(index),1)
             RETURN SUPER:__GetElement(item)

        PUBLIC OVERRIDE METHOD __GetElement(indices PARAMS INT[] ) AS USUAL
            IF  indices:Length == 1
                RETURN SUPER:__GetElement(indices[1])
            ELSEIF indices:Length == 2 .AND. SELF:MultiDimensional
                RETURN SELF:__GetElement(indices[1], indices[2])
            ELSE
                VAR err     := Error.BoundError(ProcName(1),nameof(indices), 1, indices)
                err:Stack   := ErrorStack(1)
                err:Description := i"Bound error: Too many dimensions)"
                THROW err
            ENDIF

       PUBLIC OVERRIDE METHOD __SetElement(u AS USUAL, index AS INT) AS USUAL
             IF IsArray(u)
                  THROW __NestedArrayError()
             ENDIF
             SUPER:__CheckArrayElement(SELF, index, nameof(index),1)
             RETURN SUPER:__SetElement(u, index)

        PUBLIC OVERRIDE METHOD __SetElement(u AS USUAL, index AS INT, index2 AS INT) AS USUAL
             IF IsArray(u)
                  THROW __NestedArrayError()
             ENDIF
             VAR item := SELF:__GetIndex(index, index2)
             SUPER:__CheckArrayElement(SELF, item, nameof(index),1)
             RETURN SUPER:__SetElement(u, item)


        PUBLIC OVERRIDE METHOD __SetElement(u AS USUAL, indices PARAMS INT[] ) AS USUAL
            IF indices:Length == 1
                RETURN SUPER:__SetElement(u, indices[1])
            ELSEIF indices:Length == 2 .AND. SELF:MultiDimensional
                RETURN SELF:__SetElement(u, indices[1], indices[2])
            ELSE
                VAR err     := Error.BoundError(ProcName(1),"indices", 2, indices)
                err:Stack   := ErrorStack(1)
                err:Description := i"Bound error: Too many dimensions"
                THROW err
            ENDIF

        PRIVATE METHOD __GetIndex(nRow AS INT, nColumn AS INT) AS INT
            // Note that nRow and nColumn are already Zero based !
            // And also that FoxPro does not complain when you indicate a row number even
            // when the array is single dimensional. FoxPro then simply returns the element indicated with nColumn
            IF SELF:MultiDimensional
                RETURN (nRow * _nCols) + nColumn
            ELSE
                RETURN nRow
            ENDIF

        /// <inheritdoc />
        PUBLIC OVERRIDE PROPERTY DefaultValue AS USUAL GET Usual{__UsualType.Logic, FALSE}

        /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
        /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
        OVERRIDE PUBLIC PROPERTY SELF[index AS INT] AS USUAL
            GET
                 SUPER:__CheckArrayElement(SELF, index, nameof(index),1)
                 RETURN SUPER:__GetElement(index)
            END GET
            SET
                IF IsArray(value)
                    THROW __NestedArrayError()
                ENDIF
                SUPER:__CheckArrayElement(SELF, index, nameof(index),1)
                SUPER:__SetElement(value,index)
            END SET
        END PROPERTY


        /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
        /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        /// <param name="index2"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
        OVERRIDE PUBLIC PROPERTY SELF[index AS INT, index2 AS INT] AS USUAL
            GET
                IF ! SELF:MultiDimensional
                    THROW SELF:__GetDimensionError(2)
                ELSE
                    RETURN SELF:__GetElement(index, index2)
                ENDIF
            END GET
            SET
                IF IsArray(value)
                    THROW __NestedArrayError()
                ENDIF
                IF ! SELF:MultiDimensional
                    THROW SELF:__GetDimensionError(2)
                ELSE
                    SELF:__SetElement(value,SELF:__GetIndex(index, index2))
                ENDIF
            END SET
        END PROPERTY


        /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
        /// <param name="indices"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
        OVERRIDE PUBLIC PROPERTY SELF[indices PARAMS INT[]] AS USUAL
            GET
                IF indices:Length == 1
                    SUPER:__CheckArrayElement(SELF, indices[1], nameof(indices),1)
                    RETURN SELF:__GetElement(indices[1])
                ELSEIF indices:Length == 2 .and. SELF:MultiDimensional
                    VAR index := SELF:__GetIndex(indices[1], indices[2])
                    SUPER:__CheckArrayElement(SELF, index, nameof(indices),1)
                    RETURN SELF:__GetElement(index)
                ELSE
                    THROW SELF:__GetDimensionError(indices:Length)
                ENDIF
            END GET
            SET
                IF IsArray(value)
                    THROW __NestedArrayError()
                ENDIF
                IF indices:Length == 1
                    SELF:__CheckArrayElement(SELF, indices[1], nameof(indices),1)
                    SELF:__SetElement(indices[1], value)
                ELSEIF indices:Length == 2 .and. SELF:MultiDimensional
                    VAR index := SELF:__GetIndex(indices[1], indices[2])
                    SELF:__CheckArrayElement(SELF, index, nameof(indices),1)
                    SELF:__SetElement(index, value)
                ELSE
                    THROW SELF:__GetDimensionError(indices:Length)
                ENDIF
            END SET
        END PROPERTY


        METHOD ReDim(nRows AS DWORD, nCols := 0 AS DWORD) AS __FoxArray
            SELF:_nCols := nCols
            SELF:Resize( (INT) IIF(SELF:MultiDimensional, nRows * nCols, nRows ))
            RETURN SELF


       /// <summary>Delete a row from a multi dimensional array.</summary>
        /// <param name="nRow">1 based row number to delete</param>
        /// <remarks>Deleting rows will shift other rows up and will add a blank row at the bottom of the array</remarks>
        METHOD DeleteRow(nRow AS INT) AS __FoxArray
            // __GetIndex expects 0 based indices. DeleteRow works with 1 based rows
            VAR nStart := SELF:__GetIndex(nRow-1, 0)              // first element on the row
            VAR nEnd   := _internalList:Count - _nCols-1
            FOR VAR nIndex := nStart UPTO nEnd
                _internalList[nIndex] := _internalList[nIndex+_nCols]
            NEXT
            nStart := _internalList:Count - _nCols
            nEnd   := _internalList:Count-1
            FOR VAR nIndex :=  nStart UPTO nEnd
                _internalList[nIndex] := NIL
            NEXT
            RETURN SELF

       /// <summary>Delete a column from a multi dimensional array.</summary>
        /// <param name="nColumn">1 based column number to delete</param>
       /// <remarks>When no columns are defined then nothing happens. <br/>
        /// When an array has columns then the elements are deleted and shifted.<br/>
        /// Assume a 4 x 4 array. When DeleteColumn() is called with column 2 then the elements 2,6,10 and 14 are deleted
        /// and the elements 3 &amp; 4, 7 &amp; 8, 11 &amp; 12 and 15 &amp; 16 are shifted to the left.
        /// New empty elements are inserted on locations 4,8,12 and 16.</remarks>
       METHOD DeleteColumn(nColumn AS INT) AS __FoxArray
            IF SELF:MultiDimensional .AND. nColumn > 0 .AND. nColumn <= _nCols
                LOCAL nRowStart := 0 AS INT
                LOCAL nColumns  := SELF:Columns AS LONG
                DO WHILE nRowStart < SELF:_internalList:Count
                    FOR VAR nElement := nColumn UPTO nColumns-1
                        SELF:_internalList[nRowStart+nElement -1] := SELF:_internalList[nRowStart+nElement ]
                    NEXT
                    SELF:_internalList[nRowStart + nColumns-1 ] := NIL
                    nRowStart += nColumns
                ENDDO
            ENDIF
            RETURN SELF


        /// <summary>Insert a row into a multi dimensional array.</summary>
        /// <param name="nRow">1 based row number to insert</param>
        /// <remarks>Inserting rows shifts rows up and deletes a row at the end of the array: FoxPro arrays do not grow automatically</remarks>
        METHOD InsertRow(nRow AS INT) AS __FoxArray
            VAR nStart := SELF:__GetIndex(nRow-1, 0)              // first element on the row
            FOR VAR nIndex := _internalList:Count-1 DOWNTO nStart+_nCols
                SELF:_internalList[nIndex] := SELF:_internalList[nIndex-_nCols]
            NEXT
            FOR VAR nIndex := nStart UPTO nStart + _nCols-1
                SELF:_internalList[nIndex] := NIL
            NEXT
            RETURN SELF

        /// <summary>Insert a column into a multi dimensional array.</summary>
        /// <param name="nColumn">1 based column number to insert</param>
        /// <remarks>Inserting columns deletes columns at the end of each row.</remarks>
        METHOD InsertColumn(nColumn AS INT) AS __FoxArray
            IF nColumn <= SELF:Columns .and. nColumn > 0
                // this is more difficult to do
                // we cannot simply insert because that would shift elements to new rows
                // so we loop through the rows and shift the elements forward
                // assume we have a [3,5] element array and we insert column 3
                // then for each row
                //    element 5 has to be replaced by element 4,
                //    element 4 has to be replaced by element 3
                //    element 3 will be cleared
                // note that _GetIndex is ZERO based
                VAR nToMove  := SELF:Columns - nColumn
                FOR VAR nRow := 0 UPTO SELF:Rows-1
                    VAR nLastInRow := SELF:__GetIndex(nRow,  SELF:Columns-1)
                    FOR VAR nCell := 0 UPTO nToMove-1
                        _internalList[nLastInRow - nCell] := _internalList[nLastInRow - nCell-1]
                    NEXT
                    _internalList[nLastInRow - nToMove] := NIL
                NEXT
            ENDIF
            RETURN SELF

        /// <exclude />
        PUBLIC OVERRIDE METHOD Resize(newSize AS INT) AS VOID
            // Make sure we have a multiple of the # of columns
            // Assume we have a 3 column array and we resize to 10
            // then we must make the size a multiple of 3, so it will become 12
            IF SELF:MultiDimensional
                VAR nDiff := newSize % SELF:Columns
                IF nDiff != 0
                    newSize += (SELF:Columns - nDiff)
                ENDIF
            ENDIF
            SUPER:Resize(newSize)


        INTERNAL METHOD GetRow(nElement AS LONG) AS LONG
            // calculate the row
            VAR nZeroBased := nElement -1
            RETURN (nZeroBased / (INT) SELF:Columns) +1

        INTERNAL METHOD GetColumn(nElement AS LONG) AS LONG
            VAR nZeroBased := nElement -1
            RETURN (nZeroBased % (INT) SELF:Columns) +1

        INTERNAL METHOD GetSubScript(nElement AS LONG) AS STRING
            IF !SELF:MultiDimensional
                RETURN "["+nElement:ToString()+"]"
            ELSE
                RETURN "["+SELF:GetRow(nElement):ToString()+","+SELF:GetColumn(nElement):ToString()+"]"
            ENDIF


        PROTECTED OVERRIDE METHOD DebuggerString() AS STRING
            LOCAL sb AS StringBuilder
            sb := StringBuilder{}
            sb:Append("FoxArray[")
            VAR nRows := SELF:Rows
            IF ! SELF:MultiDimensional
                sb:Append(_internalList:Count)
            ELSE
                sb:Append(nRows)
                sb:Append(",")
                sb:Append(_nCols)
            ENDIF
            sb:Append("] (")
            FOR VAR i := 1 TO Math.Min(SELF:Length,10)
                IF i > 1
                    sb:Append(",")
                ENDIF
                sb:Append(AsString(_internalList[i-1]))
            NEXT
            IF SELF:Length > 10
                sb:Append(",..")
            ENDIF
            sb:Append(")")
            RETURN sb:ToString()

        [DebuggerBrowsable(DebuggerBrowsableState.Never)];
        INTERNAL PROPERTY Values AS STRING[]
            GET
                VAR result := List<STRING>{}
                FOR VAR nElement := 1 TO SELF:Count
                    result:Add(SELF:GetSubScript(nElement)+" "+AsString(_internalList[nElement-1]))
                NEXT
                RETURN result:ToArray()
            END GET
        END PROPERTY

        /// <exclude />
        PUBLIC OVERRIDE METHOD Add(u AS USUAL) AS VOID
            VAR err     := Error{NotSupportedException{"The FoxPro array type does not support the Add functionality because it cannot dynamically grow. " + ;
                            "You will have to use the DIMENSION statement to change its size"}}
            THROW err


    END CLASS
END NAMESPACE

