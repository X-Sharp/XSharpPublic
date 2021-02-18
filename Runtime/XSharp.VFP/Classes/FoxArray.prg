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
    [DebuggerDisplay("{DebuggerString(),nq}", Type := "FOXARRAY")] ;
    PUBLIC SEALED CLASS __FoxArray INHERIT __Array IMPLEMENTS IIndexer
        PRIVATE _nCols := 1 AS DWORD
        PROPERTY MultiDimensional AS LOGIC GET _nCols > 1
        PROPERTY Columns AS LONG GET (LONG) _nCols
        PROPERTY Rows    AS LONG GET _internalList:Count / (LONG) _nCols 
         /// <inheritdoc />
        CONSTRUCTOR()
            SUPER()

            /// <inheritdoc />
        CONSTRUCTOR(capacity AS DWORD)
            SUPER(capacity, TRUE)


        CONSTRUCTOR(nRows as DWORD, nCols as DWORD)
            SUPER( nRows * nCols, TRUE)
            SELF:_nCols := nCols

            /// <inheritdoc />
        CONSTRUCTOR( elements AS OBJECT[] )
            SUPER(elements)
            

        INTERNAL METHOD __GetDimensionError(nDimensionsAsked as LONG) AS Error
            VAR err         := Error.BoundError(ProcName(2),"indices", 1)
            IF SELF:MultiDimensional
               err:Description := i"Bound error: You cannot access a 2 dimensional array with {nDimensionsAsked} indices"
            ELSE
               err:Description := i"Bound error: You cannot access a single dimensional array with {nDimensionsAsked} indices"
            ENDIF
            err:Stack   := ErrorStack(1)
            RETURN err

        INTERNAL METHOD __NestedArrayError() AS Error
            VAR err     := Error.ArgumentError(ProcName(2),"value", "You cannot assign an array to an element inside a Fox Array")
            err:Stack   := ErrorStack(1)
            RETURN err

        PUBLIC OVERRIDE METHOD __GetElement(index AS INT, index2 AS INT) AS USUAL
             var item := SELF:__GetIndex(index, index2)
             SUPER:__CheckArrayElement(SELF, item, nameof(index),1)
             RETURN SELF:__GetElement(item)

        PUBLIC OVERRIDE METHOD __SetElement(u AS USUAL, index AS INT, index2 AS INT) AS USUAL
            var item := SELF:__GetIndex(index, index2)
             SUPER:__CheckArrayElement(SELF, item, nameof(index),1)
             RETURN SELF:__SetElement(u, item)


        PRIVATE METHOD __GetIndex(nRow as int, nColumn as int) as int
            // Note that nRow and nColumn are already Zero based !
            IF SELF:_nCols > 1
                RETURN (nRow * _nCols) + nColumn
            ELSE
                THROW SELF:__GetDimensionError(2)                  
            ENDIF

        /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" /> 
        /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
        OVERRIDE PUBLIC PROPERTY SELF[index AS INT] AS USUAL
            GET
                 SUPER:__CheckArrayElement(SELF, index, nameof(index),1)
                 RETURN SELF:__GetElement(index)
            END GET
            SET
                IF IsArray(value)
                    THROW SELF:__NestedArrayError()
                ENDIF
                SUPER:__CheckArrayElement(SELF, index, nameof(index),1)
                SELF:__SetElement(value,index)
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
                    THROW SELF:__NestedArrayError()
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
                    var index := SELF:__GetIndex(indices[1], indices[2])
                    SUPER:__CheckArrayElement(SELF, index, nameof(indices),1)                    
                    RETURN SELF:__GetElement(index)
                ELSE
                    THROW SELF:__GetDimensionError(indices:Length)                    
                ENDIF
            END GET
            SET
                IF IsArray(value)
                    THROW SELF:__NestedArrayError()
                ENDIF
                IF indices:Length == 1
                    SELF:__CheckArrayElement(SELF, indices[1], nameof(indices),1)                    
                    SELF:__SetElement(indices[1], value)
                ELSEIF indices:Length == 2 .and. SELF:MultiDimensional
                    var index := SELF:__GetIndex(indices[1], indices[2])
                    SELF:__CheckArrayElement(SELF, index, nameof(indices),1)                    
                    SELF:__SetElement(index, value)
                ELSE
                    THROW SELF:__GetDimensionError(indices:Length)                               
                ENDIF
            END SET
        END PROPERTY


        /// <summary>Redimension the array. Existing data will be saved if the # of cells in the new array &lt;= the # of cells in the old array. </summary>
        METHOD ReDim(nRows as DWORD, nCols := 1 as DWORD) AS __FoxArray
            _nCols := nCols
            SELF:Resize( (INT) (nRows * nCols))
            RETURN SELF


       /// <summary>Delete a row from a multi dimensional array.</summary>
        /// <param name="nRow">1 based row number to delete</param>
        /// <remarks>Deleting rows will shift other rows up and will add a blank row at the bottom of the array</remarks>
        METHOD DeleteRow(nRow as INT) AS __FoxArray
            // __GetIndex expects 0 based indices. DeleteRow works with 1 based rows
            VAR nStart := SELF:__GetIndex(nRow-1, 0)              // first element on the row
            VAR nEnd   := _internalList:Count - _nCols-1
            FOR VAR nIndex := nStart upto nEnd
                _internalList[nIndex] := _internalList[nIndex+_nCols]
            NEXT
            nStart := _internalList:Count - _nCols
            nEnd   := _internalList:Count-1
            FOR var nIndex :=  nStart upto nEnd
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
       METHOD DeleteColumn(nColumn as INT) AS __FoxArray
            IF SELF:_nCols > 1 .and. nColumn > 0 .and. nColumn <= _nCols
                LOCAL nRowStart := 0 AS INT
                LOCAL nColumns  := SELF:Columns as LONG
                DO WHILE nRowStart < SELF:_internalList:Count
                    FOR VAR nElement := nColumn upto nColumns-1
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
        METHOD InsertRow(nRow as INT) AS __FoxArray
            VAR nStart := SELF:__GetIndex(nRow-1, 0)              // first element on the row
            FOR VAR nIndex := _internalList:Count-1 DOWNTO nStart+_nCols
                SELF:_internalList[nIndex] := SELF:_internalList[nIndex-_nCols]
            NEXT
            FOR var nIndex := nStart upto nStart + _nCols-1
                SELF:_internalList[nIndex] := NIL
            NEXT
            RETURN SELF

        /// <summary>Insert a column into a multi dimensional array.</summary>
        /// <param name="nColumn">1 based column number to insert</param>
        /// <remarks>Inserting columns deletes columns at the end of each row.</remarks>
        METHOD InsertColumn(nColumn as INT) AS __FoxArray
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
                var nToMove  := SELF:Columns - nColumn 
                FOR VAR nRow := 0 upto SELF:Rows-1
                    var nLastInRow := SELF:__GetIndex(nRow,  self:Columns-1)
                    FOR VAR nCell := 0 upto nToMove-1
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
            if SELF:_nCols != 1
                VAR nDiff := newSize % SELF:Columns
                IF nDiff != 0
                    newSize += (SELF:Columns - nDiff)
                ENDIF
            ENDIF
            SUPER:Resize(newSize)
            

        INTERNAL METHOD GetRow(nElement AS LONG) AS LONG
            // calculate the row
            VAR nZeroBased := nElement -1
            RETURN (nZeroBased / (int) SELF:Columns) +1

        INTERNAL METHOD GetColumn(nElement AS LONG) AS LONG
            VAR nZeroBased := nElement -1
            RETURN (nZeroBased % (int) SELF:Columns) +1
            
        INTERNAL METHOD GetSubScript(nElement as LONG) AS STRING
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
            if _nCols == 1
                sb:Append(_internalList:Count)
            else
                sb:Append(nRows)
                sb:Append(",")
                sb:Append(_nCols)
            endif
            sb:Append("] (")
            for var i := 1 to Math.Min(SELF:Length,10)
                if i > 1
                    sb:Append(",")
                endif
                sb:Append(AsString(_internalList[i-1]))
            next
            if SELF:Length > 10
                sb:Append(",..")
            ENDIF
            sb:Append(")")
            RETURN sb:ToString()
            
        INTERNAL PROPERTY Values as string[]
            GET
                VAR result := List<String>{}
                FOR VAR nElement := 1 to SELF:Count
                    result:Add(SELF:GetSubScript(nElement)+" "+AsString(_internalList[nElement-1]))
                NEXT
                RETURN result:ToArray()
            END GET
        END PROPERTY

        /// <exclude />
        PUBLIC OVERRIDE METHOD Add(u AS USUAL) AS VOID
            VAR err     := Error{NotSupportedException{"The FoxPro array type does not support the Add functionality because it cannot dynamically grow. " + ;
                            "You will have to use the DIMENSION statement to change its size"}}
            throw err            

    END CLASS
END NAMESPACE

