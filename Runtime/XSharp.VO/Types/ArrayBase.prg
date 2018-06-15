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
USING XSharp
BEGIN NAMESPACE XSharp	
	/// <summary>Internal type that implements the new TYPED ARRAY type.<br/>
	/// This type has methods and properties that normally are never directly called from user code.
	/// </summary>
	PUBLIC CLASS __ArrayBase<T> IMPLEMENTS IEnumerable<T> where T IS NEW()
		INTERNAL _internalList AS List<T> 
		PRIVATE _islocked AS LOGIC 
		#region constructors
		/// <summary>Create an empty array</summary>
		CONSTRUCTOR()
			_internalList := List<T>{}
			RETURN  
			
			/// <summary>Create an array with a certain number of elements. Each element will be filled with a default value.</summary>
		CONSTRUCTOR(capacity AS DWORD)
			_internalList := List<T>{ (INT) capacity}
			_internalList:AddRange(Enumerable.Repeat(Default(T),(INT) capacity))
			RETURN 
			
			/// <summary>Create an array and fill it with elements from an existing collection.</summary>
		CONSTRUCTOR( collection AS IEnumerable<T>)
			_internalList := List<T>{collection}
			RETURN 
			
			/// <summary>Create an array and fill it with elements from an existing .Net array of objects. Note that the objects must be of the right type.</summary>
		CONSTRUCTOR( elements AS OBJECT[] )
			SELF()
			IF elements == NULL
				THROW ArgumentNullException{NAMEOF(elements)}
			ENDIF
			FOREACH element AS OBJECT IN elements
				IF element == NULL
					_internalList:add(Default(T))
				ELSEIF element IS T
					_internalList:Add( (T) element)
				ELSE
					THROW ArgumentException{"Object array contains element of incorrect type "+element:GetType():FullName}
				ENDIF
			NEXT
			RETURN
			
			/// <summary>Create an array and fill it with elements from an existing .Net array.</summary>
		CONSTRUCTOR( elements AS T[] )
			_internalList := List<T>{elements}
			RETURN
			#endregion
			
		#region properties
		/// <summary>Is the array empty.</summary>
		PUBLIC PROPERTY IsEmpty AS LOGIC
			GET
				RETURN (_internalList:Count == 0)
			END GET 
		END PROPERTY
		/// <summary>Length of the array.</summary>
		PUBLIC PROPERTY Length AS DWORD
			GET
				RETURN (DWORD)_internalList:Count
			END GET
		END PROPERTY
		#endregion
		
		#region Enumerators
		PUBLIC METHOD IEnumerable<T>.GetEnumerator() AS IEnumerator<T>
			RETURN _internalList:GetEnumerator()
			
		PUBLIC METHOD IEnumerable.GetEnumerator() AS IEnumerator
			RETURN _internalList:GetEnumerator()
			
			#endregion
			
			
		#region Cloning
		
		INTERNAL METHOD Clone() AS __ArrayBase<T>
			LOCAL aResult AS __ArrayBase<T>
			LOCAL nCount AS DWORD
			nCount := (DWORD) _internalList:Count
			aResult := (__ArrayBase<T>) Activator.CreateInstance(SELF:GetType(), NULL)
			ASize(aResult, nCount)
			IF nCount == 0
				// warning, nCount-1 below will become MAXDWORD for nCount == 0
				RETURN aResult
			END IF
			FOR VAR I := 0 TO nCount-1
				aResult:_internalList[i] := _internalList[i]
			NEXT
			RETURN aResult
			
			#endregion
			
			
		#region Indexers and to Get / Set Elements. 
		///
		/// <summary>Access the array element using ZERO based array index</summary>
		///
		PUBLIC METHOD __GetElement(index AS INT) AS T
			RETURN SELF:_internalList[ index ]
			
			/// <summary>Set array elements with a ZERO based array index</summary>
		PUBLIC METHOD __SetElement(u AS T,index AS INT) AS T
			IF SELF:CheckLock()
				_internalList[index]:=u
			ENDIF
			RETURN u
			
		PRIVATE METHOD __GetProperty(sName AS STRING) AS PropertyInfo
			LOCAL type AS System.Type
			LOCAL oProp AS PropertyInfo
			type := TYPEOF(T)
			oProp := type:GetProperty(sName, BindingFlags.IgnoreCase | BindingFlags.Instance | BindingFlags.Public)
			IF oProp == NULL_OBJECT
				THROW ArgumentException{"Unknown property: "+sName}
			ENDIF
			RETURN oProp
			
		// Note: Zero based , compiler handles subtraction			
		/// <summary>Get/Set array elements with a ZERO based array index</summary>
		/// <param name="index">0 based offset in the location. Please note that the compiler automatically subtracts one from the index unless the /az compiler option is used.</param>
		/// <returns>The element stored at the indicated location in the collection.</returns>
		PUBLIC PROPERTY SELF[i AS INT] AS T
			GET
				IF  i > _internalList:Count-1
					THROW ArgumentOutOfRangeException{}
				ENDIF
				RETURN _internalList[i ]
			END GET
			SET
				IF SELF:CheckLock()
					IF i > _internalList:Count-1
						THROW ArgumentOutOfRangeException{}
					ENDIF
					_internalList[i] := VALUE
				ENDIF
			END SET
		END PROPERTY
		
		// Note: Zero based, compiler handles subtraction			
		/// <summary>Get/Set array elements with a ZERO based array index</summary>
		/// <param name="index">0 based offset in the location. Please note that the compiler automatically subtracts one from the index unless the /az compiler option is used.</param>
		/// <param name="sPropertyName">Name of the property from the element stored in the location index</param>
		/// <returns>The value of the property of the element stored at the indicated location in the collection.</returns>
		PUBLIC PROPERTY SELF[index AS INT, sPropertyName AS STRING] AS USUAL
			GET
				LOCAL oElement AS T
				LOCAL oProp    AS PropertyInfo
				IF  index > _internalList:Count-1
					THROW ArgumentOutOfRangeException{}
				ENDIF
				oProp	 := __GetProperty( sPropertyName)
				oElement := _internalList[index ]
				RETURN oProp:GetValue(oElement, NULL)
			END GET
			SET
				IF  index > _internalList:Count-1
					THROW ArgumentOutOfRangeException{}
				ENDIF
				IF SELF:CheckLock()
					LOCAL oElement AS T
					LOCAL oProp    AS PropertyInfo
					oProp	 := __GetProperty( sPropertyName)
					oElement := _internalList[index ]
					oProp:SetValue(oElement, VALUE,NULL)
				ENDIF
			END SET
		END PROPERTY
		
		
		#endregion
		
		#region Insert and Delete elements
		INTERNAL METHOD ADD(u AS T) AS VOID
			IF SELF:CheckLock()
				_internalList:Add(u)
			ENDIF
			RETURN
			
		INTERNAL METHOD Delete(position AS INT) AS __ArrayBase<T>
			SELF:RemoveAt(position)
			SELF:Add(T{})
			RETURN SELF	
			
		INTERNAL METHOD Insert(index AS INT,u AS T) AS VOID
			IF SELF:CheckLock()
				_internalList:RemoveAt(_internalList:Count - 1)
				_internalList:Insert((INT) index-__ARRAYBASE__ ,u)
			ENDIF
			RETURN
			
		INTERNAL METHOD Insert(position AS INT) AS __ArrayBase<T>
			SELF:Insert( position, Default(T))
			RETURN SELF
			
			
		INTERNAL METHOD RemoveAt(index AS INT) AS VOID
			IF SELF:CheckLock()
				_internalList:RemoveRange(index-__ARRAYBASE__,1 )
			ENDIF
			RETURN
			
		INTERNAL METHOD Resize(newSize AS INT) AS VOID
			IF SELF:CheckLock()
				IF newSize == 0 
					_internalList:Clear()
				ELSE
					LOCAL count := _internalList:Count as INT
					IF newSize <= count 
						_internalList:RemoveRange(newSize, count - newSize)
					ELSE
						count+=1
						DO WHILE count <= newSize
							LOCAL u := T{} AS T
							_internalList:Add(u)
							count++
						ENDDO
					ENDIF
				ENDIF
			ENDIF
			RETURN
			
			
			
			#endregion
			
		PUBLIC METHOD ToString() AS STRING
			RETURN string.Format("[{0}]",_internalList:Count)
			
		INTERNAL METHOD Sort(startIndex AS INT, count AS INT, comparer AS IComparer<T>) AS VOID
			_internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
			RETURN

		INTERNAL METHOD Sort(comparer AS IComparer<T>) AS VOID
			_internalList:Sort(comparer)
			RETURN
			
		INTERNAL METHOD Swap(position AS INT, element AS T) AS T
			LOCAL original := _internalList[position - __ARRAYBASE__] AS T
			_internalList[ position - __ARRAYBASE__]:=element
			RETURN original
			
		INTERNAL METHOD Swap(position AS INT, element AS OBJECT) AS T
			//try
			VAR elementT := (T) (OBJECT) element
			RETURN Swap( position, elementT)
			//catch
			//	throw ArgumentException{"Parameter is of incorrect type "+element:GetType():FullName,nameof(element)}
			//end try
			
			
		INTERNAL METHOD Tail() AS T
			RETURN _internalList:LastOrDefault()
			
			#region locking
		INTERNAL METHOD Lock(lLocked AS LOGIC) AS LOGIC
			LOCAL wasLocked AS LOGIC
			wasLocked := SELF:_islocked
			SELF:_islocked := lLocked
			RETURN wasLocked
			/// <summary>Is the array locked?</summary>
		PROPERTY Locked AS LOGIC GET _islocked
		INTERNAL METHOD CheckLock AS LOGIC
			IF SELF:_islocked
				THROW Error{Gencode.EG_Protection}
			ENDIF
			RETURN ! SELF:_islocked
			
			#endregion
			
			
		#region operators
		/// <summary>Implicitely convert an array of USUALs to a typed array. Note that the usuals must contain a value of the correct type.</summary>
		STATIC OPERATOR IMPLICIT ( a AS ARRAY) AS __ArrayBase<T> 
			VAR aResult := __ArrayBase<T>{}
			LOCAL oErr AS Error
			FOREACH VAR u IN a
				LOCAL o := u AS OBJECT
				IF o IS T
					aResult:Add( (T) o)
				ELSE
					LOCAL nArg AS INT
					LOCAL oActType AS System.Type
					oActType := o:GetType()
					nArg := a:_internalList:IndexOf(u)+1
					oErr := Error{GenCode.EG_DATATYPE}
					oErr:Arg := "Array Element : "+nArg:ToString()
					oErr:Description := "Cannot convert array element " +nArg:ToString() + " from type "+oActType:ToString()+" to type "+TYPEOF(T):ToString()
					THROW oErr 
				ENDIF
			NEXT
			RETURN aResult			
			
			
			/// <summary>Implicitely convert a typed array to an array of USUALs.</summary>
		STATIC OPERATOR IMPLICIT ( a AS __ArrayBase<T> ) AS ARRAY
			VAR aResult := __Array{}
			FOREACH VAR o IN a
				aResult:Add(  o)
			NEXT
			RETURN aResult			
			
			#endregion
			
	END	CLASS
END NAMESPACE
