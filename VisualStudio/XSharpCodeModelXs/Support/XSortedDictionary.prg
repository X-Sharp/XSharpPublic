//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Collections
USING System.Diagnostics
USING XSharpModel



BEGIN NAMESPACE XSharpModel
   /// <summary>
   /// The XCollection class.
   /// </summary>
[DebuggerDisplay("{ToDebuggerDisplay()}")];
CLASS XSortedDictionary<T,U>   IMPLEMENTS IEnumerable<T>, IEnumerable
	protected _list 		   as List<T>     
	protected _dictionary   as Dictionary <T, IList<U> >
	protected _valuelist    as List<U>     
	protected _sorted 		as LOGIC   
	protected _comparer 	   as IComparer<T>
	
CONSTRUCTOR(comparer as IComparer<T>)
	_list 			:= List<T>{}  
	_valuelist     := List<U>{}  
	_dictionary    := Dictionary<T, IList<U> >{}
	_sorted 		   := FALSE     
	_comparer 		:= comparer
RETURN

CONSTRUCTOR(capacity as int,comparer as IComparer<T>)
	_list           := List<T>{capacity}
	_valuelist      := List<U>{capacity}
	_dictionary     := Dictionary<T,IList<U> > {}
	_sorted := FALSE
	_comparer := comparer
RETURN


METHOD Sort() as VOID
   if ! SELF:_sorted
	   _list:Sort()
	   _sorted := TRUE
   endif
   RETURN
	
METHOD FindMatching(crit as T) AS IList<U>
	var result   := List<U>{}
	var index    := SELF:FirstIndex(crit)
	if index >= 0
      result:AddRange(_dictionary[_list[index]])
		index++
		do while index < _list:Count .and. _comparer:Compare(_list[index], crit) == 0
         result:AddRange(_dictionary[_list[index]])
			index++
		enddo
	endif
return result
METHOD FirstIndex(crit as T) AS LONG
   SELF:Sort()
	var index    := _list:BinarySearch(crit,_comparer)
	if index > 0
		var pos := index -1
		do while pos >= 0 .and. _comparer:Compare(_list[pos], crit) == 0
			pos--
		enddo
		pos++
		return pos
	endif
return index

    #region ICollection<T>
PROPERTY Count      AS INT GET _list:Count
PROPERTY IsReadOnly AS LOGIC GET false

PROPERTY SELF[name as T]   AS IList<U>
	GET     
		if _dictionary:ContainsKey(name)
			return _dictionary[name]
		endif
		return List<U>{}
	END GET
END PROPERTY	

PROPERTY SortedKeys as IEnumerable<T> GET SELF:_list:AsReadOnly()
	
PROPERTY Keys 	 as T[]		GET SELF:_list:ToArray()
PROPERTY Values as U[]		GET SELF:_valuelist:ToArray()
	
METHOD Add(name as T, item as U) AS VOID
   IF ! SELF:_valuelist:Contains(item)
      SELF:_valuelist:Add(item)
   ENDIF
   IF !SELF:_sorted 
      IF !self:_list:Contains(name)
		   SELF:_list:Add(name)   
      ENDIF
	ELSE
		var index := SELF:FirstIndex(name)
      // do not include duplicate keys
		IF index < 0
			index := _NOT(index)     
			_list:Insert(index, name)
		endif
   ENDIF
   if !SELF:_dictionary:ContainsKey(name)
      self:_dictionary:Add( name, List<U>{})
   ENDIF
   self:_dictionary[name]:Add(item)
	
METHOD Clear() AS VOID
	SELF:_list:Clear()
	SELF:_valuelist:Clear()
	SELF:_dictionary:Clear()


METHOD Remove(name as T) AS LOGIC
   var result := SELF:_list:Remove(name)
   if self:_dictionary:ContainsKey(name)
      self:_dictionary:Remove(name)
   ENDIF
   RETURN result

METHOD ContainsKey(name as T) AS LOGIC
	return _list:Contains(name)

    #endregion
    #region IEnumerable<T>
METHOD GetEnumerator as IEnumerator<T>
return _list:GetEnumerator()

METHOD IEnumerable.GetEnumerator as IEnumerator
return _list:GetEnumerator()
    #endregion

METHOD ToDebuggerDisplay() AS STRING
	return "<"+typeof(T):ToString()+","+typeof(U):ToString()+">:"+SELF:Count:ToString()

END CLASS



CLASS PartialComparer IMPLEMENTS IComparer<STRING>
	METHOD Compare(x as string, y as string)  as LONG
		if x == null .or. y == null
			return 0
		endif
		return String.Compare(x,0,y,0,y:Length,TRUE)
		
END CLASS	
END NAMESPACE 