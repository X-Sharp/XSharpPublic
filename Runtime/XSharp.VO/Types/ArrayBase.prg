//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using System.Diagnostics
using XSharp
begin namespace XSharp	
    public class __ArrayBase<T> implements IEnumerable<T>, IArray where T is new()
        internal _internalList as List<T> 
        private _islocked as logic 
        #region constructors
            constructor()
                _internalList := List<T>{}
                return  
            
            constructor(capacity as int)
                _internalList := List<T>{capacity}
                _internalList:AddRange(Enumerable.Repeat(default(T),capacity))
                return 
            
            constructor( collection as IEnumerable<T>)
                _internalList := List<T>{collection}
                return 
            
            constructor( elements as object[] )
                self()
                if elements == null
                    throw ArgumentNullException{nameof(elements)}
                endif
                foreach element as object in elements
                    if element == null
                        _internalList:add(default(T))
                    elseif element is T
                        _internalList:Add( (T) element)
                    else
                        throw ArgumentException{"Object array contains element of incorrect type "+element:GetType():FullName}
                    endif
                next
                return
            
            constructor( elements as T[] )
                _internalList := List<T>{elements}
                return
        #endregion
        
        #region properties
            public property IsEmpty as logic
                get
                    return (_internalList:Count == 0)
                end get 
            end property
            
            public property Length as dword
                get
                    return (dword)_internalList:Count
                end get
            end property
        #endregion
        
        #region Enumerators
            public method GetEnumerator() as IEnumerator<T>
                return _internalList:GetEnumerator()
            
            public method IEnumerable.GetEnumerator() as IEnumerator
                return _internalList:GetEnumerator()
            
        #endregion
        
        
        #region Cloning
            public method Clone() as IArray
                local aResult as __ArrayBase<T>
                local nCount as int
                nCount := _internalList:Count
                aResult := __ArrayBase<T>{nCount}
                for var I := 0 to nCount-1
                    var u := _internalList[i]
                    if u is IArray
                        var aElement := (IArray) u 
                        aResult:_internalList[i] := (T) aElement:Clone()
                    else
                        aResult:_internalList[i] := u
                    endif
                next
                return aResult
            
            public method CloneShallow() as IArray
                local aResult as __ArrayBase<T>
                local nCount as int
                nCount := _internalList:Count
                aResult := __ArrayBase<T>{nCount}
                for var I := 0 to nCount-1
                    aResult:_internalList[i] := _internalList[i]
                next
                return aResult
            
        #endregion
        
        
        #region Indexers and to Get / Set Elements. 
            ///
            /// <Summary>Access the array element using ZERO based array index</Summary>
            ///
            public method __GetElement(index as int) as T
                return self:_internalList[ index ]
            
            public method __SetElement(u as T,index as int) as T
                if self:CheckLock()
                    _internalList[index]:=u
                endif
                return u
            
            
            public property self[index as dword] as T
                get
                    return self[ (int) index]
                end get
                set
                    self[ (int) index] := value
                end set
            end property
            
            public property self[i as int] as T
                get
                    if i<__ARRAYBASE__ || i > _internalList:Count
                        throw ArgumentOutOfRangeException{}
                    endif
                    return _internalList[i - __ARRAYBASE__ ]
                end get
                set
                    if self:CheckLock()
                        if i<__ARRAYBASE__|| i > _internalList:Count
                            throw ArgumentOutOfRangeException{}
                        endif
                        _internalList[i-__ARRAYBASE__] := value
                    endif
                end set
            end property
            
        #endregion
        
        #region Insert and Delete elements
            public method Add(u as T) as void
                if self:CheckLock()
                    _internalList:Add(u)
                endif
                return
            
            public method Add(o as object) as void
                if o is T
                    self:Add((T)o)
                else
                    throw ArgumentException{"Parameter is of incorrect type "+o:GetType():FullName,nameof(o)}
                endif
                return
            
            public method Delete(position as dword) as __ArrayBase<T>
                self:RemoveAt(position)
                self:Add(T{})
                return self	
            
            public method Delete(position as int) as __ArrayBase<T>
                self:RemoveAt(position)
                self:Add(T{})
                return self	
            
            public method Insert(index as int,o as object) as void
                if (o is T)
                    self:Insert(index, (T)o)
                else
                    throw ArgumentException{"Parameter is of incorrect type "+o:GetType():FullName,nameof(o)}
                endif
                return
            
            public method Insert(index as dword,o as object) as void
                if (o is T)
                    self:Insert((int)index, (T)o)
                else
                    throw ArgumentException{"Parameter is of incorrect type "+o:GetType():FullName,nameof(o)}
                endif
                
                return
            
            public method Insert(index as int,u as T) as void
                if self:CheckLock()
                    _internalList:Insert(index-__ARRAYBASE__ ,u)
                endif
                return
            
            public method Insert(index as dword,u as T) as void
                self:Insert( (int) index, u)
                return
            
            public method Insert(position as int) as __ArrayBase<T>
                self:Insert(position,default(T))
                return self
            
            public method Insert(position as dword) as __ArrayBase<T>
                self:Insert((int) position, default(T))
                return self
            
            public method RemoveAt(index as int , count as int) as void
                if self:CheckLock()
                    _internalList:RemoveRange(index-__ARRAYBASE__ ,count)
                endif
                return
            
            public method RemoveAt(index as dword , count as int) as void 
                self:RemoveAt( (int) index, count)
                return
            
            public method RemoveAt(index as int) as void
                if self:CheckLock()
                    _internalList:RemoveRange(index-__ARRAYBASE__,1 )
                endif
                return
            
            public method RemoveAt(index as dword) as void
                self:RemoveAt( (int) index)
                return
            
            public method Resize(newSize as dword) as void
                self:Resize( ( int) newSize)
            
            public method Resize(newSize as int) as void
                local count := self:Length as dword
                if self:CheckLock()
                    if newSize < 0 
                        throw ArgumentException{"Size must be greater or equal than zero."}
                    endif
                    if newSize == 0 
                        _internalList:Clear()
                    else
                        if newSize <= count 
                            _internalList:RemoveRange((int)newSize, (int)(count - newSize))
                        else
                            count+=1
                            do while count <= newSize
                                local u := T{} as T
                                _internalList:Add(u)
                                count++
                            enddo
                        endif
                    endif
                endif
                return
            
            
            
        #endregion
        
        public method ToString() as string
            return string.Format("[{0}]",_internalList:Count)
        
        public method Sort(startIndex as int, count as int, comparer as IComparer<T>) as void
            _internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
            return
        
        public method Swap(position as int, element as T) as T
            return Swap( (dword) position, element)
        
        public method Swap(position as dword, element as T) as T
            local original := _internalList[(int) position - __ARRAYBASE__] as T
            _internalList[(int) position - __ARRAYBASE__]:=element
            return original
        
        public method Swap(position as int, element as object) as T
            if (element is T)
                return Swap( position, (T) element)
            else
                throw ArgumentException{"Parameter is of incorrect type "+element:GetType():FullName,nameof(element)}
            endif
        
        public method Swap(position as dword, element as object) as T
            if (element is T)
                return Swap( position, (T) element)
            else
                throw ArgumentException{"Parameter is of incorrect type "+element:GetType():FullName,nameof(element)}
            endif
        
        public method Tail() as T
            return _internalList:LastOrDefault()
        
        
        #region locking
            method Lock(lLocked as logic) as logic
                local wasLocked as logic
                wasLocked := self:_islocked
                self:_islocked := lLocked
                return wasLocked
            
            property Locked as logic get _islocked
            protected method CheckLock as logic
                if self:_islocked
                    throw Error{Gencode.EG_Protection}
                endif
                return ! self:_islocked
            
        #endregion
        
        
        #region static function
            public static method Copy(aSource as IArray,aTarget as IArray,parameter params int[] ) as __Array
                throw NotImplementedException{"__ArrayBase.Copy is not implemented yet."}
            
            
        #endregion
        
    end	class
end namespace
