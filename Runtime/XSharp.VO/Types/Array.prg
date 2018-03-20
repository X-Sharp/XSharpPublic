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
    
    [DebuggerDisplay("USUAL{ToString(),nq}", Type := "ARRAY")];
    [DebuggerTypeProxy(typeof(ArrayDebugView))];
    public sealed class __Array inherit __ArrayBase<usual>
        
        constructor()
            super()
        
        constructor(capacity as int)
            super(capacity)
        
        constructor( elements as object[] )
            self()
            if elements == null
                throw ArgumentNullException{nameof(elements)}
            endif
            foreach element as object in elements
                if element == null
                    _internalList:add(default(usual))
                else
                    _internalList:Add( usual{ element})
                endif
            next
            return
        
        public static method ArrayCreate(dimensions params int[] ) as __Array
            local count := dimensions:Length as int
            if count <= 0
                throw ArgumentException{"No dimensions provided."}
            endif
            local initializer := object[]{dimensions[1]} as object[]
            local arrayNew as __Array
            arrayNew := __Array{initializer}
            
            if count > 1
                local i as int
                for i:=0+__ARRAYBASE__  upto dimensions[1]-1+__ARRAYBASE__
                    local newParams := int[]{count-1} as int[]
                    Array.Copy(dimensions,1,newParams,0,count-1)
                    arrayNew:_internalList[(int) i-__ARRAYBASE__ ] := __ArrayNew(newParams)
                next
            endif
            return arrayNew
        
        public static method __ArrayNew( dimensions params int[] ) as __Array
            local newArray as __Array 
            if dimensions:Length != 0 
                newArray := __ArrayNewHelper(dimensions,1)
            else
                newArray := __Array{}
            endif
            return newArray
        
        public static method __ArrayNewHelper(dimensions as int[], currentDim as int) as __Array
            local capacity  as int // one based ?
            local newArray as __Array
            capacity := dimensions[currentDim-1]
            newArray := __Array{capacity} 
            if currentDim != dimensions:Length
                local nextDim := currentDim+1 as int
                local index   := 1 as int
                do while index <= capacity
                    newArray:Add(usual{__ArrayNewHelper(dimensions,nextDim)})
                    index+=1
                enddo
                return newArray
            endif
            local i as int
            for i:=1 upto capacity
                newArray:Add(default(usual))
            next
            return newArray
        
        property Item[index params int[]] as usual
            get
                return __GetElement(index)
            end get
            set
                __SetElement(value, index)
            end set
        end property
        
        property Item[index as int] as usual
            get
                return __GetElement(index)
            end get
            set
                __SetElement(value, index)
            end set
        end property
        
        ///
        /// <Summary>Access the array element using ZERO based array index</Summary>
        ///
        public method __GetElement(index params int[]) as usual
            local indexLength := index:Length as int
            local currentArray := self as __Array
            local i as int
            
            for i:= 1  upto indexLength  -1 // walk all but the last level
                local u := currentArray:_internalList[ index[i] ] as usual
                if u:IsNil 
                    return u
                endif
                if u:UsualType != __UsualType.ARRAY
                    throw InvalidOperationException{"out of range error."}
                endif
                currentArray := (__Array) u
            next
            return currentArray:_internalList[ index[i] ]
        
        public method __SetElement(u as usual, index params int[] ) as usual
            // indices are 0 based
            if self:CheckLock()
                local length := index:Length as int
                local currentArray := self as __Array
                
                for var i := 1 upto length-1
                    local uArray := _internalList[index[i]] as usual
                    if !(uArray:UsualType == __UsualType.ARRAY)
                        throw InvalidOperationException{"Out of range error."}
                    endif
                    currentArray := (__Array) uArray
                next
                currentArray:_internalList[index[length]] := u
            endif
            return u
        
        internal class ArrayDebugView
            private _value as __Array
            public constructor (a as __Array)
                _value := a
            //[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
            public property Elements as List<usual> get _value:_internalList
            
        end class
        
    end	class
    
    
    public interface IArray
        public method Clone() as IArray
        public method CloneShallow() as IArray
        public method RemoveAt(pos as int) as void
    end	interface
end namespace
