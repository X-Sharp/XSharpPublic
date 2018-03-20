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
    
    [DebuggerDisplay("{ToString(),nq}", Type := "LOGIC")];
    public struct __WinBool 
        private static trueValue := __WinBool{1}	as __WinBool
        private static falseValue := __WinBool{0}	as __WinBool
        private _value as int			// 0 = false, 1 = true
        
        private constructor(value as int)
            _value := value
        
        constructor (lValue as logic)
            _value := iif(lValue, 1, 0)
        
        virtual method GetHashCode() as int
            return _value:GetHashCode()
        
        method GetTypeCode() as TypeCode
            return TypeCode.Boolean
        
        
        #region Unary Operators
            static operator !(wb as __WinBool) as logic
                return wb:_value == 0
        #endregion
        
        #region Binary Operators
            operator == (lhs as __WinBool, rhs as __WinBool) as logic
                return lhs:_value == rhs:_value
            
            operator != (lhs as __WinBool, rhs as __WinBool) as logic
                return lhs:_value != rhs:_value
            
            operator == (lhs as __WinBool, rhs as logic) as logic
                return iif (rhs, lhs:_value != 0, lhs:_value == 0)
            
            operator != (lhs as __WinBool, rhs as logic) as logic
                return iif (rhs, lhs:_value == 0, lhs:_value != 0)
            
            operator == (lhs as logic, rhs as __WinBool) as logic
                return iif (lhs, rhs:_value != 0, rhs:_value == 0)
            
            operator != (lhs as logic, rhs as __WinBool) as logic
                return iif (lhs, rhs:_value == 0, rhs:_value != 0)
            
            public method Equals(obj as object) as logic
                if obj is __WinBool
                    return self:_value == ((__WinBool) obj):_value
                endif
                return false
        #endregion 
        
        #region Implicit Converters
            static operator implicit(wb as __WinBool) as logic
                return wb:_value != 0
            
            static operator implicit(u as usual) as __WinBool
                return __WinBool{(logic) u}
            
            static operator implicit(l as logic) as __WinBool
                return iif(l, trueValue, falseValue)
            
            static operator implicit(wb as __WinBool) as usual
                return wb:_value != 0
            
        #endregion
        #region Logical operators
            static operator &(lhs as __WinBool, rhs as __WinBool) as __WinBool
                return iif( lhs:_value == 1 .and. rhs:_value == 1, trueValue, falseValue)
            
            static operator |(lhs as __WinBool, rhs as __WinBool) as __WinBool
                return iif(lhs:_value == 1 .or. rhs:_value == 1, trueValue, falseValue)
            
            static operator true(wb as __WinBool ) as logic
                return wb:_value == 1
            
            static operator false(wb as __WinBool )as logic
                return wb:_value == 0
            
        #endregion
    end	struct
end namespace