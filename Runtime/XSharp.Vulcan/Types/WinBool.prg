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
PUBLIC STRUCT __WinBool 
	private static trueValue := __WinBool{1}	as __WinBool
	private static falseValue := __WinBool{0}	as __WinBool
	PRIVATE _value AS INT			// 0 = false, 1 = true

	PRIVATE CONSTRUCTOR(value as INT)
		_value := value

	CONSTRUCTOR (lValue as LOGIC)
		_value := iif(lValue, 1, 0)

	VIRTUAL METHOD GetHashCode() AS INT
		return _value:GetHashCode()

	#region Unary Operators
	STATIC OPERATOR !(wb as __WinBool) AS LOGIC
		RETURN wb._value == 0
	#endregion

	#region Binary Operators
	OPERATOR == (lhs as __WinBool, rhs as __WinBool) as LOGIC
		return lhs:_value == rhs:_value

	OPERATOR != (lhs as __WinBool, rhs as __WinBool) as LOGIC
		return lhs:_value != rhs:_value

	OPERATOR == (lhs as __WinBool, rhs as LOGIC) as LOGIC
		return iif (rhs, lhs:_value != 0, lhs:_value == 0)

	OPERATOR != (lhs AS __WinBool, rhs AS LOGIC) AS LOGIC
		return iif (rhs, lhs:_value == 0, lhs:_value != 0)

	OPERATOR == (lhs as LOGIC, rhs as __WinBool) as LOGIC
		return iif (lhs, rhs:_value != 0, rhs:_value == 0)

	OPERATOR != (lhs as LOGIC, rhs as __WinBool) as LOGIC
		return iif (lhs, rhs:_value == 0, rhs:_value != 0)

	public method Equals(obj as object) as logic
		if obj is __WinBool
			return self:_value == ((__WinBool) obj):_value
		endif
		return false
	#endregion 

	#region Implicit Converters
	STATIC OPERATOR IMPLICIT(wb AS __WinBool) AS LOGIC
		return wb:_value != 0

	STATIC OPERATOR IMPLICIT(u AS __Usual) AS __WinBool
		return __WinBool{(LOGIC) u}

	STATIC OPERATOR IMPLICIT(l AS LOGIC) AS __WinBool
		return iif(l, trueValue, falseValue)

	STATIC OPERATOR IMPLICIT(wb AS __WinBool) AS __USUAL
		return wb:_value != 0

	#endregion
	#region Logical operators
	STATIC OPERATOR &(lhs as __WinBool, rhs as __WinBool) as __WinBool
		return iif( lhs._value == 1 .and. rhs:_value == 1, trueValue, falseValue)

	STATIC OPERATOR |(lhs AS __WinBool, rhs AS __WinBool) as __WinBool
		return iif(lhs._value == 1 .or. rhs:_value == 1, trueValue, falseValue)

	static operator true(wb as __WinBool ) as logic
		return wb._value == 1

	static operator false(wb as __WinBool )as logic
		return wb._value == 0

	#endregion
END STRUCT
END NAMESPACE