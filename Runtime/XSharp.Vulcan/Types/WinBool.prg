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
	PRIVATE _value as INT
	CONSTRUCTOR (lValue as LOGIC)
		_value := iif(lValue, 1, 0)

	VIRTUAL METHOD GetHashCode() AS INT
		return _value:GetHashCode()
	#region Unary Operators
	STATIC OPERATOR !(value as __WinBool) AS LOGIC
		RETURN value._value == 0
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
		if obj != null .and. obj:getType() == typeof(LOGIC)
			return self == (LOGIC) obj
		endif
		return SUPER:Equals(obj)
	#endregion 
	#region Implicit Converters
	STATIC OPERATOR IMPLICIT(b AS __WinBool) AS LOGIC
		return b:_value != 0

	STATIC OPERATOR IMPLICIT(u AS __Usual) AS __WinBool
		return __WinBool{(LOGIC) u}

	STATIC OPERATOR IMPLICIT(l AS LOGIC) AS __WinBool
		return __WinBool{l}

	STATIC OPERATOR IMPLICIT(b AS __WinBool) AS __USUAL
		return b:_value != 0

	#endregion
	#region Logical operators
	STATIC OPERATOR &(lhs as __WinBool, rhs as __WinBool) as LOGIC
		return lhs._value == 1 .and. rhs:_value == 1

	STATIC OPERATOR &(lhs as __WinBool, rhs as LOGIC) as LOGIC
		return lhs._value == 1 .and. rhs

	STATIC OPERATOR &(lhs as LOGIC, rhs as __WinBool) as LOGIC
		return lhs .and. rhs:_value == 1

	STATIC OPERATOR |(lhs AS __WinBool, rhs AS __WinBool) AS LOGIC
		return lhs._value == 1 .or. rhs:_value == 1

	STATIC OPERATOR |(lhs AS __WinBool, rhs AS LOGIC) AS LOGIC
		return lhs._value == 1 .or. rhs == true

	STATIC OPERATOR |(lhs AS LOGIC, rhs AS __WinBool) AS LOGIC
		return lhs == true  .or. rhs:_value == 1

	#endregion
END STRUCT
END NAMESPACE