//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.InteropServices
begin namespace XSharp
	[StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)];
	STRUCTURE __VOFloat IMPLEMENTS IFLoat
		[FieldOffset(0)];
		PRIVATE _value as Real8
		[FieldOffset(8)];
		PRIVATE _length as ShortInt
		[FieldOffset(10)];
		PRIVATE _decimals as ShortInt
		CONSTRUCTOR (r8 as Real8)
			self:_value    := r8
			self:_length   := 0
			self:_decimals := 0

		CONSTRUCTOR (r8 as Real8, decimals as int)
			self:_value    := r8
			self:_length   := 0
			self:_decimals := (shortint) decimals 

		CONSTRUCTOR (r8 as Real8, length as int, decimals as int )
			self:_value    := r8
			self:_decimals := (shortint) decimals
			self:_length   := (shortint) length

		OPERATOR IMPLICIT( i as INT) AS __VOFloat
			RETURN __VOFloat{i}

		PROPERTY Value    AS REAL8	GET _value		SET _value		:= Value
		PROPERTY Digits   AS INT	GET _length		SET _length		:= (ShortInt) Value
		PROPERTY Decimals AS INT	GET _decimals	SET _decimals	:= (ShortInt)Value
	END STRUCTURE

end namespace