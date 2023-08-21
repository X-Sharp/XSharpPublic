// 379. Compiling this in vulcan results to
// error VN4026: illegal use of array subscript operator;  operand is not an array, indexed property or typed pointer
// Also note that the _winNMCBEENDEDIT VOSTRUCT is emitted with [VOStruct(100, 100)], 
// instead of [VOStruct(100, 1)] that vulcan uses

// NOTE: This is compiled with vulcan!
FUNCTION Start() AS VOID
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT
	sNMCBEENDEDIT := MemAlloc(SizeOf(_winNMCBEENDEDIT))
	sNMCBEENDEDIT:szText[1] := 127
	? sNMCBEENDEDIT:szText[1]
RETURN

/*
VOSTRUCT _winNMCBEENDEDIT ALIGN 1
	MEMBER DIM szText[100] AS BYTE

in vulcan this is emitted as:
[VOStruct(100, 1)]
[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct _winNMCBEENDEDIT
{
	[CompilerGenerated, UnsafeValueType]
	[StructLayout(LayoutKind.Sequential, Size = 100)]
	public struct $DIM_Array_szText
	{
		public byte FixedElementField;
	}

	[FixedBuffer(typeof(byte), 100)]
	public _winNMCBEENDEDIT.$DIM_Array_szText szText;
}

while in x# as:

[VOStruct(100, 100)]
[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct _winNMCBEENDEDIT
{
	[CompilerGenerated, UnsafeValueType]
	[StructLayout(LayoutKind.Sequential, Size = 100)]
	public struct <szText>e__FixedBuffer
	{
		public byte FixedElementField;
	}

	[FixedBuffer(typeof(byte), 100)]
	public _winNMCBEENDEDIT.<szText>e__FixedBuffer szText;
}
*/
