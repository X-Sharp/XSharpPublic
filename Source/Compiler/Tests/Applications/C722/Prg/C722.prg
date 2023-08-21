// 722. Problem with defining attributes - https://github.com/X-Sharp/XSharpPublic/issues/366
USING System.Reflection
USING System.Runtime.CompilerServices
USING System.Runtime.InteropServices

/*
C722.prg(22,5): error XS0531: 'ITest.Timeout.get': interface members cannot have a definition
C722.prg(1,8): error XS0592: Attribute 'IN' is not valid on this declaration type. It is only valid on 'parameter' declarations.

Note that in Core dialect, square brackets should not be supported for string delims at all,
but probably this code below should be supported for all dialects anyway.

Note also that the second error message points to line 1, not to the correct line in code
*/

[ComImport];
[DefaultMember("Timeout")];
[Guid("00000002-4000-11CE-AAA8-00608C50C597")];
[TypeLibType(TypeLibTypeFlags.FDispatchable)];
[InterfaceType(ComInterfaceType.InterfaceIsIDispatch)];
PUBLIC INTERFACE ITest
    [DispId(0)]PROPERTY Timeout AS LONG;
    [MethodImpl(MethodImplOptions.PreserveSig | MethodImplOptions.InternalCall)][DispId(0)] GET;
    [MethodImpl(MethodImplOptions.PreserveSig | MethodImplOptions.InternalCall)][DispId(0)][param: in] SET 
END INTERFACE

CLASS TestClass IMPLEMENTS ITest
	PROPERTY Timeout AS LONG AUTO
END CLASS
