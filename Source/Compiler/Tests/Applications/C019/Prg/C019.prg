// 19. In vulcan, SizeOf() works differently
//  error XS0233: 'Point' does not have a predefined size, therefore sizeof can only be used in an unsafe context (consider using System.Runtime.InteropServices.Marshal.SizeOf)
FUNCTION Start() AS VOID
? SizeOf(System.Drawing.Point)

