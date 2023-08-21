// 727. Cannot locally turn option /vo16 off
USING System.Runtime.InteropServices

// Note that of course the same goes for many other /vo options!


// warning XS9096: Unrecognized or unsupported #pragma option value '"vo16"'
// error XS0669: A class with the ComImport attribute cannot have a user-defined constructor
#pragma options ("vo16", off)
[ComImport, Guid("17D6CCD8-3B7B-11D2-B9E0-00C04FD8DBF7")];
INTERNAL CLASS DSObjectPicker
END CLASS
#pragma options ("vo16", default)

