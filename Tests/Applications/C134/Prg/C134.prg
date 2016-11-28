// 134. error: no viable alternative at input '[System.Runtime.InteropServices.DllImportAttribute("USER32.dll")];\r\n_DLL'
// not sure if this makes sense, but that existed in existing vulcan code

// x# now ignores the attribute, need to check if this was actually used in the original code
[System.Runtime.InteropServices.DllImportAttribute("USER32.dll")];
_DLL FUNC SetForegroundWindow(hWnd AS IntPtr) AS LOGIC PASCAL:USER32.SetForegroundWindow

