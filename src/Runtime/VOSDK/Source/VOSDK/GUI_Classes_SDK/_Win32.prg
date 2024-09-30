// _Win32.prg
// Created by    : robert
// Creation Date : 9/20/2024 3:29:07 PM
// Created for   :
// WorkStation   : LEDA



_DLL FUNCTION Win32SetFocus( hwnd AS PTR ) AS PTR PASCAL:USER32.SetFocus
_DLL FUNCTION Win32IsIconic( hwnd AS PTR) AS LOGIC PASCAL:USER32.IsIconic
_DLL FUNCTION Win32IsZoomed(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsZoomed
_DLL FUNCTION Win32LineTo(hdc AS PTR, nXEnd AS INT, nYEnd AS INT) AS LOGIC PASCAL:GDI32.LineTo
_DLL FUNCTION Win32GetSubMenu(hMenu AS PTR, nPos AS INT) AS PTR PASCAL:USER32.GetSubMenu
_DLL FUNCTION Win32SetParent( hWndChild AS PTR, hWndNewParent AS PTR) AS PTR PASCAL:USER32.SetParent
