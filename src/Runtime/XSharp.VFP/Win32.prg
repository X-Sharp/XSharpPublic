// Proyecto: XSharp.VFP
// Fichero: Win32.prg

USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.VFP

    INTERNAL PARTIAL STATIC CLASS Win32
        PUBLIC CONST DESKTOP_HORZRES := 117 AS INT
        PUBLIC CONST DESKTOP_VERTRES := 118 AS INT

        [DllImport("gdi32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        PUBLIC STATIC EXTERN METHOD GetDeviceCaps(hDC AS IntPtr, nIndex AS INT) AS INT

        [DllImport("user32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        PUBLIC STATIC EXTERN METHOD GetDC(hWnd AS IntPtr) AS IntPtr

        [DllImport("user32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        PUBLIC STATIC EXTERN METHOD ReleaseDC(hWnd AS IntPtr, hDC AS IntPtr) AS INT

        [DllImport("kernel32.dll", CharSet := CharSet.Auto,  SetLastError := TRUE)];
        PUBLIC STATIC EXTERN METHOD GetDriveType(lpRootPathName AS STRING) AS DWORD

        [DllImport("user32.dll", CharSet := CharSet.Auto, ExactSpelling := TRUE, SetLastError := TRUE)];
        PUBLIC STATIC EXTERN METHOD GetSystemMetrics(nIndex AS INT) AS INT

    END CLASS

END NAMESPACE
