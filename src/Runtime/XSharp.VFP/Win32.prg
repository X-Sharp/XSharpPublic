// Proyecto: XSharp.VFP
// Fichero: Win32.prg

USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.VFP

    INTERNAL PARTIAL STATIC CLASS Win32

        [DllImport("kernel32.dll", CharSet := CharSet.Auto,  SetLastError := TRUE)];
        PUBLIC STATIC EXTERN METHOD GetDriveType(lpRootPathName AS STRING) AS DWORD

    END CLASS

END NAMESPACE
