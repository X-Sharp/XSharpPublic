// 561. No error reported on syntax OBJECT(_CAST , <primitive type>)
FUNCTION Start() AS VOID
LOCAL n AS INT
LOCAL d AS DWORD
LOCAL l AS LOGIC
//LOCAL p AS PTR

// those are ok to be allowed:
? OBJECT(n)
? OBJECT(d)
? OBJECT(l)
//? OBJECT(p) // this one throws a runtime exception
// also this should be allowed of course
? DWORD(_CAST , n)
? INT(_CAST , d)

// those should be errors, see below why:
? OBJECT(_CAST , n)
? OBJECT(_CAST , d)
? OBJECT(_CAST , l)
//? OBJECT(_CAST , p) // this one throws a runtime exception

RETURN

// this is original code from bBrowser, which did compile, but of course did not work properly at runtime
// see the OBJECT(_CAST, dwCookie)
// I assume this was directly ported from VO where this did work at runtime
STATIC FUNCTION __bRTFDocumentSetValue(dwCookie AS DWORD, pbBuff AS BYTE PTR, cb AS DWORD, pcb AS DWORD PTR) AS DWORD /* WINCALL */
	   // RTF-Wert setzen
	LOCAL oRTF				AS bRTFDocument
	LOCAL pStream			AS BYTE PTR
	   // Puffer für den RTF-Wert ermitteln

	oRTF := OBJECT(_CAST, dwCookie) // requires /vo7+
	oRTF := (bRTFDocument) OBJECT(_CAST, dwCookie) // compiles also without /vo7+
	pStream := oRTF:__Stream
RETURN 0

CLASS bRTFDocument
	EXPORT __Stream AS BYTE PTR
END CLASS

