// 404. error XS0103: The name 'Xs$PszList' does not exist in the current context
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
? o:TestProp
o:TestProp := "x#"

IF o:TestProp != "Abc"
	THROW Exception{"Incorrect result"}
END IF

CLASS TestClass
	ACCESS Dummy AS PSZ
	RETURN String2Psz("abc") // OK in ACCESS
	PROPERTY TestProp AS STRING
		GET
			LOCAL p AS PSZ
			LOCAL c AS STRING
			c := "Abc"
			p := String2Psz("ImgSrc") // error
			? p
			p := String2Psz(c) // error
			c := Psz2String(p)
			? c
			RETURN c
		END GET
		SET
			LOCAL p AS STRING
			p := String2Psz(VALUE) // error
			? p
		END SET
	END PROPERTY
END CLASS


// original code from Bernhard
PUBLIC STATIC CLASS MDVNGlobalSettings
   STATIC PRIVATE _pIcoHand AS PTR
       STATIC PROPERTY pIcoHand AS PTR
         GET
            IF (_pIcoHand == NULL_PTR)
               // error pointing to String2Psz() XS0103: The name 'Xs$PszList' does not exist in the current context
               _pIcoHand := LoadImage(;
                              GetModuleHandle(String2Psz("ImgSrc")),;
                              String2Psz("CURHAND"),;
                              1,;
                              0,;
                              0,;
                              1)
            ENDIF
            RETURN _pIcoHand
         END GET
         SET
            _pIcoHand := VALUE
         END SET
       END PROPERTY
END CLASS

_DLL FUNCTION LoadImage(hinst AS PTR, lpszName AS PSZ, uType AS DWORD, cxDesired AS INT,;
	cydesired AS INT, fuLoad AS DWORD);
	AS PTR PASCAL:USER32.LoadImageA
_DLL FUNC GetModuleHandle(lpModuleName AS PSZ) AS PTR PASCAL:KERNEL32.GetModuleHandleA

