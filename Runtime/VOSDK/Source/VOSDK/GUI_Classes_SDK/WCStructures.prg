/// <exclude />
VOSTRUCT __WCDropFiles
	MEMBER wSize AS DWORD				// Size of data structure
	MEMBER ptMousePos IS _winPoint		//Position of mouse in the
	MEMBER fInNonClientArea AS LOGIC	//window's non-client area
	MEMBER fUnicode AS LOGIC			// are the pathnames in Unicaode?


/// <exclude/>
VOSTRUCT SelfPtr
	MEMBER ptrSelf AS PTR


/// <exclude/>
VOSTRUCT strColor
	MEMBER s1 AS DWORD
	MEMBER s2 AS DWORD
	MEMBER s3 AS DWORD
	MEMBER s4 AS DWORD
	MEMBER s5 AS DWORD
	MEMBER s6 AS DWORD
	MEMBER s7 AS DWORD
	MEMBER s8 AS DWORD
	MEMBER s9 AS DWORD
	MEMBER s10 AS DWORD
	MEMBER s11 AS DWORD
	MEMBER s12 AS DWORD
	MEMBER s13 AS DWORD
	MEMBER s14 AS DWORD
	MEMBER s15 AS DWORD
	MEMBER s16 AS DWORD


/// <exclude/>
VOSTRUCT WCColor
	MEMBER bBlue 	AS BYTE
	MEMBER bGreen 	AS BYTE
	MEMBER bRed 	AS BYTE
	MEMBER bNotUsed AS BYTE


/// <exclude />
VOSTRUCT __WCDialog_VARS
	MEMBER hFont AS PTR // handle to the new font
	MEMBER hDlg AS PTR // Window handle of dialog box
	MEMBER Xbase AS LONGINT // original base X
	MEMBER Ybase AS LONGINT // original base Y
	MEMBER fontX AS LONGINT // font width
	MEMBER fontY AS LONGINT // font height


