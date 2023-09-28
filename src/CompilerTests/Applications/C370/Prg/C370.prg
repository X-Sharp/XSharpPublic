// 360. error XS0029: Cannot implicitly convert type '_WinHostEnt' to '_WinHostEnt*'
#pragma warnings(165, off) // unassigned local
_DLL FUNCTION GetHostByName( name AS PSZ) AS _WinHostEnt PASCAL:wsock32.gethostbyname
_DLL FUNCTION inet_ntoa(_in IS _winin_addr) AS PSZ PASCAL:WSOCK32.inet_ntoa
_DLL FUNCTION WSAGetLastError() AS INT PASCAL:WSOCK32.WSAGetLastError
_DLL FUNCTION WSAStartup(wVersionRequired AS WORD, lpWSAData AS _winWSADATA) AS INT PASCAL:WSOCK32.WSAStartup

VOSTRUCT __S_un_b_win
	MEMBER s_b1 AS BYTE
	MEMBER s_b2 AS BYTE
	MEMBER s_b3 AS BYTE
	MEMBER s_b4 AS BYTE

VOSTRUCT __S_un_w_win
	MEMBER  s_w1 AS WORD
	MEMBER  s_w2 AS WORD

UNION __S_un_win
	MEMBER S_un_b IS __s_un_b_win
	MEMBER S_un_w IS __S_un_w_win
	MEMBER S_addr AS DWORD

VOSTRUCT _winin_addr
	MEMBER s_un IS __S_un_win


VOSTRUCT  _WinHostEnt
	MEMBER  h_name AS PTR
	MEMBER  h_aliases AS PTR
	MEMBER  h_addrtype AS SHORTINT
	MEMBER  h_length AS SHORTINT
	MEMBER  h_addr_list AS PTR

FUNCTION Start() AS VOID
	? WinSockInit()
	? GetIPAddress("www.microsoft.com")
	wait


FUNCTION GetIPAddress(cHost AS STRING) AS STRING PASCAL
   LOCAL cRet      AS STRING
   LOCAL pHostEnt  AS _WInHostEnt
   LOCAL inaddr    IS _winin_addr
   LOCAL pDword    AS DWORD PTR
   IF SLen(cHost) > 0
         pHostEnt := gethostbyname(String2Psz(cHost))
         ? pHostEnt
         ? WSAGetLastError()
         IF (pHostEnt != NULL_PTR)
         // h_addr_list is a CHAR PTR PTR and must be deferenced twice
         // to get the 4 bytes of the IP Address
         pDword := pHostEnt:h_addr_list
         pDword := PTR(_CAST, pDword[1])
         inaddr:s_un:s_addr := pDword[1]
         cRet := Psz2String(inet_ntoa(Inaddr))
      ENDIF
   ENDIF
   RETURN cRet

FUNCTION WinSockInit()

   RETURN WSAStartup( 0x0101, @wsaData)


GLOBAL wsaData    IS _WinWSAData

#DEFINE WSADESCRIPTION_LEN        256
#DEFINE WSASYS_STATUS_LEN         128

VOSTRUCT _winWSAData
	MEMBER  wVersion AS WORD
	MEMBER  wHighVersion AS WORD
	MEMBER  DIM szDescription[WSADESCRIPTION_LEN+1] AS BYTE
	MEMBER  DIM szSystemStatus[WSASYS_STATUS_LEN+1] AS BYTE
	MEMBER  iMaxSockets AS WORD
	MEMBER  iMaxUdpDg AS WORD
	MEMBER  lpVendorInfo AS PTR


FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length
