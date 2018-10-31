VOSTRUCT INTERNET_ASYNC_RESULT
	//
	// dwResult - the HINTERNET, DWORD or BOOL return code from an async API
	//
	MEMBER dwResult AS DWORD
	
	//
	// dwError - the error code if the API failed
	//
	
	MEMBER dwError  AS DWORD
	
	
	//
	// INTERNET_PREFETCH_STATUS -
	//
	
VOSTRUCT INTERNET_PREFETCH_STATUS
	//
	// dwStatus - status of download. See INTERNET_PREFETCH_ flags
	//
	
	MEMBER dwStatus AS DWORD
	
	//
	// dwSize - size of file downloaded so far
	//
	MEMBER dwSize   AS DWORD
	
	
	//
	// INTERNET_PREFETCH_STATUS - dwStatus values
	//
	
VOSTRUCT INTERNET_PROXY_INFO
	//
	// dwAccessType - INTERNET_OPEN_TYPE_DIRECT, INTERNET_OPEN_TYPE_PROXY, or
	// INTERNET_OPEN_TYPE_PRECONFIG (set only)
	//
	MEMBER dwAccessType AS DWORD
	
	//
	// lpszProxy - proxy server list
	//
	MEMBER lpszProxy  AS PSZ
	
	//
	// lpszProxyBypass - proxy bypass list
	//
	
	MEMBER lpszProxyBypass AS PSZ
	
	
	//
	// INTERNET_VERSION_INFO - version information returned via
	// InternetQueryOption(..., INTERNET_OPTION_VERSION, ...)
	//
	
VOSTRUCT INTERNET_VERSION_INFO
	MEMBER dwMajorVersion   AS DWORD
	MEMBER dwMinorVersion   AS DWORD
	


VOSTRUCT _winINTERNET_CONNECTED_INFO
  MEMBER dwConnectedState  AS DWORD
  MEMBER dwFlags AS DWORD


VOSTRUCT URL_COMPONENTS
	MEMBER  dwStructSize        AS DWORD    // size of this structure. Used in version check
	MEMBER  lpszScheme          AS PSZ      // pointer to scheme name
	MEMBER  dwSchemeLength      AS DWORD    // length of scheme name
	MEMBER  nScheme             AS INT      // enumerated scheme type (if known)
	MEMBER  lpszHostName        AS PSZ      // pointer to host name
	MEMBER  dwHostNameLength    AS DWORD    // length of host name
	MEMBER  nPort               AS INT      // converted port number
	MEMBER  lpszUserName        AS PSZ      // pointer to user name
	MEMBER  dwUserNameLength    AS DWORD    // length of user name
	MEMBER  lpszPassword        AS PSZ      // pointer to password
	MEMBER  dwPasswordLength    AS DWORD    // length of password
	MEMBER  lpszUrlPath         AS PSZ      // pointer to URL-path
	MEMBER  dwUrlPathLength     AS DWORD    // length of URL-path
	MEMBER  lpszExtraInfo       AS PSZ      // pointer to extra information (e.g. ?foo or #foo)
	MEMBER  dwExtraInfoLength   AS DWORD    // length of extra information
	
	
	//
	// INTERNET_CERTIFICATE_INFO lpBuffer - contains the certificate returned from
	// the server
	//
	
VOSTRUCT INTERNET_CERTIFICATE_INFO
	//
	// ftExpiry - date the certificate expires.
	//
	MEMBER ftExpiry     IS _WINFILETIME
	
	//
	// ftStart - date the certificate becomes valid.
	//
	MEMBER ftStart      IS _WINFILETIME
	
	//
	// lpszSubjectInfo - the name of organization, site, and server
	//   the cert. was issued for.
	//
	MEMBER lpszSubjectInfo  AS PSZ
	
	//
	// lpszIssuerInfo - the name of organization, site, and server
	//   the cert was issues by.
	//
	MEMBER lpszIssuerInfo   AS PSZ
	
	//
	// lpszProtocolName - the name of the protocol used to provide the secure
	//   connection.
	//
	MEMBER lpszProtocolName AS PSZ
	
	//
	// lpszSignatureAlgName - the name of the algorithm used for signing
	//  the certificate.
	//
	
	MEMBER lpszSignatureAlgName AS PSZ
	//
	// lpszEncryptionAlgName - the name of the algorithm used for
	//  doing encryption over the secure channel (SSL/PCT) connection.
	//
	
	MEMBER lpszEncryptionAlgName    AS PSZ
	//
	// dwKeySize - size of the key.
	//
	
	MEMBER dwKeySize                AS PSZ
	
	
	//
	// FTP manifests
	//
	
	
	//
	// access types for InternetOpen()
	//
	
_DLL FUNC InternetOpen     (lpszAgent       AS PSZ,  ;
		dwAccess        AS DWORD,;
		lpszProxy       AS PSZ,  ;
		lpszProxyBypass AS PSZ,  ;
		dwFlags        AS DWORD) ;
		AS PTR PASCAL:WININET.InternetOpenA
	
_DLL FUNC InternetCloseHandle(hOpen AS PTR) ;
		AS LOGIC PASCAL:WININET.InternetCloseHandle
	
//
// options manifests for Internet{Query|Set}Option
//

_DLL FUNC InternetErrorDlg  (   hWnd AS PTR, ;
		hRequest AS PTR, ;
		dwError  AS DWORD,;
		dwFlags  AS DWORD,;
		ppData   AS PTR);
		AS DWORD PASCAL:WININET.InternetErrorDlg
	
	
	
	
	//
	// status manifests for Internet status callback
	//
	
_DLL FUNC InternetConnect  (hOpen           AS PTR,  ;
		lpszServerName  AS PSZ,  ;
		nPort           AS WORD, ;
		lpszUserName    AS PSZ,  ;
		lpszPassword    AS PSZ,  ;
		dwService       AS DWORD,;
		dwFlags         AS DWORD,;
		dwContext       AS DWORD );
		AS PTR PASCAL:WININET.InternetConnectA
	
	
	//
	
	//
	// FTP
	//
	
	//
	// manifests
	//
	
FUNCTION FTP_TRANSFER_TYPE_MASK     AS DWORD STRICT
	RETURN _FTP_TRANSFER_TYPE_MASK
	// prototypes (FTP
	//
	
_DLL FUNC FtpFindFirstFile (hConnect       AS PTR,  ;              //  In
	lpszSearchFile AS PSZ,  ;              // In, OPTIONAL
	lpFindFileData AS _WINWIN32_FIND_DATA, ;  // Out
	dwFlags        AS DWORD, ;             // In
	dwContext      AS DWORD );             // In
	AS PTR PASCAL:WININET.FtpFindFirstFileA
	
_DLL FUNC InternetFindNextFile( hFind          AS PTR, ;
		lpFindFileData AS _WINWIN32_FIND_DATA);
		AS LOGIC PASCAL:WININET.InternetFindNextFileA
	
	
_DLL FUNC FtpGetFile       (hConnect        AS PTR, ;
		lpszRemoteFile  AS PSZ, ;
		lpszNewFile     AS PSZ, ;
		fFailIfExists   AS LOGIC,;
		dwFlagsAndAttributes AS DWORD,;
		dwFlags         AS DWORD,;
		dwContext      AS DWORD );
		AS LOGIC PASCAL:WININET.FtpGetFileA
	
	
_DLL FUNC FtpPutFile       (hConnect        AS PTR, ;
		lpszLocalFile   AS PSZ, ;
		lpszRemoteFile  AS PSZ, ;
		dwFlags         AS DWORD,;
		dwContext       AS DWORD );
		AS LOGIC PASCAL:WININET.FtpPutFileA
	
	
_DLL FUNC FtpDeleteFile    (hConnect        AS PTR, ;
		lpszRemoteFile  AS PSZ );
		AS LOGIC PASCAL:WININET.FtpDeleteFileA
	
	
_DLL FUNC FtpRenameFile    (hConnect        AS PTR, ;
		lpszExistingFile AS PSZ, ;
		lpszNewFile     AS PSZ );
		AS LOGIC PASCAL:WININET.FtpRenameFileA
	
	
_DLL FUNC FtpOpenFile      (hConnect       AS PTR,  ;              //  In
	lpszFile       AS PSZ,  ;              // In, OPTIONAL
	dwAccess       AS DWORD, ;             // In
	dwFlags        AS DWORD, ;             // In
	dwContext      AS DWORD );             // In
	AS PTR PASCAL:WININET.FtpOpenFileA
	
_DLL FUNC FtpCreateDirectory(hConnect      AS PTR, ;
		lpszDir        AS PSZ );
		AS LOGIC PASCAL:WININET.FtpCreateDirectoryA
	
	
_DLL FUNC FtpRemoveDirectory(hConnect      AS PTR, ;
		lpszDir        AS PSZ );
		AS LOGIC PASCAL:WININET.FtpRemoveDirectoryA
	
	
_DLL FUNC FtpSetCurrentDirectory(hConnect  AS PTR, ;
		lpszDir        AS PSZ );
		AS LOGIC PASCAL:WININET.FtpSetCurrentDirectoryA
	
_DLL FUNC FtpGetCurrentDirectory(hConnect  AS PTR, ;
		lpszDir        AS PSZ ,;
		pSize          AS DWORD PTR );
		AS LOGIC PASCAL:WININET.FtpGetCurrentDirectoryA
	
_DLL FUNC InternetReadFile      (hFile     AS PTR, ;
		pBuffer   AS PTR, ;
		dwBytesToRead AS DWORD,;
		lpBytesRead AS DWORD PTR);
		AS LOGIC PASCAL:WININET.InternetReadFile
	
_DLL FUNC InternetWriteFile     (hFile     AS PTR, ;
		pBuffer   AS PTR, ;
		dwBytesToWrite AS DWORD,;
		lpBytesWritten AS DWORD PTR);
		AS LOGIC PASCAL:WININET.InternetWriteFile
	
	
_DLL FUNC InternetSetStatusCallback (hInternet  AS PTR, ;
		pf         AS PTR) ;
		AS PTR PASCAL:WININET.InternetSetStatusCallback
	
	
	
_DLL FUNC InternetOpenUrl(  hSession    AS PTR,;
		pUrl        AS PSZ,;
		pHeaders    AS PSZ,;
		dwHeadersLength AS DWORD,;
		dwFlags     AS DWORD,;
		dwContext   AS DWORD)   AS PTR PASCAL:WININET.InternetOpenUrlA
	
	//
	// the default major/minor HTTP version numbers
	//
	
_DLL FUNC HttpOpenRequest(  hConnect        AS PTR,;
		pszVerb         AS PSZ,;
		pszObjectName   AS PSZ,;
		pszVersion      AS PSZ,;
		pszReferrer     AS PSZ,;
		ppszAcceptTypes AS PSZ PTR,;
		dwFlags         AS DWORD,;
		dwContext       AS DWORD);
		AS PTR PASCAL:WININET.HttpOpenRequestA
	
_DLL FUNC HttpAddRequestHeaders(;
		hRequest        AS PTR,;
		pszHeaders      AS PSZ,;
		dwHeadersLength AS DWORD,;
		dwModifiers     AS DWORD);
		AS LOGIC PASCAL:WININET.HttpAddRequestHeadersA
	
	//
	// values for dwModifiers parameter of HttpAddRequestHeaders()
	//
	
_DLL FUNC HttpSendRequest(  hRequest        AS PTR,;
		pszHeaders      AS PSZ,;
		dwHeadersLength AS DWORD,;
		pOptional       AS PTR,;
		dwOptionalLength AS DWORD);
		AS LOGIC PASCAL:WININET.HttpSendRequestA
	
	//
	// flags for HttpSendRequestEx(), HttpEndRequest()
	//
	
_DLL FUNC HttpQueryInfo(    hRequest        AS PTR,;
		dwInfoLevel     AS DWORD,;
		pBuffer         AS PTR,;
		lpdwBufferLength AS DWORD PTR,;            
		lpdwIndex 		 AS DWORD PTR)	AS LOGIC PASCAL:WININET.HttpQueryInfoA 
	
	//
	// Internet API error returns
	//
	
_DLL FUNCTION InternetGetLastResponseInfo(pdw AS DWORD PTR, pBuffer AS PSZ, pdwSize AS DWORD PTR) AS LOGIC PASCAL:WININET.InternetGetLastResponseInfoA
_DLL FUNCTION FtpCommand(hConnect AS PTR, fExpectResponse AS LOGIC, dwFlags AS DWORD, pCommand AS PSZ, dwContext AS DWORD, phFtpCommand AS PTR) AS LOGIC PASCAL:WININET.FtpCommandA
	
_DLL FUNC InternetAttemptConnect(dwReserved AS DWORD) AS DWORD PASCAL:WININET.InternetAttemptConnect
_DLL FUNC InternetAutodial(dwFlags AS DWORD, hwndParent AS PTR) AS LOGIC PASCAL:WININET.InternetAutodial
_DLL FUNC InternetAutodialHangup(dwReserved AS DWORD) AS LOGIC PASCAL:WININET.InternetAutodialHangup
_DLL FUNC InternetCheckConnection(lpszUrl AS PSZ, dwFlags AS DWORD, dwReserved AS DWORD);
     AS LOGIC PASCAL:WININET.InternetCheckConnectionA
_DLL FUNC InternetDial(hwndParent AS PTR, lpszConnectoid AS PSZ, dwFlags AS DWORD, lpdwConnection AS PTR, dwReserved AS DWORD) AS DWORD PASCAL:WININET.InternetDial
_DLL FUNC InternetGetConnectedState(lpdwFlags AS PTR, dwReserved AS DWORD) AS LOGIC PASCAL:WININET.InternetGetConnectedState

_DLL FUNC InternetHangUp(dwConnection AS DWORD, dwReserved AS DWORD) AS DWORD PASCAL:WININET.InternetHangUp#247
_DLL FUNC InternetSetOption(HINTERNET AS PTR, dwOption AS DWORD, lpBuffer AS PTR, dwBufferLength AS DWORD) AS LOGIC PASCAL:WININET.InternetSetOptionA

/*
TEXTBLOCK E:\Program Files\CAVO26\SOURCE\win32\WININET.PRG
//
	// Internet APIs
	//
	
	//
	// manifests
	//
	

ENDTEXT
*/

FUNCTION INTERNET_MAX_URL_LENGTH    AS INT STRICT
	RETURN _INTERNET_MAX_URL_LENGTH
	
	//
	// values returned by InternetQueryOption() with INTERNET_OPTION_KEEP_CONNECTION:
	//
	
FUNCTION SECURITY_INTERNET_MASK     AS DWORD STRICT
	
	RETURN _SECURITY_INTERNET_MASK
	
	
FUNCTION SECURITY_SET_MASK          AS DWORD STRICT
	RETURN _SECURITY_INTERNET_MASK
	
FUNCTION INTERNET_FLAGS_MASK AS DWORD STRICT
	RETURN _INTERNET_FLAGS_MASK
	
FUNCTION InternetStatusCallback(    hInternet           AS PTR   , ;
		dwContext           AS DWORD ,;
		dwInternetStatus    AS DWORD,;
		pStatusInformation  AS PTR  ,;
		dwStatusInfoLength  AS DWORD )  AS VOID STRICT
	RETURN
	
	
	


#region defines
DEFINE INTERNET_INVALID_PORT_NUMBER     := 0           // use the protocol-specific default
DEFINE INTERNET_DEFAULT_FTP_PORT        := 21          // default for FTP servers
DEFINE INTERNET_DEFAULT_GOPHER_PORT     := 70          //    "     "  gopher "
DEFINE INTERNET_DEFAULT_HTTP_PORT       := 80          //    "     "  HTTP   "
DEFINE INTERNET_DEFAULT_HTTPS_PORT      := 443         //    "     "  HTTPS  "
DEFINE INTERNET_DEFAULT_SOCKS_PORT      := 1080        // default for SOCKS firewall servers.
DEFINE MAX_CACHE_ENTRY_INFO_SIZE        := 4096
	//
	// maximum field lengths (arbitrary)
	//
DEFINE INTERNET_MAX_HOST_NAME_LENGTH    := 256
DEFINE INTERNET_MAX_USER_NAME_LENGTH    := 128
DEFINE INTERNET_MAX_PASSWORD_LENGTH     := 128
DEFINE INTERNET_MAX_PORT_NUMBER_LENGTH  := 5           // INTERNET_PORT is unsigned short
DEFINE INTERNET_MAX_PORT_NUMBER_VALUE   := 65535       // maximum unsigned short value
DEFINE INTERNET_MAX_PATH_LENGTH         := 2048
DEFINE INTERNET_MAX_PROTOCOL_NAME       := "gopher"    // longest protocol name
DEFINE INTERNET_MAX_SCHEME_LENGTH       := 32          // longest protocol name length
DEFINE _INTERNET_MAX_URL_LENGTH 			 :=  INTERNET_MAX_SCHEME_LENGTH + 3 /*_SIZEOF("://") */ +INTERNET_MAX_PATH_LENGTH
DEFINE INTERNET_KEEP_ALIVE_UNKNOWN      := 0xFFFFFFFF
DEFINE INTERNET_KEEP_ALIVE_ENABLED      := 1
DEFINE INTERNET_KEEP_ALIVE_DISABLED     := 0
	//
	// flags returned by InternetQueryOption() with INTERNET_OPTION_REQUEST_FLAGS
	//
DEFINE INTERNET_REQFLAG_FROM_CACHE      := 0x00000001
DEFINE INTERNET_REQFLAG_ASYNC           := 0x00000002
DEFINE INTERNET_REQFLAG_VIA_PROXY      := 0x00000004  // request was made via a proxy
DEFINE INTERNET_REQFLAG_NO_HEADERS     := 0x00000008  // orginal response contained no headers
DEFINE INTERNET_REQFLAG_PASSIVE        := 0x00000010  // FTP: passive-mode connection
DEFINE INTERNET_REQFLAG_CACHE_WRITE_DISABLED := 0x00000040  // HTTPS: this request not cacheable
DEFINE INTERNET_REQFLAG_NET_TIMEOUT    := 0x00000080  // w/ _FROM_CACHE: net request timed out
	//
	// flags common to open functions (not InternetOpen()):
	//
DEFINE INTERNET_FLAG_RELOAD             := 0x80000000  // retrieve the original item
	//
	// flags for InternetOpenUrl():
	//
DEFINE INTERNET_FLAG_RAW_DATA           := 0x40000000  // receive the item as raw data
DEFINE INTERNET_FLAG_EXISTING_CONNECT   := 0x20000000  // do not create new connection object
	//
	// flags for InternetOpen():
	//
DEFINE INTERNET_FLAG_ASYNC              := 0x10000000  // this request is asynchronous (where supported)
	//
	// protocol-specific flags:
	//
DEFINE INTERNET_FLAG_PASSIVE            := 0x08000000  // used for FTP connections
	//
	// additional cache flags
	//
DEFINE INTERNET_FLAG_NO_CACHE_WRITE     := 0x04000000  // don't write this item to the cache
DEFINE INTERNET_FLAG_DONT_CACHE         := INTERNET_FLAG_NO_CACHE_WRITE
DEFINE INTERNET_FLAG_MAKE_PERSISTENT    := 0x02000000  // make this item persistent in cache
DEFINE INTERNET_FLAG_FROM_CACHE         := 0x01000000  // use offline semantics
DEFINE INTERNET_FLAG_OFFLINE            := INTERNET_FLAG_FROM_CACHE  // use offline semantics
	//
	// additional flags
	//
DEFINE INTERNET_FLAG_SECURE             := 0x00800000  // use PCT/SSL if applicable (HTTP)
DEFINE INTERNET_FLAG_KEEP_CONNECTION    := 0x00400000  // use keep-alive semantics
DEFINE INTERNET_FLAG_NO_AUTO_REDIRECT   := 0x00200000  // don't handle redirections automatically
DEFINE INTERNET_FLAG_READ_PREFETCH      := 0x00100000  // do background read prefetch
DEFINE INTERNET_FLAG_NO_COOKIES         := 0x00080000  // no automatic cookie handling
DEFINE INTERNET_FLAG_NO_AUTH            := 0x00040000  // no automatic authentication handling
DEFINE INTERNET_FLAG_RESTRICTED_ZONE    := 0x00020000  // apply restricted zone policies for cookies, auth
DEFINE INTERNET_FLAG_CACHE_IF_NET_FAIL  := 0x00010000  // return cache file if net request fails
	//
	// Security Ignore Flags, Allow HttpOpenRequest to overide
	//  Secure Channel (SSL/PCT) failures of the following types.
	//
DEFINE INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP    := 0x00008000 // ex: https:// to http://
DEFINE INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS   := 0x00004000 // ex: http:// to https://
DEFINE INTERNET_FLAG_IGNORE_CERT_DATE_INVALID   := 0x00002000 // expired X509 Cert.
DEFINE INTERNET_FLAG_IGNORE_CERT_CN_INVALID     := 0x00001000 // bad common name in X509 Cert.
	//more caching flags                                              
DEFINE INTERNET_FLAG_BGUPDATE := 0x00000008
DEFINE INTERNET_FLAG_MUST_CACHE_REQUEST         := 0x00000010 // fails if unable to cache request
DEFINE INTERNET_FLAG_RESYNCHRONIZE              := 0x00000800 // asking wininet to update an item if it is newer
DEFINE INTERNET_FLAG_HYPERLINK                  := 0x00000400 // asking wininet to
DEFINE INTERNET_FLAG_NO_UI                      := 0x00000200
DEFINE INTERNET_FLAG_PRAGMA_NOCACHE    := 0x00000100  // asking wininet to add "pragma: no-cache"
DEFINE INTERNET_FLAG_CACHE_ASYNC       := 0x00000080  // ok to perform lazy cache-write
DEFINE INTERNET_FLAG_FORMS_SUBMIT      := 0x00000040  // this is a forms submit
DEFINE INTERNET_FLAG_FWD_BACK          := 0x00000020  // fwd-back button op
DEFINE INTERNET_FLAG_NEED_FILE         := 0x00000010  // need a file for this request
	//
	// flags for FTP
	//
DEFINE INTERNET_FLAG_TRANSFER_ASCII     := FTP_TRANSFER_TYPE_ASCII
DEFINE INTERNET_FLAG_TRANSFER_BINARY    := FTP_TRANSFER_TYPE_BINARY
	//
	// flags field masks
	//
DEFINE _SECURITY_INTERNET_MASK  := INTERNET_FLAG_IGNORE_CERT_CN_INVALID    |  ;
		INTERNET_FLAG_IGNORE_CERT_DATE_INVALID  |  ;
		INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS  |  ;
		INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP   
DEFINE _INTERNET_FLAGS_MASK  	 := INTERNET_FLAG_RELOAD            ;
		| INTERNET_FLAG_RAW_DATA            ;
		| INTERNET_FLAG_EXISTING_CONNECT    ;
		| INTERNET_FLAG_ASYNC               ;
		| INTERNET_FLAG_PASSIVE             ;
		| INTERNET_FLAG_NO_CACHE_WRITE      ;
		| INTERNET_FLAG_MAKE_PERSISTENT     ;
		| INTERNET_FLAG_FROM_CACHE          ;
		| INTERNET_FLAG_SECURE              ;
		| INTERNET_FLAG_KEEP_CONNECTION     ;
		| INTERNET_FLAG_NO_AUTO_REDIRECT    ;
		| INTERNET_FLAG_READ_PREFETCH       ;
		| INTERNET_FLAG_NO_COOKIES          ;
		| INTERNET_FLAG_NO_AUTH             ;
		| INTERNET_FLAG_CACHE_IF_NET_FAIL   ;
		| _SECURITY_INTERNET_MASK           ;
		| INTERNET_FLAG_RESYNCHRONIZE       ;
		| INTERNET_FLAG_HYPERLINK           ;
		| INTERNET_FLAG_NO_UI               ;
		| INTERNET_FLAG_PRAGMA_NOCACHE      ;
		| INTERNET_FLAG_CACHE_ASYNC         ;
		| INTERNET_FLAG_FORMS_SUBMIT        ;
		| INTERNET_FLAG_NEED_FILE           ;
		| INTERNET_FLAG_RESTRICTED_ZONE     ;
		| INTERNET_FLAG_TRANSFER_BINARY     ;
		| INTERNET_FLAG_TRANSFER_ASCII      ;
		| INTERNET_FLAG_FWD_BACK            ;
		| INTERNET_FLAG_BGUPDATE            
DEFINE INTERNET_ERROR_MASK_INSERT_CDROM                    := 0x1
DEFINE INTERNET_ERROR_MASK_COMBINED_SEC_CERT               := 0x2
DEFINE INTERNET_ERROR_MASK_NEED_MSN_SSPI_PKG               := 0X4
DEFINE INTERNET_ERROR_MASK_LOGIN_FAILURE_DISPLAY_ENTITY_BODY := 0x8
DEFINE INTERNET_OPTIONS_MASK   := _NOT(_INTERNET_FLAGS_MASK)	
	//
	// common per-API flags (new APIs)
	//
DEFINE WININET_API_FLAG_ASYNC          := 0x00000001  // force async operation
DEFINE WININET_API_FLAG_SYNC           := 0x00000004  // force sync operation
DEFINE WININET_API_FLAG_USE_CONTEXT    := 0x00000008  // use value supplied in dwContext (even if 0)
	//
	// INTERNET_NO_CALLBACK - if this value is presented as the dwContext parameter
	// then no call-backs will be made for that API
	//
DEFINE INTERNET_NO_CALLBACK     := 0
	//
	// structures/types
	//
	//
	// INTERNET_SCHEME - enumerated URL scheme type
	//
DEFINE INTERNET_SCHEME_PARTIAL := -2
DEFINE INTERNET_SCHEME_UNKNOWN := -1
DEFINE INTERNET_SCHEME_DEFAULT := 0
DEFINE INTERNET_SCHEME_FTP     := 1
DEFINE INTERNET_SCHEME_GOPHER  := 2
DEFINE INTERNET_SCHEME_HTTP    := 3
DEFINE INTERNET_SCHEME_HTTPS   := 4
DEFINE INTERNET_SCHEME_FILE    := 5
DEFINE INTERNET_SCHEME_NEWS    := 6
DEFINE INTERNET_SCHEME_MAILTO  := 7
DEFINE INTERNET_SCHEME_SOCKS   := 8
DEFINE INTERNET_SCHEME_JAVASCRIPT := 9
DEFINE INTERNET_SCHEME_VBSCRIPT	:= 10
DEFINE INTERNET_SCHEME_RES		:= 11
DEFINE INTERNET_SCHEME_FIRST   := INTERNET_SCHEME_FTP
DEFINE INTERNET_SCHEME_LAST    := INTERNET_SCHEME_RES
	//
	// INTERNET_ASYNC_RESULT - this structure is returned to the application via
	// the callback with INTERNET_STATUS_REQUEST_COMPLETE. It is not sufficient to
	// just return the result of the async operation. If the API failed then the
	// app cannot call GetLastError() because the thread context will be incorrect.
	// Both the value returned by the async API and any resultant error code are
	// made available. The app need not check dwError if dwResult indicates that
	// the API succeeded (in this case dwError will be ERROR_SUCCESS)
	//
DEFINE INTERNET_PREFETCH_PROGRESS  := 0
DEFINE INTERNET_PREFETCH_COMPLETE  := 1
DEFINE INTERNET_PREFETCH_ABORTED   := 2
//
// INTERNET_DIAGNOSTIC_SOCKET_INFO.Flags definitions
//
DEFINE IDSI_FLAG_KEEP_ALIVE    := 0x00000001  // set if from keep-alive pool
DEFINE IDSI_FLAG_SECURE        := 0x00000002  // set if secure connection
DEFINE IDSI_FLAG_PROXY         := 0x00000004  // set if using proxy
DEFINE IDSI_FLAG_TUNNEL        := 0x00000008  // set if tunnelling through proxy
	//
	// INTERNET_PROXY_INFO - structure supplied with INTERNET_OPTION_PROXY to get/
	// set proxy information on a InternetOpen() handle
	//
DEFINE ISO_FORCE_DISCONNECTED := 0x01
	//
	// URL_COMPONENTS - the constituent parts of an URL. Used in InternetCrackUrl()
	// and InternetCreateUrl()
	//
	// For InternetCrackUrl(), if a pointer field and its corresponding length field
	// are both 0 then that component is not returned; If the pointer field is NULL
	// but the length field is not zero, then both the pointer and length fields are
	// returned; if both pointer and corresponding length fields are non-zero then
	// the pointer field points to a buffer where the component is copied. The
	// component may be un-escaped, depending on dwFlags
	//
	// For InternetCreateUrl(), the pointer fields should be NULL if the component
	// is not required. If the corresponding length field is zero then the pointer
	// field is the address of a zero-terminated string. If the length field is not
	// zero then it is the string length of the corresponding pointer field
	//
DEFINE INTERNET_OPEN_TYPE_PRECONFIG    := 0   // use registry configuration
DEFINE INTERNET_OPEN_TYPE_DIRECT       := 1   // direct to net
DEFINE INTERNET_OPEN_TYPE_PROXY        := 3   // via named proxy
DEFINE INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY  := 4   // prevent using java/script/INS
DEFINE PRE_CONFIG_INTERNET_ACCESS      := INTERNET_OPEN_TYPE_PRECONFIG
DEFINE LOCAL_INTERNET_ACCESS           := INTERNET_OPEN_TYPE_DIRECT
DEFINE GATEWAY_INTERNET_ACCESS         := 2   // Internet via gateway
DEFINE CERN_PROXY_INTERNET_ACCESS      := INTERNET_OPEN_TYPE_PROXY
	//
	// service types for InternetConnect()
	//
DEFINE INTERNET_SERVICE_FTP    := 1
DEFINE INTERNET_SERVICE_GOPHER := 2
DEFINE INTERNET_SERVICE_HTTP   := 3
	//
	// prototypes
	//
DEFINE INTERNET_OPTION_CALLBACK               := 1
DEFINE INTERNET_OPTION_CONNECT_TIMEOUT        := 2
DEFINE INTERNET_OPTION_CONNECT_RETRIES        := 3
DEFINE INTERNET_OPTION_CONNECT_BACKOFF        := 4
DEFINE INTERNET_OPTION_SEND_TIMEOUT           := 5
DEFINE INTERNET_OPTION_CONTROL_SEND_TIMEOUT   := INTERNET_OPTION_SEND_TIMEOUT
DEFINE INTERNET_OPTION_RECEIVE_TIMEOUT        := 6
DEFINE INTERNET_OPTION_CONTROL_RECEIVE_TIMEOUT:= INTERNET_OPTION_RECEIVE_TIMEOUT
DEFINE INTERNET_OPTION_DATA_SEND_TIMEOUT      := 7
DEFINE INTERNET_OPTION_DATA_RECEIVE_TIMEOUT   := 8
DEFINE INTERNET_OPTION_HANDLE_TYPE            := 9
DEFINE INTERNET_OPTION_LISTEN_TIMEOUT         := 11
DEFINE INTERNET_OPTION_READ_BUFFER_SIZE       := 12
DEFINE INTERNET_OPTION_WRITE_BUFFER_SIZE      := 13
DEFINE INTERNET_OPTION_ASYNC_ID               := 15
DEFINE INTERNET_OPTION_ASYNC_PRIORITY         := 16
DEFINE INTERNET_OPTION_PARENT_HANDLE          := 21
DEFINE INTERNET_OPTION_KEEP_CONNECTION        := 22
DEFINE INTERNET_OPTION_REQUEST_FLAGS          := 23
DEFINE INTERNET_OPTION_EXTENDED_ERROR         := 24
DEFINE INTERNET_OPTION_OFFLINE_MODE           :=  26
DEFINE INTERNET_OPTION_CACHE_STREAM_HANDLE    :=  27
DEFINE INTERNET_OPTION_USERNAME               :=  28
DEFINE INTERNET_OPTION_PASSWORD               :=  29
DEFINE INTERNET_OPTION_ASYNC                  :=  30
DEFINE INTERNET_OPTION_SECURITY_FLAGS         :=  31
DEFINE INTERNET_OPTION_SECURITY_CERTIFICATE_STRUCT:= 32
DEFINE INTERNET_OPTION_DATAFILE_NAME           := 33
DEFINE INTERNET_OPTION_URL                     := 34
DEFINE INTERNET_OPTION_SECURITY_CERTIFICATE    := 35
DEFINE INTERNET_OPTION_SECURITY_KEY_BITNESS    := 36
DEFINE INTERNET_OPTION_REFRESH                 := 37
DEFINE INTERNET_OPTION_PROXY                   := 38
DEFINE INTERNET_OPTION_SETTINGS_CHANGED        := 39
DEFINE INTERNET_OPTION_VERSION                 := 40
DEFINE INTERNET_OPTION_USER_AGENT              := 41
DEFINE INTERNET_OPTION_END_BROWSER_SESSION     := 42
DEFINE INTERNET_OPTION_PROXY_USERNAME          := 43
DEFINE INTERNET_OPTION_PROXY_PASSWORD          := 44
DEFINE INTERNET_OPTION_CONTEXT_VALUE           := 45
DEFINE INTERNET_OPTION_CONNECT_LIMIT           := 46
DEFINE INTERNET_OPTION_SECURITY_SELECT_CLIENT_CERT := 47
DEFINE INTERNET_OPTION_POLICY                  := 48
DEFINE INTERNET_OPTION_DISCONNECTED_TIMEOUT    := 49
DEFINE INTERNET_OPTION_CONNECTED_STATE 			:= 50
DEFINE INTERNET_OPTION_IDLE_STATE              :=51
DEFINE INTERNET_OPTION_OFFLINE_SEMANTICS       :=52
DEFINE INTERNET_OPTION_SECONDARY_CACHE_KEY     :=53
DEFINE INTERNET_OPTION_CALLBACK_FILTER         :=54
DEFINE INTERNET_OPTION_CONNECT_TIME            :=55
DEFINE INTERNET_OPTION_SEND_THROUGHPUT         :=56
DEFINE INTERNET_OPTION_RECEIVE_THROUGHPUT      :=57
DEFINE INTERNET_OPTION_REQUEST_PRIORITY        :=58
DEFINE INTERNET_OPTION_HTTP_VERSION            :=59
DEFINE INTERNET_OPTION_RESET_URLCACHE_SESSION  :=60
DEFINE INTERNET_OPTION_ERROR_MASK              :=62
DEFINE INTERNET_OPTION_FROM_CACHE_TIMEOUT      :=63
DEFINE INTERNET_OPTION_BYPASS_EDITED_ENTRY     :=64
DEFINE INTERNET_OPTION_DIAGNOSTIC_SOCKET_INFO  :=67
DEFINE INTERNET_OPTION_CODEPAGE                :=68
DEFINE INTERNET_OPTION_CACHE_TIMESTAMPS        :=69
DEFINE INTERNET_OPTION_DISABLE_AUTODIAL        :=70
DEFINE INTERNET_OPTION_MAX_CONNS_PER_SERVER    := 73
DEFINE INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER :=74
DEFINE INTERNET_OPTION_PER_CONNECTION_OPTION   :=75
DEFINE INTERNET_OPTION_DIGEST_AUTH_UNLOAD      := 76
DEFINE INTERNET_OPTION_IGNORE_OFFLINE          := 77
DEFINE INTERNET_OPTION_IDENTITY                := 78
DEFINE INTERNET_OPTION_REMOVE_IDENTITY         := 79
DEFINE INTERNET_OPTION_ALTER_IDENTITY          := 80
DEFINE INTERNET_OPTION_SUPPRESS_BEHAVIOR       := 81
DEFINE INTERNET_OPTION_AUTODIAL_MODE           := 82
DEFINE INTERNET_OPTION_AUTODIAL_CONNECTION     := 83
DEFINE INTERNET_OPTION_CLIENT_CERT_CONTEXT     := 84
DEFINE INTERNET_OPTION_AUTH_FLAGS              := 85
DEFINE INTERNET_OPTION_COOKIES_3RD_PARTY       := 86
DEFINE INTERNET_OPTION_DISABLE_PASSPORT_AUTH   := 87
DEFINE INTERNET_OPTION_SEND_UTF8_SERVERNAME_TO_PROXY         :=88
DEFINE INTERNET_OPTION_EXEMPT_CONNECTION_LIMIT  :=89
DEFINE INTERNET_OPTION_ENABLE_PASSPORT_AUTH     :=90
DEFINE INTERNET_OPTION_HIBERNATE_INACTIVE_WORKER_THREADS       :=91
DEFINE INTERNET_OPTION_ACTIVATE_WORKER_THREADS                 :=92
DEFINE INTERNET_OPTION_RESTORE_WORKER_THREAD_DEFAULTS          :=93
DEFINE INTERNET_OPTION_SOCKET_SEND_BUFFER_LENGTH               :=94
DEFINE INTERNET_OPTION_PROXY_SETTINGS_CHANGED                  :=95
DEFINE INTERNET_OPTION_DATAFILE_EXT                            :=96
DEFINE INTERNET_FIRST_OPTION                   := INTERNET_OPTION_CALLBACK
DEFINE INTERNET_LAST_OPTION                    := INTERNET_OPTION_DATAFILE_EXT
//
// values for INTERNET_OPTION_PRIORITY
//
DEFINE INTERNET_PRIORITY_FOREGROUND            := 1000
//
// handle types
//
DEFINE INTERNET_HANDLE_TYPE_INTERNET          := 1
DEFINE INTERNET_HANDLE_TYPE_CONNECT_FTP       := 2
DEFINE INTERNET_HANDLE_TYPE_CONNECT_GOPHER    := 3
DEFINE INTERNET_HANDLE_TYPE_CONNECT_HTTP      := 4
DEFINE INTERNET_HANDLE_TYPE_FTP_FIND          := 5
DEFINE INTERNET_HANDLE_TYPE_FTP_FIND_HTML     := 6
DEFINE INTERNET_HANDLE_TYPE_FTP_FILE          := 7
DEFINE INTERNET_HANDLE_TYPE_FTP_FILE_HTML     := 8
DEFINE INTERNET_HANDLE_TYPE_GOPHER_FIND       := 9
DEFINE INTERNET_HANDLE_TYPE_GOPHER_FIND_HTML  := 10
DEFINE INTERNET_HANDLE_TYPE_GOPHER_FILE       := 11
DEFINE INTERNET_HANDLE_TYPE_GOPHER_FILE_HTML  := 12
DEFINE INTERNET_HANDLE_TYPE_HTTP_REQUEST      := 13
DEFINE INTERNET_HANDLE_TYPE_FILE_REQUEST      := 14
//
// values for INTERNET_OPTION_AUTH_FLAGS
//
DEFINE AUTH_FLAG_DISABLE_NEGOTIATE            := 0x00000001
DEFINE AUTH_FLAG_ENABLE_NEGOTIATE             := 0x00000002
DEFINE AUTH_FLAG_DISABLE_BASIC_CLEARCHANNEL   := 0x00000004
//
// values for INTERNET_OPTION_SECURITY_FLAGS
//
// query only
DEFINE SECURITY_FLAG_SECURE                 :=   0x00000001 // can query only
DEFINE SECURITY_FLAG_STRENGTH_WEAK          :=   0x10000000
DEFINE SECURITY_FLAG_STRENGTH_MEDIUM        :=   0x40000000
DEFINE SECURITY_FLAG_STRENGTH_STRONG        :=   0x20000000
DEFINE SECURITY_FLAG_UNKNOWNBIT             :=   0x80000000
DEFINE SECURITY_FLAG_FORTEZZA               :=   0x08000000
DEFINE SECURITY_FLAG_NORMALBITNESS          :=   SECURITY_FLAG_STRENGTH_WEAK
// The following are unused
DEFINE SECURITY_FLAG_SSL                     :=  0x00000002
DEFINE SECURITY_FLAG_SSL3                    :=  0x00000004
DEFINE SECURITY_FLAG_PCT                     :=  0x00000008
DEFINE SECURITY_FLAG_PCT4                    :=  0x00000010
DEFINE SECURITY_FLAG_IETFSSL4                :=  0x00000020
// The following are for backwards compatability only.
DEFINE SECURITY_FLAG_40BIT                  :=  SECURITY_FLAG_STRENGTH_WEAK
DEFINE SECURITY_FLAG_128BIT                 :=  SECURITY_FLAG_STRENGTH_STRONG
DEFINE SECURITY_FLAG_56BIT                  :=  SECURITY_FLAG_STRENGTH_MEDIUM
// setable flags
DEFINE SECURITY_FLAG_IGNORE_REVOCATION       :=  0x00000080
DEFINE SECURITY_FLAG_IGNORE_UNKNOWN_CA       :=  0x00000100
DEFINE SECURITY_FLAG_IGNORE_WRONG_USAGE      :=  0x00000200
DEFINE SECURITY_FLAG_IGNORE_CERT_CN_INVALID    := INTERNET_FLAG_IGNORE_CERT_CN_INVALID
DEFINE SECURITY_FLAG_IGNORE_CERT_DATE_INVALID  := INTERNET_FLAG_IGNORE_CERT_DATE_INVALID
DEFINE SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS  := INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS
DEFINE SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP   := INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP
// valid autodial modes
DEFINE AUTODIAL_MODE_NEVER                     := 1
DEFINE AUTODIAL_MODE_ALWAYS                    := 2
DEFINE AUTODIAL_MODE_NO_NETWORK_PRESENT        := 4
DEFINE INTERNET_STATUS_RESOLVING_NAME           := 10
DEFINE INTERNET_STATUS_NAME_RESOLVED            := 11
DEFINE INTERNET_STATUS_CONNECTING_TO_SERVER     := 20
DEFINE INTERNET_STATUS_CONNECTED_TO_SERVER      := 21
DEFINE INTERNET_STATUS_SENDING_REQUEST          := 30
DEFINE INTERNET_STATUS_REQUEST_SENT             := 31
DEFINE INTERNET_STATUS_RECEIVING_RESPONSE       := 40
DEFINE INTERNET_STATUS_RESPONSE_RECEIVED        := 41
DEFINE INTERNET_STATUS_CTL_RESPONSE_RECEIVED    := 42
DEFINE INTERNET_STATUS_PREFETCH                 := 43
DEFINE INTERNET_STATUS_CLOSING_CONNECTION       := 50
DEFINE INTERNET_STATUS_CONNECTION_CLOSED        := 51
DEFINE INTERNET_STATUS_HANDLE_CREATED           := 60
DEFINE INTERNET_STATUS_HANDLE_CLOSING           := 70
DEFINE INTERNET_STATUS_DETECTING_PROXY         := 80
DEFINE INTERNET_STATUS_REQUEST_COMPLETE         := 100
DEFINE INTERNET_STATUS_REDIRECT                 := 110
DEFINE INTERNET_STATUS_INTERMEDIATE_RESPONSE   := 120
DEFINE INTERNET_STATUS_USER_INPUT_REQUIRED     := 140
DEFINE INTERNET_STATUS_STATE_CHANGE            := 200
DEFINE INTERNET_STATUS_COOKIE_SENT             := 320
DEFINE INTERNET_STATUS_COOKIE_RECEIVED         := 321
DEFINE INTERNET_STATUS_PRIVACY_IMPACTED        := 324
DEFINE INTERNET_STATUS_P3P_HEADER              := 325
DEFINE INTERNET_STATUS_P3P_POLICYREF           := 326
DEFINE INTERNET_STATUS_COOKIE_HISTORY          := 327
	//
	// the following can be indicated in a state change notification:
	//
DEFINE INTERNET_STATE_CONNECTED                := 0x00000001  // connected state (mutually exclusive with disconnected)
DEFINE INTERNET_STATE_DISCONNECTED             := 0x00000002  // disconnected from network
DEFINE INTERNET_STATE_DISCONNECTED_BY_USER     := 0x00000010  // disconnected by user request
DEFINE INTERNET_STATE_IDLE                     := 0x00000100  // no network requests being made (by Wininet)
DEFINE INTERNET_STATE_BUSY                     := 0x00000200  // network requests being made (by Wininet)
	//
	// the following values are used for cookie state:
	//
DEFINE COOKIE_STATE_UNKNOWN        := 0x0 
DEFINE COOKIE_STATE_ACCEPT         := 0x1
DEFINE COOKIE_STATE_PROMPT         := 0x2
DEFINE COOKIE_STATE_LEASH          := 0x3
DEFINE COOKIE_STATE_DOWNGRADE      := 0x4
DEFINE COOKIE_STATE_REJECT         := 0x5
DEFINE   COOKIE_STATE_MAX            := COOKIE_STATE_REJECT
DEFINE FTP_TRANSFER_TYPE_UNKNOWN    := 0x00000000
DEFINE FTP_TRANSFER_TYPE_ASCII      := 0x00000001
DEFINE FTP_TRANSFER_TYPE_BINARY     := 0x00000002
DEFINE _FTP_TRANSFER_TYPE_MASK      := (FTP_TRANSFER_TYPE_ASCII | FTP_TRANSFER_TYPE_BINARY)
DEFINE HTTP_MAJOR_VERSION      := 1
DEFINE HTTP_MINOR_VERSION      := 0
DEFINE HTTP_VERSION            := "HTTP/1.0"
	//
	// HttpQueryInfo info levels. Generally, there is one info level
	// for each potential RFC822/HTTP/MIME header that an HTTP server
	// may send as part of a request response.
	//
	// The HTTP_QUERY_RAW_HEADERS info level is provided for clients
	// that choose to perform their own header parsing.
	//
DEFINE HTTP_QUERY_MIME_VERSION                 := 0
DEFINE HTTP_QUERY_CONTENT_TYPE                 := 1
DEFINE HTTP_QUERY_CONTENT_TRANSFER_ENCODING    := 2
DEFINE HTTP_QUERY_CONTENT_ID                   := 3
DEFINE HTTP_QUERY_CONTENT_DESCRIPTION          := 4
DEFINE HTTP_QUERY_CONTENT_LENGTH               := 5
DEFINE HTTP_QUERY_CONTENT_LANGUAGE             := 6
DEFINE HTTP_QUERY_ALLOW                        := 7
DEFINE HTTP_QUERY_PUBLIC                       := 8
DEFINE HTTP_QUERY_DATE                         := 9
DEFINE HTTP_QUERY_EXPIRES                      := 10
DEFINE HTTP_QUERY_LAST_MODIFIED                := 11
DEFINE HTTP_QUERY_MESSAGE_ID                   := 12
DEFINE HTTP_QUERY_URI                          := 13
DEFINE HTTP_QUERY_DERIVED_FROM                 := 14
DEFINE HTTP_QUERY_COST                         := 15
DEFINE HTTP_QUERY_LINK                         := 16
DEFINE HTTP_QUERY_PRAGMA                       := 17
DEFINE HTTP_QUERY_VERSION                      := 18  // special: part of status line
DEFINE HTTP_QUERY_STATUS_CODE                  := 19  // special: part of status line
DEFINE HTTP_QUERY_STATUS_TEXT                  := 20  // special: part of status line
DEFINE HTTP_QUERY_RAW_HEADERS                  := 21  // special: all headers as ASCIIZ
DEFINE HTTP_QUERY_RAW_HEADERS_CRLF             := 22  // special: all headers
DEFINE HTTP_QUERY_CONNECTION                   := 23
DEFINE HTTP_QUERY_ACCEPT                       := 24
DEFINE HTTP_QUERY_ACCEPT_CHARSET               := 25
DEFINE HTTP_QUERY_ACCEPT_ENCODING              := 26
DEFINE HTTP_QUERY_ACCEPT_LANGUAGE              := 27
DEFINE HTTP_QUERY_AUTHORIZATION                := 28
DEFINE HTTP_QUERY_CONTENT_ENCODING             := 29
DEFINE HTTP_QUERY_FORWARDED                    := 30
DEFINE HTTP_QUERY_FROM                         := 31
DEFINE HTTP_QUERY_IF_MODIFIED_SINCE            := 32
DEFINE HTTP_QUERY_LOCATION                     := 33
DEFINE HTTP_QUERY_ORIG_URI                     := 34
DEFINE HTTP_QUERY_REFERER                      := 35
DEFINE HTTP_QUERY_RETRY_AFTER                  := 36
DEFINE HTTP_QUERY_SERVER                       := 37
DEFINE HTTP_QUERY_TITLE                        := 38
DEFINE HTTP_QUERY_USER_AGENT                   := 39
DEFINE HTTP_QUERY_WWW_AUTHENTICATE             := 40
DEFINE HTTP_QUERY_PROXY_AUTHENTICATE           := 41
DEFINE HTTP_QUERY_ACCEPT_RANGES                := 42
DEFINE HTTP_QUERY_SET_COOKIE                   := 43
DEFINE HTTP_QUERY_COOKIE                       := 44
DEFINE HTTP_QUERY_REQUEST_METHOD               := 45  // special: GET/POST etc.
DEFINE HTTP_QUERY_REFRESH                      :=46
DEFINE HTTP_QUERY_CONTENT_DISPOSITION          :=47
	//
	// HTTP 1.1 defined headers
	//
DEFINE HTTP_QUERY_AGE                         := 48
DEFINE HTTP_QUERY_CACHE_CONTROL               := 49
DEFINE HTTP_QUERY_CONTENT_BASE                := 50
DEFINE HTTP_QUERY_CONTENT_LOCATION            := 51
DEFINE HTTP_QUERY_CONTENT_MD5                 := 52
DEFINE HTTP_QUERY_CONTENT_RANGE               := 53
DEFINE HTTP_QUERY_HOST                        :=55
DEFINE HTTP_QUERY_IF_MATCH                    :=56
DEFINE HTTP_QUERY_IF_NONE_MATCH               :=57
DEFINE HTTP_QUERY_IF_RANGE                    :=58
DEFINE HTTP_QUERY_IF_UNMODIFIED_SINCE         :=59
DEFINE HTTP_QUERY_MAX_FORWARDS                :=60
DEFINE HTTP_QUERY_PROXY_AUTHORIZATION         :=61
DEFINE HTTP_QUERY_RANGE                       :=62
DEFINE HTTP_QUERY_TRANSFER_ENCODING           :=63
DEFINE HTTP_QUERY_UPGRADE                     :=64
DEFINE HTTP_QUERY_VARY                        :=65
DEFINE HTTP_QUERY_VIA                         :=66
DEFINE HTTP_QUERY_WARNING                     :=67
DEFINE HTTP_QUERY_EXPECT                      :=68
DEFINE HTTP_QUERY_PROXY_CONNECTION            :=69
DEFINE HTTP_QUERY_UNLESS_MODIFIED_SINCE       :=70
DEFINE HTTP_QUERY_ECHO_REQUEST                 :=71
DEFINE HTTP_QUERY_ECHO_REPLY                   :=72
	// These are the set of headers that should be added back to a request when
	// re-doing a request after a RETRY_WITH response.
DEFINE HTTP_QUERY_ECHO_HEADERS                :=73
DEFINE HTTP_QUERY_ECHO_HEADERS_CRLF           :=74
DEFINE HTTP_QUERY_PROXY_SUPPORT               :=75
DEFINE HTTP_QUERY_AUTHENTICATION_INFO         :=76
DEFINE HTTP_QUERY_PASSPORT_URLS               :=77
DEFINE HTTP_QUERY_PASSPORT_CONFIG             :=78
	//
	// HTTP_QUERY_CUSTOM - if this special value is supplied as the dwInfoLevel
	// parameter of HttpQueryInfo() then the lpBuffer parameter contains the name
	// of the header we are to query
	//
DEFINE HTTP_QUERY_CUSTOM                       := 65535
	//
	// HTTP_QUERY_FLAG_REQUEST_HEADERS - if this bit is set in the dwInfoLevel
	// parameter of HttpQueryInfo() then the request headers will be queried for the
	// request information
	//
DEFINE HTTP_QUERY_FLAG_REQUEST_HEADERS         := 0x80000000
	//
	// HTTP_QUERY_FLAG_SYSTEMTIME - if this bit is set in the dwInfoLevel parameter
	// of HttpQueryInfo() AND the header being queried contains date information,
	// e.g. the "Expires:" header then lpBuffer will contain a SYSTEMTIME structure
	// containing the date and time information converted from the header string
	//
DEFINE HTTP_QUERY_FLAG_SYSTEMTIME              := 0x40000000
	//
	// HTTP_QUERY_FLAG_NUMBER - if this bit is set in the dwInfoLevel parameter of
	// HttpQueryInfo(), then the value of the header will be converted to a number
	// before being returned to the caller, if applicable
	//
DEFINE HTTP_QUERY_FLAG_NUMBER                  :=  0x20000000
	//
	// HTTP_QUERY_FLAG_COALESCE - combine the values from several headers of the
	// same name into the output buffer
	//
DEFINE HTTP_QUERY_FLAG_COALESCE                := 0x10000000
DEFINE HTTP_QUERY_MODIFIER_FLAGS_MASK          :=(HTTP_QUERY_FLAG_REQUEST_HEADERS    ;
		| HTTP_QUERY_FLAG_SYSTEMTIME        ;
		| HTTP_QUERY_FLAG_NUMBER            ;
		| HTTP_QUERY_FLAG_COALESCE          ;
		)
	//DEFINE HTTP_QUERY_HEADER_MASK                  (~HTTP_QUERY_MODIFIER_FLAGS_MASK)
	//
	// HTTP Response Status Codes:
	//
DEFINE HTTP_STATUS_CONTINUE            :=100 // OK to continue with request
DEFINE HTTP_STATUS_SWITCH_PROTOCOLS    :=101 // server has switched protocols in upgrade header
DEFINE HTTP_STATUS_OK              := 200     // request completed
DEFINE HTTP_STATUS_CREATED         := 201     // object created, reason = new URI
DEFINE HTTP_STATUS_ACCEPTED        := 202     // async completion (TBS)
DEFINE HTTP_STATUS_PARTIAL         := 203     // partial completion
DEFINE HTTP_STATUS_NO_CONTENT      := 204     // no info to return
DEFINE HTTP_STATUS_RESET_CONTENT       :=205 // request completed, but clear form
DEFINE HTTP_STATUS_PARTIAL_CONTENT     :=206 // partial GET furfilled
DEFINE HTTP_STATUS_AMBIGUOUS       := 300     // server couldn't decide what to return
DEFINE HTTP_STATUS_MOVED           := 301     // object permanently moved
DEFINE HTTP_STATUS_REDIRECT        := 302     // object temporarily moved
DEFINE HTTP_STATUS_REDIRECT_METHOD := 303     // redirection w/ new access method
DEFINE HTTP_STATUS_NOT_MODIFIED    := 304     // if-modified-since was not modified
DEFINE HTTP_STATUS_USE_PROXY           :=305 // redirection to proxy, location header specifies proxy to use
DEFINE HTTP_STATUS_REDIRECT_KEEP_VERB  :=307 // HTTP/1.1: keep same verb
DEFINE HTTP_STATUS_BAD_REQUEST     := 400     // invalid syntax
DEFINE HTTP_STATUS_DENIED          := 401     // access denied
DEFINE HTTP_STATUS_PAYMENT_REQ     := 402     // payment required
DEFINE HTTP_STATUS_FORBIDDEN       := 403     // request forbidden
DEFINE HTTP_STATUS_NOT_FOUND       := 404     // object not found
DEFINE HTTP_STATUS_BAD_METHOD      := 405     // method is not allowed
DEFINE HTTP_STATUS_NONE_ACCEPTABLE := 406     // no response acceptable to client found
DEFINE HTTP_STATUS_PROXY_AUTH_REQ  := 407     // proxy authentication required
DEFINE HTTP_STATUS_REQUEST_TIMEOUT := 408     // server timed out waiting for request
DEFINE HTTP_STATUS_CONFLICT        := 409     // user should resubmit with more info
DEFINE HTTP_STATUS_GONE            := 410     // the resource is no longer available
DEFINE HTTP_STATUS_AUTH_REFUSED    := 411     // couldn't authorize client
DEFINE HTTP_STATUS_PRECOND_FAILED      :=412 // precondition given in request failed
DEFINE HTTP_STATUS_REQUEST_TOO_LARGE   :=413 // request entity was too large
DEFINE HTTP_STATUS_URI_TOO_LONG        :=414 // request URI too long
DEFINE HTTP_STATUS_UNSUPPORTED_MEDIA   :=415 // unsupported media type
DEFINE HTTP_STATUS_RETRY_WITH          :=449 // retry after doing the appropriate action.
DEFINE HTTP_STATUS_SERVER_ERROR    := 500     // internal server error
DEFINE HTTP_STATUS_NOT_SUPPORTED   := 501     // required not supported
DEFINE HTTP_STATUS_BAD_GATEWAY     := 502     // error response received from gateway
DEFINE HTTP_STATUS_SERVICE_UNAVAIL := 503     // temporarily overloaded
DEFINE HTTP_STATUS_GATEWAY_TIMEOUT := 504     // timed out waiting for gateway
DEFINE HTTP_STATUS_VERSION_NOT_SUP     :=505 // HTTP version not supported
DEFINE HTTP_STATUS_FIRST               :=HTTP_STATUS_CONTINUE
DEFINE HTTP_STATUS_LAST                :=HTTP_STATUS_VERSION_NOT_SUP
	//
	// prototypes
	//
DEFINE HTTP_ADDREQ_INDEX_MASK      := 0x0000FFFF
DEFINE HTTP_ADDREQ_FLAGS_MASK      := 0xFFFF0000
	//
	// HTTP_ADDREQ_FLAG_ADD_IF_NEW - the header will only be added if it doesn't
	// already exist
	//
DEFINE HTTP_ADDREQ_FLAG_ADD_IF_NEW := 0x10000000
	//
	// HTTP_ADDREQ_FLAG_ADD - if HTTP_ADDREQ_FLAG_REPLACE is set but the header is
	// not found then if this flag is set, the header is added anyway, so long as
	// there is a valid header-value
	//
DEFINE HTTP_ADDREQ_FLAG_ADD        := 0x20000000
	//
	// HTTP_ADDREQ_FLAG_COALESCE - coalesce headers with same name. e.g.
	// "Accept: text/*" and "Accept: audio/*" with this flag results in a single
	// header: "Accept: text/*, audio/*"
	//
DEFINE HTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA       := 0x40000000
DEFINE HTTP_ADDREQ_FLAG_COALESCE_WITH_SEMICOLON   := 0x01000000
DEFINE HTTP_ADDREQ_FLAG_COALESCE                  := HTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA
	//
	// HTTP_ADDREQ_FLAG_REPLACE - replaces the specified header. Only one header can
	// be supplied in the buffer. If the header to be replaced is not the first
	// in a list of headers with the same name, then the relative index should be
	// supplied in the low 8 bits of the dwModifiers parameter. If the header-value
	// part is missing, then the header is removed
	//
DEFINE HTTP_ADDREQ_FLAG_REPLACE    := 0x80000000
DEFINE HSR_ASYNC       := WININET_API_FLAG_ASYNC          // force async
DEFINE HSR_SYNC        := WININET_API_FLAG_SYNC           // force sync
DEFINE HSR_USE_CONTEXT := WININET_API_FLAG_USE_CONTEXT    // use dwContext value
DEFINE HSR_INITIATE    := 0x00000008                      // iterative operation (completed by HttpEndRequest)
DEFINE HSR_DOWNLOAD    := 0x00000010                      // download to file
DEFINE HSR_CHUNKED     := 0x00000020                      // operation is send of chunked data
DEFINE INTERNET_ERROR_BASE                      := 12000
DEFINE ERROR_INTERNET_OUT_OF_HANDLES            := (INTERNET_ERROR_BASE + 1)
DEFINE ERROR_INTERNET_TIMEOUT                   := (INTERNET_ERROR_BASE + 2)
DEFINE ERROR_INTERNET_EXTENDED_ERROR            := (INTERNET_ERROR_BASE + 3)
DEFINE ERROR_INTERNET_INTERNAL_ERROR            := (INTERNET_ERROR_BASE + 4)
DEFINE ERROR_INTERNET_INVALID_URL               := (INTERNET_ERROR_BASE + 5)
DEFINE ERROR_INTERNET_UNRECOGNIZED_SCHEME       := (INTERNET_ERROR_BASE + 6)
DEFINE ERROR_INTERNET_NAME_NOT_RESOLVED         := (INTERNET_ERROR_BASE + 7)
DEFINE ERROR_INTERNET_PROTOCOL_NOT_FOUND        := (INTERNET_ERROR_BASE + 8)
DEFINE ERROR_INTERNET_INVALID_OPTION            := (INTERNET_ERROR_BASE + 9)
DEFINE ERROR_INTERNET_BAD_OPTION_LENGTH         := (INTERNET_ERROR_BASE + 10)
DEFINE ERROR_INTERNET_OPTION_NOT_SETTABLE       := (INTERNET_ERROR_BASE + 11)
DEFINE ERROR_INTERNET_SHUTDOWN                  := (INTERNET_ERROR_BASE + 12)
DEFINE ERROR_INTERNET_INCORRECT_USER_NAME       := (INTERNET_ERROR_BASE + 13)
DEFINE ERROR_INTERNET_INCORRECT_PASSWORD        := (INTERNET_ERROR_BASE + 14)
DEFINE ERROR_INTERNET_LOGIN_FAILURE             := (INTERNET_ERROR_BASE + 15)
DEFINE ERROR_INTERNET_INVALID_OPERATION         := (INTERNET_ERROR_BASE + 16)
DEFINE ERROR_INTERNET_OPERATION_CANCELLED       := (INTERNET_ERROR_BASE + 17)
DEFINE ERROR_INTERNET_INCORRECT_HANDLE_TYPE     := (INTERNET_ERROR_BASE + 18)
DEFINE ERROR_INTERNET_INCORRECT_HANDLE_STATE    := (INTERNET_ERROR_BASE + 19)
DEFINE ERROR_INTERNET_NOT_PROXY_REQUEST         := (INTERNET_ERROR_BASE + 20)
DEFINE ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND  := (INTERNET_ERROR_BASE + 21)
DEFINE ERROR_INTERNET_BAD_REGISTRY_PARAMETER    := (INTERNET_ERROR_BASE + 22)
DEFINE ERROR_INTERNET_NO_DIRECT_ACCESS          := (INTERNET_ERROR_BASE + 23)
DEFINE ERROR_INTERNET_NO_CONTEXT                := (INTERNET_ERROR_BASE + 24)
DEFINE ERROR_INTERNET_NO_CALLBACK               := (INTERNET_ERROR_BASE + 25)
DEFINE ERROR_INTERNET_REQUEST_PENDING           := (INTERNET_ERROR_BASE + 26)
DEFINE ERROR_INTERNET_INCORRECT_FORMAT          := (INTERNET_ERROR_BASE + 27)
DEFINE ERROR_INTERNET_ITEM_NOT_FOUND            := (INTERNET_ERROR_BASE + 28)
DEFINE ERROR_INTERNET_CANNOT_CONNECT            := (INTERNET_ERROR_BASE + 29)
DEFINE ERROR_INTERNET_CONNECTION_ABORTED        := (INTERNET_ERROR_BASE + 30)
DEFINE ERROR_INTERNET_CONNECTION_RESET          := (INTERNET_ERROR_BASE + 31)
DEFINE ERROR_INTERNET_FORCE_RETRY               := (INTERNET_ERROR_BASE + 32)
DEFINE ERROR_INTERNET_INVALID_PROXY_REQUEST     := (INTERNET_ERROR_BASE + 33)
DEFINE ERROR_INTERNET_NEED_UI                   := (INTERNET_ERROR_BASE + 34)
DEFINE ERROR_INTERNET_HANDLE_EXISTS             := (INTERNET_ERROR_BASE + 36)
DEFINE ERROR_INTERNET_SEC_CERT_DATE_INVALID     := (INTERNET_ERROR_BASE + 37)
DEFINE ERROR_INTERNET_SEC_CERT_CN_INVALID       := (INTERNET_ERROR_BASE + 38)
DEFINE ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR    := (INTERNET_ERROR_BASE + 39)
DEFINE ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR    := (INTERNET_ERROR_BASE + 40)
DEFINE ERROR_INTERNET_MIXED_SECURITY            := (INTERNET_ERROR_BASE + 41)
DEFINE ERROR_INTERNET_CHG_POST_IS_NON_SECURE    := (INTERNET_ERROR_BASE + 42)
DEFINE ERROR_INTERNET_POST_IS_NON_SECURE        := (INTERNET_ERROR_BASE + 43)
DEFINE ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED   := (INTERNET_ERROR_BASE + 44)
DEFINE ERROR_INTERNET_INVALID_CA                := (INTERNET_ERROR_BASE + 45)
DEFINE ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP     := (INTERNET_ERROR_BASE + 46)
DEFINE ERROR_INTERNET_ASYNC_THREAD_FAILED       := (INTERNET_ERROR_BASE + 47)
DEFINE ERROR_INTERNET_REDIRECT_SCHEME_CHANGE    := (INTERNET_ERROR_BASE + 48)
DEFINE ERROR_INTERNET_DIALOG_PENDING            :=(INTERNET_ERROR_BASE + 49)
DEFINE ERROR_INTERNET_RETRY_DIALOG            :=( INTERNET_ERROR_BASE + 50)
DEFINE ERROR_INTERNET_HTTPS_HTTP_SUBMIT_REDIR := (INTERNET_ERROR_BASE + 52)
DEFINE ERROR_INTERNET_INSERT_CDROM            := (INTERNET_ERROR_BASE + 53)
DEFINE ERROR_INTERNET_FORTEZZA_LOGIN_NEEDED   := (INTERNET_ERROR_BASE + 54)
DEFINE ERROR_INTERNET_SEC_CERT_ERRORS         := (INTERNET_ERROR_BASE + 55)
DEFINE ERROR_INTERNET_SEC_CERT_NO_REV         := (INTERNET_ERROR_BASE + 56)
DEFINE ERROR_INTERNET_SEC_CERT_REV_FAILED     := (INTERNET_ERROR_BASE + 57)
//
// FTP API errors
//
DEFINE ERROR_FTP_TRANSFER_IN_PROGRESS         := (INTERNET_ERROR_BASE + 110)
DEFINE ERROR_FTP_DROPPED                      := (INTERNET_ERROR_BASE + 111)
DEFINE ERROR_FTP_NO_PASSIVE_MODE              := (INTERNET_ERROR_BASE + 112)
//
// gopher API errors
//
DEFINE ERROR_GOPHER_PROTOCOL_ERROR            := (INTERNET_ERROR_BASE + 130)
DEFINE ERROR_GOPHER_NOT_FILE                  := (INTERNET_ERROR_BASE + 131)
DEFINE ERROR_GOPHER_DATA_ERROR                := (INTERNET_ERROR_BASE + 132)
DEFINE ERROR_GOPHER_END_OF_DATA               := (INTERNET_ERROR_BASE + 133)
DEFINE ERROR_GOPHER_INVALID_LOCATOR           := (INTERNET_ERROR_BASE + 134)
DEFINE ERROR_GOPHER_INCORRECT_LOCATOR_TYPE    := (INTERNET_ERROR_BASE + 135)
DEFINE ERROR_GOPHER_NOT_GOPHER_PLUS           := (INTERNET_ERROR_BASE + 136)
DEFINE ERROR_GOPHER_ATTRIBUTE_NOT_FOUND       := (INTERNET_ERROR_BASE + 137)
DEFINE ERROR_GOPHER_UNKNOWN_LOCATOR           := (INTERNET_ERROR_BASE + 138)
//
// HTTP API errors
//
DEFINE ERROR_HTTP_HEADER_NOT_FOUND            :=(INTERNET_ERROR_BASE + 150)
DEFINE ERROR_HTTP_DOWNLEVEL_SERVER            :=(INTERNET_ERROR_BASE + 151)
DEFINE ERROR_HTTP_INVALID_SERVER_RESPONSE     :=(INTERNET_ERROR_BASE + 152)
DEFINE ERROR_HTTP_INVALID_HEADER              :=(INTERNET_ERROR_BASE + 153)
DEFINE ERROR_HTTP_INVALID_QUERY_REQUEST       :=(INTERNET_ERROR_BASE + 154)
DEFINE ERROR_HTTP_HEADER_ALREADY_EXISTS       :=(INTERNET_ERROR_BASE + 155)
DEFINE ERROR_HTTP_REDIRECT_FAILED             :=(INTERNET_ERROR_BASE + 156)
DEFINE ERROR_HTTP_NOT_REDIRECTED              :=(INTERNET_ERROR_BASE + 160)
DEFINE ERROR_HTTP_COOKIE_NEEDS_CONFIRMATION   :=(INTERNET_ERROR_BASE + 161)
DEFINE ERROR_HTTP_COOKIE_DECLINED             :=(INTERNET_ERROR_BASE + 162)
DEFINE ERROR_HTTP_REDIRECT_NEEDS_CONFIRMATION :=(INTERNET_ERROR_BASE + 168)
//
// additional Internet API error codes
//
DEFINE ERROR_INTERNET_SECURITY_CHANNEL_ERROR    := (INTERNET_ERROR_BASE + 157) // BUGBUG
DEFINE ERROR_INTERNET_UNABLE_TO_CACHE_FILE      := (INTERNET_ERROR_BASE + 158) // BUGBUG
DEFINE ERROR_INTERNET_TCPIP_NOT_INSTALLED       := (INTERNET_ERROR_BASE + 159) // BUGBUG
DEFINE ERROR_INTERNET_DISCONNECTED             := (INTERNET_ERROR_BASE + 163)
DEFINE ERROR_INTERNET_SERVER_UNREACHABLE       := (INTERNET_ERROR_BASE + 164)
DEFINE ERROR_INTERNET_PROXY_SERVER_UNREACHABLE := (INTERNET_ERROR_BASE + 165)
DEFINE ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT    := (INTERNET_ERROR_BASE + 166)
DEFINE ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT := (INTERNET_ERROR_BASE + 167)
DEFINE ERROR_INTERNET_SEC_INVALID_CERT         := (INTERNET_ERROR_BASE + 169)
DEFINE ERROR_INTERNET_SEC_CERT_REVOKED         := (INTERNET_ERROR_BASE + 170)
	// InternetAutodial specific errors
DEFINE ERROR_INTERNET_FAILED_DUETOSECURITYCHECK  :=(INTERNET_ERROR_BASE + 171)
DEFINE ERROR_INTERNET_NOT_INITIALIZED          :=(INTERNET_ERROR_BASE + 172)
DEFINE ERROR_INTERNET_NEED_MSN_SSPI_PKG          :=(INTERNET_ERROR_BASE + 173)
DEFINE ERROR_INTERNET_LOGIN_FAILURE_DISPLAY_ENTITY_BODY   :=(INTERNET_ERROR_BASE + 174)
DEFINE INTERNET_ERROR_LAST                      := ERROR_INTERNET_LOGIN_FAILURE_DISPLAY_ENTITY_BODY
DEFINE FLAG_ICC_FORCE_CONNECTION := 0x00000001
DEFINE INTERNET_AUTODIAL_FORCE_ONLINE        	:= 1
DEFINE INTERNET_AUTODIAL_FORCE_UNATTENDED    	:= 2
DEFINE INTERNET_AUTODIAL_FAILIFSECURITYCHECK 	:= 4
DEFINE INTERNET_AUTODIAL_OVERRIDE_NET_PRESENT 	:= 8
DEFINE INTERNET_CONNECTION_MODEM      :=   0x01
DEFINE INTERNET_CONNECTION_LAN        :=   0x02
DEFINE INTERNET_CONNECTION_PROXY      :=   0x04
DEFINE INTERNET_CONNECTION_MODEM_BUSY     :=  0x08  /* no longer used */
DEFINE INTERNET_RAS_INSTALLED              := 0x10
DEFINE INTERNET_CONNECTION_OFFLINE    :=   0x20
DEFINE INTERNET_CONNECTION_CONFIGURED :=   0x40
// Flags for custom dial handler
DEFINE INTERNET_CUSTOMDIAL_CONNECT         := 0
DEFINE INTERNET_CUSTOMDIAL_UNATTENDED      := 1
DEFINE INTERNET_CUSTOMDIAL_DISCONNECT      := 2
DEFINE INTERNET_CUSTOMDIAL_SHOWOFFLINE     := 4
// Custom dial handler supported functionality flags
DEFINE INTERNET_CUSTOMDIAL_SAFE_FOR_UNATTENDED := 1
DEFINE INTERNET_CUSTOMDIAL_WILL_SUPPLY_STATE   := 2
DEFINE INTERNET_CUSTOMDIAL_CAN_HANGUP          := 4
DEFINE INTERNET_IDENTITY_FLAG_PRIVATE_CACHE      := 0x01
DEFINE INTERNET_IDENTITY_FLAG_SHARED_CACHE       := 0x02
DEFINE INTERNET_IDENTITY_FLAG_CLEAR_DATA         := 0x04
DEFINE INTERNET_IDENTITY_FLAG_CLEAR_COOKIES      := 0x08
DEFINE INTERNET_IDENTITY_FLAG_CLEAR_HISTORY      := 0x10
DEFINE INTERNET_IDENTITY_FLAG_CLEAR_CONTENT      := 0x20
DEFINE INTERNET_SUPPRESS_RESET_ALL               := 0x00
DEFINE INTERNET_SUPPRESS_COOKIE_POLICY           := 0x01
DEFINE INTERNET_SUPPRESS_COOKIE_POLICY_RESET     := 0x02
//
// Privacy settings values and APIs
//
DEFINE PRIVACY_TEMPLATE_NO_COOKIES    :=  0
DEFINE PRIVACY_TEMPLATE_HIGH           := 1
DEFINE PRIVACY_TEMPLATE_MEDIUM_HIGH    := 2
DEFINE PRIVACY_TEMPLATE_MEDIUM         := 3
DEFINE PRIVACY_TEMPLATE_MEDIUM_LOW     := 4
DEFINE PRIVACY_TEMPLATE_LOW            := 5
DEFINE PRIVACY_TEMPLATE_CUSTOM         := 100
DEFINE PRIVACY_TEMPLATE_ADVANCED       := 101
DEFINE PRIVACY_TEMPLATE_MAX            := PRIVACY_TEMPLATE_LOW
DEFINE PRIVACY_TYPE_FIRST_PARTY        := 0
DEFINE PRIVACY_TYPE_THIRD_PARTY        := 1
#endregion
