VOSTRUCT _winfd_set
	MEMBER   fd_count AS DWORD
	MEMBER  DIM fd_array[FD_SETSIZE] AS DWORD


VOSTRUCT _wintimeval
	MEMBER      tv_sec AS LONGINT
	MEMBER   tv_usec AS LONGINT






/*
 * Commands for ioctlsocket(),  taken from the BSD file fcntl.h.
 *
 *
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 128 bytes.
 */
VOSTRUCT  _winhostent
	MEMBER  h_name AS PTR
	MEMBER  h_aliases AS PTR
	MEMBER  h_addrtype AS SHORTINT
	MEMBER  h_length AS SHORTINT
	MEMBER  h_addr_list AS PTR





VOSTRUCT  _winnetent
	MEMBER  n_name AS PTR
	MEMBER  n_aliases AS PTR
	MEMBER  n_addrtype AS SHORTINT
	MEMBER  n_net AS DWORD

VOSTRUCT  _winservent
	MEMBER  s_name AS PTR
	MEMBER  s_aliases AS PTR
	MEMBER  s_port AS SHORTINT
	MEMBER  s_proto AS PTR

VOSTRUCT  _winprotoent
	MEMBER  p_name AS PTR
	MEMBER  p_aliases AS PTR
	MEMBER  p_proto AS SHORTINT




/*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981, taken from the BSD file netinet/in.h.
 * IPv6 additions per RFC 2292.
 */

/*
 * Protocols
 */

VOSTRUCT __S_un_b_win
	MEMBER s_b1 AS BYTE
	MEMBER s_b2 AS BYTE
	MEMBER s_b3 AS BYTE
	MEMBER s_b4 AS BYTE

VOSTRUCT __S_un_w_win
	MEMBER  s_w1 AS WORD
	MEMBER  s_w2 AS WORD

VOSTRUCT _winin_addr
	MEMBER s_un IS __S_un_win






/*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 */


VOSTRUCT _WINsockaddr_in
	MEMBER   sin_family AS SHORTINT
	MEMBER   sin_port AS WORD
	MEMBER   sin_addr IS _winin_addr
	MEMBER   DIM sin_zero[8] AS BYTE

VOSTRUCT _winWSAData
	MEMBER  wVersion AS WORD
	MEMBER  wHighVersion AS WORD
	MEMBER  DIM szDescription[WSADESCRIPTION_LEN+1] AS BYTE
	MEMBER  DIM szSystemStatus[WSASYS_STATUS_LEN+1] AS BYTE
	MEMBER  iMaxSockets AS WORD
	MEMBER  iMaxUdpDg AS WORD
	MEMBER  lpVendorInfo AS PTR




VOSTRUCT _winip_mreq
	MEMBER  imr_multiaddr IS _winin_addr
	MEMBER   imr_interface IS _winin_addr


/*
 * Definitions related to sockets: types, address families, options,
 * taken from the BSD file sys/socket.h.
 */

/*
 * This is used instead of -1, since the
 * SOCKET type is unsigned.
 */
VOSTRUCT _WINsockaddr
	MEMBER  sa_family AS WORD				//RvdH 070412 changed from DWORD to WORD
	MEMBER  DIM sa_data[14] AS BYTE
VOSTRUCT _winsockproto
	MEMBER sp_family AS WORD
	MEMBER sp_protocol AS WORD


/*
 * Protocol families, same as address families for now.
 */
VOSTRUCT  _winlinger
	MEMBER  l_onoff AS WORD
	MEMBER  l_linger AS WORD

/*
 * Level number for (get/set)sockopt() to apply to socket itself.
 */
VOSTRUCT _winTRANSMIT_FILE_BUFFERS
	MEMBER  Head AS PTR
	MEMBER HeadLength AS DWORD
	MEMBER Tail AS PTR
	MEMBER TailLength AS DWORD



UNION __S_un_win
	MEMBER S_un_b IS __s_un_b_win
	MEMBER S_un_w IS __S_un_w_win
	MEMBER S_addr AS DWORD





FUNCTION IN_CLASSA(i AS DWORD) AS DWORD

	LOCAL val1 AS DWORD
	val1 := _AND(DWORD(_CAST, i) , 0x80000000)
	IF val1 = 0x00000000
		RETURN 1
	ENDIF
	RETURN 0

FUNCTION IN_CLASSB(i) AS DWORD

	LOCAL val1  AS DWORD

	VAL1 := _AND(DWORD(_CAST, i) , 0xC0000000)
	IF val1 = 0x80000000
		RETURN 1
	ENDIF
	RETURN 0

_DLL FUNCTION accept(s AS DWORD, _Addr AS _WINsockaddr, addrlen AS INT PTR);
	AS DWORD PASCAL:WSOCK32.accept
/*
 * WinSock 2 extension -- manifest constants for return values of the condition function
 */
_DLL FUNCTION bind(s AS DWORD,  _Addr AS _WINsockaddr, namelen AS INT);
	AS INT PASCAL:WSOCK32.bind
	
#ifdef __VULCAN__
   // These functions can also take a sockaddr_in structure
   // VO doesn't type check pointers but Vulcan needs an overload to prevent compile-time errors.
   // We could also use casts at the call sites, but adding these overloads allows the existing code
   // in the Internet SDK to compile as-is.
   _DLL FUNCTION bind( s AS DWORD,  _Addr AS _WINsockaddr_in, namelen AS INT ) AS INT PASCAL:WSOCK32.bind
   _DLL FUNCTION getpeername( s AS DWORD, name AS _winsockaddr_in , namelen AS INT PTR ) AS INT PASCAL:WSOCK32.getpeername
   _DLL FUNCTION getsockname( s AS DWORD, name AS _winsockaddr_in, namelen AS INT PTR ) AS INT PASCAL:WSOCK32.getsockname
   _DLL FUNCTION recvfrom( s AS DWORD, buf AS PSZ, len AS INT, flags AS INT, FROM AS _winsockaddr_in, fromlen AS INT PTR ) AS INT PASCAL:WSOCK32.recvfrom
   _DLL FUNCTION sendto( s AS DWORD, buf AS PSZ, len AS INT, flags AS INT, _to AS _winsockaddr_in, tolen AS INT ) AS INT PASCAL:WSOCK32.sendto

   // get/setsockopt treats optval as a pointer to an int when certain options are being retrieved.  We also need
   // overloads for this so existing code will compile.
   _DLL FUNCTION getsockopt( s AS DWORD, level AS INT, optname AS INT, optval AS INT PTR, optlen AS PTR ) AS INT PASCAL:WSOCK32.getsockopt
   _DLL FUNCTION setsockopt( s AS DWORD, level AS INT, optname AS INT, optval AS INT PTR, optlen AS INT ) AS INT PASCAL:WSOCK32.setsockopt
   _DLL FUNCTION setsockopt( s AS DWORD, level AS INT, optname AS INT, optval AS _WINLINGER PTR, optlen AS INT ) AS INT PASCAL:WSOCK32.setsockopt
  
#endif	


_DLL FUNCTION closesocket(s AS DWORD) AS INT PASCAL:WSOCK32.closesocket


_DLL FUNCTION connect(s AS DWORD, name AS _winsockaddr, namelen AS INT);
	AS INT PASCAL:WSOCK32.connect


_DLL FUNCTION ioctlsocket(s AS DWORD, cmd AS LONG, argp AS DWORD PTR);
	AS INT PASCAL:WSOCK32.ioctlsocket


_DLL FUNCTION getpeername(s AS DWORD, name AS _winsockaddr , namelen AS INT PTR) AS INT PASCAL:WSOCK32.getpeername

_DLL FUNCTION getsockname( s AS DWORD, name AS _winsockaddr, namelen AS INT PTR) AS INT PASCAL:WSOCK32.getsockname


_DLL FUNCTION getsockopt(s AS DWORD, level AS INT, optname AS INT, optval AS PSZ,;
	optlen AS PTR) AS INT PASCAL:WSOCK32.getsockopt


_DLL FUNCTION htonl (hostlong AS DWORD) AS DWORD PASCAL:WSOCK32.htonl


_DLL FUNCTION htons (hostshort AS WORD) AS WORD PASCAL:WSOCK32.htons


_DLL FUNCTION inet_addr(cp AS PSZ) AS DWORD PASCAL:WSOCK32.inet_addr


_DLL FUNCTION inet_ntoa(_in IS _winin_addr) AS PSZ PASCAL:WSOCK32.inet_ntoa


_DLL FUNCTION listen( s AS DWORD, backlog AS INT) AS INT PASCAL:WSOCK32.listen


_DLL FUNCTION ntohl( netlong AS DWORD) AS DWORD PASCAL:WSOCK32.ntohl


_DLL FUNCTION ntohs(netshort AS WORD) AS WORD PASCAL:WSOCK32.ntohs


_DLL FUNCTION recv(s AS DWORD, buf AS PSZ, len AS INT, flags AS INT);
	AS INT PASCAL:WSOCK32.recv



_DLL FUNCTION recvfrom(s AS DWORD, buf AS PSZ, len AS INT, flags AS INT,;
	FROM AS _winsockaddr, fromlen AS INT PTR) AS INT PASCAL:WSOCK32.recvfrom


_DLL FUNCTION _WinSockSelect(nfds AS INT, readfds AS _winfd_set, writefds AS _winfd_set,;
	exceptfds AS _winfd_set, timeout AS _wintimeval);
	AS INT PASCAL:WSOCK32.select


_DLL FUNCTION WSockSend(s AS DWORD, buf AS PSZ, len AS INT, flags AS INT) AS INT PASCAL:WSOCK32.send


_DLL FUNCTION sendto(s AS DWORD, buf AS PSZ, len AS INT, flags AS INT,;
	_to AS _winsockaddr, tolen AS INT) AS INT PASCAL:WSOCK32.sendto



_DLL FUNCTION setsockopt(s AS DWORD, level AS INT, optname AS INT, optval AS PSZ,;
	optlen AS INT) AS INT PASCAL:WSOCK32.setsockopt

_DLL FUNCTION shutdown(dwSocket AS DWORD,liHow AS INT) AS INT PASCAL:WSOCK32.shutdown
 
 



_DLL FUNCTION socket(af AS INT, type AS INT, protocal AS INT) AS DWORD PASCAL:WSOCK32.socket




_DLL FUNCTION gethostbyaddr( _addr AS PSZ, len AS INT, type AS INT);
	AS _winhostent PASCAL:WSOCK32.gethostbyaddr


_DLL FUNCTION gethostbyname( name AS PSZ) AS _winhostent PASCAL:WSOCK32.gethostbyname


_DLL FUNCTION gethostname(neme AS PSZ, namelen AS INT) AS INT PASCAL:WSOCK32.gethostname


_DLL FUNCTION getservbyport(port AS INT, proto AS PSZ) AS _winservent PASCAL:wsock32.getservbyport


_DLL FUNCTION getservbyname(name AS PSZ, porto AS PSZ) AS _winservent PASCAL:WSOCK32.getservbyname


_DLL FUNCTION getprotobynumber(ptoto AS INT) AS _winprotoent PASCAL:WSOCK32.getprotobynumber


_DLL FUNCTION getprotobyname( name AS PSZ) AS _winprotoent PASCAL:WSOCK32.getprotobyname




_DLL FUNCTION WSAStartup(wVersionRequired AS WORD, lpWSAData AS _winWSADATA) AS INT PASCAL:WSOCK32.WSAStartup


_DLL FUNCTION WSACleanup() AS INT PASCAL:WSOCK32.WSACleanup


_DLL FUNCTION WSASetLastError(iError AS INT) AS VOID PASCAL:WSOCK32.WSASetLastError


_DLL FUNCTION WSAGetLastError() AS INT PASCAL:WSOCK32.WSAGetLastError


_DLL FUNCTION WSAIsBlocking() AS LOGIC PASCAL:WSOCK32.WSAIsBlocking


_DLL FUNCTION WSAUnhookBlockingHook() AS INT PASCAL:WSOCK32.WSAUnhookBlockingHook


_DLL FUNCTION WSASetBlockingHook(lpBlockFunc AS PTR) AS PTR PASCAL:WSOCK32.WSASetBlockingHook


_DLL FUNCTION WSACancelBlockingCall() AS INT PASCAL:WSOCK32.WSACancelBlockingCall


_DLL FUNCTION WSAAsyncGetServByName(hWnd AS PTR, wMsg AS DWORD, name AS PSZ,;
	proto AS PSZ, buf AS PSZ, buflen AS INT);
	AS PTR PASCAL:WSOCK32.WSAAsyncGetServByName


_DLL FUNCTION WSAAsyncGetServByPort(hWnd AS PTR, wMsg AS DWORD, port AS INT,;
	proto AS PSZ, buf AS PSZ, buflen AS INT);
	AS PTR PASCAL:WSOCK32.WSAAsyncGetServByPort


_DLL FUNCTION WSAAsyncGetProtoByName(hWnd AS PTR, wMsg AS DWORD, name AS PSZ, buf AS PSZ,;
	buflen AS INT) AS PTR PASCAL:WSOCK32.WSAAsyncGetProtoByName


_DLL FUNCTION WSAAsyncGetProtoByNumber(hWnd AS PTR, wMsg AS DWORD, number AS INT,;
	buf AS PSZ, buflen AS INT) AS PTR PASCAL:WSOCK32.WSAAsyncGetProtoByNumber


_DLL FUNCTION WSAAsyncGetHostByName(hWnd AS PTR, wMsg AS DWORD, name AS PSZ, buf AS PSZ,;
	buflen AS INT) AS PTR  PASCAL:WSOCK32.WSAAsyncGetHostByName


_DLL FUNCTION WSAAsyncGetHostByAddr(hwnd AS PTR, wMsg AS DWORD, _addr AS PSZ, len AS INT,;
	type AS INT, buf    AS PSZ, buflen AS INT);
	AS PTR PASCAL:WSOCK32.WSAAsyncGetHostByAddr


_DLL FUNCTION WSACancelAsyncRequest(hAsyncTaskHandle AS PTR) AS INT PASCAL:WSOCK32.WSACancelAsyncRequest


_DLL FUNCTION WSAAsyncSelect(s AS DWORD, hWnd AS PTR, wMsg AS DWORD, lEvent AS LONG);
	AS INT PASCAL:WSOCK32.WSAAsyncSelect


_DLL FUNCTION WSARecvEx(s AS DWORD, buf AS PSZ, len AS INT, flags AS INT PTR);
	AS INT PASCAL:WSOCK32.WSARecvEx


_DLL FUNCTION TransmitFile(hSocket AS DWORD, hFile AS PTR, nNumberOfBytesToWrite AS DWORD,;
	nMumberOfBytesPerSend AS DWORD, lpOverlapped AS _winOVERLAPPED,;
	lpTransmitBuffers AS _winTRANSMIT_FILE_BUFFERS, dwReserved AS DWORD);
	AS LOGIC PASCAL:WSOCK32.TransmitFile




FUNCTION _IO(x AS DWORD, y AS DWORD) AS DWORD
	//PP-030924 correct 51422
	RETURN DWORD(_CAST,(_OR(IOC_VOID, _OR(LONGINT(_CAST,x<<8), LONGINT(_CAST,y)))))















FUNCTION IN_CLASSC(i AS LONGINT) AS DWORD
	LOCAL val1 AS DWORD

	VAL1 := _AND(DWORD(_CAST, i) , 0xE0000000)
	IF val1 = 0xc0000000
		RETURN 1
	ENDIF
	RETURN 0

FUNCTION WSAMAKEASYNCREPLY(buflen AS WORD, error AS WORD) AS LONGINT
	RETURN MakeLong(buflen,error)




FUNCTION WSAMAKESELECTREPLY(wEvent AS WORD, error AS WORD) AS LONGINT
	RETURN  MakeLong(wEvent,error)




#region defines
DEFINE FD_SETSIZE        := 64
DEFINE IOCPARM_MASK      	:= 0x0000007f
DEFINE IOC_VOID          	:= 0x20000000
DEFINE IOC_OUT           	:= 0x40000000
DEFINE IOC_IN            	:= 0x80000000
DEFINE IOC_INOUT       		:= (IOC_IN|IOC_OUT)
DEFINE IPPROTO_IP             := 0               /* dummy for IP */
DEFINE IPPROTO_HOPOPTS        := 0               /* IPv6 hop-by-hop options */
DEFINE IPPROTO_ICMP           := 1               /* control message protocol */
DEFINE IPPROTO_IGMP           := 2               /* internet group management protocol */
DEFINE IPPROTO_GGP            := 3               /* gateway^2 (deprecated) */
DEFINE IPPROTO_IPV4           := 4               /* IPv4 */
DEFINE IPPROTO_TCP            := 6               /* tcp */
DEFINE IPPROTO_PUP            := 12              /* pup */
DEFINE IPPROTO_UDP            := 17              /* user datagram protocol */
DEFINE IPPROTO_IDP            := 22              /* xns idp */
DEFINE IPPROTO_IPV6           := 41              /* IPv6 */
DEFINE IPPROTO_ROUTING        := 43              /* IPv6 routing header */
DEFINE IPPROTO_FRAGMENT       := 44              /* IPv6 fragmentation header */
DEFINE IPPROTO_ESP            := 50              /* IPsec ESP header */
DEFINE IPPROTO_AH             := 51              /* IPsec AH */
DEFINE IPPROTO_ICMPV6         := 58              /* ICMPv6 */
DEFINE IPPROTO_NONE           := 59              /* IPv6 no next header */
DEFINE IPPROTO_DSTOPTS        := 60              /* IPv6 destination options */
DEFINE IPPROTO_ND             := 77              /* UNOFFICIAL net disk proto */
DEFINE IPPROTO_ICLFXBM        := 78
DEFINE IPPROTO_RAW                       := 255
DEFINE IPPROTO_MAX                       := 256
/*
 * Port/socket numbers: network standard functions
 */
DEFINE IPPORT_ECHO                       := 7
DEFINE IPPORT_DISCARD                := 9
DEFINE IPPORT_SYSTAT                     := 11
DEFINE IPPORT_DAYTIME                := 13
DEFINE IPPORT_NETSTAT                := 15
DEFINE IPPORT_FTP                        := 21
DEFINE IPPORT_TELNET                     := 23
DEFINE IPPORT_SMTP                       := 25
DEFINE IPPORT_TIMESERVER             := 37
DEFINE IPPORT_NAMESERVER             := 42
DEFINE IPPORT_WHOIS                      := 43
DEFINE IPPORT_MTP                        := 57
/*
 * Port/socket numbers: host specific functions
 */
DEFINE IPPORT_TFTP                       := 69
DEFINE IPPORT_RJE                        := 77
DEFINE IPPORT_FINGER                     := 79
DEFINE IPPORT_TTYLINK                := 87
DEFINE IPPORT_SUPDUP                     := 95
/*
 * UNIX TCP sockets
 */
DEFINE IPPORT_EXECSERVER             := 512
DEFINE IPPORT_LOGINSERVER        := 513
DEFINE IPPORT_CMDSERVER              := 514
DEFINE IPPORT_EFSSERVER              := 520
/*
 * UNIX UDP sockets
 */
DEFINE IPPORT_BIFFUDP                := 512
DEFINE IPPORT_WHOSERVER              := 513
DEFINE IPPORT_ROUTESERVER        := 520
                                        /* 520+1 also used */
/*
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 */
DEFINE IPPORT_RESERVED               := 1024
/*
 * Link numbers
 */
DEFINE IMPLINK_IP                        := 155
DEFINE IMPLINK_LOWEXPER              := 156
DEFINE IMPLINK_HIGHEXPER             := 158
DEFINE IN_CLASSA_NET                     := 0xff000000
DEFINE IN_CLASSA_NSHIFT              := 24
DEFINE IN_CLASSA_HOST                := 0x00ffffff
DEFINE IN_CLASSA_MAX                     := 128
DEFINE IN_CLASSB_NET                     := 0xffff0000
DEFINE IN_CLASSB_NSHIFT              := 16
DEFINE IN_CLASSB_HOST                := 0x0000ffff
DEFINE IN_CLASSB_MAX                     := 65536
DEFINE IN_CLASSC_NET                     := 0xffffff00
DEFINE IN_CLASSC_NSHIFT              := 8
DEFINE IN_CLASSC_HOST                := 0x000000ff
DEFINE INADDR_ANY                        := DWORD(_CAST, 0x00000000)
DEFINE INADDR_LOOPBACK               := 0x7f000001
DEFINE INADDR_BROADCAST              := DWORD(_CAST, 0xffffffff)
DEFINE INADDR_NONE                       := 0xffffffff
/*
 * Socket address, internet style.
 */
DEFINE WSADESCRIPTION_LEN        := 256
DEFINE WSASYS_STATUS_LEN             := 128
DEFINE IP_OPTIONS                := 1
DEFINE IP_MULTICAST_IF       := 2
DEFINE IP_MULTICAST_TTL      := 3
DEFINE IP_MULTICAST_LOOP     := 4
DEFINE IP_ADD_MEMBERSHIP     := 5
DEFINE IP_DROP_MEMBERSHIP  := 6
DEFINE IP_TTL              := 7           /* set/get IP Time To Live          */
DEFINE IP_TOS              := 8           /* set/get IP Type Of Service       */
DEFINE IP_DONTFRAGMENT     := 9           /* set/get IP Don't Fragment flag   */
DEFINE IP_DEFAULT_MULTICAST_TTL     := 1
DEFINE IP_DEFAULT_MULTICAST_LOOP    := 1
DEFINE IP_MAX_MEMBERSHIPS               := 20
DEFINE INVALID_SOCKET   := DWORD(_CAST, 0xFFFFFFFF)
DEFINE SOCKET_ERROR     := (-1)
/*
 * The  following  may  be used in place of the address family, socket type, or
 * protocol  in  a  call  to WSASocket to indicate that the corresponding value
 * should  be taken from the supplied WSAPROTOCOL_INFO structure instead of the
 * parameter itself.
 */
DEFINE FROM_PROTOCOL_INFO := (-1)
/*
 * Types
 */
DEFINE SOCK_STREAM       := 1
DEFINE SOCK_DGRAM        := 2
DEFINE SOCK_RAW              := 3
DEFINE SOCK_RDM              := 4
DEFINE SOCK_SEQPACKET  := 5
/*
 * Option flags per-socket.
 */
DEFINE SO_DEBUG              := 0x0001
DEFINE SO_ACCEPTCONN     := 0x0002
DEFINE SO_REUSEADDR      := 0x0004
DEFINE SO_KEEPALIVE      := 0x0008
DEFINE SO_DONTROUTE      := 0x0010
DEFINE SO_BROADCAST      := 0x0020
DEFINE SO_USELOOPBACK  := 0x0040
DEFINE SO_LINGER             := 0x0080
DEFINE SO_OOBINLINE      := 0x0100
DEFINE SO_DONTLINGER     := DWORD(_CAST, 0xff7f)
/*
 * Additional options.
 */
DEFINE SO_SNDBUF             := 0x1001
DEFINE SO_RCVBUF             := 0x1002
DEFINE SO_SNDLOWAT       := 0x1003
DEFINE SO_RCVLOWAT       := 0x1004
DEFINE SO_SNDTIMEO       := 0x1005
DEFINE SO_RCVTIMEO       := 0x1006
DEFINE SO_ERROR              := 0x1007
DEFINE SO_TYPE               := 0x1008
/*
 * WinSock 2 extension -- new options
 */
DEFINE SO_GROUP_ID       := 0x2001      /* ID of a socket group */
DEFINE SO_GROUP_PRIORITY := 0x2002      /* the relative priority within a group*/
DEFINE SO_MAX_MSG_SIZE   := 0x2003      /* maximum message size */
DEFINE SO_PROTOCOL_INFOA := 0x2004      /* WSAPROTOCOL_INFOA structure */
DEFINE SO_PROTOCOL_INFOW := 0x2005      /* WSAPROTOCOL_INFOW structure */
/*
 * Options for connect and disconnect data and options.  Used only by
 * non-TCP/IP transports such as DECNet, OSI TP4, etc.
 */
DEFINE SO_CONNDATA       := 0x7000
DEFINE SO_CONNOPT        := 0x7001
DEFINE SO_DISCDATA       := 0x7002
DEFINE SO_DISCOPT        := 0x7003
DEFINE SO_CONNDATALEN  := 0x7004
DEFINE SO_CONNOPTLEN     := 0x7005
DEFINE SO_DISCDATALEN  := 0x7006
DEFINE SO_DISCOPTLEN     := 0x7007
/*
 * Option for opening sockets for synchronous access.
 */
DEFINE SO_OPENTYPE                       := 0x7008
DEFINE SO_SYNCHRONOUS_ALERT      := 0x10
DEFINE SO_SYNCHRONOUS_NONALERT := 0x20
/*
 * Other NT-specific options.
 */
DEFINE SO_MAXDG              := 0x7009
DEFINE SO_MAXPATHDG      := 0x700A
DEFINE SO_UPDATE_ACCEPT_CONTEXT := 0x700B
DEFINE SO_CONNECT_TIME := 0x700C
/*
 * TCP options.
 */
DEFINE TCP_NODELAY       := 0x0001
DEFINE TCP_BSDURGENT     := 0x7000
/*
 * Address families.
 */
DEFINE AF_UNSPEC             := 0
/*
 * Although  AF_UNSPEC  is  defined for backwards compatibility, using
 * AF_UNSPEC for the "af" parameter when creating a socket is STRONGLY
 * DISCOURAGED.    The  interpretation  of  the  "protocol"  parameter
 * depends  on the actual address family chosen.  As environments grow
 * to  include  more  and  more  address families that use overlapping
 * protocol  values  there  is  more  and  more  chance of choosing an
 * undesired address family when AF_UNSPEC is used.
 */
DEFINE AF_UNIX               := 1
DEFINE AF_INET               := 2
DEFINE AF_IMPLINK        := 3
DEFINE AF_PUP                := 4
DEFINE AF_CHAOS              := 5
DEFINE AF_IPX                := 6
DEFINE AF_NS                     := 6
DEFINE AF_ISO                := 7
DEFINE AF_OSI                := AF_ISO
DEFINE AF_ECMA               := 8
DEFINE AF_DATAKIT        := 9
DEFINE AF_CCITT              := 10
DEFINE AF_SNA                := 11
DEFINE AF_DECnet             := 12
DEFINE AF_DLI                := 13
DEFINE AF_LAT                := 14
DEFINE AF_HYLINK             := 15
DEFINE AF_APPLETALK      := 16
DEFINE AF_NETBIOS        := 17
DEFINE AF_VOICEVIEW      := 18
DEFINE AF_FIREFOX     := 19              /* FireFox */
DEFINE AF_UNKNOWN1    := 20              /* Somebody is using this! */
DEFINE AF_BAN         := 21              /* Banyan */
DEFINE AF_ATM         := 22              /* Native ATM Services */
DEFINE AF_INET6       := 23              /* Internetwork Version 6 */
DEFINE AF_CLUSTER     := 24              /* Microsoft Wolfpack */
DEFINE AF_12844       := 25              /* IEEE 1284.4 WG AF */
DEFINE AF_IRDA        := 26              /* IrDA */
DEFINE AF_NETDES      := 28              /* Network Designers OSI & gateway
                                          enabled protocols */
DEFINE AF_TCNPROCESS  := 29
DEFINE AF_TCNMESSAGE  := 30
DEFINE AF_ICLFXBM     := 31
DEFINE AF_MAX                := 32
DEFINE _SS_MAXSIZE := 128                  // Maximum size.
DEFINE PF_UNSPEC             := AF_UNSPEC
DEFINE PF_UNIX               := AF_UNIX
DEFINE PF_INET               := AF_INET
DEFINE PF_IMPLINK        := AF_IMPLINK
DEFINE PF_PUP                := AF_PUP
DEFINE PF_CHAOS              := AF_CHAOS
DEFINE PF_NS                     := AF_NS
DEFINE PF_IPX                := AF_IPX
DEFINE PF_ISO                := AF_ISO
DEFINE PF_OSI                := AF_OSI
DEFINE PF_ECMA               := AF_ECMA
DEFINE PF_DATAKIT        := AF_DATAKIT
DEFINE PF_CCITT              := AF_CCITT
DEFINE PF_SNA                := AF_SNA
DEFINE PF_DECnet             := AF_DECnet
DEFINE PF_DLI                := AF_DLI
DEFINE PF_LAT                := AF_LAT
DEFINE PF_HYLINK             := AF_HYLINK
DEFINE PF_APPLETALK      := AF_APPLETALK
DEFINE PF_VOICEVIEW      := AF_VOICEVIEW
DEFINE PF_FIREFOX      := AF_FIREFOX
DEFINE PF_UNKNOWN1     := AF_UNKNOWN1
DEFINE PF_BAN          := AF_BAN
DEFINE PF_ATM          := AF_ATM
DEFINE PF_INET6        := AF_INET6
DEFINE PF_MAX                := AF_MAX
DEFINE SOL_SOCKET        := 0xffff
/*
 * Maximum queue length specifiable by listen.
 */
DEFINE SOMAXCONN       := 0x7fffffff
DEFINE MSG_OOB               := 0x1
DEFINE MSG_PEEK              := 0x2
DEFINE MSG_DONTROUTE     := 0x4
DEFINE MSG_WAITALL     := 0x8             /* do not complete until packet is completely filled */
DEFINE MSG_PARTIAL     := 0x8000          /* partial send or recv for message xport */
/*
 * WinSock 2 extension -- new flags for WSASend(), WSASendTo(), WSARecv() and
 *                          WSARecvFrom()
 */
DEFINE MSG_INTERRUPT   := 0x10            /* send/recv in the interrupt context */
DEFINE MSG_MAXIOVLEN     := 16
/*
 * Define constant based on rfc883, used by gethostbyxxxx() calls.
 */
DEFINE MAXGETHOSTSTRUCT              := 1024
/*
 * WinSock 2 extension -- bit values and indices for FD_XXX network events
 */
DEFINE FD_READ_BIT      := 0
DEFINE FD_READ          := (1 << FD_READ_BIT)
DEFINE FD_WRITE_BIT     := 1
DEFINE FD_WRITE         := (1 << FD_WRITE_BIT)
DEFINE FD_OOB_BIT       := 2
DEFINE FD_OOB           := (1 << FD_OOB_BIT)
DEFINE FD_ACCEPT_BIT     :=3
DEFINE FD_ACCEPT         :=(1 << FD_ACCEPT_BIT)
DEFINE FD_CONNECT_BIT    :=4
DEFINE FD_CONNECT        :=(1 << FD_CONNECT_BIT)
DEFINE FD_CLOSE_BIT      :=5
DEFINE FD_CLOSE          :=(1 << FD_CLOSE_BIT)
DEFINE FD_QOS_BIT       := 6
DEFINE FD_QOS           := (1 << FD_QOS_BIT)
DEFINE FD_GROUP_QOS_BIT  :=7
DEFINE FD_GROUP_QOS      :=(1 << FD_GROUP_QOS_BIT)
DEFINE FD_ROUTING_INTERFACE_CHANGE_BIT  :=8
DEFINE FD_ROUTING_INTERFACE_CHANGE      :=(1 << FD_ROUTING_INTERFACE_CHANGE_BIT)
DEFINE FD_ADDRESS_LIST_CHANGE_BIT := 9
DEFINE FD_ADDRESS_LIST_CHANGE     := (1 << FD_ADDRESS_LIST_CHANGE_BIT)
DEFINE FD_MAX_EVENTS    := 10
DEFINE FD_ALL_EVENTS    := ((1 << FD_MAX_EVENTS) - 1)
/*
 * WinSock error codes are also defined in winerror.h
 * Hence the IFDEF.
 */
/*
 * All Windows Sockets error constants are biased by WSABASEERR from
 * the "normal"
 */
DEFINE WSABASEERR                        := 10000
/*
 * Windows Sockets definitions of regular Microsoft C error constants
 */
DEFINE WSAEINTR                              := (WSABASEERR+4)
DEFINE WSAEBADF                              := (WSABASEERR+9)
DEFINE WSAEACCES                             := (WSABASEERR+13)
DEFINE WSAEFAULT                             := (WSABASEERR+14)
DEFINE WSAEINVAL                             := (WSABASEERR+22)
DEFINE WSAEMFILE                             := (WSABASEERR+24)
/*
 * Windows Sockets definitions of regular Berkeley error constants
 */
DEFINE WSAEWOULDBLOCK                := (WSABASEERR+35)
DEFINE WSAEINPROGRESS                := (WSABASEERR+36)
DEFINE WSAEALREADY                       := (WSABASEERR+37)
DEFINE WSAENOTSOCK                       := (WSABASEERR+38)
DEFINE WSAEDESTADDRREQ               := (WSABASEERR+39)
DEFINE WSAEMSGSIZE                       := (WSABASEERR+40)
DEFINE WSAEPROTOTYPE                     := (WSABASEERR+41)
DEFINE WSAENOPROTOOPT                := (WSABASEERR+42)
DEFINE WSAEPROTONOSUPPORT        := (WSABASEERR+43)
DEFINE WSAESOCKTNOSUPPORT        := (WSABASEERR+44)
DEFINE WSAEOPNOTSUPP                     := (WSABASEERR+45)
DEFINE WSAEPFNOSUPPORT               := (WSABASEERR+46)
DEFINE WSAEAFNOSUPPORT               := (WSABASEERR+47)
DEFINE WSAEADDRINUSE                     := (WSABASEERR+48)
DEFINE WSAEADDRNOTAVAIL              := (WSABASEERR+49)
DEFINE WSAENETDOWN                       := (WSABASEERR+50)
DEFINE WSAENETUNREACH                := (WSABASEERR+51)
DEFINE WSAENETRESET                      := (WSABASEERR+52)
DEFINE WSAECONNABORTED               := (WSABASEERR+53)
DEFINE WSAECONNRESET                     := (WSABASEERR+54)
DEFINE WSAENOBUFS                        := (WSABASEERR+55)
DEFINE WSAEISCONN                        := (WSABASEERR+56)
DEFINE WSAENOTCONN                       := (WSABASEERR+57)
DEFINE WSAESHUTDOWN                      := (WSABASEERR+58)
DEFINE WSAETOOMANYREFS               := (WSABASEERR+59)
DEFINE WSAETIMEDOUT                      := (WSABASEERR+60)
DEFINE WSAECONNREFUSED               := (WSABASEERR+61)
DEFINE WSAELOOP                              := (WSABASEERR+62)
DEFINE WSAENAMETOOLONG               := (WSABASEERR+63)
DEFINE WSAEHOSTDOWN                      := (WSABASEERR+64)
DEFINE WSAEHOSTUNREACH               := (WSABASEERR+65)
DEFINE WSAENOTEMPTY                      := (WSABASEERR+66)
DEFINE WSAEPROCLIM                       := (WSABASEERR+67)
DEFINE WSAEUSERS                             := (WSABASEERR+68)
DEFINE WSAEDQUOT                             := (WSABASEERR+69)
DEFINE WSAESTALE                             := (WSABASEERR+70)
DEFINE WSAEREMOTE                        := (WSABASEERR+71)
/*
 * Extended Windows Sockets error constant definitions
 */
DEFINE WSASYSNOTREADY                := (WSABASEERR+91)
DEFINE WSAVERNOTSUPPORTED        := (WSABASEERR+92)
DEFINE WSANOTINITIALISED             := (WSABASEERR+93)
DEFINE WSAEDISCON              := (WSABASEERR+101)
DEFINE WSAENOMORE              := (WSABASEERR+102)
DEFINE WSAECANCELLED           := (WSABASEERR+103)
DEFINE WSAEINVALIDPROCTABLE    := (WSABASEERR+104)
DEFINE WSAEINVALIDPROVIDER     := (WSABASEERR+105)
DEFINE WSAEPROVIDERFAILEDINIT  := (WSABASEERR+106)
DEFINE WSASYSCALLFAILURE       := (WSABASEERR+107)
DEFINE WSASERVICE_NOT_FOUND    := (WSABASEERR+108)
DEFINE WSATYPE_NOT_FOUND       := (WSABASEERR+109)
DEFINE WSA_E_NO_MORE           := (WSABASEERR+110)
DEFINE WSA_E_CANCELLED         := (WSABASEERR+111)
DEFINE WSAEREFUSED             := (WSABASEERR+112)
/*
 * Error return codes from gethostbyname() and gethostbyaddr()
 * (when using the resolver). Note that these errors are
 * retrieved via WSAGetLastError() and must therefore follow
 * the rules for avoiding clashes with error numbers from
 * specific implementations or language run-time systems.
 * For this reason the codes are based at WSABASEERR+1001.
 * Note also that [WSA]NO_ADDRESS is defined only for
 * compatibility purposes.
 */
/* Authoritative Answer: Host not found */
DEFINE WSAHOST_NOT_FOUND             := (WSABASEERR+1001)
DEFINE HOST_NOT_FOUND                := WSAHOST_NOT_FOUND
/* Non-Authoritative: Host not found, or SERVERFAIL */
DEFINE WSATRY_AGAIN                     :=  (WSABASEERR+1002)
/* Non-recoverable errors, FORMERR, REFUSED, NOTIMP */
DEFINE WSANO_RECOVERY          :=  (WSABASEERR+1003)
/* Valid name, no data record of requested type */
DEFINE WSANO_DATA              :=  (WSABASEERR+1004)
/*
 * Define QOS related error return codes
 *
 */
DEFINE  WSA_QOS_RECEIVERS               :=  (WSABASEERR + 1005)
         /* at least one Reserve has arrived */
DEFINE  WSA_QOS_SENDERS                 :=  (WSABASEERR + 1006)
         /* at least one Path has arrived */
DEFINE  WSA_QOS_NO_SENDERS              :=  (WSABASEERR + 1007)
         /* there are no senders */
DEFINE  WSA_QOS_NO_RECEIVERS            :=  (WSABASEERR + 1008)
         /* there are no receivers */
DEFINE  WSA_QOS_REQUEST_CONFIRMED       :=  (WSABASEERR + 1009)
         /* Reserve has been confirmed */
DEFINE  WSA_QOS_ADMISSION_FAILURE       :=  (WSABASEERR + 1010)
         /* error due to lack of resources */
DEFINE  WSA_QOS_POLICY_FAILURE          :=  (WSABASEERR + 1011)
         /* rejected for administrative reasons - bad credentials */
DEFINE  WSA_QOS_BAD_STYLE               :=  (WSABASEERR + 1012)
         /* unknown or conflicting style */
DEFINE  WSA_QOS_BAD_OBJECT              :=  (WSABASEERR + 1013)
         /* problem with some part of the filterspec or providerspecific
         * buffer in general */
DEFINE  WSA_QOS_TRAFFIC_CTRL_ERROR      :=  (WSABASEERR + 1014)
         /* problem with some part of the flowspec */
DEFINE  WSA_QOS_GENERIC_ERROR           :=  (WSABASEERR + 1015)
         /* general error */
DEFINE  WSA_QOS_ESERVICETYPE            :=  (WSABASEERR + 1016)
         /* invalid service type in flowspec */
DEFINE  WSA_QOS_EFLOWSPEC               :=  (WSABASEERR + 1017)
         /* invalid flowspec */
DEFINE  WSA_QOS_EPROVSPECBUF            :=  (WSABASEERR + 1018)
        /* invalid provider specific buffer */
DEFINE  WSA_QOS_EFILTERSTYLE            :=  (WSABASEERR + 1019)
        /* invalid filter style */
DEFINE  WSA_QOS_EFILTERTYPE             :=  (WSABASEERR + 1020)
        /* invalid filter type */
DEFINE  WSA_QOS_EFILTERCOUNT            :=  (WSABASEERR + 1021)
        /* incorrect number of filters */
DEFINE  WSA_QOS_EOBJLENGTH              :=  (WSABASEERR + 1022)
        /* invalid object length */
DEFINE  WSA_QOS_EFLOWCOUNT              :=  (WSABASEERR + 1023)
        /* incorrect number of flows */
DEFINE  WSA_QOS_EUNKOWNPSOBJ            :=  (WSABASEERR + 1024)
        /* unknown object in provider specific buffer */
DEFINE  WSA_QOS_EPOLICYOBJ              :=  (WSABASEERR + 1025)
        /* invalid policy object in provider specific buffer */
DEFINE  WSA_QOS_EFLOWDESC               :=  (WSABASEERR + 1026)
        /* invalid flow descriptor in the list */
DEFINE  WSA_QOS_EPSFLOWSPEC             :=  (WSABASEERR + 1027)
        /* inconsistent flow spec in provider specific buffer */
DEFINE  WSA_QOS_EPSFILTERSPEC           :=  (WSABASEERR + 1028)
        /* invalid filter spec in provider specific buffer */
DEFINE  WSA_QOS_ESDMODEOBJ              :=  (WSABASEERR + 1029)
        /* invalid shape discard mode object in provider specific buffer */
DEFINE  WSA_QOS_ESHAPERATEOBJ           :=  (WSABASEERR + 1030)
        /* invalid shaping rate object in provider specific buffer */
DEFINE  WSA_QOS_RESERVED_PETYPE         :=  (WSABASEERR + 1031)
         /* reserved policy element in provider specific buffer */
DEFINE TRY_AGAIN                            :=  WSATRY_AGAIN
DEFINE NO_RECOVERY                      :=  WSANO_RECOVERY
DEFINE NO_DATA                              :=  WSANO_DATA
DEFINE WSANO_ADDRESS                    :=  WSANO_DATA
DEFINE NO_ADDRESS                       :=  WSANO_ADDRESS
DEFINE CF_ACCEPT      :=  0x0000
DEFINE CF_REJECT      :=  0x0001
DEFINE CF_DEFER       :=  0x0002
/*
 * WinSock 2 extension -- manifest constants for shutdown()
 */
DEFINE SD_RECEIVE     :=  0x00
DEFINE SD_SEND        :=  0x01
DEFINE SD_BOTH        :=  0x02
/*                      
 * WinSock 2 extension -- data type and manifest constants for socket groups
 */
DEFINE SG_UNCONSTRAINED_GROUP   := 0x01
DEFINE SG_CONSTRAINED_GROUP     := 0x02
DEFINE MAX_PROTOCOL_CHAIN := 7
DEFINE BASE_PROTOCOL      := 1
DEFINE LAYERED_PROTOCOL   := 0
/*
 * WinSock 2 extension -- manifest constants for WSAJoinLeaf()
 */
DEFINE JL_SENDER_ONLY   :=  0x01
DEFINE JL_RECEIVER_ONLY :=  0x02
DEFINE JL_BOTH          :=  0x04
/*
 * WinSock 2 extension -- manifest constants for WSASocket()
 */
DEFINE WSA_FLAG_OVERLAPPED          := 0x01
DEFINE WSA_FLAG_MULTIPOINT_C_ROOT   := 0x02
DEFINE WSA_FLAG_MULTIPOINT_C_LEAF   := 0x04
DEFINE WSA_FLAG_MULTIPOINT_D_ROOT   := 0x08
DEFINE WSA_FLAG_MULTIPOINT_D_LEAF   := 0x10
/*
 * WinSock 2 extension -- manifest constants for WSAIoctl()
 */
DEFINE IOC_UNIX                      := 0x00000000
DEFINE IOC_WS2                       := 0x08000000
DEFINE IOC_PROTOCOL                  := 0x10000000
DEFINE IOC_VENDOR                    := 0x18000000
/*
 * Service Install Flags
 */
DEFINE SERVICE_MULTIPLE       := (0x00000001)
/*
 *& Name Spaces
 */
DEFINE NS_ALL                     :=  (0)
DEFINE NS_SAP                     :=  (1)
DEFINE NS_NDS                     :=  (2)
DEFINE NS_PEER_BROWSE             :=  (3)
DEFINE NS_SLP                     :=  (5)
DEFINE NS_DHCP                    :=  (6)
DEFINE NS_TCPIP_LOCAL            :=  (10)
DEFINE NS_TCPIP_HOSTS            :=  (11)
DEFINE NS_DNS                    :=  (12)
DEFINE NS_NETBT                  :=  (13)
DEFINE NS_WINS                   :=  (14)
DEFINE NS_NLA                    := (15)    /* Network Location Awareness */
DEFINE NS_NBP                    :=  (20)
DEFINE NS_MS                     :=  (30)
DEFINE NS_STDA                   :=  (31)
DEFINE NS_NTDS                   :=  (32)
DEFINE NS_X500                   :=  (40)
DEFINE NS_NIS                    :=  (41)
DEFINE NS_NISPLUS                :=  (42)
DEFINE NS_WRQ                    :=  (50)
DEFINE NS_NETDES                 :=  (60)    /* Network Designers Limited */
/*
 * Resolution flags for WSAGetAddressByName().
 * Note these are also used by the 1.1 API GetAddressByName, so
 * leave them around.
 */
DEFINE RES_UNUSED_1               := (0x00000001)
DEFINE RES_FLUSH_CACHE            := (0x00000002)
DEFINE RES_SERVICE                := (0x00000004)
#endregion
