//////////////////////////////////////////////////////////////////////
//
//  DLL_CH.PRG
//
//  Contents:
//       Constants used for DLL calling conventions
//       Command to load non Xbase++ DLLs during runtime and call
//       DLL functions dynamically
//   
//////////////////////////////////////////////////////////////////////
// calling convention constants
DEFINE  DLL_SYSTEM           :=     4
DEFINE  DLL_CDECL            :=     8
DEFINE  DLL_STDCALL          :=    32
DEFINE  DLL_XPPCALL          :=   128

DEFINE DLL_OSAPI             := DLL_STDCALL

// calling mode constants
DEFINE  DLL_CALLMODE_NORMAL    := 0x0000
DEFINE  DLL_CALLMODE_COPY      := 0x2000


// DllInfo() DEFINE s
DEFINE DLL_INFO_HANDLE       :=  1
DEFINE DLL_INFO_NAME         :=  2
DEFINE DLL_INFO_PATHNAME     :=  3
DEFINE DLL_INFO_LOADED       :=  4
DEFINE DLL_INFO_TYPE         :=  5
DEFINE DLL_INFO_LIST         :=  6
DEFINE DLL_INFO_PREFIX       :=  7
DEFINE DLL_INFO_UNLOADABLE   :=  8
DEFINE DLL_INFO_USAGELIST    :=  9
DEFINE DLL_INFO_FUNCLIST     := 10
DEFINE DLL_INFO_CLASSFUNCLIST:= 11
DEFINE DLL_INFO_IMPORTS      := 12
DEFINE DLL_TYPE_UNKNOWN      :=  0
DEFINE DLL_TYPE_GENERAL      :=  1
DEFINE DLL_TYPE_XPP_STATIC   :=  2
DEFINE DLL_TYPE_XPP_DYNAMIC  :=  3
DEFINE DLL_TYPE_XPP_DYNAMIC_NOUNLOAD := 4

/*
// DLLFUNCTION command
#command  DLLFUNCTION <Func>([<x,...>]) ;
                USING <sys:CDECL,OSAPI,STDCALL,SYSTEM> ;
                 FROM <(Dll)> ;
       => ;
             FUNCTION <Func>([<x>]);;
                LOCAL nDll:=DllLoad(<(Dll)>);;
                LOCAL xRet:=DllCall(nDll,__Sys(<sys>),<(Func)>[,<x>]);;
                      DllUnLoad(nDll);;
               RETURN xRet

#command  STATIC DLLFUNCTION <Func>([<x,...>]) ;
                USING <sys:CDECL,OSAPI,STDCALL,SYSTEM> ;
                 FROM <(Dll)> ;
       => ;
             STATIC FUNCTION <Func>([<x>]);;
                LOCAL nDll:=DllLoad(<(Dll)>);;
                LOCAL xRet:=DllCall(nDll,__Sys(<sys>),<(Func)>[,<x>]);;
                      DllUnLoad(nDll);;
               RETURN xRet

#xtrans __Sys( CDECL )     =>   DLL_CDECL  
#xtrans __Sys( OSAPI )     =>   DLL_OSAPI  
#xtrans __Sys( STDCALL )   =>   DLL_STDCALL
#xtrans __Sys( SYSTEM )    =>   DLL_SYSTEM 


*/




DEFINE RES_STRING         :=       6
DEFINE RES_VERSION        :=      16
DEFINE RES_RAWSTRING      :=    1006
DEFINE RES_VERSIONFIXED   :=    1016

// constants for LoadResource() - version return 
DEFINE RES_VERSION_KEY     :=     1
DEFINE RES_VERSION_VALUE   :=     2
                           
DEFINE RES_PRODVER_LS      :=     1
DEFINE RES_PRODVER_MS      :=     2
DEFINE RES_FILEVER_LS      :=     3
DEFINE RES_FILEVER_MS      :=     4
DEFINE RES_FILETIME_LS     :=     5
DEFINE RES_FILETIME_MS     :=     6



// some predefined module handles
//DEFINE XPP_MOD_EXE     := 0
//DEFINE XPP_MOD_NLS     := 0xFFFFFFFF
//DEFINE XPP_MOD_RT1     := 0xFFFFFFFE
//DEFINE XPP_MOD_UI1     := 0xFFFFFFFD
//DEFINE XPP_MOD_UI2     := 0xFFFFFFFC

