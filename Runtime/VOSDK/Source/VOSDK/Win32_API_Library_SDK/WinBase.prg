VOSTRUCT _WINOVERLAPPED
	MEMBER Internal AS DWORD
	MEMBER InternalHigh AS DWORD
	MEMBER Offset AS DWORD
	MEMBER OffsetHigh AS DWORD
	MEMBER hEvent AS  PTR

VOSTRUCT _WINSECURITY_ATTRIBUTES
	MEMBER nLength AS DWORD
	MEMBER lpSecurityDescriptor AS PTR
	MEMBER bInheritHandle AS LOGIC

VOSTRUCT _WINPROCESS_INFORMATION
	MEMBER hProcess AS PTR
	MEMBER hThread AS PTR
	MEMBER dwProcessId AS DWORD
	MEMBER dwThreadId AS DWORD


VOSTRUCT _WINFILETIME
	MEMBER dwLowDateTime AS  DWORD
	MEMBER dwHighDateTime AS DWORD


VOSTRUCT _WINSYSTEMTIME
	MEMBER wYear AS WORD
	MEMBER wMonth AS WORD
	MEMBER wDayOfWeek AS WORD
	MEMBER wDay AS WORD
	MEMBER wHour AS WORD
	MEMBER wMinute AS WORD
	MEMBER wSecond AS WORD
	MEMBER wMilliseconds AS WORD





VOSTRUCT _WINCOMMPROP
	MEMBER wPacketLength AS WORD
	MEMBER wPacketVersion AS WORD
	MEMBER dwServiceMask AS DWORD
	MEMBER dwReserved1 AS DWORD
	MEMBER dwMaxTxQueue AS DWORD
	MEMBER dwMaxRxQueue AS DWORD
	MEMBER dwMaxBaud AS DWORD
	MEMBER dwProvSubType AS DWORD
	MEMBER dwProvCapabilities AS DWORD
	MEMBER dwSettableParams AS DWORD
	MEMBER dwSettableBaud AS DWORD
	MEMBER wSettableData AS WORD
	MEMBER wSettableStopParity AS WORD
	MEMBER dwCurrentTxQueue AS DWORD
	MEMBER dwCurrentRxQueue AS DWORD
	MEMBER dwProvSpec1 AS DWORD
	MEMBER dwProvSpec2 AS DWORD
	MEMBER  DIM  wcProvChar[1] AS WORD

VOSTRUCT _WINCOMSTAT










	MEMBER uBitfied AS DWORD

	MEMBER cbInQue AS DWORD
	MEMBER cbOutQue AS DWORD

VOSTRUCT _WINDCB ALIGN 1
	MEMBER DCBlength AS DWORD
	MEMBER BaudRate AS DWORD














	MEMBER cmbflags AS DWORD
	MEMBER wReserved AS WORD
	MEMBER XonLim AS WORD
	MEMBER XoffLim AS WORD
	MEMBER ByteSize AS BYTE
	MEMBER Parity AS BYTE
	MEMBER StopBits AS BYTE
	MEMBER XonChar AS BYTE
	MEMBER XoffChar AS BYTE
	MEMBER ErrorChar AS BYTE
	MEMBER EofChar AS BYTE
	MEMBER EvtChar AS BYTE
	MEMBER wReserved1 AS WORD

VOSTRUCT _WINCOMMTIMEOUTS
	MEMBER ReadIntervalTimeout AS DWORD
	MEMBER ReadTotalTimeoutMultiplier AS DWORD
	MEMBER ReadTotalTimeoutConstant AS DWORD
	MEMBER WriteTotalTimeoutMultiplier AS DWORD
	MEMBER  WriteTotalTimeoutConstant AS DWORD

VOSTRUCT _WINCOMMCONFIG
	MEMBER dwSize AS DWORD
	MEMBER wVersion AS WORD
	MEMBER wReserved AS WORD
	MEMBER dcb IS _WINDCB
	MEMBER dwProviderSubType AS DWORD
	MEMBER dwProviderOffset AS DWORD
	MEMBER dwProviderSize AS DWORD
	MEMBER DIM wcProviderData[1] AS WORD

VOSTRUCT _winSYSTEM_INFO
	MEMBER  dwOemId AS DWORD
	MEMBER  dwPageSize AS DWORD
	MEMBER  lpMinimumApplicationAddress AS PTR
	MEMBER  lpMaximumApplicationAddress AS PTR
	MEMBER  dwActiveProcessorMask AS     DWORD
	MEMBER  dwNumberOfProcessors AS DWORD
	MEMBER  dwProcessorType AS DWORD
	MEMBER  dwAllocationGranularity AS DWORD
	MEMBER  wProcessorLevel AS WORD
	MEMBER  wProcessorRevision AS WORD


VOSTRUCT _WINMEMORYSTATUS
	MEMBER  dwLength AS DWORD
	MEMBER dwMemoryLoad AS DWORD
	MEMBER dwTotalPhys AS DWORD
	MEMBER dwAvailPhys AS DWORD
	MEMBER dwTotalPageFile AS DWORD
	MEMBER dwAvailPageFile AS DWORD
	MEMBER dwTotalVirtual AS DWORD
	MEMBER dwAvailVirtual AS DWORD




VOSTRUCT _WINEXCEPTION_DEBUG_INFO
	MEMBER ExceptionRecord IS _WINEXCEPTION_RECORD
	MEMBER dwFirstChance AS DWORD

VOSTRUCT _WINCREATE_THREAD_DEBUG_INFO
	MEMBER hThread AS PTR
	MEMBER lpThreadLocalBase AS PTR
	MEMBER lpStartAddress AS PTR

VOSTRUCT _WINCREATE_PROCESS_DEBUG_INFO
	MEMBER hFile AS PTR
	MEMBER hProcess AS PTR
	MEMBER hThread AS PTR
	MEMBER lpBaseOfImage AS PTR
	MEMBER dwDebugInfoFileOffset AS DWORD
	MEMBER nDebugInfoSize AS DWORD
	MEMBER lpThreadLocalBase AS PTR
	MEMBER lpStartAddress AS PTR
	MEMBER lpImageName AS PTR
	MEMBER fUnicode AS WORD

VOSTRUCT _WINEXIT_THREAD_DEBUG_INFO
	MEMBER dwExitCode AS DWORD

VOSTRUCT _WINEXIT_PROCESS_DEBUG_INFO
	MEMBER dwExitCode AS DWORD

VOSTRUCT _WINLOAD_DLL_DEBUG_INFO
	MEMBER hFile AS PTR
	MEMBER lpBaseOfDll AS PTR
	MEMBER dwDebugInfoFileOffset AS DWORD
	MEMBER nDebugInfoSize AS DWORD
	MEMBER lpImageName AS PTR
	MEMBER fUnicode AS DWORD

VOSTRUCT _WINUNLOAD_DLL_DEBUG_INFO
	MEMBER lpBaseOfDll AS PTR

VOSTRUCT _WINOUTPUT_DEBUG_STRING_INFO
	MEMBER lpDebugStringData AS PTR
	MEMBER fUnicode AS WORD
	MEMBER nDebugStringLength AS WORD

VOSTRUCT _WINRIP_INFO
	MEMBER dwError AS DWORD
	MEMBER dwType  AS DWORD

VOSTRUCT _WINDEBUG_EVENT
	MEMBER dwDebugEventCode AS DWORD
	MEMBER dwProcessId AS DWORD
	MEMBER dwThreadId AS DWORD
	MEMBER u IS u_windebugevent












VOSTRUCT _WINOFSTRUCT
	MEMBER cBytes AS BYTE
	MEMBER fFixedDisk AS BYTE
	MEMBER nErrCode AS WORD
	MEMBER Reserved1 AS WORD
	MEMBER Reserved2 AS WORD
	MEMBER DIM szPathName[OFS_MAXPATHNAME] AS BYTE




VOSTRUCT Block_win
	MEMBER hMem AS PTR
	MEMBER DIM dwReserved[ 3 ] AS DWORD

VOSTRUCT Region_win
	MEMBER dwCommittedSize AS DWORD
	MEMBER dwUnCommittedSize AS DWORD
	MEMBER lpFirstBlock AS PTR
	MEMBER  lpLastBlock AS PTR

VOSTRUCT _WINPROCESS_HEAP_ENTRY
	MEMBER lpData AS PTR
	MEMBER cbData AS DWORD
	MEMBER cbOverhead AS BYTE
	MEMBER iRegionIndex AS BYTE
	MEMBER wFlags AS WORD
	MEMBER u IS u_winprocess_heap_entry



VOSTRUCT _WINBY_HANDLE_FILE_INFORMATION
	MEMBER  dwFileAttributes AS DWORD
	MEMBER  ftCreationTime IS _WINFILETIME
	MEMBER  ftLastAccessTime IS _WINFILETIME
	MEMBER  ftLastWriteTime IS _WINFILETIME
	MEMBER  dwVolumeSerialNumber AS DWORD
	MEMBER  nFileSizeHigh AS DWORD
	MEMBER  nFileSizeLow AS DWORD
	MEMBER  nNumberOfLinks AS DWORD
	MEMBER  nFileIndexHigh AS DWORD
	MEMBER  nFileIndexLow AS DWORD


VOSTRUCT _WINTIME_ZONE_INFORMATION
	MEMBER Bias AS LONGINT
	MEMBER DIM  StandardName[ 32 ]  AS WORD
	MEMBER StandardDate IS _WINSYSTEMTIME
	MEMBER StandardBias AS LONGINT
	MEMBER DIM DaylightName[ 32 ]  AS WORD
	MEMBER DaylightNDate IS _WINSYSTEMTIME
	MEMBER DaylightBias AS LONGINT


VOSTRUCT _WINWIN32_STREAM_ID
	MEMBER dwStreamId  AS DWORD
	MEMBER dwStreamAttributes AS DWORD
	MEMBER Size  IS _WINLARGE_INTEGER
	MEMBER dwStreamNameSize AS DWORD
	MEMBER DIM cStreamName[ ANYSIZE_ARRAY ] AS WORD


VOSTRUCT _WINSTARTUPINFO
	MEMBER  cb AS DWORD
	MEMBER lpReserved AS PSZ
	MEMBER lpDesktop AS PSZ
	MEMBER lpTitle AS PSZ
	MEMBER dwX AS DWORD
	MEMBER dwY AS DWORD
	MEMBER dwXSize AS DWORD
	MEMBER dwYSize AS DWORD
	MEMBER dwXCountChars AS DWORD
	MEMBER dwYCountChars AS DWORD
	MEMBER dwFillAttribute AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER wShowWindow AS WORD
	MEMBER cbReserved2 AS WORD
	MEMBER lpReserved2 AS BYTE PTR
	MEMBER hStdInput AS PTR
	MEMBER hStdOutput AS PTR
	MEMBER hStdError AS PTR


VOSTRUCT _WINWIN32_FIND_DATA
	MEMBER dwFileAttributes AS DWORD
	MEMBER ftCreationTime IS _WINFILETIME
	MEMBER ftLastAccessTime IS _WINFILETIME
	MEMBER ftLastWriteTime IS _WINFILETIME
	MEMBER nFileSizeHigh AS DWORD
	MEMBER nFileSizeLow AS DWORD
	MEMBER dwReserved0 AS DWORD
	MEMBER dwReserved1 AS DWORD
	MEMBER DIM cFileName[ MAX_PATH ] AS BYTE
	MEMBER DIM cAlternateFileName[ 14 ] AS BYTE


VOSTRUCT _WINOSVERSIONINFO
	MEMBER dwOSVersionInfoSize AS DWORD
	MEMBER dwMajorVersion AS DWORD
	MEMBER dwMinorVersion AS DWORD
	MEMBER dwBuildNumber    AS DWORD
	MEMBER dwPlatformId AS DWORD
	MEMBER DIM szCSDVersion[ 128 ] AS BYTE



VOSTRUCT _WINSYSTEM_POWER_STATUS
	MEMBER ACLineStatus AS BYTE
	MEMBER BatteryFlag AS BYTE
	MEMBER BatteryLifePercent AS BYTE
	MEMBER Reserved1 AS BYTE
	MEMBER BatteryLifeTime AS DWORD
	MEMBER BatteryFullLifeTime AS DWORD


VOSTRUCT _WINOSVERSIONINFOEX
                // Added for VO2.6, because it does not exist there!
       MEMBER dwOSVersionInfoSize AS DWORD
       MEMBER dwMajorVersion AS DWORD
       MEMBER dwMinorVersion AS DWORD
       MEMBER dwBuildNumber    AS DWORD
       MEMBER dwPlatformId AS DWORD
       MEMBER DIM szCSDVersion[ 128 ] AS BYTE
       MEMBER wServicePackMajor AS WORD
       MEMBER wServicePackMinor AS WORD
       MEMBER wSuiteMask AS WORD
       MEMBER wProductType AS BYTE
       MEMBER wReserved AS BYTE

// Product Types
FUNCTION   GetCurrentTime() AS DWORD
	RETURN GetTickCount()



FUNCTION FreeModule(hLibModule AS PTR) AS LOGIC
	RETURN FreeLibrary(hLibModule)






FUNCTION GlobalDiscard(h AS PTR) AS PTR
	RETURN GlobalReAlloc( H, 0, GMEM_MOVEABLE)



FUNCTION   LocalDiscard(h AS PTR) AS PTR STRICT
	RETURN LocalReAlloc(h,0,LMEM_MOVEABLE)


UNION u_windebugevent
	MEMBER  Exception IS _WINEXCEPTION_DEBUG_INFO
	MEMBER  CreateThread IS _WINCREATE_THREAD_DEBUG_INFO
	MEMBER  CreateProcessInfo IS _winCREATE_PROCESS_DEBUG_INFO
	MEMBER  ExitThread IS _WINEXIT_THREAD_DEBUG_INFO
	MEMBER  ExitProcess IS _WINEXIT_PROCESS_DEBUG_INFO
	MEMBER  LoadDll    IS _WINLOAD_DLL_DEBUG_INFO
	MEMBER  UnloadDll IS _WINUNLOAD_DLL_DEBUG_INFO
	MEMBER  DebugString IS _WINOUTPUT_DEBUG_STRING_INFO
	MEMBER  RipInfo IS _WINRIP_INFO



_DLL FUNC InterlockedIncrement(lpAddend AS LONG PTR) AS LONG PASCAL:KERNEL32.InterlockedIncrement


_DLL FUNC InterlockedDecrement(lpAddend AS LONG PTR) AS LONG PASCAL:KERNEL32.InterlockedDecrement


_DLL FUNC InterlockedExchange(Target AS LONG PTR, VALUE AS LONG) AS LONG PASCAL:KERNEL32.InterlockedExchange


_DLL FUNCTION InterlockedCompareExchange(lpDestenation AS LONG PTR,liExchange AS LONGINT,liComperand AS LONGINT) AS LONG PASCAL:KERNEL32.InterlockedCompareExchange
 
_DLL FUNCTION InterlockedExchangeAdd(lpAddend AS LONG PTR,liAdd AS LONGINT) AS LONG PASCAL:KERNEL32.InterlockedExchangeAdd


_DLL FUNC FreeResource(hResData AS PTR) AS LOGIC PASCAL:KERNEL32.FreeResource


_DLL FUNC LockResource(hResData AS PTR) AS PTR PASCAL:KERNEL32.LockResource


_DLL FUNC FreeLibrary(hLibModule AS PTR) AS LOGIC PASCAL:KERNEL32.FreeLibrary


_DLL FUNC FreeLibraryAndExitThread(hLibModule AS PTR, dwExitCode AS DWORD);
	AS VOID PASCAL:KERNEL32.FreeLibraryAndExitThread


_DLL FUNC DisableThreadLibraryCalls(hLibModule AS PTR) AS LOGIC PASCAL:KERNEL32.DisableThreadLibraryCalls



_DLL FUNC GetProcAddress(hModule AS PTR, lpProcName AS PSZ) AS PTR PASCAL:KERNEL32.GetProcAddress


_DLL FUNC GetVersion() AS DWORD PASCAL:KERNEL32.GetVersion


_DLL FUNC GlobalAlloc (uFlags AS DWORD, dwBytes AS DWORD) AS PTR PASCAL:KERNEL32.GlobalAlloc


_DLL FUNC GlobalReAlloc(hMem AS PTR, dwBytes AS DWORD, uFlags AS DWORD);
	AS PTR PASCAL:KERNEL32.GlobalReAlloc


_DLL FUNC GlobalSize(hMem AS PTR) AS DWORD PASCAL:KERNEL32.GlobalSize



_DLL FUNC GlobalFlags(hMem AS PTR) AS DWORD PASCAL:KERNEL32.GlobalFlags



_DLL FUNC GlobalLock(hMem AS PTR) AS PTR PASCAL:KERNEL32.GlobalLock


_DLL FUNC GlobalHandle(pMem AS PTR) AS PTR PASCAL:KERNEL32.GlobalHandle



_DLL FUNC GlobalUnlock(hMem AS PTR) AS LOGIC PASCAL:KERNEL32.GlobalUnlock


_DLL FUNC GlobalFree(hMem AS PTR) AS PTR PASCAL:KERNEL32.GlobalFree


_DLL FUNC GlobalCompact(dwMinFree AS DWORD) AS DWORD PASCAL:KERNEL32.GlobalCompact


_DLL FUNC GlobalFix(hMem AS PTR) AS VOID PASCAL:KERNEL32.GlobalFix


_DLL FUNC GlobalUnfix(hMem AS PTR) AS VOID PASCAL:KERNEL32.GlobalUnfix


_DLL FUNC GlobalWire(hMem AS PTR) AS PTR PASCAL:KERNEL32.GlobalWire


_DLL FUNC GlobalUnWire(hMem AS PTR) AS LOGIC PASCAL:KERNEL32.GlobalUnWire


_DLL FUNC GlobalMemoryStatus(lpBuffer AS _WINMEMORYSTATUS);
	AS VOID PASCAL:KERNEL32.GlobalMemoryStatus


_DLL FUNC LocalAlloc(uFlags AS DWORD, uBytes AS DWORD) AS PTR PASCAL:KERNEL32.LocalAlloc



_DLL FUNC LocalReAlloc(hMem AS PTR, uBytes AS DWORD, uFlags AS DWORD) ;
	AS PTR PASCAL:KERNEL32.LocalReAlloc


_DLL FUNC LocalLock(hMem AS DWORD) AS PTR PASCAL:KERNEL32.LocalLock


_DLL FUNC LocalHandle(pMem AS PTR) AS PTR PASCAL:KERNEL32.LocalHandle


_DLL FUNC LocalUnlock(hMem AS PTR) AS LOGIC PASCAL:KERNEL32.LocalUnlock


_DLL FUNC LocalSize( hMem AS PTR) AS DWORD PASCAL:KERNEL32.LocalSize


_DLL FUNC LocalFlags(hMem AS PTR) AS DWORD PASCAL:KERNEL32.LocalFlags


_DLL FUNC LocalFree(hMem AS PTR) AS PTR PASCAL:KERNEL32.LocalFree


_DLL FUNC LocalShrink(hMem AS PTR, cbNewSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.LocalShrink

_DLL FUNC LocalCompact(uMinFree AS DWORD) AS DWORD PASCAL:KERNEL32.LocalCompact


_DLL FUNC FlushInstructionCache(hProcess AS PTR, lpBaseaddress AS PTR, dwSize AS DWORD);
	AS LOGIC PASCAL:KERNEL32.FlushInstructionCache


_DLL FUNC VirtualAlloc(lpAddress AS PTR, dwSize AS DWORD, flAllocationType AS DWORD,;
	flProtect AS DWORD) AS PTR PASCAL:KERNEL32.VirtualAlloc


_DLL FUNC VirtualFree(lpAddress AS PTR, dwSize AS DWORD, dwFreeType AS DWORD);
	AS LOGIC PASCAL:KERNEL32.VirtualFree



_DLL FUNC VirtualProtect(lpAddress AS PTR, dwSize AS DWORD, flNewProtect AS DWORD,;
	lpfloldProtect AS DWORD PTR) AS LOGIC PASCAL:KERNEL32.VirtualProtect


_DLL FUNC VirtualQuery(lpAddress AS PTR, lpBuffer AS _WINMEMORY_BASIC_INFORMATION,;
	dwLength AS DWORD) AS DWORD PASCAL:KERNEL32.VirtualQuery


_DLL FUNC VirtualProtectEx(hProcess AS PTR, lpAddress AS   PTR, dwSize AS DWORD,;
	flNewProtect AS DWORD, lpflOldProtect  AS DWORD PTR) ;
	AS LOGIC PASCAL:KERNEL32.VirtualProtectEx


_DLL FUNC VirtualQueryEx(hProcess AS PTR, lpAddress AS PTR, lpBuffer AS _WINMEMORY_BASIC_INFORMATION,;
	dwLength AS DWORD) AS DWORD PASCAL:KERNEL32.VirtualQueryEx


_DLL FUNC HeapCreate(flOptions AS DWORD, dwInitialSize AS DWORD, dwMaximumSize AS DWORD);
	AS PTR PASCAL:KERNEL32.HeapCreate


_DLL FUNC HeapDestroy(hHeap AS PTR) AS LOGIC PASCAL:KERNEL32.HeapDestroy


_DLL FUNC HeapAlloc( hHeap AS PTR, dwFlags AS DWORD, dwBytes AS DWORD) AS PTR PASCAL:KERNEL32.HeapAlloc


_DLL FUNC HeapReAlloc( hHeap AS PTR, dwFlags AS DWORD, lpMem AS PTR, dwBytes AS DWORD);
	AS PTR PASCAL:KERNEL32.HeapReAlloc


_DLL FUNC HeapFree(hHeap AS PTR, dwFlags AS DWORD, lpMem AS PTR);
	AS LOGIC PASCAL:KERNEL32.HeapFree


_DLL FUNC HeapSize(hHeap AS PTR, dwFlags AS DWORD, lpMem AS PTR);
	AS DWORD PASCAL:KERNEL32.HeapSize


_DLL FUNC HeapValidate(hHeap AS PTR, dwFlags AS DWORD, lpMem AS PTR) AS LOGIC PASCAL:KERNEL32.HeapValidate


_DLL FUNC HeapCompact(hHeap AS PTR, dwFlags AS DWORD) AS DWORD PASCAL:KERNEL32.HeapCompact


_DLL FUNC GetProcessHeap() AS PTR PASCAL:KERNEL32.GetProcessHeap


_DLL FUNC GetProcessHeaps(NumberOfHeaps AS DWORD, ProcessHeaps AS PTR);
	AS DWORD PASCAL:KERNEL32.GetProcessHeaps



UNION u_winprocess_heap_entry
	MEMBER Block IS Block_win
	MEMBER Region IS Region_win

_DLL FUNC HeapLock(hHeap AS PTR) AS LOGIC PASCAL:KERNEL32.HeapLock


_DLL FUNC HeapUnlock(hHeap AS PTR) AS LOGIC PASCAL:KERNEL32.HeapUnlock



_DLL FUNC HeapWalk(hHeap AS PTR, lpEntry AS _WINPROCESS_HEAP_ENTRY ) AS LOGIC PASCAL:KERNEL32.HeapWalk


_DLL FUNC GetBinaryType(lpAppliCationName AS PTR, lpBinaryType AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetBinaryTypeA




_DLL FUNC GetShortPathName(lpszLongPath AS PTR, lpszShortPath AS PSZ, cchBuffer AS DWORD);
	AS DWORD PASCAL :KERNEL32.GetShortPathNameA



_DLL FUNC GetProcessAffinityMask(hPrOcess AS PTR, lpProcessAffinityMask AS DWORD PTR,;
	lpSystemAffinitymask AS DWORD PTR) AS LOGIC PASCAL:KERNEL32.GetProcessAffinityMask



_DLL FUNC GetProcessWorkingSetSize(hProcess AS PTR,;
	lpMinimumWorkingSetSize AS DWORD PTR,;
	lpMaximumWorkingSetSize AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetProcessWorkingSetSize



_DLL FUNC SetProcessWorkingSetSize(hProcess AS PTR,;
	lpMinimumWorkingSetSize AS DWORD,;
	lpMaximumWorkingSetSize AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetProcessWorkingSetSize


_DLL FUNC OpenProcess(dwDesiredAccess AS DWORD, bInheritHandle AS LOGIC,;
	dwProcessId AS DWORD) AS PTR PASCAL:KERNEL32.OpenProcess


_DLL FUNC GetCurrentProcess() AS PTR PASCAL:KERNEL32.GetCurrentProcess


_DLL FUNC GetCurrentProcessId() AS DWORD PASCAL:KERNEL32.GetCurrentProcessId


_DLL FUNC ExitProcess(uExitCode AS DWORD) AS VOID PASCAL:KERNEL32.ExitProcess


_DLL FUNC TerminateProcess(hProcess AS PTR, uExitCode AS DWORD) AS LOGIC PASCAL:KERNEL32.TerminateProcess


_DLL FUNC GetExitCodeProcess(hProcess AS PTR, lpExitCode AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetExitCodeProcess


_DLL FUNC FatalExit(ExitCode AS INT) AS VOID PASCAL:KERNEL32.FatalExit






_DLL FUNC GetEnvironmentStrings() AS PSZ PASCAL:KERNEL32.GetEnvironmentStrings





_DLL FUNC FreeEnvironmentStrings( freeEnvStr AS PSZ) AS LOGIC PASCAL:KERNEL32.FreeEnvironmentStringsA



_DLL FUNC RaiseException(dwExceptionCode AS DWORD, dwExceptionFlags AS DWORD, nNumberOfArguments AS DWORD,;
	lpArguments AS DWORD PTR) AS VOID PASCAL:KERNEL32.RaiseException


_DLL FUNC UnhandledExceptionFilter (ExceptionInfo AS _WINEXCEPTION_POINTERS);
	AS LONG PASCAL:KERNEL32.UnhandledExceptionFilter



_DLL FUNC SetUnhandledExceptionFilter(lpTopLevelExceptionFilter AS PTR);
	AS PTR PASCAL:KERNEL32.SetUnhandledExceptionFilter


_DLL FUNC CreateThread(lpThreadAttributes AS _WINSECURITY_ATTRIBUTES, dwStackSize AS DWORD,;
	lpStartAddress AS PTR, lpParameter AS PTR,;
	dwCreationFlags AS DWORD, lpThreadId AS DWORD PTR);
	AS PTR PASCAL:KERNEL32.CreateThread

_DLL FUNC CreateRemoteThread(hProcess AS DWORD,  lpThreadAttributes  AS  _WINSECURITY_ATTRIBUTES,;
	dwStackSize AS DWORD,lpStartAddress AS PTR, lpParameter AS PTR,;
	dwCreationFlags AS DWORD, lpThreadId AS DWORD PTR);
   AS PTR PASCAL:KERNEL32.CreateRemoteThread

_DLL FUNC GetCurrentThread() AS PTR PASCAL:KERNEL32.GetCurrentThread

_DLL FUNC GetCurrentThreadId() AS DWORD PASCAL:KERNEL32.GetCurrentThreadId

_DLL FUNC SetThreadAffinityMask(hThread AS PTR, dwThreadAffinityMask AS DWORD);
	AS DWORD PASCAL:KERNEL32.SetThreadAffinityMask


_DLL FUNC SetThreadPriority( hThread AS PTR, nPriority AS INT) AS LOGIC PASCAL:KERNEL32.SetThreadPriority


_DLL FUNC GetThreadPriority(hThread AS PTR) AS INT PASCAL:KERNEL32.GetThreadPriority


_DLL FUNC GetThreadTimes(hThread AS PTR, lpCreationTime AS _WINFILETIME,;
	lpExitTime AS _WINFILETIME, lpKernelTime AS _WINFILETIME,;
	lpUserTime AS _WINFILETIME) AS LOGIC PASCAL:kernel32.GetThreadTimes

_DLL FUNC ExitThread(dwExitCode AS DWORD) AS VOID PASCAL:KERNEL32.ExitThread

_DLL FUNC TerminateThread(hThread AS PTR, dwExitCode AS DWORD) AS LOGIC PASCAL:KERNEL32.TerminateThread


_DLL FUNC GetExitCodeThread(hThread AS PTR, lpExitCode AS DWORD PTR);
	AS  LOGIC PASCAL:KERNEL32.GetExitCodeThread


_DLL FUNC GetThreadSelectorEntry(hThread AS PTR, dwSelector AS DWORD,;
	lpSelectorEntry AS PTR) AS LOGIC PASCAL:KERNEL32.GetThreadSelectorEntry


#ifdef __VULCAN__
FUNCTION GetLastError() AS DWORD
   RETURN System.Runtime.InteropServices.Marshal.GetLastWin32Error()
#else
   _DLL FUNC GetLastError() AS DWORD PASCAL:KERNEL32.GetLastError
#endif

_DLL FUNC SetLastError(dwErrCode AS DWORD) AS VOID PASCAL:KERNEL32.SetLastError


_DLL FUNC GetOverlappedResult(hFile AS PTR, lpOverlapped AS _WINOVERLAPPED,;
	lpNumberOfBytesTransferred AS DWORD PTR, bWait AS LOGIC);
	AS LOGIC PASCAL:KERNEL32.GetOverlappedResult


_DLL FUNC CreateIoCompletionPort(FileHandle AS PTR, ExistingCompletionPort AS PTR,;
	CompletionKey AS DWORD,;
	NumberOfConcurrentThreads AS DWORD);
	AS PTR PASCAL:KERNEL32.CreateIoCompletionPort


_DLL FUNC GetQueuedCompletionStatus(CompletionPort AS PTR, lpNumbrtOfBytesTransferred AS DWORD PTR,;
	lpCompletionKey AS DWORD PTR, lpOverlapped AS _winOVERLAPPED,;
	dwMilliseconds AS DWORD);
	AS LOGIC PASCAL:KERNEL32.GetQueuedCompletionStatus


_DLL FUNC PostQueuedCompletionStatus(CompletionPot AS PTR, dwNumberOfBytesTransfered AS DWORD,;
	dwCompletionKey AS DWORD,;
	lpOverlapped AS _winOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.PostQueuedCompletionStatus


_DLL FUNC SetErrorMode(uMode AS DWORD) AS DWORD PASCAL:KERNEL32.SetErrorMode


_DLL FUNC ReadProcessMemory(hProcess AS PTR, lpBaseAddress AS PTR, lpBuffer AS PTR,;
	nSize AS DWORD, lpNumberOfBytesRead AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.ReadProcessMemory


_DLL FUNC WriteProcessMemory(hProcess AS PTR, lpBaseAddress AS PTR, lpBuffer AS PTR,;
	nSize AS DWORD, lpNumberOfBytesWrite AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.WriteProcessMemory





_DLL FUNC SuspendThread(hThread AS PTR) AS DWORD PASCAL:KERNEL32.SuspendThread


_DLL FUNC ResumeThread(hThread AS PTR) AS DWORD PASCAL:KERNEL32.ResumeThread


_DLL FUNC DebugBreak() AS VOID PASCAL:KERNEL32.DebugBreak

_DLL FUNC WaitForDebugEvent(lpDebugEvent AS _WINDEBUG_EVENT, dwMilliseconds AS DWORD);
	AS LOGIC PASCAL:KERNEL32.WaitForDebugEvent


_DLL FUNC ContinueDebugEvent(dwProcessId AS DWORD, dwThreadId AS DWORD,;
	dwContinueStatus AS DWORD) AS LOGIC PASCAL:KERNEL32.ContinueDebugEvent


_DLL FUNC DebugActiveProcess(dwProcessId AS DWORD) AS LOGIC PASCAL:KERNEL32.DebugActiveProcess


_DLL FUNC InitializeCriticalSection(lpCriticalSection AS _WINRTL_CRITICAL_SECTION) AS VOID PASCAL:KERNEL32.InitializeCriticalSection


_DLL FUNC EnterCriticalSection(lpCriticalSection AS _WINRTL_CRITICAL_SECTION);
	AS VOID PASCAL:KERNEL32.EnterCriticalSection


_DLL FUNC LeaveCriticalSection(lpCriticalSection AS _WINRTL_CRITICAL_SECTION) AS VOID PASCAL:KERNEL32.LeaveCriticalSection


_DLL FUNC DeleteCriticalSection(lpCriticalSection AS _WINRTL_CRITICAL_SECTION);
	AS VOID PASCAL:KERNEL32.DeleteCriticalSection


_DLL FUNC SetEvent(hEvent AS PTR) AS LOGIC PASCAL:KERNEL32.SetEvent


_DLL FUNC ResetEvent(hEvent AS PTR) AS LOGIC PASCAL:KERNEL32.ResetEvent


_DLL FUNC PulseEvent(hEvent AS PTR) AS LOGIC PASCAL:KERNEL32.PulseEvent


_DLL FUNC ReleaseSemaphore(hSemaphore AS PTR, lReleaseCount AS LONG,;
	lpPreviouseCount AS LONG PTR) AS LOGIC PASCAL:KERNEL32.ReleaseSemaphore


_DLL FUNC ReleaseMutex(hMutex AS PTR) AS LOGIC PASCAL:KERNEL32.ReleaseMutex


_DLL FUNC WaitForSingleObject(hHandle AS PTR, dwMilliseconds AS DWORD);
	AS DWORD PASCAL:KERNEL32.WaitForSingleObject


_DLL FUNC WaitForMultipleObjects(nCount AS DWORD, lpHandles AS PTR, bWaitAll AS LOGIC,;
	dwMilliseconds AS DWORD);
	AS DWORD PASCAL:KERNEL32.WaitForMultipleObjects


_DLL FUNC Sleep( dwMilliseconds AS DWORD) AS VOID PASCAL:KERNEL32.Sleep


_DLL FUNC LoadResource(hModule AS PTR, hResInfo AS PTR);
	AS PTR PASCAL:KERNEL32.LoadResource



_DLL FUNC SizeofResource(hModuke AS PTR, hResInfo AS PTR);
	AS DWORD PASCAL:KERNEL32.SizeofResource


_DLL FUNC GlobalDeleteAtom(nAtom AS WORD) AS WORD PASCAL:KERNEL32.GlobalDeleteAtom


_DLL FUNC InitAtomTable(nSize AS DWORD) AS LOGIC PASCAL:KERNEL32.InitAtomTable


_DLL FUNC DeleteAtom(nAtom AS WORD) AS WORD PASCAL:KERNEL32.DeleteAtom


_DLL FUNC SetHandleCount(nNumber AS DWORD) AS DWORD PASCAL:KERNEL32.SetHandleCount


_DLL FUNC GetLogicalDrives() AS DWORD PASCAL:KERNEL32.GetLogicalDrives


_DLL FUNC LockFile(hFile AS PTR, dwFileOffsetLow AS DWORD, dwFileOffsetHigh AS DWORD, nNumberOfBytesToLockLow AS DWORD, nNumberOfBytesToLockHigh AS DWORD) AS LOGIC PASCAL:KERNEL32.LockFile


_DLL FUNC UnlockFile(hFile AS PTR, dwFileOffsetLow AS DWORD, dwFileOffsetHigh AS DWORD,;
	nNumberOfBytesToUnLockLow AS DWORD,;
	nNumberOfBytesToUnLockHigh AS DWORD);
	AS LOGIC PASCAL:KERNEL32.UnlockFile


_DLL FUNC LockFileEx( hFile AS PTR, dwFlags AS DWORD, dwReserved AS DWORD,;
	nNumberOfBytesToLockLow AS DWORD,;
	nNumberOfBytesToLockHigh AS DWORD,;
	lpOverlapped AS _WINOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.LockFileEx

_DLL FUNC UnlockFileEx(hFile AS PTR, dwReserved AS DWORD,;
	nNumberOfBytesToUnLockLow AS DWORD,;
	nNumberOfBytesToUnLockHigh AS DWORD,;
	lpOverlapped AS _WINOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.UnlockFileEx

_DLL FUNC GetFileInformationByHandle(hFile AS PTR, lpFileInformation AS _WINBY_HANDLE_FILE_INFORMATION);
	AS LOGIC PASCAL:KERNEL32.GetFileInformationByHandle


_DLL FUNC GetFileType(hFile AS PTR) AS DWORD PASCAL:KERNEL32.GetFileType


_DLL FUNC GetFileSize(hFile AS PTR, lpFileSizeHigh AS DWORD PTR);
	AS DWORD PASCAL:KERNEL32.GetFileSize


_DLL FUNC GetStdHandle(nStdHandle AS DWORD) AS PTR PASCAL:KERNEL32.GetStdHandle


_DLL FUNC SetStdHandle(nStdHandle AS DWORD, hHandle AS PTR);
	AS LOGIC PASCAL:KERNEL32.SetStdHandle


_DLL FUNC WriteFile(hFile AS PTR, lpBuffer AS PTR, nNumberOfBytesToWrite AS DWORD,;
	lpNumberOfBytesWritten AS DWORD PTR, lpOverlapped AS _WINOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.WriteFile


_DLL FUNC ReadFile(hFile AS PTR, lpBuffer AS PTR, nNumberOfBytesToRead AS DWORD,;
	lpNumberOfBytesRead AS DWORD PTR, lpOverlapped AS _WINOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.ReadFile


_DLL FUNC FlushFileBuffers(hFile AS PTR) AS LOGIC PASCAL:KERNEL32.FlushFileBuffers


_DLL FUNC DeviceIoControl(hDevice AS PTR, deIoControlCode AS DWORD, lpInBufferSize AS PTR,;
	nInBufferSize AS DWORD, lpOutBuffer AS PTR,;
	nOutBufferSize AS DWORD, lpBytesReturned AS DWORD PTR,;
	lpOverlapped AS _WINOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.DeviceIoControl


_DLL FUNC SetEndOfFile(hFile AS PTR) AS LOGIC PASCAL:KERNEL32.SetEndOfFile


_DLL FUNC SetFilePointer(fFile AS PTR,lDistanceToMove AS LONG, lpDistanceToMoveHigh AS LONG PTR,;
	dwMoveMethod AS DWORD);
	AS DWORD PASCAL:KERNEL32.SetFilePointer


_DLL FUNC FindClose(hFindFile AS PTR) AS LOGIC PASCAL:KERNEL32.FindClose


_DLL FUNC GetFileTime(hFile AS PTR, lpCreationTime AS _WINFILETIME ,;
	lpLastAccessTime AS _WINFILETIME, lpLastWriteTime AS _WINFILETIME);
	AS LOGIC PASCAL:KERNEL32.GetFileTime


_DLL FUNC SetFileTime(hFile AS PTR, lpCreationTime AS _WINFILETIME,;
	lpLastAccessTime AS _WINFILETIME, lpLastWriteTime AS _WINFILETIME);
	AS LOGIC PASCAL:KERNEL32.SetFileTime



_DLL FUNC CloseHandle(hObject AS PTR) AS LOGIC PASCAL:KERNEL32.CloseHandle


_DLL FUNC DuplicateHandle(hSourceProcessHandle AS PTR, hSourdeHandle AS PTR,;
	hTargetProcessHandele AS PTR, lpTargetHandel AS PTR,;
	dwDesiredAccess AS DWORD,bInheritHandl AS LOGIC,;
	dwOptions AS DWORD) AS LOGIC PASCAL:KERNEL32.DuplicateHandle



_DLL FUNC GetHandleInformation(hObject AS PTR, lpdwFlags AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetHandleInformation


_DLL FUNC SetHandleInformation(hObject AS PTR, dwMask AS DWORD, dwFlags AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetHandleInformation

_DLL FUNC LoadModule(lpMiduleName AS PTR, lpParameterBlock AS PTR);
	AS DWORD PASCAL:KERNEL32.LoadModule


_DLL FUNC WinExec(lpCmdLine AS PTR, uCmdShow AS DWORD);
	AS DWORD PASCAL:KERNEL32.WinExec

_DLL FUNC ClearCommBreak(hFile AS PTR) AS LOGIC PASCAL:KERNEL32.ClearCommBreak


_DLL FUNC ClearCommError(hFile AS PTR, lpErrors AS DWORD PTR, lpStat AS _WINCOMSTAT);
	AS LOGIC PASCAL:KERNEL32.ClearCommError


_DLL FUNC setupComm(hFile AS PTR, dwInQueue AS DWORD, dwouQueue AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetupComm


_DLL FUNC EscapeCommFunction(hFile AS PTR, dwFunc AS DWORD);
	AS LOGIC PASCAL:KERNEL32.EscapeCommFunction


_DLL FUNC GetCommConfig(hCommDev AS PTR, lpCC AS _WINCOMMCONFIG,;
	lpdwSize AS DWORD PTR) AS LOGIC PASCAL:KERNEL32.GetCommConfig


_DLL FUNC GetCommMask(hFile AS PTR, lpEvtMask AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetCommMask


_DLL FUNC GetCOmmProperties(hFile AS PTR, lpCommProp AS _WINCOMMPROP);
	AS LOGIC PASCAL:KERNEL32.GetCommProperties


_DLL FUNC GetCommModemStatus(hFile AS PTR, lpModemStat AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetCommModemStatus


_DLL FUNC GetCommState(hFile AS PTR, lpDCB AS _WINDCB);
	AS LOGIC PASCAL:KERNEL32.GetCommState


_DLL FUNC GetCommTimeouts(hFile AS PTR,lpCommTimeouts AS _WINCOMMTIMEOUTS);
	AS LOGIC PASCAL:KERNEL32.GetCommTimeouts


_DLL FUNC PurgeComm(hFile AS PTR, dwFlags AS DWORD);
	AS LOGIC PASCAL:KERNEL32.PurgeComm


_DLL FUNC SetCommBreak(hFile AS PTR) AS LOGIC PASCAL:KERNEL32.SetCommBreak


_DLL FUNC SetCommComfig(hCommDev AS PTR, lpCC AS _WINCOMMCONFIG, dwSize AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetCommComfig


_DLL FUNC SetCommMask(hFile AS PTR, dwEvtMask AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetCommMask


_DLL FUNC SetCommState(hFile AS PTR, lpDCB AS _WINDCB);
	AS LOGIC PASCAL:KERNEL32.SetCommState


_DLL FUNC SetCommTimeouts(hFile AS PTR, lpCommTimeouts AS _WINCOMMTIMEOUTS);
	AS LOGIC PASCAL:KERNEL32.SetCommTimeouts


_DLL FUNC TransmitCommChar(hFile AS PTR, cChar AS BYTE);
	AS LOGIC PASCAL:KERNEL32.TransmitCommChar


_DLL FUNC WaitCommEvent(hFile AS PTR, lpEvtMask AS DWORD PTR, lpOverlapped AS _WINOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.WaitCommEvent


_DLL FUNC SetTapePosition(hDevice AS PTR, dwPositionMethod AS DWORD,;
	dwPartition AS DWORD, dwOffsetLow AS DWORD,;
	dwOffsetHigh AS DWORD, bImmediate AS LOGIC);
	AS DWORD PASCAL:kernel32.SetTapePosition

_DLL FUNC GetTapePosition(hDevice AS PTR, dwPositionMethod AS DWORD,;
	lpdwPartition AS DWORD PTR, lpdwOffsetLow AS DWORD PTR,;
	lpdwOffsetHigh AS DWORD PTR);
	AS DWORD PASCAL:kernel32.GetTapePosition

_DLL FUNC PrepareTape(hDevice AS PTR, dwOperation AS DWORD, bInmmediate AS LOGIC);
	AS DWORD PASCAL:KERNEL32.PrepareTape


_DLL FUNC EraseTape(hDevice AS PTR, dwEraseType AS DWORD, bImediate AS LOGIC);
	AS DWORD PASCAL:KERNEL32.EraseTape


_DLL FUNC CreateTapePartition(hDevice AS PTR, dwPartitionMethod AS DWORD,;
	dwCount AS DWORD, dwSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.CreateTapePartition


_DLL FUNC WriteTapemark(hDevice AS PTR, dwTapemarkType AS DWORD,;
	deTapemarkCount AS DWORD, bImmediate AS LOGIC);
	AS DWORD PASCAL:KERNEL32.WriteTapemark

_DLL FUNC GetTapeStatus(hDevice AS PTR) AS DWORD PASCAL:KERNEL32.GetTapeStatus


_DLL FUNC GetTapeParameters(hDevice AS PTR, dwOperation AS DWORD,;
	lpdwSize AS DWORD PTR, lpTapeInformation AS DWORD PTR);
	AS DWORD PASCAL:KERNEL32.GetTapeParameters

_DLL FUNC SetTapeParameters(hDevice AS PTR,dwOperation AS DWORD,;
	lpTapeInformation AS PTR);
	AS DWORD PASCAL:KERNEL32.SetTapeParameters

_DLL FUNC Beep(dwFreq AS DWORD, dwDuration AS DWORD) AS LOGIC PASCAL:KERNEL32.Beep


_DLL FUNC MulDiv(nNumber AS INT, nNumerator AS INT, nDenominator AS INT);
	AS INT PASCAL:KERNEL32.MulDiv

_DLL FUNC GetSystemTime(lpSystemTime AS _WINSYSTEMTIME);
	AS VOID PASCAL:KERNEL32.GetSystemTime


_DLL FUNC GetSystemTimeAsFileTime(lpSystemTimeAsFileTIme AS _WINFILETIME) ;
	AS VOID PASCAL:KERNEL32.GetSystemTimeAsFileTime


_DLL FUNC SetSystemTime(lpSystemTime AS _WINSYSTEMTIME);
	AS LOGIC PASCAL:KERNEL32.SetSystemTime


_DLL FUNC GetLocalTime(lpSystemTime AS _WINSYSTEMTIME);
	AS VOID PASCAL:KERNEL32.GetLocalTime


_DLL FUNC SetLocalTime(lpSystemTime AS _WINSYSTEMTIME);
	AS LOGIC PASCAL:KERNEL32.SetLocalTime


_DLL FUNC GetSystemInfo(lpSystemInfo AS _WINSYSTEM_INFO);
	AS VOID PASCAL:KERNEL32.GetSystemInfo

_DLL FUNC SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation AS _WINTIME_ZONE_INFORMATION,;
	lpUniversalTime AS _WINSYSTEMTIME,;
	lpLocaltime AS _WINSYSTEMTIME);
	AS LOGIC PASCAL:KERNEL32.SystemTimeToTzSpecificLocalTime

_DLL FUNC GetTimeZoneInformation(lpTimeZoneInformation AS _WINTIME_ZONE_INFORMATION);
	AS DWORD PASCAL:KERNEL32.GetTimeZoneInformation


_DLL FUNC SetTimeZoneInformation(lpTimeZoneInformation AS _WINTIME_ZONE_INFORMATION);
	AS DWORD PASCAL:KERNEL32.SetTimeZoneInformation



_DLL FUNC SystemTimeToFileTime(lpSystemTime AS _WINSYSTEMTIME,;
	lpFileTime AS _WINFILETIME);
	AS LOGIC PASCAL:KERNEL32.SystemTimeToFileTime

_DLL FUNC FileTimeToLocalFileTime(lpFileTime AS _WINFILETIME,;
	lpLocalFileTime AS _WINFILETIME);
	AS LOGIC PASCAL:KERNEL32.FileTimeToLocalFileTime


_DLL FUNC  LocalFileTimeToFileTime(lpLocalFileTime AS _WINFILETIME,;
	lpFileTime AS _WINFILETIME);
	AS LOGIC PASCAL:KERNEL32.LocalFileTimeToFileTime


_DLL FUNC FileTimeToSystemTime(lpFileTime AS _WINFILETIME,;
   lpSystemTime AS  _WINSYSTEMTIME);
	AS LOGIC PASCAL:KERNEL32.FileTimeToSystemTime
//RvdH 040122 Changed lpSystemTime parameter. Report from Igor.

_DLL FUNC CompareFileTime(lpFiletime1 AS _WINFILETIME,;
	lpFiletime2 AS _WINFILETIME);
	AS LONG PASCAL:KERNEL32.CompareFileTime


_DLL FUNC FileTimeToDosDateTime(lpFileTime AS _WINFILETIME,;
	lpFatDate AS WORD PTR, lpFatTime AS WORD PTR);
	AS LOGIC PASCAL:KERNEL32.FileTimeToDosDateTime

_DLL FUNC DosDateTimeToFileTime( wFatDate AS WORD, wFatTime AS WORD, lpFileTime AS _WINFILETIME);
	AS LOGIC PASCAL:KERNEL32.DosDateTimeToFileTime

_DLL FUNC GetTickCount() AS DWORD PASCAL:KERNEL32.GetTickCount


_DLL FUNC SetSystemTimeAdjustment(dwTimeAdjustment AS DWORD,;
	bTimeAdjustmentDisabled AS LOGIC);
	AS LOGIC PASCAL:KERNEL32.SetSystemTimeAdjustment

_DLL FUNC GetSystemTimeAdjustment(lpTimeAdjustment AS DWORD PTR,;
	lpTimeIncrement AS DWORD PTR,;
	lpTimeAdjustmentDisabled AS LOGIC PTR);
	AS LOGIC PASCAL:KERNEL32.GetSystemTimeAdjustment

#ifndef __VULCAN__
_DLL FUNC FormatMessage(dwFlags AS DWORD, lpSource AS PTR, dwMessageId AS DWORD, ;
	dwLanguageId AS DWORD, lpBuffer AS PSZ,;
	nSize AS DWORD, Arguments AS PSZ);
	AS DWORD PASCAL:KERNEL32.FormatMessageA
#else
   // IF flags contains FORMAT_MESSAGE_ALLOCATE_BUFFER, lpBuffer is a pointer to a variable
   // that will receive a pointer to the system-allocated buffer.
   // Vulcan needs an explicit overload, VO doesn't type check pointers so it doesn't care.
// _DLL FUNCTION FormatMessage(dwFlags AS DWORD, lpSource AS PTR, dwMessageId AS DWORD, dwLanguageId AS DWORD, lpBuffer AS PSZ PTR, nSize AS DWORD, Arguments AS PSZ) AS DWORD PASCAL:KERNEL32.FormatMessageA
   _DLL FUNCTION FormatMessage(dwFlags AS DWORD, lpSource AS PTR, dwMessageId AS DWORD, dwLanguageId AS DWORD, lpBuffer AS PTR, nSize AS DWORD, Arguments AS PSZ) AS DWORD PASCAL:KERNEL32.FormatMessageA
#endif	


FUNCTION GetSystemMessage(dwId AS DWORD) AS STRING
	LOCAL pMsg AS PSZ PTR    // dcaton 070405 changed from PTR to PSZ PTR
	LOCAL cReturn AS STRING

	pMsg := MemAlloc(512)
	FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM, NULL, dwId, ;
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),  ; // Default language
		pMsg, 512, NULL )
	cReturn := Psz2String(PSZ(_CAST,PTR(_CAST,pMsg)))
	MemFree(pMsg)

	RETURN cReturn

_DLL FUNC CreatePipe(hReadPipe AS PTR, hWritePipe AS PTR,;
	lpPipeAttributes AS _WINSECURITY_ATTRIBUTES,;
	nSize AS DWORD) AS LOGIC PASCAL:KERNEL32.CreatePipe


_DLL FUNC ConnectNamedPipe(hNamedPipe AS PTR, lpOverlapped AS _WINOVERLAPPED);
	AS     LOGIC PASCAL:KERNEL32.ConnectNamedPipe


_DLL FUNC DisconnectNamedPipe(hNamedPipe AS PTR);
	AS LOGIC PASCAL:KERNEL32.DisconnectNamedPipe


_DLL FUNC SetNamedPipeHandleState(hNamedPipe AS PTR, lpMode AS DWORD PTR,;
	lpMaxCollectionCount AS DWORD PTR,;
	lpCollectionDataTime AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.SetNamedPipeHandleState

_DLL FUNC GetNamedPipeInfo(hNamedPipe AS PTR, hFlags AS DWORD PTR,;
	lpOutBufferSize AS DWORD PTR,;
	lpInBufferSize AS DWORD PTR,;
	lpMaxInstances AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetNamedPipeInfo

_DLL FUNC PeekNamedPipe(hNamedPipe AS PTR, lpBuffer AS PTR,;
	nBufferSize AS DWORD, lpBytesRead AS DWORD PTR,;
	lpTotalBytesAvail AS DWORD PTR,;
	lpBytesLeftThisMessage AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.PeekNamedPipe

_DLL FUNC TransactNamedPipe(hNamedPipe AS PTR, lpInbuffer AS PTR,;
	nInbufferSize AS DWORD, lpOutBuffer AS PTR,;
	nOutBufferSize AS DWORD, lpBytesRead AS DWORD PTR,;
	lpOverlapped AS _WINOVERLAPPED);
	AS LOGIC PASCAL:KERNEL32.TransactNamedPipe

_DLL FUNC CreateMailslot( lpName AS PTR, nMaxMessageSize AS DWORD, lReadTimeout AS DWORD,;
	lpSecurityAttributes AS _WINSECURITY_ATTRIBUTES);
	AS PTR PASCAL:KERNEL32.CreateMailslotA



_DLL FUNC GetMailslotInfo(hMailslot AS PTR, lpMaxMessageSize AS DWORD PTR,;
	lpNextSize AS DWORD PTR, lpMessageCount AS DWORD PTR,;
	lpReadTimeout AS DWORD PTR) AS LOGIC PASCAL:KERNEL32.GetMailslotInfo


_DLL FUNC SetMailslotInfo(hMailslot AS PTR, lReadTimeout AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetMailslotInfo


_DLL FUNC MapViewOfFile(hFileMappingObject AS PTR, dwDesiredAccess AS DWORD,;
	dwFileOffsetHigh AS DWORD, dwFileOffsetLow AS DWORD,;
	dwNumberOfBytesToMap AS DWORD);
	AS PTR PASCAL:KERNEL32.MapViewOfFile

_DLL FUNC FlushViewOfFile(lpBaseAddress AS PTR,dwNumberOfBytesToFlush AS DWORD);
	AS LOGIC PASCAL:KERNEL32.FlushViewOfFile


_DLL FUNC UnmapViewOfFile(lpBaseAddress AS PTR);
	AS LOGIC PASCAL:KERNEL32.UnmapViewOfFile




_DLL FUNC lstrcmp(lpString1 AS PTR, lpString2 AS PTR) AS INT PASCAL:KERNEL32.lstrcmpA



_DLL FUNC lstrcmpi(lpString1 AS PTR, lpString2 AS PTR) AS INT PASCAL:KERNEL32.lstrcmpiA



_DLL FUNC lstrcpyn(lpString1 AS PSZ, lpString2 AS PSZ, iMaxlength AS INT);
	AS PSZ PASCAL:KERNEL32.lstrcpynA




_DLL FUNC lstrcpy(lpString1 AS PSZ, lpString2 AS PSZ);
	AS PSZ PASCAL:KERNEL32.lstrcpyA





_DLL FUNC lstrcat(lpString1 AS PSZ, lpString2 AS PSZ) AS PSZ PASCAL:KERNEL32.lstrcatA



_DLL FUNC lstrlen (lpString AS PSZ) AS INT PASCAL:KERNEL32.lstrlenA


_DLL FUNC OpenFile(lpFileName AS PSZ, lpReOPenBuffer AS _WINOFSTRUCT,;
	uStyle AS DWORD) AS PTR PASCAL:KERNEL32.OpenFile


_DLL FUNC _lopen(lpPathName AS PSZ, iReadWrite AS INT) AS PTR PASCAL:KERNEL32._lopen


_DLL FUNC _lcreat(lpPathName AS PSZ, iAttribute AS INT) AS PTR PASCAL:KERNEL32._lcreat


_DLL FUNC _lread(hFile AS PTR, lpBuffer AS PTR, uBytes AS DWORD) AS DWORD PASCAL:KERNEL32._lread


_DLL FUNC _lwrite(hFile AS PTR, lpBuffer AS PTR, uBytes AS DWORD);
	AS DWORD PASCAL:KERNEL32._lwrite


_DLL FUNC _hread(hFile AS PTR, lpBuffer AS PTR, uBytes AS LONG);
	AS LONG PASCAL:KERNEL32._hread


_DLL FUNC _hwrite(hFile AS PTR, lpBuffer AS PSZ, uBytes AS LONG);
	AS LONG PASCAL:KERNEL32._hwrite


_DLL FUNC _lclose(hFile AS PTR) AS INT PASCAL:KERNEL32._lclose


_DLL FUNC _llseek(hFile AS PTR, loffset AS LONG,iOrigin AS INT) AS LONG PASCAL:KERNEL32._llseek




_DLL FUNC TlsAlloc() AS DWORD PASCAL:KERNEL32.TlsAlloc

_DLL FUNC TlsGetValue(dwTlsIndex AS DWORD) AS PTR PASCAL:KERNEL32.TlsGetValue


_DLL FUNC TlsSetValue(dwTlsIndex AS DWORD, lpTlsValue AS PTR);
	AS LOGIC PASCAL:KERNEL32.TlsSetValue


_DLL FUNC TlsFree(dwTlsIndex AS DWORD) AS LOGIC PASCAL:KERNEL32.TlsFree



_DLL FUNC SleepEx( dwMilliseconds AS DWORD, bAlertable AS LOGIC);
	AS DWORD PASCAL:KERNEL32.SleepEx


_DLL FUNC WaitForSingleObjectEx(hHandle AS PTR, dwMillseconds AS DWORD,bAlertable AS LOGIC);
	AS DWORD PASCAL:KERNEL32.WaitForSingleObjectEx


_DLL FUNC WaitForMultipleObjectsEx(nCount AS DWORD, lpHandle AS PTR,;
	bWaitAll AS LOGIC, dwMilliseconds AS DWORD,bAlertable AS LOGIC);
	AS DWORD PASCAL:KERNEL32.WaitForMultipleObjectsEx



_DLL FUNC ReadFileEx(hFile AS PTR, lpBuffer AS PTR,;
	nNumberOfBytesToRead AS DWORD, lpOverlapped AS _WINOVERLAPPED,;
	lpCompletionRoutine AS PTR) AS LOGIC PASCAL:KERNEL32.ReadFileEx



_DLL FUNC WriteFileEx(hFile AS PTR, lpBUffer AS PTR, nNumberOfBytesToWrite AS DWORD,;
	lpOverlapped AS _WINOVERLAPPED, lpCompletionRoutine AS PTR);
	AS LOGIC PASCAL:KERNEL32.WriteFileEx

_DLL FUNC BackupRead(hFile AS PTR, lpBuffer AS BYTE PTR,;
	nNumberOfBytesToRead AS DWORD, lpNumberOfBytesRead AS DWORD PTR,;
	bAbort AS LOGIC, bProcessSecurity AS LOGIC ,;
	lpContext AS PTR);
	AS LOGIC PASCAL:KERNEL32.BackupRead


_DLL FUNC BackupSeek(hFile AS PTR, dwLowBytesToSeek AS DWORD,;
	dwHighBytesToSeek AS DWORD, lpdwLowByteSeeked AS DWORD PTR,;
	lpdwHighBYteSeeked AS DWORD PTR, lpContext AS PTR);
	AS LOGIC PASCAL:KERNEL32.BackupSeek


_DLL FUNC BackupWrite(hFile AS PTR, lpBuffer AS BYTE PTR,;
	nNumgberOfBytesTOWrite AS DWORD,;
	lpNumberOfBytesWritten AS DWORD PTR ,;
	bAbort AS LOGIC, bProcessSecurity AS LOGIC);
	AS LOGIC PASCAL:KERNEL32.BackupWrite


_DLL FUNC CreateMutex(lpMutexAttributes AS _WINSECURITY_ATTRIBUTES,;
	bInitialOwner AS LOGIC, lpName AS PSZ);
	AS PTR PASCAL:KERNEL32.CreateMutexA


_DLL FUNC OpenMutex(dwDesiredAccess AS DWORD, bInheritHandle AS LOGIC,;
	lpName AS PSZ) AS PTR PASCAL:KERNEL32.OpenMutexA



_DLL FUNC CreateEvent(lpEventAttributes AS _WINSECURITY_ATTRIBUTES,;
	bManualReset AS LOGIC, bInitialState AS LOGIC,;
	lpName AS PSZ) AS PTR PASCAL:KERNEL32.CreateEventA



_DLL FUNC OpenEvent(deDesiredAccess AS DWORD, bInheritHandle AS LOGIC,;
	lpName AS PSZ) AS PTR PASCAL:KERNEL32.OpenEventA


_DLL FUNC CreateSemaphore(lpSemaphoreAttributes AS _WINSECURITY_ATTRIBUTES,;
	lInitialCount AS LONG, lMaximumCount AS LONG,;
	lpName AS PSZ) AS PTR PASCAL:KERNEL32.CreateSemaphoreA



_DLL FUNC OpenSemaphore(dwDesiredAccess AS DWORD, bInheritHandle AS LOGIC,;
	lpName AS PSZ) AS PTR PASCAL:KERNEL32.OpenSemaphoreA



_DLL FUNC CreateFileMapping(hFile AS PTR, lpFileMappingAttributes AS _WINSECURITY_ATTRIBUTES,;
	flProtect AS DWORD, dwMaximunSizeHigh AS DWORD,;
	dwMaximumSizeLow AS DWORD,lpName AS PSZ);
	AS PTR PASCAL:KERNEL32.CreateFileMappingA



_DLL FUNC OpenFileMapping(dwDesiredAccess AS DWORD, bInheritHandle AS LOGIC,;
	lpName AS PSZ) AS PTR PASCAL:KERNEL32.OpenFileMappingA




_DLL FUNC GetLogicalDriveStrings(nBufferLength AS DWORD, lpBuffer AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetLogicalDriveStringsA



_DLL FUNC LoadLibrary(lpLibFileName AS PSZ) AS PTR PASCAL:KERNEL32.LoadLibraryA

#ifdef __VULCAN__
_DLL FUNC LoadLibraryW(lpLibFileName AS STRING) AS PTR PASCAL:KERNEL32.LoadLibraryW
#endif


_DLL FUNC LoadLibraryEx(lpLibFileName AS PSZ, hFile AS PTR, dwFile AS DWORD);
	AS PTR PASCAL:KERNEL32.LoadLibraryExA



_DLL FUNC GetModuleFileName(hModule AS PTR,lpFilename AS PSZ, nSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.GetModuleFileNameA



_DLL FUNC GetModuleHandle(lpModuleName AS PSZ) AS PTR PASCAL:KERNEL32.GetModuleHandleA


_DLL FUNC CreateProcess(lpApplicationName AS PSZ, lpCommandLine AS PSZ,;
	lpProcessAttributes AS _WINSECURITY_ATTRIBUTES,;
	lpThreadAttributes AS _WINSECURITY_ATTRIBUTES,;
	bInheritHandles AS LOGIC, dwCreationFlags AS DWORD,;
	lpEnvironment AS PTR, lpCurrentDirectory AS PSZ,;
	lpStautupInfo AS  _WINSTARTUPINFO,;
	lpProcessInfomation AS _WINPROCESS_INFORMATION);
	AS LOGIC PASCAL:KERNEL32.CreateProcessA




_DLL FUNC SetProcessShutdownParameterS(dwLevel AS DWORD, dwFlags AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetProcessShutdownParameters


_DLL FUNC GetProcessShutdownParameters(lpdeLevel AS DWORD PTR, lpdwFlags AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetProcessShutdownParameters


_DLL FUNC GetProcessVersion(ProcessId AS DWORD) AS DWORD PASCAL:KERNEL32.GetProcessVersion

_DLL FUNC FatalAppExit(uAction AS DWORD, lpMessageText AS PSZ);
	AS VOID PASCAL:KERNEL32.FatalAppExitA



_DLL FUNC GetStartupInfo(lpStartupInfo AS _WINSTARTUPINFO);
	AS VOID PASCAL:KERNEL32.GetStartupInfoA


_DLL FUNC GetCommandLine() AS PSZ PASCAL:KERNEL32.GetCommandLineA



_DLL FUNC GetEnvironmentVariable(lpName AS PSZ, lpBuffer AS PSZ, nSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.GetEnvironmentVariableA



_DLL FUNC SetEnvironmentVariable(lpName AS PSZ, lpValue AS PSZ);
	AS LOGIC PASCAL:KERNEL32.SetEnvironmentVariableA



_DLL FUNC ExpandEnvironmentStrings(lpSrc AS PSZ, lpDst AS PSZ, nSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.ExpandEnvironmentStringsA




_DLL FUNC OutputDebugString(lpOutputString AS PSZ) AS VOID PASCAL:KERNEL32.OutputDebugStringA



_DLL FUNC FindResource(hModule AS PTR, lpName AS PSZ, lpType AS PSZ);
	AS PTR PASCAL:KERNEL32.FindResourceA



_DLL FUNC FindResourceEx(hModule AS PTR, lpType AS PSZ, lpName AS PSZ, wLanguage AS WORD);
	AS PTR PASCAL:KERNEL32.FindResourceExA





_DLL FUNC EnumResourceTypes(hModule AS PTR, lpEnumFunc AS PTR,;
	lParam AS LONG) AS LONG PASCAL:KERNEL32.EnumResourceTypesA




_DLL FUNC EnumResourceNames(hModule AS PTR, lpType AS PSZ, lpEnumFunc AS PTR,;
	lParam AS LONG) AS LOGIC PASCAL:KERNEL32.EnumResourceNamesA



_DLL FUNC EnumResourceLanguages(hModule AS PTR, lpType AS PSZ, lpName AS PSZ, lpEnumFunc AS PTR,;
	lParam AS LONG) AS LOGIC PASCAL:KERNEL32.EnumResourceLanguagesA



_DLL FUNC BeginUpdateResource(pFileName AS PSZ, bDeleteExistingResources AS LOGIC);
	AS PTR PASCAL:KERNEL32.BeginUpdateResourceA



_DLL FUNC UpdateResource(hUpdate AS PTR, lpType AS PSZ, lpName AS PSZ,;
	wLanguage AS WORD, lpData AS PTR, cbData  AS DWORD);
	AS LOGIC PASCAL:KERNEL32.UpdateResourceA



_DLL FUNC EndUpdateResource(hUpdate AS PTR, fDiscard AS LOGIC);
	AS LOGIC PASCAL:KERNEL32.EndUpdateResourceA




_DLL FUNC GlobalAddAtom(lpString AS PSZ) AS WORD PASCAL:KERNEL32.GlobalAddAtomA




_DLL FUNC GlobalFindAtom(lpString AS PSZ) AS WORD PASCAL:KERNEL32.GlobalFindAtomA




_DLL FUNC GlobalGetAtomName(nAtom AS WORD, lpBuffer AS PSZ, nSize AS INT);
	AS DWORD PASCAL:KERNEL32.GlobalGetAtomNameA


_DLL FUNC AddAtom(lpString AS PSZ) AS WORD PASCAL:KERNEL32.AddAtomA



_DLL FUNC FindAtom(lpString AS PSZ) AS WORD PASCAL:KERNEL32.FindAtomA



_DLL FUNC GetAtomName(nAtom AS WORD, lpBuffer AS PSZ, nSize AS INT);
	AS DWORD PASCAL:KERNEL32.GetAtomNameA




_DLL FUNC GetProfileInt(lpAppName AS PSZ, lpKeyName AS PSZ, nDefault AS INT );
	AS DWORD PASCAL:KERNEL32.GetProfileIntA



_DLL FUNC GetProfileString(lpAppName AS PSZ, lpKeyName AS PSZ, lpDefault AS PSZ,;
	lpReturnedString AS PSZ, nSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.GetProfileStringA



_DLL FUNC WriteProfileString(lpAppName AS PSZ, lpKeyName AS PSZ, lpString AS PSZ);
	AS LOGIC PASCAL:KERNEL32.WriteProfileStringA


_DLL FUNC GetProfileSection(lpAppName AS PSZ, lpReturnedString AS PSZ, nSize AS DWORD );
	AS DWORD PASCAL:KERNEL32.GetProfileSectionA



_DLL FUNC WriteProfileSection(lpAppName AS PSZ, lpString AS PSZ);
	AS LOGIC PASCAL:KERNEL32.WriteProfileSectionA



_DLL FUNC GetPrivateProfileInt(lpAppName AS PSZ, lpKeyName AS PSZ, nDefault AS INT,;
	lpFileName AS PSZ) AS DWORD PASCAL:KERNEL32.GetPrivateProfileIntA



_DLL FUNC GetPrivateProfileString(lpAppName AS PSZ, lpKeyName AS PSZ, lpDefault AS PSZ,;
	lpReturnedString AS PSZ, nSize AS DWORD, lpFileName AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetPrivateProfileStringA


_DLL FUNC WritePrivateProfileString(lpAppName AS PSZ, lpKeyname AS PSZ,;
	lpString AS PSZ, lpFileName AS PSZ);
	AS LOGIC PASCAL:KERNEL32.WritePrivateProfileStringA


_DLL FUNC GetPrivateProfileSection(lpAppName AS PSZ, lpReturnedString AS PSZ,;
	nSize AS DWORD, lpFileName AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetPrivateProfileSectionA



_DLL FUNC WritePrivateProfileSection(lpAppName AS PSZ, lpString AS PSZ,lpFileName AS PSZ);
	AS LOGIC PASCAL:KERNEL32.WritePrivateProfileSectionA




_DLL FUNC GetPrivateProfileSectionNames(lpszReturnBuffer AS PSZ, nSize AS DWORD,;
	lpFileName AS PSZ) AS DWORD PASCAL:KERNEL32.GetPrivateProfileSectionNamesA


_DLL FUNC GetPrivateProfileStruct(lpszSection AS PSZ, lpszKey AS PSZ,;
	lpStruct AS PTR, uSizeStruct AS DWORD, szFile AS PSZ);
	AS LOGIC PASCAL:KERNEL32.GetPrivateProfileStructA




_DLL FUNC WritePrivateProfileStruct(lpszSection AS PSZ, lpszKey AS PSZ,;
	lpStruct AS PTR, uSizeSreuct AS DWORD, szFile AS PSZ);
	AS LOGIC PASCAL:KERNEL32.WritePrivateProfileStructA




_DLL FUNC GetDriveType(lpRootPathName AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetDriveTypeA



_DLL FUNC GetSystemDirectory(lpBuffer AS PSZ, uSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.GetSystemDirectoryA



_DLL FUNC GetTempPath(nBufferLength AS DWORD, lpBuffer AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetTempPathA



_DLL FUNC GetTempFileName(lpPathName AS PSZ, lpPrefixString AS PSZ,;
	uUnique AS DWORD, lpTempFileName AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetTempFileNameA



_DLL FUNC GetWindowsDirectory(lpBuffer AS PSZ, uSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.GetWindowsDirectoryA



_DLL FUNC SetCurrentDirectory(lpPathName AS PSZ);
	AS LOGIC PASCAL:KERNEL32.SetCurrentDirectoryA

_DLL FUNC GetCurrentDirectory(nBufferLength AS DWORD, lpBuffer AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetCurrentDirectoryA
//RvdH 070418 Added for Filip.
_DLL FUNC SetDllDirectory(lpPathName AS PSZ);
	AS LOGIC PASCAL:KERNEL32.SetDllDirectoryA


_DLL FUNC GetDllDirectory(nBufferLength AS DWORD, lpBuffer AS PSZ);
	AS DWORD PASCAL:KERNEL32.GetDllDirectoryA


_DLL FUNC GetDiskFreeSpace(lpRootPathName AS PSZ, lpSectorsPerCluster AS DWORD PTR,;
	lpBytesPerSector AS DWORD PTR, lpNumberOfFreeClusters AS DWORD PTR,;
	lpTotalNumberOfClusters AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetDiskFreeSpaceA



_DLL FUNC CreateDirectory(lpPathName AS PSZ, lpSecurityAttributes AS _WINSECURITY_ATTRIBUTES);
	AS LOGIC PASCAL:KERNEL32.CreateDirectoryA



_DLL FUNC CreateDirectoryEx(lpTemplateDirectory AS PSZ, lpNewDirectory AS PSZ,;
	lpSecurityAttributes AS _WINSECURITY_ATTRIBUTES);
	AS LOGIC PASCAL:KERNEL32.CreateDirectoryExA



_DLL FUNC RemoveDirectory(lpPathName AS PSZ) AS LOGIC PASCAL:KERNEL32.RemoveDirectoryA



_DLL FUNC GetFullPathName(lpFileName AS PSZ, nBufferLength AS DWORD,;
	lpBuffer AS PSZ, lpFilePart AS PTR);
	AS DWORD PASCAL:KERNEL32.GetFullPathNameA



_DLL FUNC DefineDosDevice(dwFlags AS DWORD, lpDeviceName AS PSZ, lpTargetPath AS PSZ);
	AS LOGIC PASCAL:KERNEL32.DefineDosDeviceA



_DLL FUNC QueryDosDevice(lpDeviceName AS PSZ, lpTargetPath AS PSZ, ucchMax AS DWORD);
	AS DWORD PASCAL:KERNEL32.QueryDosDeviceA



_DLL FUNC CreateFile(lpFileName AS PSZ, dwDesiredAccess AS DWORD,;
	dwShareMode AS DWORD, lpSrcurityAttributes AS _WINSECURITY_ATTRIBUTES,;
	dwCreationDisposition AS DWORD, dwFlagsAndAttributes AS DWORD,;
	hTemplateFile AS PTR) AS PTR PASCAL:KERNEL32.CreateFileA




_DLL FUNC SetFileAttributes(lpFileName AS PSZ, dwFileAttributes AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetFileAttributesA




_DLL FUNC GetFileAttributes(lpFileName AS PSZ) AS DWORD PASCAL:KERNEL32.GetFileAttributesA



_DLL FUNC GetCompressedFileSize(lpFileName AS PSZ, lpFileSizeHigh AS DWORD PTR);
	AS DWORD PASCAL:KERNEL32.GetCompressedFileSizeA



_DLL FUNC  DeleteFile(lpFileName AS PSZ) AS LOGIC PASCAL:KERNEL32.DeleteFileA



_DLL FUNC FindFirstFile(lpFilename AS PSZ, lpFindFileData AS _WINWIN32_FIND_DATA);
	AS PTR PASCAL:KERNEL32.FindFirstFileA



_DLL FUNC FindNextFile(hFindFile AS PTR,;
	lpFindFileData AS _WINWIN32_FIND_DATA);
	AS LOGIC PASCAL:KERNEL32.FindNextFileA


_DLL FUNC SearchPath(lpPath AS PSZ, lpFileName AS PSZ, lpExtension AS PSZ,;
	nBufferLength AS DWORD, lpBuffer AS PSZ, lpFilePart AS PTR);
	AS WORD PASCAL:KERNEL32.SearchPathA




_DLL FUNC CopyFile(lpExistingFileName AS PSZ, lpNewFileName AS PSZ, bFailIfExists AS LOGIC);
	AS LOGIC PASCAL:KERNEL32.CopyFileA



_DLL FUNC MoveFile(lpExistingFileName AS PSZ, lpNewFileName AS PSZ);
	AS LOGIC PASCAL:KERNEL32.MoveFileA



_DLL FUNC MoveFileEx(lpExistingFileName AS PSZ, lpNewFileName AS PSZ, dwFlags AS DWORD);
	AS LOGIC PASCAL:KERNEL32.MoveFileExA


_DLL FUNC CreateNamedPipe(lpName AS PSZ, dwOpenMode AS DWORD, dwPopeMode AS DWORD,;
	nMaxInstances AS DWORD, nOutBufferSize AS DWORD,;
	nInBufferSize AS DWORD, nDefaultTimeOut AS DWORD,;
	lpSecurityAttributes AS _WINSECURITY_ATTRIBUTES);
	AS PTR PASCAL:KERNEL32.CreateNamedPipeA




_DLL FUNC GetNamedPipeHandleState(hNamedPipe AS PTR, lpState AS DWORD PTR,;
	lpCurInstances AS DWORD PTR, lpMaxCollectionCount AS DWORD PTR,;
	lpCollectDataTimeOut AS DWORD PTR, lpUserNmae AS PSZ ,;
	nMaxUserNameSize AS DWORD);
	AS LOGIC PASCAL:KERNEL32.GetNamedPipeHandleStateA





_DLL FUNC CallNamedPipe(lpNamedPipeName AS PSZ, lpInBuffer AS PSZ,;
	nInBufferSize AS DWORD, lpOutBuffer AS PTR,;
	nOutBufferSize  AS DWORD, lpBytesRead  AS DWORD PTR,;
	nTimeOut AS DWORD) AS LOGIC PASCAL:KERNEL32.CallNamedPipeA





_DLL FUNC WaitNamedPipe(lpNamedPipeName AS PSZ, nTimeOut AS DWORD);
	AS LOGIC PASCAL:KERNEL32.WaitNamedPipeA



_DLL FUNC SetVolumeLabel(lpRootPathName AS PSZ, lpVolumeName AS PSZ);
	AS LOGIC PASCAL:KERNEL32.SetVolumeLabelA



_DLL FUNC SetFileApisToOEM() AS VOID PASCAL:KERNEL32.SetFileApisToOEM


_DLL FUNC SetFileApisToANSI() AS VOID PASCAL:KERNEL32.SetFileApisToANSI

_DLL FUNC AreFileApisANSI() AS LOGIC PASCAL:KERNEL32.AreFileApisANSI


_DLL FUNC GetVolumeInformation(lpRootPathName AS PSZ, lpVolumeNameBuffer AS PSZ,;
	nVolumeNameSize AS DWORD, lpVolumeSerialNumber AS DWORD PTR,;
	lpMaximumComponentLength AS DWORD PTR, lpFileSystemFlags AS DWORD PTR,;
	lpFileSystemNameBuffer AS PSZ, nFileSystemNameSize AS DWORD);
	AS LOGIC PASCAL:KERNEL32.GetVolumeInformationA




_DLL FUNC ClearEventLog(hEventLog AS PTR, lpBackupFileName AS PSZ);
	AS LOGIC PASCAL:ADVAPI32.ClearEventLogA



_DLL FUNC BackupEventLog(hEventLog AS PTR, lpBackupFileName AS PSZ);
	AS LOGIC PASCAL:ADVAPI32.BackupEventLogA



_DLL FUNC CloseEventLog(hEventLog AS PTR) AS LOGIC PASCAL:ADVAPI32.CloseEventLog


_DLL FUNC DeregisterEventSource(hEventLog AS PTR) AS LOGIC PASCAL:ADVAPI32.DeregisterEventSource


_DLL FUNC NotifyChangeEventLog(hEventLog AS PTR, hEvent AS PTR);
	AS LOGIC PASCAL:ADVAPI32.NotifyChangeEventLog


_DLL FUNC GetNumberOfEventLogRecords(hEventLog AS PTR, NumberOfRecords AS DWORD PTR);
	AS LOGIC PASCAL:ADVAPI32.GetNumberOfEventLogRecords


_DLL FUNC GetOldestEventLogRecord(hEventLOg AS PTR, OldestRecord AS DWORD PTR);
	AS LOGIC PASCAL:ADVAPI32.GetOldestEventLogRecord


_DLL FUNC OpenEventLog(lpUNCServerName AS PSZ, lpSourceName AS PSZ);
	AS PTR PASCAL:ADVAPI32.OpenEventLogA



_DLL FUNC RegisterEventSource(lpUNCServerName AS PSZ, lpSourceName AS PSZ);
	AS PTR PASCAL:ADVAPI32.RegisterEventSourceA


_DLL FUNC OpenBackupEventLog(lpUNCServerName AS PSZ, lpFileName AS PSZ);
	AS PTR PASCAL:ADVAPI32.OpenBackupEventLogA


_DLL FUNC ReadEventLog(hEventLog AS PTR,dwReadFlags AS DWORD, dwRecordOffset AS DWORD,;
	lpBuffer AS PTR, nNumberOfBytesToRead AS DWORD,;
	pnBytesRead AS DWORD PTR, pnMinNumberOfBytesNeedes AS DWORD PTR);
	AS LOGIC PASCAL:ADVAPI32.ReadEventLogA


_DLL FUNC ReportEvent(hEventLog AS PTR,lwType AS WORD, wCategory AS WORD,;
	dwEventID AS DWORD, lpUserSid AS PTR, wNumStrings AS WORD,;
	dwDataSize AS DWORD, lpStrings AS PSZ,;
	lpRawData AS PTR) AS LOGIC PASCAL:ADVAPI32.ReportEventA





_DLL FUNC DuplicateToken(ExistingTokenHandle AS PTR, ImpersonationLevel AS WORD,;
	DuplicaiteTokenHandel AS PTR);
	AS LOGIC PASCAL:KERNEL32.DuplicateToken


_DLL FUNC GetKernelObjectSecurity(Handle AS PTR, RequestedInformation AS DWORD,;
	pSecurityDescriptor AS PTR, nLength AS DWORD,;
	lpnLengthNeeded  AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetKernelObjectSecurity


_DLL FUNC ImpersonateNamedPipeClient(hNamedPipe AS PTR) AS LOGIC PASCAL:ADVAPI32.ImpersonateNamedPipeClient


_DLL FUNC ImpersonateSelf(ImpersonationLevel   AS WORD) AS LOGIC PASCAL:KERNEL32.ImpersonateSelf


_DLL FUNC RevertToSelf() AS LOGIC PASCAL:ADVAPI32.RevertToSelf


_DLL FUNC SetThreadToken(Thread AS PTR, Token AS PTR) AS LOGIC PASCAL:ADVAPI32.SetThreadToken


_DLL FUNC AccessChecK(pSecurityDescriptor AS PTR,;
	ClientToken AS PTR, DesiredAccess AS DWORD,;
	GenericMapping AS _WINGENERIC_MAPPING,;
	PrivilegeSet AS _winPRIVILEGE_SEt, PrivilegeSetLength AS DWORD PTR,;
	GrantedAccess AS DWORD PTR, AccessStatus AS LOGIC PTR);
	AS LOGIC PASCAL:ADVAPI32.AccessCheck


_DLL FUNC OpenProcessToken(ProcessHandle AS PTR, DesiredAccess AS DWORD,;
	TokenHandle AS PTR);
	AS LOGIC PASCAL:ADVAPI32.OpenProcessToken



_DLL FUNC OpenThreadToken(ThreadHandle AS PTR, DesiredAccess AS DWORD,;
	OpenAsSelf AS LOGIC, TokenHandle AS PTR);
	AS LOGIC PASCAL:ADVAPI32.OpenThreadToken



_DLL FUNC GetTokenInformation(TokenHabdle AS PTR, TokenInformationClass AS WORD,;
	TokenInformation AS PTR, TokenInformationLength AS DWORD,;
	ReturnLength AS DWORD PTR);
	AS LOGIC PASCAL:ADVAPI32.GetTokenInformation

_DLL FUNC SetTokenInformation(TokenHandle AS PTR, TokenInformationClass AS WORD,;
	TokenInformation AS PTR, TokenInformationLength AS DWORD);
	AS LOGIC PASCAL:ADVAPI32.SetTokenInformation

// UH 02/04/19979
// _DLL FUNC AdjustTokenPrivileges(TokenHandle AS PTR,
//                                 DisableAllPrivileges AS LOGIC,;
//                                 BufferLength AS DWORD,
//                                 PreviousState as _WINTOKEN_PRIVILEGES,;
//                                 ReturnLength AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.AdjustTokenPrivileges

_DLL FUNC AdjustTokenPrivileges (  TokenHandle     AS PTR,                 ;
	lDisableAll     AS LOGIC,               ;
	ptkpNew         AS _WINTOKEN_PRIVILEGES,;
	BufferLength    AS DWORD,               ;
	ptkpOld         AS _WINTOKEN_PRIVILEGES,;
	pdwRetLen       AS DWORD PTR)   AS LOGIC PASCAL:ADVAPI32.AdjustTokenPrivileges



_DLL FUNC AdjustTokenGroups(TokenHandle AS PTR, ResetToDefault AS LOGIC,;
	NewState AS _WINTOKEN_GROUPS, BufferLength AS DWORD,;
	PreviousState AS _WINTOKEN_GROUPS,;
	ReturnLength AS DWORD PTR);
	AS LOGIC PASCAL:ADVAPI32.AdjustTokenGroups


_DLL FUNC PrivilegeCheck( ClientToken AS PTR, RequiredPrivileges AS _winPRIVILEGE_SET);
	AS LOGIC PASCAL:ADVAPI32.PrivilegeCheck



_DLL FUNC AccessCheckAndAuditAlarm(SubsystemName AS PTR, HandleId AS PTR,;
	ObjectTypeName AS PSZ, ObjectName AS PSZ,;
	SecurityDescriptor AS PTR, DesiredAccess AS DWORD,;
	GenericMapping AS _WINGENERIC_MAPPING,;
	ObjectCreation AS LOGIC, GrantedAccess AS DWORD PTR,;
	AccessStatus AS LOGIC PTR, pfGenerateOnClose AS LOGIC PTR);
	AS LOGIC PASCAL:ADVAPI32.AccessCheckAndAuditAlarmA








_DLL FUNC ObjectOpenAuditAlarm(SubsystemName AS PSZ, HandleId AS PTR, ObjectTypeName AS PSZ,;
	ObjectName AS PSZ, pSecurityDescriptor AS PTR,;
	ClientTokeN AS PTR, DesiredAccess AS DWORD,;
	GrantedAccess AS DWORD, Privileges AS _winPRIVILEGE_SET,;
	ObjectCreation AS LOGIC, AccessGranted AS LOGIC,;
	GenerateOnClose AS LOGIC PTR) AS LOGIC PASCAL:ADVAPI32.ObjectOpenAuditAlarmA






_DLL FUNC ObjectPrivilegeAuditAlarm(SubsystenName AS PSZ, HandleId AS PTR,;
	ClientToken AS PTR, DesiredAccess AS DWORD,;
	Privileges AS _winPRIVILEGE_SET,;
	AccessGranted AS LOGIC);
	AS LOGIC PASCAL:ADVAPI32.ObjectPrivilegeAuditAlarmA




_DLL FUNC ObjectCloseAuditAlarm(SubsystemName AS PSZ, HandleId AS PTR,;
	GenerateOnClose AS LOGIC);
	AS LOGIC PASCAL:ADVAPI32.ObjectCloseAuditAlarmA




_DLL FUNC PrivilegedServiceAuditAlarm(SubsystemName AS PSZ, ServiceName AS PSZ,;
	ClientToken AS PTR, Privileges AS _winPRIVILEGE_SET,;
	AccessGranted AS LOGIC) AS LOGIC PASCAL:ADVAPI32.PrivilegedServiceAuditAlarmA




_DLL FUNC IsValidSid(pSid AS PTR) AS LOGIC PASCAL:ADVAPI32.IsValidSid


_DLL FUNC EqualSid(pSid1 AS PTR, pSid2 AS PTR) AS LOGIC PASCAL:ADVAPI32.EqualSid


_DLL FUNC EqualPrefixSid(pSid1 AS PTR, pSid2 AS PTR) AS LOGIC PASCAL:ADVAPI32.EqualPrefixSid


_DLL FUNC GetSidLengthRequired(nSubAuthorityCount AS BYTE) AS DWORD PASCAL:ADVAPI32.GetSidLengthRequired


_DLL FUNC AllocateAndInitializeSid(pIdentifierAuthority AS _WINSID_IDENTIFIER_AUTHORITY,;
	nSubAuthorityCount AS BYTE, nSubAuthority0 AS DWORD,;
	nSubAuthority1 AS DWORD, nSubAuthority2 AS DWORD,;
	nSubAuthority3 AS DWORD, nSubAuthority4 AS DWORD,;
	nSubAuthority5 AS DWORD, nSubAuthority6 AS DWORD,;
	nSubAuthority7 AS DWORD, pSid AS PTR);
	AS LOGIC PASCAL:ADVAPI32.AllocateAndInitializeSid


_DLL FUNC FreeSid(pSid AS PTR) AS PTR PASCAL:ADVAPI32.FreeSid


_DLL FUNC InitializeSid(Sid AS PTR,pIdentifierAuthority AS _WINSID_IDENTIFIER_AUTHORITY,;
	nSubAuthorityCount AS BYTE);
	AS LOGIC PASCAL:ADVAPI32.InitializeSid


_DLL FUNC GetSidIdentifierAuthority(pSid AS PTR) AS PTR PASCAL:ADVAPI32.GetSidIdentifierAuthority


_DLL FUNC GetSidSubAuthority(pSid AS PTR, nSubAuthority AS DWORD) AS DWORD PTR PASCAL:ADVAPI32.GetSidSubAuthority


_DLL FUNC GetSidSubAuthorityCount( pSid AS PTR) AS PTR PASCAL:ADVAPI32.GetSidSubAuthorityCount


_DLL FUNC GetLengthSid( pSid AS PTR) AS DWORD PASCAL:ADVAPI32.GetLengthSid

_DLL FUNC CopySid(nDestinationSidLength AS DWORD, pDestinationSid AS PTR,;
	pSourceSid AS PTR) AS LOGIC PASCAL:ADVAPI32.CopySid


_DLL FUNC AreAllAccessesGranted(GrantedAccess AS DWORD, DesiredAccess AS DWORD);
	AS LOGIC PASCAL:ADVAPI32.AreAllAccessesGranted


_DLL FUNC AreAnyAccessesGranted(GrantedAccess AS DWORD, DesiredAccess AS DWORD);
	AS LOGIC PASCAL:ADVAPI32.AreAnyAccessesGranted


_DLL FUNC MapGenericMask(AccessMask AS DWORD PTR,;
	GernericMapping AS _WINGENERIC_MAPPING);
	AS VOID PASCAL:ADVAPI32.MapGenericMask


_DLL FUNC IsValidAcl(pAcl AS _WINACL) AS LOGIC PASCAL:ADVAPI32.IsValidAcl


_DLL FUNC  InitializeAcl(pAcl AS _WINACL, nAclLength AS DWORD, dwAclRevision AS DWORD);
	AS LOGIC PASCAL:ADVAPI32.InitializeAcl


_DLL FUNC GetAclInformation(pAcl AS _WINACL, pAclInformation AS PTR,;
	nAclInformationLength AS DWORD,;
	dwAclInformationClass AS WORD);
	AS LOGIC PASCAL:ADVAPI32.GetAclInformation


_DLL FUNC SetAclInformation(pAcl AS _WINACL, pAclInformation AS PTR,;
	nAclInformationLength AS DWORD,;
	dwAclInformationClass AS WORD) AS LOGIC PASCAL:ADVAPI32.SetAclInformation


_DLL FUNC AddAce(pAcl AS _WINACL, dwAceRevision AS DWORD, dwStartingAceIndex AS DWORD,;
	pAceList AS PTR, nAceListLength AS DWORD);
	AS LOGIC PASCAL:ADVAPI32.AddAce


_DLL FUNC DeleteAce(pAcl AS _WINACL, dwAceIndex AS DWORD);
	AS LOGIC PASCAL:ADVAPI32.DeleteAce


_DLL FUNC GetAce(pAcl AS _WINACL, dwAceIndex AS DWORD, pAce AS PTR);
	AS LOGIC PASCAL:ADVAPI32.GetAce


_DLL FUNC AddAccessAllowedAce(pAcl AS _WINACL, dwAceRevision AS DWORD,;
	AccessMask AS DWORD, pSid AS PTR);
	AS LOGIC PASCAL:ADVAPI32.AddAccessAllowedAce
_DLL FUNC AddAccessDeniedAce(pAcl AS _WINACL, dwAceRevision AS DWORD,;
	AccessMask AS DWORD, pSid AS PTR);
	AS LOGIC PASCAL:ADVAPI32.AddAccessDeniedAce

_DLL FUNC AddAuditAccessAce(pAcl AS _WINACL, dwAceRevision AS DWORD,;
	dwAccessMask AS DWORD, pSid AS PTR, bAuditSuccess AS LOGIC,;
	bAuditFailure AS LOGIC);
	AS LOGIC PASCAL:ADVAPI32.AddAuditAccessAce
_DLL FUNC FindFirstFreeAce(pAcl AS _WINACL, pAce AS PTR) AS LOGIC PASCAL:ADVAPI32.FindFirstFreeAce

_DLL FUNC InitializeSecurityDescriptor(pSecurityDescriptor AS PTR,;
	dwRevision AS DWORD);
	AS LOGIC PASCAL:ADVAPI32.InitializeSecurityDescriptor
_DLL FUNC IsValidSecurityDescriptor(pSecurityDescriptor AS PTR);
	AS LOGIC PASCAL:ADVAPI32.IsValidSecurityDescriptor

_DLL FUNC GetSecurityDescriptorLength(pSecurityDescriptor AS PTR);
	AS DWORD PASCAL:ADVAPI32.GetSecurityDescriptorLength

_DLL FUNC GetSecurityDescriptorControl(pSecurityDescriptor AS PTR,;
	pControl AS WORD,;
	lpdwRevision AS DWORD PTR);
	AS LOGIC PASCAL:ADVAPI32.GetSecurityDescriptorControl

_DLL FUNC SetSecurityDescriptorDacl(pSecurityDescriptor AS PTR,;
	bDaclPresent AS LOGIC, pDacl AS _WINACL,;
	bDaclDefaulted AS LOGIC);
	AS LOGIC PASCAL:ADVAPI32.SetSecurityDescriptorDacl
_DLL FUNC GetSecurityDescriptorDacl(pSecurityDescriptor AS PTR, lpbDaclPresent AS LOGIC PTR,;
	pDacl AS _WINACL, lpbDaclDefaulted AS LOGIC PTR);
	AS LOGIC PASCAL:ADVAPI32.GetSecurityDescriptorDacl
_DLL FUNC SetSecurityDescriptorSacl(pSecurityDescriptor AS PTR, bSaclPresent AS LOGIC,;
	pSacl AS _WINACL, bSaclDefaulted AS LOGIC);
	AS LOGIC PASCAL:ADVAPI32.SetSecurityDescriptorSacl

_DLL FUNC GetSecurityDescriptorSacl(pSecurityDescriptor AS PTR, lpbSaclPresent AS LOGIC PTR,;
	pSacl AS _WINACL, lpbSaclDefaulted AS LOGIC PTR);
	AS LOGIC PASCAL:ADVAPI32.GetSecurityDescriptorSacl



_DLL FUNC SetSecurityDescriptorOwner(pSecurityDescriptor AS PTR, pOwner AS PTR,;
	bOwnerDefaulted AS LOGIC) AS LOGIC PASCAL:ADVAPI32.SetSecurityDescriptorOwner
_DLL FUNC GetSecurityDescriptorOwner(pSecurityDescriptor AS PTR, pOwner AS PTR, ;
	lpbOwnerDefaulted AS LOGIC PTR) AS LOGIC PASCAL:ADVAPI32.GetSecurityDescriptorOwner

_DLL FUNC SetSecurityDescriptorGroup(pSecurityDescriptor AS PTR, pGroup AS PTR,;
	bGroupDefaulted AS LOGIC) AS LOGIC PASCAL:ADVAPI32.SetSecurityDescriptorGroup


_DLL FUNC GetSecurityDescriptorGroup(pSecurityDescriptor AS PTR, pGroup AS PTR,;
	lpbGroupDefaulted AS LOGIC PTR) AS LOGIC PASCAL:ADVAPI32.GetSecurityDescriptorGroup

_DLL FUNC CreatePrivateObjectSecurity(ParentDescriptor AS PTR,;
	CreatorDescriptor AS PTR,;
	NewDescriptor AS PTR,;
	IsDirectoryObject AS LOGIC,;
	Token AS DWORD,;
	GenericMapping AS _WINGENERIC_MAPPING);
	AS LOGIC PASCAL:ADVAPI32.CreatePrivateObjectSecurity
_DLL FUNC SetPrivateObjectSecurity(SecurityInformation AS WORD,;
	ModificationDescriptor AS PTR,;
	ObjectsSecurityDescriptor AS PTR,;
	GenericMapping AS _WINGENERIC_MAPPING, Token AS PTR);
	AS LOGIC PASCAL:ADVAPI32.SetPrivateObjectSecurity
_DLL FUNC GetPrivateObjectSecurity(ObjectDescriptor AS PTR,;
	SecurityInformation AS WORD,;
	ResultantDescriptor AS PTR,;
	DescriptorLength AS DWORD,;
	ReturnLength AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.GetPrivateObjectSecurity
_DLL FUNC DestroyPrivateObjectSecurity( ObjectDescriiptor AS PTR) AS LOGIC PASCAL:ADVAPI32.DestroyPrivateObjectSecurity
_DLL FUNC MakeSelfRelativeSD(pAbsoluteSecurityDescriptor AS PTR,;
	pSelfRelativeSecurityDescriptor AS PTR,;
	lpdwBufferLength AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.MakeSelfRelativeSD
_DLL FUNC MakeAbsoluteSD(pSelfRelativeSecurityDescriptor AS PTR,;
	pAbsoluteSecurityDescriptor AS PTR,;
	lpdwAbsoluteSecurityDescriptorSize AS DWORD PTR,;
	pDacl AS _WINACL, lpdwDaclSize AS DWORD PTR,;
	pSacl AS _WINACL, lpdwSaclSize AS DWORD PTR,;
	pOwner AS PTR, lpdwOwnerSize AS DWORD PTR,;
	pPrimaryGroup AS PTR, lpdwPrimaryGroupSize AS DWORD PTR);
	AS LOGIC PASCAL:ADVAPI32.MakeAbsoluteSD


_DLL FUNC SetFileSecurity(lpFileName AS PSZ,;
	SecurityInformation AS WORD,;
	pSecurityDescriptor AS PTR) AS LOGIC PASCAL:ADVAPI32.SetFileSecurityA


_DLL FUNC GetFileSecurity(lpFileName AS PSZ,;
	RequestedInformation AS WORD,;
	pSecurityDescriptor AS PTR,;
	nLength AS DWORD,;
	lpnLengthNeeded AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.GetFileSecurityA

_DLL FUNC SetKernelObjectSecurity( Handle AS PTR, SecurityInformation AS WORD,;
	SecurityDescriptor AS PTR) AS LOGIC PASCAL:ADVAPI32.SetKernelObjectSecurity

_DLL FUNC FindFirstChangeNotification(lpPathname AS PSZ, bWatchSubtree AS LOGIC,;
	dwNotifyFilter AS DWORD);
	AS PTR PASCAL:KERNEL32.FindFirstChangeNotificationA


_DLL FUNC FindNextChangeNotification(hChangeHandle AS PTR);
	AS LOGIC PASCAL:KERNEL32.FindNextChangeNotification


_DLL FUNC FindCloseChangeNotification(hChangeHandle AS PTR);
	AS LOGIC PASCAL:KERNEL32.FindCloseChangeNotification


_DLL FUNC VirtualLock(lpAddress AS PTR, dwSize AS DWORD) AS LOGIC PASCAL:KERNEL32.VirtualLock


_DLL FUNC VirtualUnlock(lpAddress AS PTR, dwSize AS DWORD) AS LOGIC PASCAL:KERNEL32.VirtualUnlock


_DLL FUNC MapViewOfFileEx(hFileMappingObject AS PTR, dwDesiredAccess AS DWORD,;
	dwFileOffsetHigh AS DWORD, dwFileOffsetLow AS DWORD,;
	dwNumberOfBytesToMap AS DWORD, lpBaseAddress AS PTR);
	AS PTR PASCAL:KERNEL32.MapViewOfFileEx

_DLL FUNC SetPriorityClass(hProcess AS PTR, dwPriorityClass AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetPriorityClass


_DLL FUNC GetPriorityClass(hProcess AS PTR) AS DWORD PASCAL:KERNEL32.GetPriorityClass


_DLL FUNC IsBadReadPtr(lp AS PTR, ucb AS DWORD) AS LOGIC PASCAL:KERNEL32.IsBadReadPtr


_DLL FUNC IsBadWritePtr(lp AS PTR, ucb AS DWORD) AS LOGIC PASCAL:KERNEL32.IsBadWritePtr


_DLL FUNC IsBadHugeReadPtr(lp AS PTR, ucb AS DWORD) AS LOGIC PASCAL:KERNEL32.IsBadHugeReadPtr


_DLL FUNC IsBadHugeWritePtr(lp AS PTR, ucb AS DWORD) AS LOGIC PASCAL:KERNEL32.IsBadHugeWritePtr


_DLL FUNC IsBadCodePtr(lpfn AS PTR) AS LOGIC PASCAL:KERNEL32.IsBadCodePtr

_DLL FUNC IsBadStringPtr(lpsz AS PSZ, ucchMax AS DWORD) AS LOGIC PASCAL:KERNEL32.IsBadStringPtrA



_DLL FUNC LookupAccountSid(lpSystemName AS PSZ, Sid AS PTR, Name AS PSZ,;
	cbName AS DWORD PTR, ReferencedDomainName AS PSZ,;
	cbReferencedDomainName AS DWORD PTR, peUse AS WORD) AS LOGIC PASCAL:ADVAPI32.lookupLookupAccountSidA



// RvdH 041210 changed prototype below. Bug report 13150
_DLL FUNC LookupAccountName(lpSystemName AS PSZ, lpAccountName AS PSZ,Sid AS PTR,;
	cbSid AS DWORD PTR, ReferencedDomainName AS PSZ,;
	cbReferencedDomainName AS DWORD PTR, peUse AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.LookupAccountNameA





_DLL FUNC LookupPrivilegeValue(lpSystemName AS PSZ, lpName AS PSZ, lpLuid AS _WINLUID);
	AS LOGIC PASCAL:ADVAPI32.LookupPrivilegeValueA




_DLL FUNC LookupPrivilegeName(lpSystemName AS PSZ, lpLuid AS _WINLUID,;
	lpName AS PSZ, cbName AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.LookupPrivilegeNameA


_DLL FUNC LookupPrivilegeDisplayName(lpSystemName AS PSZ, lpName AS PSZ,;
	lpDisplayName AS PSZ, cbDisplayName AS DWORD PTR,;
	lpLanguageId AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.LookupPrivilegeDisplayNameA



_DLL FUNC AllocateLocallyUniqueId(Luid AS _WINLARGE_INTEGER);
	AS LOGIC PASCAL:ADVAPI32.AllocateLocallyUniqueId

_DLL FUNC BuildCommDCB(lpDef AS PSZ, lpDCB AS _WINDCB) AS LOGIC PASCAL:KERNEL32.BuildCommDCBA




_DLL FUNC BuildCommDCBAndTimeouts(lpDef AS PSZ, lpDCB AS _WINDCB,;
	lpCommTimeouts AS _WINCOMMTIMEOUTS) AS LOGIC PASCAL:KERNEL32.BuildCommDCBAndTimeoutsA


_DLL FUNC CommConfigDialog(lpszName AS PSZ, hWnd AS DWORD, LPCC AS _WINCOMMCONFIG);
	AS LOGIC PASCAL:KERNEL32.CommConfigDialogA



_DLL FUNC GetDefaultCommConfig(lpszName AS PSZ, lpcc AS _WINCOMMCONFIG,;
	lpdwSize AS DWORD PTR) AS LOGIC PASCAL:KERNEL32.GetDefaultCommConfigA




_DLL FUNC SetDefaultCommConfig(lpszName AS PSZ, lpcc AS _WINCOMMCONFIG,;
	dwSize AS DWORD) AS LOGIC PASCAL:KERNEL32.SetDefaultCommConfigA


_DLL FUNC GetComputerName(lpBuffer AS PSZ, nSize AS DWORD PTR) AS LOGIC PASCAL:KERNEL32.GetComputerNameA



_DLL FUNC SetComputerName(lpComputerName AS PSZ) AS LOGIC PASCAL:KERNEL32.SetComputerNameA


_DLL FUNC GetUserName(lpBuffer AS PSZ, nSize AS DWORD PTR) AS LOGIC PASCAL:ADVAPI32.GetUserNameA



_DLL FUNC LogonUser(lpszUsername AS PSZ, lpszDomain AS PSZ, lpszPassword AS PSZ,;
	dwLogonType AS DWORD, dwLogonProvider AS DWORD,;
	phToken AS PTR) AS LOGIC PASCAL:ADVAPI32.LogonUserA



_DLL FUNC ImpersonateLoggedOnUser(hToken AS PTR) AS LOGIC PASCAL:ADVAPI32.ImpersonateLoggedOnUser


_DLL FUNC CreateProcessAsUser(hToken AS PTR, lpApplicationName AS PSZ,;
	lpCommandLIne AS PSZ,;
	lpProcessAttributes AS _WINSECURITY_ATTRIBUTES,;
	lpThreadAttributes AS _WINSECURITY_ATTRIBUTES,;
	bInheritHandles AS LOGIC, dwCreationFlags AS DWORD,;
	lpEnvironment AS PTR, lpCurrentDirectory AS PSZ,;
	lpStartupInfo AS _WINSTARTUPINFO,;
	lpProcessInformation AS _WINPROCESS_INFORMATION);
	AS LOGIC PASCAL:ADVAPI32.CreateProcessAsUserA






_DLL FUNC QueryPerformanceCounter(lpPerformanceCount AS _WINLARGE_INTEGER) AS LOGIC PASCAL:KERNEL32.QueryPerformanceCounter


_DLL FUNC QueryPerformanceFrequency(lpFreqebcy AS _WINLARGE_INTEGER) AS LOGIC PASCAL:KERNEL32.QueryPerformanceFrequency

_DLL FUNC GetVersionEx(lpVersionInformation AS _WINOSVERSIONINFO) AS LOGIC PASCAL:KERNEL32.GetVersionExA
_DLL FUNC GetVersionEx(lpVersionInformation AS _WINOSVERSIONINFOEX) AS LOGIC PASCAL:KERNEL32.GetVersionExA







_DLL FUNC GetSystemPowerStatus(lpSystemPowerStatus AS _WINSYSTEM_POWER_STATUS) AS LOGIC PASCAL:KERNEL32.GetSystemPowerStatus


_DLL FUNC SetSystemPowerState(fSuspend AS LOGIC, fForce AS LOGIC) AS LOGIC PASCAL:KERNEL32.SetSystemPowerState


#region defines
DEFINE INVALID_HANDLE_VALUE :=PTR (_CAST, 0xFFFFFFFF) 
DEFINE INVALID_FILE_SIZE := DWORD (_CAST, 0xFFFFFFFF)
DEFINE INVALID_SET_FILE_POINTER := 0xFFFFFFFFU
DEFINE INVALID_FILE_ATTRIBUTES := 0xFFFFFFFFU
DEFINE FILE_BEGIN                  := 0
DEFINE FILE_CURRENT                := 1
DEFINE FILE_END                        := 2
DEFINE TIME_ZONE_ID_INVALID := DWORD (_CAST, 0xFFFFFFFF)
DEFINE WAIT_FAILED          := DWORD (_CAST, 0xFFFFFFFF)
DEFINE WAIT_OBJECT_0        := STATUS_WAIT_0  + 0
DEFINE WAIT_ABANDONED   := (STATUS_ABANDONED_WAIT_0 + 0 )
DEFINE WAIT_ABANDONED_0  := (STATUS_ABANDONED_WAIT_0 + 0 )
DEFINE WAIT_TIMEOUT                                     :=         STATUS_TIMEOUT
DEFINE WAIT_IO_COMPLETION                       :=         STATUS_USER_APC
DEFINE STILL_ACTIVE                                     :=         STATUS_PENDING
DEFINE EXCEPTION_ACCESS_VIOLATION       :=         STATUS_ACCESS_VIOLATION
DEFINE EXCEPTION_DATATYPE_MISALIGNMENT :=      STATUS_DATATYPE_MISALIGNMENT
DEFINE EXCEPTION_BREAKPOINT                     :=         STATUS_BREAKPOINT
DEFINE EXCEPTION_SINGLE_STEP                    :=         STATUS_SINGLE_STEP
DEFINE EXCEPTION_ARRAY_BOUNDS_EXCEEDED :=      STATUS_ARRAY_BOUNDS_EXCEEDED
DEFINE EXCEPTION_FLT_DENORMAL_OPERAND  :=      STATUS_FLOAT_DENORMAL_OPERAND
DEFINE EXCEPTION_FLT_DIVIDE_BY_ZERO     :=         STATUS_FLOAT_DIVIDE_BY_ZERO
DEFINE EXCEPTION_FLT_INEXACT_RESULT     :=         STATUS_FLOAT_INEXACT_RESULT
DEFINE EXCEPTION_FLT_INVALID_OPERATION :=      STATUS_FLOAT_INVALID_OPERATION
DEFINE EXCEPTION_FLT_OVERFLOW               :=         STATUS_FLOAT_OVERFLOW
DEFINE EXCEPTION_FLT_STACK_CHECK            :=         STATUS_FLOAT_STACK_CHECK
DEFINE EXCEPTION_FLT_UNDERFLOW              :=         STATUS_FLOAT_UNDERFLOW
DEFINE EXCEPTION_INT_DIVIDE_BY_ZERO     :=         STATUS_INTEGER_DIVIDE_BY_ZERO
DEFINE EXCEPTION_INT_OVERFLOW               :=         STATUS_INTEGER_OVERFLOW
DEFINE EXCEPTION_PRIV_INSTRUCTION       :=         STATUS_PRIVILEGED_INSTRUCTION
DEFINE EXCEPTION_IN_PAGE_ERROR              :=         STATUS_IN_PAGE_ERROR
DEFINE EXCEPTION_ILLEGAL_INSTRUCTION    :=         STATUS_ILLEGAL_INSTRUCTION
DEFINE EXCEPTION_NONCONTINUABLE_EXCEPTION :=   STATUS_NONCONTINUABLE_EXCEPTION
DEFINE EXCEPTION_STACK_OVERFLOW                    :=  STATUS_STACK_OVERFLOW
DEFINE EXCEPTION_INVALID_DISPOSITION           :=  STATUS_INVALID_DISPOSITION
DEFINE EXCEPTION_GUARD_PAGE                            :=  STATUS_GUARD_PAGE_VIOLATION
DEFINE EXCEPTION_INVALID_HANDLE            := STATUS_INVALID_HANDLE
DEFINE EXCEPTION_POSSIBLE_DEADLOCK         := STATUS_POSSIBLE_DEADLOCK
DEFINE CONTROL_C_EXIT                                      :=  STATUS_CONTROL_C_EXIT
//
// File creation flags must start at the high end since they
// are combined with the attributes
//
DEFINE FILE_FLAG_WRITE_THROUGH              := 0x80000000
DEFINE FILE_FLAG_OVERLAPPED                     := 0x40000000
DEFINE FILE_FLAG_NO_BUFFERING               := 0x20000000
DEFINE FILE_FLAG_RANDOM_ACCESS              := 0x10000000
DEFINE FILE_FLAG_SEQUENTIAL_SCAN            := 0x08000000
DEFINE FILE_FLAG_DELETE_ON_CLOSE            := 0x04000000
DEFINE FILE_FLAG_BACKUP_SEMANTICS       := 0x02000000
DEFINE FILE_FLAG_POSIX_SEMANTICS            := 0x01000000
DEFINE FILE_FLAG_OPEN_REPARSE_POINT    := 0x00200000
DEFINE FILE_FLAG_OPEN_NO_RECALL        := 0x00100000
DEFINE FILE_FLAG_FIRST_PIPE_INSTANCE   := 0x00080000
//DEFINE CREATE_NEW               := 1
//DEFINE CREATE_ALWAYS            := 2
//DEFINE OPEN_EXISTING            := 3
//DEFINE OPEN_ALWAYS              := 4
DEFINE TRUNCATE_EXISTING    := 5
//
// Define possible return codes from the CopyFileEx callback routine
//
DEFINE PROGRESS_CONTINUE   := 0
DEFINE PROGRESS_CANCEL     := 1
DEFINE PROGRESS_STOP       := 2
DEFINE PROGRESS_QUIET      := 3
//
// Define CopyFileEx callback routine state change values
//
DEFINE CALLBACK_CHUNK_FINISHED         := 0x00000000
DEFINE CALLBACK_STREAM_SWITCH          := 0x00000001
//
// Define CopyFileEx option flags
//
DEFINE COPY_FILE_FAIL_IF_EXISTS              := 0x00000001
DEFINE COPY_FILE_RESTARTABLE                 := 0x00000002
DEFINE COPY_FILE_OPEN_SOURCE_FOR_WRITE       := 0x00000004
DEFINE COPY_FILE_ALLOW_DECRYPTED_DESTINATION := 0x00000008
//
//  Gap for private copyfile flags
//
DEFINE COPY_FILE_COPY_SYMLINK                := 0x00000800
//
// Define ReplaceFile option flags
//
DEFINE REPLACEFILE_WRITE_THROUGH       := 0x00000001
DEFINE REPLACEFILE_IGNORE_MERGE_ERRORS := 0x00000002
DEFINE REPLACEFILE_IGNORE_ACL_ERRORS   := 0x00000004
//
// Define the NamedPipe definitions
//
//
// Define the dwOpenMode values for CreateNamedPipe
//
DEFINE PIPE_ACCESS_INBOUND              := 0x00000001
DEFINE PIPE_ACCESS_OUTBOUND             := 0x00000002
DEFINE PIPE_ACCESS_DUPLEX               := 0x00000003
//
// Define the Named Pipe End flags for GetNamedPipeInfo
//
DEFINE PIPE_CLIENT_END                      := 0x00000000
DEFINE PIPE_SERVER_END                      := 0x00000001
//
// Define the dwPipeMode values for CreateNamedPipe
//
DEFINE PIPE_WAIT                   := 0x00000000
DEFINE PIPE_NOWAIT                 := 0x00000001
DEFINE PIPE_READMODE_BYTE          := 0x00000000
DEFINE PIPE_READMODE_MESSAGE       := 0x00000002
DEFINE PIPE_TYPE_BYTE              := 0x00000000
DEFINE PIPE_TYPE_MESSAGE           := 0x00000004
DEFINE PIPE_ACCEPT_REMOTE_CLIENTS  := 0x00000000
DEFINE PIPE_REJECT_REMOTE_CLIENTS  := 0x00000008
//
// Define the well known values for CreateNamedPipe nMaxInstances
//
DEFINE PIPE_UNLIMITED_INSTANCES    := 255
//
// Define the Security Quality of Service bits to be passed
// into CreateFile
//
DEFINE SECURITY_CONTEXT_TRACKING  := 0x00040000
DEFINE SECURITY_EFFECTIVE_ONLY    := 0x00080000
DEFINE SECURITY_SQOS_PRESENT        :=  0x00100000
DEFINE SECURITY_VALID_SQOS_FLAGS :=  0x001F0000
//
//  File structures
//
DEFINE MUTEX_MODIFY_STATE  := MUTANT_QUERY_STATE
DEFINE MUTEX_ALL_ACCESS        := MUTANT_ALL_ACCESS
DEFINE COMMPROP_INITIALIZED  := DWORD(_CAST,   0xE73CF52E)
DEFINE DTR_CONTROL_DISABLE     := 0x00
DEFINE DTR_CONTROL_ENABLE      := 0x01
DEFINE DTR_CONTROL_HANDSHAKE   := 0x02
//
// RTS Control Flow Values
//
DEFINE RTS_CONTROL_DISABLE     := 0x00
DEFINE RTS_CONTROL_ENABLE      := 0x01
DEFINE RTS_CONTROL_HANDSHAKE   := 0x02
DEFINE RTS_CONTROL_TOGGLE      := 0x03
DEFINE GMEM_FIXED           :=     0x0000
DEFINE GMEM_MOVEABLE        :=     0x0002
DEFINE GMEM_NOCOMPACT   :=     0x0010
DEFINE GMEM_NODISCARD   :=     0x0020
DEFINE GMEM_ZEROINIT        :=     0x0040
DEFINE GMEM_MODIFY          :=     0x0080
DEFINE GMEM_DISCARDABLE  :=    0x0100
DEFINE GMEM_NOT_BANKED  :=     0x1000
DEFINE GMEM_SHARE           :=     0x2000
DEFINE GMEM_DDESHARE        :=     0x2000
DEFINE GMEM_NOTIFY          :=     0x4000
DEFINE GMEM_LOWER           :=     GMEM_NOT_BANKED
DEFINE GMEM_VALID_FLAGS  :=    0x7F72
DEFINE GMEM_INVALID_HANDLE := 0x8000
DEFINE GHND                := (GMEM_MOVEABLE | GMEM_ZEROINIT)
DEFINE GPTR                := (GMEM_FIXED | GMEM_ZEROINIT)
DEFINE GMEM_DISCARDED  :=      0x4000
DEFINE GMEM_LOCKCOUNT  :=      0x00FF
DEFINE LMEM_FIXED          :=   0x0000
DEFINE LMEM_MOVEABLE       :=      0x0002
DEFINE LMEM_NOCOMPACT  :=      0x0010
DEFINE LMEM_NODISCARD  :=      0x0020
DEFINE LMEM_ZEROINIT       :=      0x0040
DEFINE LMEM_MODIFY         :=      0x0080
DEFINE LMEM_DISCARDABLE :=     0x0F00
DEFINE LMEM_VALID_FLAGS :=     0x0F72
DEFINE LMEM_INVALID_HANDLE := 0x8000
DEFINE LHND                := (LMEM_MOVEABLE | LMEM_ZEROINIT)
DEFINE LPTR                := (LMEM_FIXED | LMEM_ZEROINIT)
DEFINE NONZEROLHND              := (LMEM_MOVEABLE)
DEFINE NONZEROLPTR              := (LMEM_FIXED)
DEFINE LMEM_DISCARDED  :=      0x4000
DEFINE LMEM_LOCKCOUNT  :=      0x00FF
DEFINE DEBUG_PROCESS                           :=  0x00000001
DEFINE DEBUG_ONLY_THIS_PROCESS     :=  0x00000002
DEFINE CREATE_SUSPENDED                    :=  0x00000004
DEFINE DETACHED_PROCESS                    :=  0x00000008
DEFINE CREATE_NEW_CONSOLE              :=  0x00000010
DEFINE NORMAL_PRIORITY_CLASS           :=  0x00000020
DEFINE IDLE_PRIORITY_CLASS             :=  0x00000040
DEFINE HIGH_PRIORITY_CLASS             :=  0x00000080
DEFINE REALTIME_PRIORITY_CLASS     :=  0x00000100
DEFINE CREATE_NEW_PROCESS_GROUP    :=  0x00000200
DEFINE CREATE_UNICODE_ENVIRONMENT :=   0x00000400
DEFINE CREATE_SEPARATE_WOW_VDM     :=  0x00000800
DEFINE CREATE_SHARED_WOW_VDM           :=  0x00001000
DEFINE CREATE_FORCEDOS                   := 0x00002000
DEFINE BELOW_NORMAL_PRIORITY_CLASS       := 0x00004000
DEFINE ABOVE_NORMAL_PRIORITY_CLASS       := 0x00008000
DEFINE STACK_SIZE_PARAM_IS_A_RESERVATION := 0x00010000    // Threads only
DEFINE INHERIT_CALLER_PRIORITY           := 0x00020000
DEFINE CREATE_PROTECTED_PROCESS          := 0x00040000
DEFINE EXTENDED_STARTUPINFO_PRESENT      := 0x00080000
DEFINE PROCESS_MODE_BACKGROUND_BEGIN     := 0x00100000
DEFINE PROCESS_MODE_BACKGROUND_END       := 0x00200000
DEFINE CREATE_BREAKAWAY_FROM_JOB         := 0x01000000
DEFINE CREATE_PRESERVE_CODE_AUTHZ_LEVEL  := 0x02000000
DEFINE CREATE_DEFAULT_ERROR_MODE   :=  0x04000000
DEFINE CREATE_NO_WINDOW                    :=  0x08000000
DEFINE PROFILE_USER                            :=  0x10000000
DEFINE PROFILE_KERNEL                      :=  0x20000000
DEFINE PROFILE_SERVER                      :=  0x40000000
DEFINE CREATE_IGNORE_SYSTEM_DEFAULT      := 0x80000000
DEFINE THREAD_PRIORITY_LOWEST      :=          THREAD_BASE_PRIORITY_MIN
DEFINE THREAD_PRIORITY_BELOW_NORMAL  :=    (THREAD_PRIORITY_LOWEST+1)
DEFINE THREAD_PRIORITY_NORMAL           :=     0
DEFINE THREAD_PRIORITY_HIGHEST          :=     THREAD_BASE_PRIORITY_MAX
DEFINE THREAD_PRIORITY_ABOVE_NORMAL  :=    (THREAD_PRIORITY_HIGHEST-1)
DEFINE THREAD_PRIORITY_ERROR_RETURN  :=    (MAXLONG)
DEFINE THREAD_PRIORITY_TIME_CRITICAL :=    THREAD_BASE_PRIORITY_LOWRT
DEFINE THREAD_PRIORITY_IDLE                 :=     THREAD_BASE_PRIORITY_IDLE
DEFINE THREAD_MODE_BACKGROUND_BEGIN    := 0x00010000
DEFINE THREAD_MODE_BACKGROUND_END      := 0x00020000
//
// GetFinalPathNameByHandle
//
DEFINE VOLUME_NAME_DOS  := 0x0      //default
DEFINE VOLUME_NAME_GUID := 0x1
DEFINE VOLUME_NAME_NT   := 0x2
DEFINE VOLUME_NAME_NONE := 0x4
DEFINE FILE_NAME_NORMALIZED := 0x0  //default
DEFINE FILE_NAME_OPENED     := 0x8
//
// Debug APIs
//
DEFINE EXCEPTION_DEBUG_EVENT            := 1
DEFINE CREATE_THREAD_DEBUG_EVENT    := 2
DEFINE CREATE_PROCESS_DEBUG_EVENT  := 3
DEFINE EXIT_THREAD_DEBUG_EVENT      := 4
DEFINE EXIT_PROCESS_DEBUG_EVENT     := 5
DEFINE LOAD_DLL_DEBUG_EVENT             := 6
DEFINE UNLOAD_DLL_DEBUG_EVENT       := 7
DEFINE OUTPUT_DEBUG_STRING_EVENT    := 8
DEFINE RIP_EVENT                                    := 9
DEFINE DRIVE_UNKNOWN        :=  0
DEFINE DRIVE_NO_ROOT_DIR :=  1
DEFINE DRIVE_REMOVABLE  :=  2
DEFINE DRIVE_FIXED          :=  3
DEFINE DRIVE_REMOTE         :=  4
DEFINE DRIVE_CDROM          :=  5
DEFINE DRIVE_RAMDISK        :=  6
DEFINE FILE_TYPE_UNKNOWN   :=   0x0000
DEFINE FILE_TYPE_DISK      :=   0x0001
DEFINE FILE_TYPE_CHAR      :=   0x0002
DEFINE FILE_TYPE_PIPE      :=   0x0003
DEFINE FILE_TYPE_REMOTE    :=   0x8000
DEFINE STD_INPUT_HANDLE    := 0xFFFFFFF6
DEFINE STD_OUTPUT_HANDLE   := 0xFFFFFFF5
DEFINE STD_ERROR_HANDLE    := 0xFFFFFFF4
DEFINE NOPARITY                     :=  0
DEFINE ODDPARITY                    :=  1
DEFINE EVENPARITY               :=  2
DEFINE MARKPARITY               :=  3
DEFINE SPACEPARITY              :=  4
DEFINE ONESTOPBIT               :=  0
DEFINE ONE5STOPBITS             :=  1
DEFINE TWOSTOPBITS              :=  2
DEFINE IGNORE                       :=  0
DEFINE INFINITE                     :=  DWORD(_CAST, 0xFFFFFFFF )
//
// Baud rates at which the communication device operates
//
DEFINE CBR_110                      :=  110
DEFINE CBR_300                      :=  300
DEFINE CBR_600                      :=  600
DEFINE CBR_1200                     :=  1200
DEFINE CBR_2400                     :=  2400
DEFINE CBR_4800                     :=  4800
DEFINE CBR_9600                     :=  9600
DEFINE CBR_14400                    :=  14400
DEFINE CBR_19200                    :=  19200
DEFINE CBR_38400                    :=  38400
DEFINE CBR_56000                    :=  56000
DEFINE CBR_57600                    :=  57600
DEFINE CBR_115200               :=  115200
DEFINE CBR_128000               :=  128000
DEFINE CBR_256000               :=  256000
DEFINE CBR_28800                    :=  28800
//
// Error Flags
//
DEFINE CE_RXOVER                   :=   0x0001
DEFINE CE_OVERRUN              :=   0x0002
DEFINE CE_RXPARITY             :=   0x0004
DEFINE CE_FRAME                    :=   0x0008
DEFINE CE_BREAK                    :=   0x0010
DEFINE CE_TXFULL                   :=   0x0100
DEFINE CE_PTO                      :=   0x0200
DEFINE CE_IOE                      :=   0x0400
DEFINE CE_DNS                      :=   0x0800
DEFINE CE_OOP                      :=   0x1000
DEFINE CE_MODE                     :=   0x8000
DEFINE IE_BADID                    :=   (-1)
DEFINE IE_OPEN                     :=   (-2)
DEFINE IE_NOPEN                    :=   (-3)
DEFINE IE_MEMORY                   :=   (-4)
DEFINE IE_DEFAULT              :=   (-5)
DEFINE IE_HARDWARE             :=   (-10)
DEFINE IE_BYTESIZE             :=   (-11)
DEFINE IE_BAUDRATE             :=   (-12)
DEFINE EV_RXCHAR                   :=   0x0001
DEFINE EV_RXFLAG                   :=   0x0002
DEFINE EV_TXEMPTY              :=   0x0004
DEFINE EV_CTS                      :=   0x0008
DEFINE EV_DSR                      :=   0x0010
DEFINE EV_RLSD                     :=   0x0020
DEFINE EV_BREAK                    :=   0x0040
DEFINE EV_ERR                      :=   0x0080
DEFINE EV_RING                     :=   0x0100
DEFINE EV_PERR                     :=   0x0200
DEFINE EV_RX80FULL             :=   0x0400
DEFINE EV_EVENT1                   :=   0x0800
DEFINE EV_EVENT2                   :=   0x1000
DEFINE SETXOFF                     :=   1
DEFINE SETXON                      :=   2
DEFINE SETRTS                          :=   3
DEFINE CLRRTS                       :=     4
DEFINE SETDTR                       :=     5
DEFINE CLRDTR                      :=   6
DEFINE RESETDEV             :=     7
DEFINE SETBREAK             :=     8
DEFINE CLRBREAK             :=     9
DEFINE PURGE_TXABORT           :=   0x0001
DEFINE PURGE_RXABORT           :=   0x0002
DEFINE PURGE_TXCLEAR           :=   0x0004
DEFINE PURGE_RXCLEAR           :=   0x0008
DEFINE LPTx                            :=   0x80
DEFINE MS_CTS_ON                    :=  DWORD (_CAST, 0x0010)
DEFINE MS_DSR_ON                    :=  DWORD (_CAST, 0x0020)
DEFINE MS_RING_ON               :=  DWORD (_CAST, 0x0040)
DEFINE MS_RLSD_ON               :=  DWORD (_CAST, 0x0080)
DEFINE S_QUEUEEMPTY             := 0
DEFINE S_THRESHOLD                  :=  1
DEFINE S_ALLTHRESHOLD  :=  2
DEFINE S_NORMAL            := 0
DEFINE S_LEGATO             := 1
DEFINE S_STACCATO  := 2
DEFINE S_PERIOD512                 := 0
DEFINE S_PERIOD1024             := 1
DEFINE S_PERIOD2048             := 2
DEFINE S_PERIODVOICE   := 3
DEFINE S_WHITE512                  := 4
DEFINE S_WHITE1024              := 5
DEFINE S_WHITE2048              := 6
DEFINE S_WHITEVOICE    := 7
DEFINE S_SERDVNA                    := (-1)
DEFINE S_SEROFM                         := (-2)
DEFINE S_SERMACT                    := (-3)
DEFINE S_SERQFUL                       := (-4)
DEFINE S_SERBDNT                    := (-5)
DEFINE S_SERDLN                         := (-6)
DEFINE S_SERDCC                         := (-7)
DEFINE S_SERDTP                         := (-8)
DEFINE S_SERDVL                         := (-9)
DEFINE S_SERDMD                        := (-10)
DEFINE S_SERDSH                         := (-11)
DEFINE S_SERDPT                         := (-12)
DEFINE S_SERDFQ                         := (-13)
DEFINE S_SERDDR                        := (-14)
DEFINE S_SERDSR                         := (-15)
DEFINE S_SERDST                         := (-16)
DEFINE NMPWAIT_WAIT_FOREVER                                    :=  0xffffffff
DEFINE NMPWAIT_NOWAIT                                                               :=  0x00000001
DEFINE NMPWAIT_USE_DEFAULT_WAIT             :=  0x00000000
DEFINE FS_CASE_IS_PRESERVED                                                :=  FILE_CASE_PRESERVED_NAMES
DEFINE FS_CASE_SENSITIVE                                                                   :=  FILE_CASE_SENSITIVE_SEARCH
DEFINE FS_UNICODE_STORED_ON_DISK               :=  FILE_UNICODE_ON_DISK
DEFINE FS_PERSISTENT_ACLS                                                       :=  FILE_PERSISTENT_ACLS
DEFINE FS_VOL_IS_COMPRESSED                                             :=  FILE_VOLUME_IS_COMPRESSED
DEFINE FS_FILE_COMPRESSION                                                  :=  FILE_FILE_COMPRESSION
DEFINE FILE_MAP_COPY                                   := SECTION_QUERY
DEFINE FILE_MAP_WRITE                          := SECTION_MAP_WRITE
DEFINE FILE_MAP_READ                                   := SECTION_MAP_READ
DEFINE FILE_MAP_ALL_ACCESS  := SECTION_ALL_ACCESS
DEFINE OF_READ                                                                      := 0x00000000
DEFINE OF_WRITE                                                                 := 0x00000001
DEFINE OF_READWRITE                                         := 0x00000002
//DEFINE OF_SHARE_COMPAT                         := 0x00000000 
//DEFINE OF_SHARE_EXCLUSIVE           := 0x00000010
//DEFINE OF_SHARE_DENY_WRITE     := 0x00000020
//DEFINE OF_SHARE_DENY_READ          := 0x00000030
//DEFINE OF_SHARE_DENY_NONE       := 0x00000040
DEFINE OF_PARSE                                                                     := 0x00000100
DEFINE OF_DELETE                                                                := 0x00000200
DEFINE OF_VERIFY                                                                   := 0x00000400
DEFINE OF_CANCEL                                                               := 0x00000800
DEFINE OF_CREATE                                                               := 0x00001000
DEFINE OF_PROMPT                                                            := 0x00002000
DEFINE OF_EXIST                                                                     := 0x00004000
DEFINE OF_REOPEN                                                            := 0x00008000
DEFINE OFS_MAXPATHNAME                     := 128
DEFINE MAXINTATOM := 0xC000
// where does this come from ???
//_DLL FUNC WinMain(hInstance AS PTR, hPrevInstance AS PTR,;
//                 lpCmdLine AS PSZ, nShorCmd AS INT) AS INT PASCAL:MFC30D.WinMain
DEFINE PROCESS_HEAP_REGION                                                                          := 0x0001
DEFINE PROCESS_HEAP_UNCOMMITTED_RANGE      :=  0x0002
DEFINE PROCESS_HEAP_ENTRY_BUSY                                                     :=  0x0004
DEFINE PROCESS_HEAP_ENTRY_MOVEABLE                             :=  0x0010
DEFINE PROCESS_HEAP_ENTRY_DDESHARE                             :=  0x0020
DEFINE SCS_32BIT_BINARY             := 0
DEFINE SCS_DOS_BINARY               := 1
DEFINE SCS_WOW_BINARY          := 2
DEFINE SCS_PIF_BINARY                   := 3
DEFINE SCS_POSIX_BINARY        := 4
DEFINE SCS_OS216_BINARY            := 5
DEFINE SEM_FAILCRITICALERRORS                                              := 0x0001
DEFINE SEM_NOGPFAULTERRORBOX                                        := 0x0002
DEFINE SEM_NOALIGNMENTFAULTEXCEPT           := 0x0004
DEFINE SEM_NOOPENFILEERRORBOX                                   := 0x8000
DEFINE LOCKFILE_FAIL_IMMEDIATELY    := 0x00000001
DEFINE LOCKFILE_EXCLUSIVE_LOCK          := 0x00000002
DEFINE HANDLE_FLAG_INHERIT                                                                      := 0x00000001
DEFINE HANDLE_FLAG_PROTECT_FROM_CLOSE      := 0x00000002
DEFINE HINSTANCE_ERROR                                                                                          := 32
DEFINE GET_TAPE_MEDIA_INFORMATION           := 0
DEFINE GET_TAPE_DRIVE_INFORMATION              := 1
DEFINE SET_TAPE_MEDIA_INFORMATION  := 0
DEFINE SET_TAPE_DRIVE_INFORMATION  := 1
DEFINE FORMAT_MESSAGE_ALLOCATE_BUFFER  := 0x00000100
DEFINE FORMAT_MESSAGE_IGNORE_INSERTS            := 0x00000200
DEFINE FORMAT_MESSAGE_FROM_STRING                       := 0x00000400
DEFINE FORMAT_MESSAGE_FROM_HMODULE             := 0x00000800
DEFINE FORMAT_MESSAGE_FROM_SYSTEM                      := 0x00001000
DEFINE FORMAT_MESSAGE_ARGUMENT_ARRAY   := 0x00002000
DEFINE FORMAT_MESSAGE_MAX_WIDTH_MASK   := 0x000000FF
DEFINE TLS_OUT_OF_INDEXES   := DWORD(_CAST,0xFFFFFFFF)
DEFINE BACKUP_INVALID                                                      := 0x00000000
DEFINE BACKUP_DATA                                                                     := 0x00000001
DEFINE BACKUP_EA_DATA                                                   := 0x00000002
DEFINE BACKUP_SECURITY_DATA                     := 0x00000003
DEFINE BACKUP_ALTERNATE_DATA               := 0x00000004
DEFINE BACKUP_LINK                                                                         := 0x00000005
DEFINE BACKUP_PROPERTY_DATA                    := 0x00000006
DEFINE STREAM_NORMAL_ATTRIBUTE                      := 0x00000000
DEFINE STREAM_MODIFIED_WHEN_READ               := 0x00000001
DEFINE STREAM_CONTAINS_SECURITY                        := 0x00000002
DEFINE STREAM_CONTAINS_PROPERTIES           := 0x00000004
DEFINE STARTF_USESHOWWINDOW     := 0x00000001
DEFINE STARTF_USESIZE                                                   := 0x00000002
DEFINE STARTF_USEPOSITION                           := 0x00000004
DEFINE STARTF_USECOUNTCHARS            := 0x00000008
DEFINE STARTF_USEFILLATTRIBUTE  := 0x00000010
DEFINE STARTF_RUNFULLSCREEN                := 0x00000020
DEFINE STARTF_FORCEONFEEDBACK  := 0x00000040
DEFINE STARTF_FORCEOFFFEEDBACK := 0x00000080
DEFINE STARTF_USESTDHANDLES                 := 0x00000100
DEFINE STARTF_USEHOTKEY                                         := 0x00000200
DEFINE SHUTDOWN_NORETRY                             := 0x00000001
DEFINE DONT_RESOLVE_DLL_REFERENCES                      := 0x00000001
DEFINE LOAD_LIBRARY_AS_DATAFILE                                            := 0x00000002
DEFINE LOAD_WITH_ALTERED_SEARCH_PATH        := 0x00000008
DEFINE DDD_RAW_TARGET_PATH                                          := 0x00000001
DEFINE DDD_REMOVE_DEFINITION                                       := 0x00000002
DEFINE DDD_EXACT_MATCH_ON_REMOVE        := 0x00000004
DEFINE MOVEFILE_REPLACE_EXISTING                       := 0x00000001
DEFINE MOVEFILE_COPY_ALLOWED                                    := 0x00000002
DEFINE MOVEFILE_DELAY_UNTIL_REBOOT     := 0x00000004
DEFINE MAX_COMPUTERNAME_LENGTH := 15
DEFINE LOGON32_LOGON_INTERACTIVE    := 2
DEFINE LOGON32_LOGON_BATCH              := 4
DEFINE LOGON32_LOGON_SERVICE            := 5
DEFINE LOGON32_PROVIDER_DEFAULT     := 0
DEFINE LOGON32_PROVIDER_WINNT35     := 1
DEFINE VER_PLATFORM_WIN32s                      := 0
DEFINE VER_PLATFORM_WIN32_WINDOWS       := 1
DEFINE VER_PLATFORM_WIN32_NT                    := 2
DEFINE TC_NORMAL            :=  0
DEFINE TC_HARDERR       := 1
DEFINE TC_GP_TRAP       := 2
DEFINE TC_SIGNAL                := 3
DEFINE AC_LINE_OFFLINE                              := 0x00
DEFINE AC_LINE_ONLINE                               := 0x01
DEFINE AC_LINE_BACKUP_POWER                     := 0x02
DEFINE AC_LINE_UNKNOWN                              := 0xFF
DEFINE BATTERY_FLAG_HIGH                            := 0x01
DEFINE BATTERY_FLAG_LOW                             := 0x02
DEFINE BATTERY_FLAG_CRITICAL                    := 0x04
DEFINE BATTERY_FLAG_CHARGING                    := 0x08
DEFINE BATTERY_FLAG_NO_BATTERY              := 0x80
DEFINE BATTERY_FLAG_UNKNOWN                     := 0xFF
DEFINE BATTERY_PERCENTAGE_UNKNOWN       := 0xFF
DEFINE BATTERY_LIFE_UNKNOWN             := 0xFFFFFFFF
DEFINE VER_NT_WORKSTATION              := 0x0000001
DEFINE VER_NT_DOMAIN_CONTROLLER        := 0x0000002
DEFINE VER_NT_SERVER                   := 0x0000003
DEFINE VER_SERVER_NT                       := 0x80000000
DEFINE VER_WORKSTATION_NT                  := 0x40000000
// Suite Masks
DEFINE VER_SUITE_SMALLBUSINESS             := 0x00000001
DEFINE VER_SUITE_ENTERPRISE                := 0x00000002
DEFINE VER_SUITE_BACKOFFICE                := 0x00000004
DEFINE VER_SUITE_COMMUNICATIONS            := 0x00000008
DEFINE VER_SUITE_TERMINAL                  := 0x00000010
DEFINE VER_SUITE_SMALLBUSINESS_RESTRICTED  := 0x00000020
DEFINE VER_SUITE_EMBEDDEDNT                := 0x00000040
DEFINE VER_SUITE_DATACENTER                := 0x00000080
DEFINE VER_SUITE_SINGLEUSERTS              := 0x00000100
DEFINE VER_SUITE_PERSONAL                  := 0x00000200
DEFINE VER_SUITE_BLADE                     := 0x00000400
DEFINE VER_SUITE_EMBEDDED_RESTRICTED        := 0x00000800
DEFINE VER_SUITE_SECURITY_APPLIANCE         := 0x00001000
DEFINE VER_SUITE_STORAGE_SERVER             := 0x00002000
DEFINE VER_SUITE_COMPUTE_SERVER             := 0x00004000
DEFINE VER_SUITE_WH_SERVER                 :=  0x00008000
DEFINE PRODUCT_BUSINESS       := 0x00000006
DEFINE PRODUCT_BUSINESS_N      := 0x00000010
DEFINE PRODUCT_CLUSTER_SERVER     := 0x00000012
DEFINE PRODUCT_DATACENTER_SERVER    := 0x00000008
DEFINE PRODUCT_DATACENTER_SERVER_CORE  := 0x0000000C
DEFINE PRODUCT_DATACENTER_SERVER_CORE_V           := 0x00000027
DEFINE PRODUCT_DATACENTER_SERVER_V                := 0x00000025
DEFINE PRODUCT_ENTERPRISE      := 0x00000004
DEFINE PRODUCT_ENTERPRISE_N     := 0x0000001B
DEFINE PRODUCT_ENTERPRISE_SERVER    := 0x0000000A
DEFINE PRODUCT_ENTERPRISE_SERVER_CORE  := 0x0000000E
DEFINE PRODUCT_ENTERPRISE_SERVER_CORE_V           := 0x00000029
DEFINE PRODUCT_ENTERPRISE_SERVER_IA64  := 0x0000000F
DEFINE PRODUCT_ENTERPRISE_SERVER_V                := 0x00000026
DEFINE PRODUCT_HOME_BASIC      := 0x00000002
DEFINE PRODUCT_HOME_BASIC_N     := 0x00000005
DEFINE PRODUCT_HOME_PREMIUM     := 0x00000003
DEFINE PRODUCT_HOME_PREMIUM_N     := 0x0000001A
DEFINE PRODUCT_HOME_SERVER      := 0x00000013
DEFINE PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT   := 0x0000001E
DEFINE PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING    := 0x00000020
DEFINE PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY     := 0x0000001F
DEFINE PRODUCT_SERVER_FOR_SMALLBUSINESS      := 0x00000018
DEFINE PRODUCT_SMALLBUSINESS_SERVER    := 0x00000009
DEFINE PRODUCT_SMALLBUSINESS_SERVER_PREMIUM := 0x00000019
DEFINE PRODUCT_STANDARD_SERVER     := 0x00000007
DEFINE PRODUCT_STANDARD_SERVER_CORE    := 0x0000000D
DEFINE PRODUCT_STANDARD_SERVER_CORE_V             := 0x00000028
DEFINE PRODUCT_STANDARD_SERVER_V                  := 0x00000024
DEFINE PRODUCT_STARTER        := 0x0000000B
DEFINE PRODUCT_STORAGE_ENTERPRISE_SERVER  := 0x00000017
DEFINE PRODUCT_STORAGE_EXPRESS_SERVER   := 0x00000014
DEFINE PRODUCT_STORAGE_STANDARD_SERVER   := 0x00000015
DEFINE PRODUCT_STORAGE_WORKGROUP_SERVER  := 0x00000016
DEFINE PRODUCT_UNDEFINED       := 0x00000000
DEFINE PRODUCT_ULTIMATE        := 0x00000001
DEFINE PRODUCT_ULTIMATE_N           := 0x0000001C
DEFINE PRODUCT_WEB_SERVER       := 0x00000011
DEFINE PRODUCT_WEB_SERVER_CORE     := 0x0000001D    
DEFINE PRODUCT_SMALLBUSINESS_SERVER_PRIME         := 0x00000021
DEFINE PRODUCT_HOME_PREMIUM_SERVER                := 0x00000022
DEFINE PRODUCT_SERVER_FOR_SMALLBUSINESS_V         := 0x00000023
DEFINE PRODUCT_HYPERV                             := 0x0000002A
DEFINE PRODUCT_UNLICENSED       := 0xABCDABCD
#endregion
