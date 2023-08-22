// R757: Size declared in VoStructAttribute is calculated incorrectly for structures with PSZ members 
// https://github.com/X-Sharp/XSharpPublic/issues/547
// Compile calculates [VoStruct(40, 4)]  but this should be [VOStruct(84, 4). The PSZ members are not processed correctly
FUNCTION Start( ) AS VOID
    ? _sizeof(_winPRINTER_INFO_2)
    ? _sizeof(_WINDEVMODE)
    ? _sizeof(_WINSECURITY_DESCRIPTOR)
    ? _sizeof(_WINACL)
    xAssert(_sizeof(_winPRINTER_INFO_2)= 84)
    xAssert(_sizeof(_WINDEVMODE)= 156)
    xAssert(_sizeof(_WINSECURITY_DESCRIPTOR)= 20)
    xAssert(_sizeof(_WINACL)= 8)
RETURN


VOSTRUCT _winPRINTER_INFO_2
	MEMBER  pServerName AS PSZ
	MEMBER pPrinterName AS PSZ
	MEMBER pShareName AS PSZ
	MEMBER pPortName AS PSZ
	MEMBER pDriverName AS PSZ
	MEMBER pComment AS PSZ
	MEMBER pLocation AS PSZ
	MEMBER pDevMode AS _winDEVMODE
	MEMBER pSepFile AS PSZ
	MEMBER pPrintProcessor AS PSZ
	MEMBER pDatatype AS PSZ
	MEMBER pParameters AS PSZ
	MEMBER pSecurityDescriptor AS _winSECURITY_DESCRIPTOR
	MEMBER Attributes AS DWORD
	MEMBER Priority AS DWORD
	MEMBER DefaultPriority AS DWORD
	MEMBER StartTime AS DWORD
	MEMBER UntilTime AS DWORD
	MEMBER Status AS DWORD
	MEMBER cJobs AS DWORD
	MEMBER AveragePPM AS DWORD
DEFINE CCHDEVICENAME := 32
DEFINE CCHFORMNAME := 32
VOSTRUCT _WINDEVMODE
	MEMBER   DIM dmDeviceName[CCHDEVICENAME] AS BYTE
	MEMBER   dmSpecVersion AS WORD
	MEMBER   dmDriverVersion AS WORD
	MEMBER   dmSize AS WORD
	MEMBER   dmDriverExtra AS WORD
	MEMBER   dmFields AS DWORD
	MEMBER   dmOrientation AS SHORTINT
	MEMBER   dmPaperSize AS SHORTINT
	MEMBER   dmPaperLength AS SHORTINT
	MEMBER   dmPaperWidth AS SHORTINT
	MEMBER   dmScale AS SHORTINT
	MEMBER   dmCopies AS SHORTINT
	MEMBER   dmDefaultSource AS SHORTINT
	MEMBER   dmPrintQuality AS SHORTINT
	MEMBER   dmColor AS SHORTINT
	MEMBER   dmDuplex AS SHORTINT
	MEMBER   dmYResolution AS SHORTINT
	MEMBER   dmTTOption AS SHORTINT
	MEMBER   dmCollate AS SHORTINT
	MEMBER   DIM dmFormName[CCHFORMNAME] AS BYTE
	MEMBER   dmLogPixels AS WORD
	MEMBER   dmBitsPerPel AS DWORD
	MEMBER   dmPelsWidth AS DWORD
	MEMBER   dmPelsHeight AS DWORD
	MEMBER   dmDisplayFlags AS DWORD
	MEMBER   dmDisplayFrequency AS DWORD
	MEMBER   dmICMMethod AS DWORD
	MEMBER   dmICMIntent AS DWORD
	MEMBER   dmMediaType AS DWORD
	MEMBER   dmDitherType AS DWORD
	MEMBER   dmReserved1 AS DWORD
	MEMBER   dmReserved2 AS DWORD
	//RvdH 070412 Added  
	MEMBER dmPanningWidth AS DWORD
   MEMBER dmPanningHeight AS DWORD
   
   
VOSTRUCT _WINSECURITY_DESCRIPTOR
	MEMBER Revision  AS BYTE
	MEMBER Sbz1 AS BYTE
	MEMBER Control AS WORD
	MEMBER Owner AS PTR
	MEMBER Group AS PTR
	MEMBER Sacl AS _WINACL	// RvdH 070411 changed from IS to AS
	MEMBER Dacl AS _WINACL	// RvdH 070411 changed from IS to AS

   
   
VOSTRUCT _WINACL
	MEMBER AclRevision AS BYTE
	MEMBER Sbz1 AS BYTE
	MEMBER AclSize AS WORD
	MEMBER AceCount AS WORD
	MEMBER Sbz2 AS WORD   
	
	
PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 	
