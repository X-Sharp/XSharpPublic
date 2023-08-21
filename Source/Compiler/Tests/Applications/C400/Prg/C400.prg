// 400. error XS0535: 'cIDataObject' does not implement interface member 'IDataObject.GetData(ref FORMATETC, out STGMEDIUM)'
// Vulcan allows implementing OUT parameters with REF (because vulcan does not support OUT)
// Maybe we should allow this with a warning?

// note that after changing the code to use OUT instead of REF, then there are errors
// (as expected) about needing to assign a value to the OUT param

#using System.Runtime.InteropServices
#using System.Runtime.InteropServices.ComTypes
#define E_FAIL 0
FUNCTION Start() AS VOID
LOCAL o AS cIDataObject
o := cIDataObject{}
o:DUnadvise(1)
	
CLASS cIDataObject IMPLEMENTS IDataObject
// connection OUT LONGINT
METHOD DAdvise(pFormatet REF FORMATETC, advf AS ADVF, advisesink AS IAdviseSink, connection REF LONGINT) AS LONGINT
RETURN E_FAIL   
METHOD DUnadvise(connection AS LONGINT) AS VOID
RETURN
METHOD EnumDAdvise(enumAdvise REF IEnumSTATDATA) AS LONGINT // OUT
RETURN E_FAIL
METHOD EnumFormatEtc(direction AS DATADIR ) AS IEnumFORMATETC
RETURN NIL
METHOD GetCanonicalFormatEtc(formatIn REF FORMATETC,formatOut REF FORMATETC) AS LONGINT // OUT FORMATETC
RETURN E_FAIL
METHOD GetData(format REF FORMATETC, medium REF STGMEDIUM ) AS VOID // OUT STGMEDIUM
RETURN                                                                
METHOD GetDataHere(format REF FORMATETC, medium REF STGMEDIUM) AS VOID
RETURN     
METHOD QueryGetData(format REF FORMATETC ) AS LONGINT
RETURN E_FAIL 
METHOD SetData(formatIn REF FORMATETC, medium REF STGMEDIUM , release AS LOGIC ) AS VOID
RETURN	
END CLASS

