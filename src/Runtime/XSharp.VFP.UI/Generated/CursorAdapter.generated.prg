
// Class CursorAdapter  BaseClass   Cursoradapter  Class  Cursoradapter
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS CursorAdapter IMPLEMENTS IVFPObject
#include "VFPObject.xh"
PROPERTY ADOCODEPAGE AS USUAL AUTO
PROPERTY Alias AS USUAL AUTO
PROPERTY AllowDelete AS LOGIC AUTO
PROPERTY AllowInsert AS LOGIC AUTO
PROPERTY ALLOWSIMULTANEOUSFETCH AS LOGIC AUTO
PROPERTY AllowUpdate AS LOGIC AUTO
METHOD AUTOOPEN AS USUAL CLIPPER
RETURN NIL
PROPERTY BATCHUPDATECOUNT AS USUAL AUTO
PROPERTY BREAKONERROR AS USUAL AUTO
PROPERTY BUFFERMODEOVERRIDE AS USUAL AUTO
PROPERTY COMPAREMEMO AS USUAL AUTO
PROPERTY CONFLICTCHECKCMD AS USUAL AUTO
PROPERTY CONFLICTCHECKTYPE AS USUAL AUTO
PROPERTY CONVERSIONFUNC AS USUAL AUTO
METHOD CURSORATTACH AS USUAL CLIPPER
RETURN NIL
METHOD CURSORDETACH AS USUAL CLIPPER
RETURN NIL
METHOD CURSORFILL AS USUAL CLIPPER
RETURN NIL
METHOD CURSORREFRESH AS USUAL CLIPPER
RETURN NIL
PROPERTY CURSORSCHEMA AS USUAL AUTO
PROPERTY CURSORSTATUS AS USUAL AUTO
PROPERTY DATASOURCE AS USUAL AUTO
PROPERTY DATASOURCETYPE AS USUAL AUTO
PROPERTY DELETECMD AS USUAL AUTO
PROPERTY DELETECMDDATASOURCE AS USUAL AUTO
PROPERTY DELETECMDDATASOURCETYPE AS USUAL AUTO
PROPERTY FETCHASNEEDED AS USUAL AUTO
PROPERTY FETCHMEMO AS USUAL AUTO
PROPERTY FETCHMEMOCMDLIST AS USUAL AUTO
PROPERTY FETCHMEMODATASOURCE AS USUAL AUTO
PROPERTY FETCHMEMODATASOURCETYPE AS USUAL AUTO
PROPERTY FETCHSIZE AS USUAL AUTO
PROPERTY FLAGS AS USUAL AUTO
PROPERTY INSERTCMD AS USUAL AUTO
PROPERTY INSERTCMDDATASOURCE AS USUAL AUTO
PROPERTY INSERTCMDDATASOURCETYPE AS USUAL AUTO
PROPERTY INSERTCMDREFRESHCMD AS USUAL AUTO
PROPERTY INSERTCMDREFRESHFIELDLIST AS USUAL AUTO
PROPERTY INSERTCMDREFRESHKEYFIELDLIST AS USUAL AUTO
PROPERTY KEYFIELDLIST AS USUAL AUTO
PROPERTY MAPBINARY AS USUAL AUTO
PROPERTY MAPVARCHAR AS USUAL AUTO
PROPERTY MAXRECORDS AS USUAL AUTO
PROPERTY NODATA AS USUAL AUTO
PROPERTY PREPARED AS USUAL AUTO
METHOD RECORDREFRESH AS USUAL CLIPPER
RETURN NIL
PROPERTY SELECTCMD AS USUAL AUTO
PROPERTY SENDUPDATES AS USUAL AUTO
PROPERTY TABLES AS USUAL AUTO
PROPERTY TIMESTAMPFIELDLIST AS USUAL AUTO
PROPERTY UPDATABLEFIELDLIST AS USUAL AUTO
PROPERTY UPDATECMD AS USUAL AUTO
PROPERTY UPDATECMDDATASOURCE AS USUAL AUTO
PROPERTY UPDATECMDDATASOURCETYPE AS USUAL AUTO
PROPERTY UPDATECMDREFRESHCMD AS USUAL AUTO
PROPERTY UPDATECMDREFRESHFIELDLIST AS USUAL AUTO
PROPERTY UPDATECMDREFRESHKEYFIELDLIST AS USUAL AUTO
PROPERTY UPDATEGRAM AS USUAL AUTO
PROPERTY UPDATEGRAMSCHEMALOCATION AS USUAL AUTO
PROPERTY UPDATENAMELIST AS USUAL AUTO
PROPERTY UPDATETYPE AS USUAL AUTO
PROPERTY USECURSORSCHEMA AS USUAL AUTO
PROPERTY USEDEDATASOURCE AS USUAL AUTO
PROPERTY USEMEMOSIZE AS USUAL AUTO
PROPERTY USETRANSACTIONS AS USUAL AUTO
PROPERTY WHERETYPE AS USUAL AUTO
END CLASS
END NAMESPACE      