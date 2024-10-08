
// Class CursorAdapter  BaseClass   Cursoradapter  Class  Cursoradapter
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS CursorAdapter IMPLEMENTS IVFPObject
#include "VFPObject.xh"
PROPERTY ADOCodePage AS LONG AUTO
PROPERTY Alias AS USUAL AUTO
PROPERTY AllowDelete AS LOGIC AUTO
PROPERTY AllowInsert AS LOGIC AUTO
PROPERTY AllowSimultaneousFetch AS LOGIC AUTO
PROPERTY AllowUpdate AS LOGIC AUTO
METHOD AutoOpen AS LOGIC CLIPPER
RETURN FALSE
PROPERTY BatchUpdateCount  AS USUAL AUTO
PROPERTY BreakOnError AS LOGIC AUTO
PROPERTY BufferModeOverride AS LONG AUTO
PROPERTY CompareMemo  AS LOGIC AUTO
PROPERTY ConflictCheckCmd AS STRING AUTO
PROPERTY ConflictCheckType AS LONG AUTO
PROPERTY ConversionFunc AS STRING AUTO
METHOD CursorAttach(cAlias , lInheritCursorProperties )  AS LOGIC CLIPPER
RETURN FALSE
METHOD CursorDetach  AS LOGIC CLIPPER
RETURN FALSE
METHOD CursorFill (lUseCursorSchema , lNoData , nOptions , Source ) AS LOGIC CLIPPER
RETURN FALSE
METHOD CursorRefresh AS LOGIC CLIPPER
RETURN FALSE
PROPERTY CursorSchema  AS STRING AUTO
PROPERTY CursorStatus  AS LONG AUTO
PROPERTY DataSource  AS STRING AUTO
PROPERTY DataSourceType   AS STRING AUTO
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
