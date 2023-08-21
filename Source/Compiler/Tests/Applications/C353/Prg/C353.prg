// 353. error XS0029: Cannot implicitly convert type 'Vulcan.VO._winRECT*' to 'Vulcan.VO._winRECT'
#pragma warnings(219, off)
#pragma warnings(165, off) // uniassigned local
PARTIAL CLASS Window //INHERIT @@EventContext
	PROTECT strucPaintRect AS _WINRECT
	METHOD __GetPaintRect() AS _WINRECT STRICT
	//PP-030828 Strong typing
	RETURN strucPaintRect

	METHOD __BuildBuffer() AS VOID STRICT
		LOCAL strucRecord AS _WINRecordCore
		LOCAL iRecNo AS INT
		strucRecord := SELF:__GetRecordAtRecNo(iRecNo)
	RETURN

	METHOD __GetRecordAtRecNo(iRecNo AS INT) AS _WINRecordCore STRICT
		LOCAL strucRecord AS _WINRecordCore
	RETURN strucRecord
END CLASS

FUNCTION Start() AS VOID

