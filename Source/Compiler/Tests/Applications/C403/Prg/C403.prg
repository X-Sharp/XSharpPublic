// 403. error XS0102: The type 'RpReportDesigner' already contains a definition for 'PrinterDevice'
PARTIAL CLASS RpReportDesigner
	EXPORT oPrinter AS OBJECT
	ACCESS PrinterDevice 
	RETURN SELF:oPrinter
END CLASS

FUNCTION Start( ) AS VOID
	LOCAL o AS RpReportDesigner
	o := RpReportDesigner{}
	o:PrinterDevice := 1
	? o:PrinterDevice
RETURN
