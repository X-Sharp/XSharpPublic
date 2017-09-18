FUNCTION Start()
local oExcel as Object
LOCAL oWorkBooks as OBJECT
LOCAL oWorksheet as Object
LOCAL oRange as Object
LOCAL cFile as STRING
cFile := "C:\ExcelTest\example.xls"
DirMake(String2Psz("C:\ExcelTest"))
oExcel:=OLEAutoObject{"Excel.Application"}
oExcel:Visible:=FALSE // Don't show the EXCEL execute
oExcel:DisplayAlerts:=FALSE // Don't show messages
oWorkBooks:=oExcel:Workbooks
oWorkBooks:add() //open a new worksheet
oWorkSheet:=oExcel:ActiveSheet // active the first sheet
oRange:=oWorkSheet:Range["A1","A1"] // A1 cell
oRange:SELECT()
oRange:FormulaR1C1:="Hello my text"
oExcel:ActiveWorkBook:SaveAs(cFile,56,"","",FALSE,FALSE,NIL,NIL,NIL,NIL,NIL)  //"56" save the file in work book of EXCEL 97-2003

oWorkBooks:Close()
oExcel:Quit()	
WAIT
	
RETURN NIL	


