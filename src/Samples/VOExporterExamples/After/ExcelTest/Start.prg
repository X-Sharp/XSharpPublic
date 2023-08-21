using Microsoft.Office.Interop.Excel
using System.Reflection

FUNCTION Start() AS VOID
local oExcel as Application
LOCAL oWorkBooks as Workbooks
LOCAL oWorksheet as Worksheet
LOCAL oRange as Range
LOCAL cFile as STRING
cFile := "C:\ExcelTest\example.xls"
DirMake("C:\ExcelTest")
oExcel:=ApplicationClass{}
oExcel:Visible:=FALSE // Don't show the EXCEL execute
oExcel:DisplayAlerts:=FALSE // Don't show messages
oWorkBooks:=oExcel:Workbooks
oWorkBooks:add() //open a new worksheet
oWorkSheet:=oExcel:ActiveSheet // active the first sheet 
oExcel:WorkbookBeforeSave += OnBeforeSave
oExcel:WorkbookAfterSave += OnAfterSave
oRange:=oWorkSheet:Range["A1","A1"] // A1 cell
oRange:Select()
oRange:FormulaR1C1:="Hello my text"
oExcel:ActiveWorkBook:SaveAs(cFile,xlFileFormat.xlExcel8,"","",FALSE,FALSE)  
oWorkBooks:Close()
oExcel:Quit()
WAIT
	
RETURN 

FUNCTION OnBeforeSave (oWb as Workbook, SaveAsUI as LOGIC, Cancel REF Logic) AS VOID
? "OnBeforeSave", oWb:Path, oWb:Name, SaveAsUI
RETURN
FUNCTION OnAfterSave (oWb as Workbook, Success as LOGIC) AS VOID
? "OnAfterSave", oWb:Path, oWb:Name, Success
RETURN
