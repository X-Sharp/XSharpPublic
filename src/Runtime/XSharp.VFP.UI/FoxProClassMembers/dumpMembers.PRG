LOCAL cCls, oObj, i, nclas, nmembers, cClass, cBase
LOCAL cFile 
DIMENSION classes(40)

classes(1) = "CheckBox"
classes(2) = "Collection"
classes(3) = "Column"
classes(4) = "ComboBox"
classes(5) = "CommandButton"
classes(6) = "CommandGroup"
classes(7) = "Container"
classes(8) = "Control"
classes(9) = "Cursor"
classes(10) = "CursorAdapter"
classes(11) = "Custom"
classes(12) = "DataEnvironment"
classes(13) = "EditBox"
classes(14) = "Empty"
classes(15) = "Exception"
classes(16) = "Form"
classes(17) = "FormSet"
classes(18) = "Image"
classes(19) = "Label"
classes(20) = "Line"
classes(21) = "ListBox"
classes(22) = "OLEBoundControl"
classes(23) = "OLEControl"
classes(24) = "OptionButton"
classes(25) = "OptionGroup"
classes(26) = "Page"
classes(27) = "PageFrame"
classes(28) = "ProjectHook"
classes(29) = "Relation"
classes(30) = "ReportListener"
classes(31) = "Separator"
classes(32) = "Session"
classes(33) = "Shape"
classes(34) = "Spinner"
classes(35) = "TextBox"
classes(36) = "Timer"
classes(37) = "ToolBar"
classes(38) = "XMLAdapter"
classes(39) = "XMLField"
classes(40) = "XMLTable"

SET SAFETY OFF
ON ERROR DO ErrorHandler()

FOR nClass = 1 TO ALEN(classes)
	cCls = classes[nClass]
	cFile = "c:\XSharp\FabVFPXPorter\xsVFPLibrary\Generated\"+cCls+".generated.prg"
	SET ALTERNATE TO &cFile
	SET ALTERNATE ON
	oObj = CREATEOBJECT(cCls)
	cClass = ""
	cBase = ""
	cClass = oObj.Class
	cBase  = oObj.BaseClass
	? "// Class", cCls, "BaseClass", cBase, "Class", cClass
	DIMENSION members[1]
	nmembers = AMEMBERS(members, oObj,1)
	? "BEGIN NAMESPACE xsVFPLibrary"
	? "PARTIAL CLASS VFP"+cCls
	FOR i = 1 TO nmembers
		cMember = members[i,1]
		&& suppress some methods
		DO case
		CASE cMember = "ADDPROPERTY"
			LOOP
		CASE cMember = "ALIGNMENT"
			LOOP
		CASE cMember = "AUTOSIZE"
			LOOP
		CASE cMember = "ANCHOR"
			LOOP
		CASE cMember = "BACKCOLOR"
			LOOP
		CASE cMember = "BACKSTYLE"
			LOOP
		CASE cMember = "BORDERSTYLE"
			LOOP
		CASE cMember = "BUTTONCOUNT"
			LOOP
		CASE cMember = "BUTTONS"
			LOOP
		CASE cMember = "CAPTION"
			LOOP
		CASE cMember = "COLORSOURCE"
			LOOP
		CASE cMember = "DOCK"
			LOOP
		CASE cMember = "ENABLED"
			LOOP
		CASE LEFT(cMember,4) = "FONT"
			LOOP
		CASE cMember = "FORECOLOR"
			LOOP
		CASE cMember = "HIDE"
			LOOP
		CASE cMember = "HEIGHT"
			LOOP
		CASE cMember = "LEFT"
			LOOP
		CASE cMember = "MAXLENGTH"
			LOOP
		CASE cMember = "MARGIN"
			LOOP
		CASE cMember = "MOVE"
			LOOP
		CASE cMember = "MOUSEPOINTER"
			LOOP
		CASE cMember = "NAME"
			LOOP
		CASE cMember = "PARENT"
			LOOP
		CASE cMember = "REFRESH"
			LOOP
		CASE cMember = "RIGHTTOLEFT"
			LOOP
		CASE cMember = "SCROLLBARS"
			LOOP
		CASE cMember = "SHOW"
			LOOP
		CASE cMember = "SPECIALEFFECT"
			LOOP
		CASE cMember = "STATUSBARTEXT"
			LOOP
		CASE cMember = "TEXT"
			LOOP
		CASE cMember = "TAG"
			LOOP
		CASE cMember = "TABINDEX"
			LOOP
		CASE cMember = "TABSTOP"
			LOOP
		CASE cMember = "TOP"
			LOOP
		CASE cMember = "TOOLTIPTEXT"
			LOOP
		CASE cMember = "VALUE"
			LOOP
		CASE cMember = "VISIBLE"
			LOOP
		CASE cMember = "WIDTH"
			LOOP
		CASE cMember = "ZORDER"
			LOOP
			
		otherwise
		IF LOWER(members[i,2]) == "property"
		? "PROPERTY " + members[i,1] + " AS USUAL AUTO"
		endif
		IF LOWER(members[i,2]) == "method"
		? "METHOD " + members[i,1] + " AS USUAL CLIPPER"
		? "RETURN NIL"
		ENDIF
		ENDcase	
	NEXT
	? "END CLASS"
	? "END NAMESPACE"	
	SET ALTERNATE OFF
	CLOSE ALTERNATE
	
next
SET SAFETY ON
	


PROCEDURE ErrorHandler()
PARAMETER merror, mess, mess1, mprog, mlineno
   ?"*****************ERROR******"
   ? 'Error number: ' + LTRIM(STR(merror))
   ? 'Error message: ' + mess
   ? 'Line of code with error: ' + mess1
   ? 'Line number of error: ' + LTRIM(STR(mlineno))
   ? 'Program with error: ' + mprog
	RESUME