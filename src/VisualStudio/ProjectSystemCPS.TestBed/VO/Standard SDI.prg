#region DEFINES
define IDI_STANDARDICON := 101
define IDI_VOICON := 102
#endregion

class StandardSDIWindow inherit DataWindow
    protect oPrinter as PrintingDevice
    protect oStdMenu as Menu


METHOD DoOpenFile(cFileName, lReadOnly) 
    LOCAL oTB AS TextBox

    IF (Len(cFileName) > 3)  .AND. (Upper(Right(cFileName, 4)) == ".DBF")
          IF (SELF:Menu != oStdMenu)
            SELF:Menu := oStdMenu
                SELF:ToolBar:PressItem(IDM_StandardSDIMenu_View_Form_ID, #MenuItemID)
        ENDIF    
        
        SELF:Use(DBServer{cFileName, , lReadOnly})
          SELF:Caption := "Browse Database: " + cFileName
    ELSE
        oTB := TextBox{SELF, "File Open Error", "Cannot open " + cFileName + " - Not a DBF file"}
        oTB:Type := BUTTONOKAY
        oTB:Beep:=TRUE
        oTB:Show()
    ENDIF
RETURN SELF

METHOD Drop(oDragEvent) 
    IF File(oDragEvent:FileName(1))
        SELF:DoOpenFile(oDragEvent:FileName(1))
    ENDIF
RETURN SELF

METHOD FileClose() 
    SELF:Use()
RETURN SELF    

METHOD FileExit() 
    SELF:EndWindow()
RETURN SELF    

METHOD FileOpen() 
    LOCAL oOD AS OpenDialog
    LOCAL oTB AS TextBox
    LOCAL retval AS SHORT

    IF(oAttachedServer == NULL_OBJECT)    
        (oOD := OpenDialog{SELF, "*.dbf"}):Show()
        
        IF !Empty(oOD:FileName)
            SELF:DoOpenFile(oOD:FileName, oOD:ReadOnly)
        ENDIF
    ELSE
        oTB := TextBox{SELF, "Warning", "A DBF file is already open!" + _CHR(10)+ "Do you want close it?"}
        oTB:Type := BUTTONOKAYCANCEL
        retval := oTB:Show()
        IF retval == BOXREPLYCANCEL
            RETURN SELF
        ENDIF
        IF retVal==BOXREPLYOKAY
            (oOD := OpenDialog{SELF, "*.dbf"}):Show()
            IF !Empty(oOD:FileName)
                SELF:DoOpenFile(oOD:FileName, oOD:ReadOnly)
            ENDIF
        ENDIF
  ENDIF
RETURN SELF

METHOD FilePrint() 
    SELF:Print(oPrinter)
RETURN SELF    

METHOD FilePrinterSetup() 

    oPrinter:Setup()
    
RETURN SELF

CONSTRUCTOR(oOwnerApp) 

  SetDeleted(TRUE)

  SUPER(oOwnerApp)
    
  SELF:EnableDragDropClient()
    
  SELF:Icon      := Icon{ResourceID{IDI_STANDARDICON, _GetInst()}}
  SELF:Menu    := EmptySDIMenu{SELF}
  oStdMenu     := StandardSDIMenu{SELF}
  SELF:Caption := "VO SDI Application"
  SELF:EnableStatusBar(TRUE)
  SELF:StatusBar:DisplayTime(TRUE)
  SELF:QuitOnClose := TRUE

  SELF:Size := Dimension{850,650}

  oPrinter  := PrintingDevice{}
    
  RETURN SELF


method ViewForm() 
    self:ToolBar:UnPressItem(IDM_StandardSDIMenu_View_Table_ID)
    self:ToolBar:PressItem(IDM_StandardSDIMenu_View_Form_ID)
    
    return super:ViewForm()    

method ViewTable() 
    self:ToolBar:UnPressItem(IDM_StandardSDIMenu_View_Form_ID)
    self:ToolBar:PressItem(IDM_StandardSDIMenu_View_Table_ID)
    
    return super:ViewTable()
    


END CLASS
