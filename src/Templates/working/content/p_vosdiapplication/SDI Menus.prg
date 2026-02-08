#region DEFINES
DEFINE IDM_StandardSDIMenu_File_ID := 29000
DEFINE IDM_StandardSDIMenu_File_Open_ID := 29001
DEFINE IDM_StandardSDIMenu_File_Print_ID := 29003
DEFINE IDM_StandardSDIMenu_File_Print_Setup_ID := 29004
DEFINE IDM_StandardSDIMenu_File_Exit_ID := 29006
DEFINE IDM_StandardSDIMenu_Edit_ID := 29007
DEFINE IDM_StandardSDIMenu_Edit_Cut_ID := 29008
DEFINE IDM_StandardSDIMenu_Edit_Copy_ID := 29009
DEFINE IDM_StandardSDIMenu_Edit_Paste_ID := 29010
DEFINE IDM_StandardSDIMenu_Edit_Insert_Record_ID := 29012
DEFINE IDM_StandardSDIMenu_Edit_Delete_Record_ID := 29013
DEFINE IDM_StandardSDIMenu_Edit_Go_To_Top_ID := 29015
DEFINE IDM_StandardSDIMenu_Edit_Previous_ID := 29016
DEFINE IDM_StandardSDIMenu_Edit_Next_ID := 29017
DEFINE IDM_StandardSDIMenu_Edit_Go_To_Bottom_ID := 29018
DEFINE IDM_StandardSDIMenu_View_ID := 29019
DEFINE IDM_StandardSDIMenu_View_Form_ID := 29020
DEFINE IDM_StandardSDIMenu_View_Table_ID := 29021
DEFINE IDM_StandardSDIMenu_Help_ID := 29022
DEFINE IDM_StandardSDIMenu_Help_Index_ID := 29023
DEFINE IDM_StandardSDIMenu_Help_Context_Help_ID := 29024
DEFINE IDM_StandardSDIMenu_Help_Using_Help_ID := 29025
DEFINE IDM_StandardSDIMenu_Help_About_ID := 29027
DEFINE IDM_EmptySDIMenu_File_ID := 18500
DEFINE IDM_EmptySDIMenu_File_Open_ID := 18501
DEFINE IDM_EmptySDIMenu_File_Print_Setup_ID := 18503
DEFINE IDM_EmptySDIMenu_File_Exit_ID := 18505
DEFINE IDM_EmptySDIMenu_Help_ID := 18506
DEFINE IDM_EmptySDIMenu_Help_Index_ID := 18507
DEFINE IDM_EmptySDIMenu_Help_Using_Help_ID := 18508
DEFINE IDM_EmptySDIMenu_Help_About_ID := 18510
#endregion

PARTIAL CLASS EmptySDIMenu INHERIT Menu

CONSTRUCTOR( oOwner )

    LOCAL oTB AS Toolbar

    SELF:PreInit()

    SUPER( ResourceID { "EmptySDIMenu" , _GetInst( ) } )

    SELF:RegisterItem(IDM_EmptySDIMenu_File_ID, ;
        HyperLabel{ #File , "&File" ,  , "File" } , SELF:Handle() , 0)

    SELF:RegisterItem(IDM_EmptySDIMenu_File_Open_ID, ;
        HyperLabel{ #FileOpen , "&Open...    Ctrl+O" , "Open a file" , "File_Open" })

    SELF:RegisterItem(IDM_EmptySDIMenu_File_Print_Setup_ID, ;
        HyperLabel{ #FilePrinterSetup , "P&rint Setup..." , "Setup printer options" , "File_Printer_Setup" })

    SELF:RegisterItem(IDM_EmptySDIMenu_File_Exit_ID, ;
        HyperLabel{ #FileExit , "E&xit    Alt+F4" , "End of application" , "File_Exit" })

    SELF:RegisterItem(IDM_EmptySDIMenu_Help_ID, ;
        HyperLabel{ #Help , "&Help" ,  ,  } , SELF:Handle() , 1)

    SELF:RegisterItem(IDM_EmptySDIMenu_Help_Index_ID, ;
        HyperLabel{ #HelpIndex , "&Index    F1" , "Index of help" , "Help_Index" })

    SELF:RegisterItem(IDM_EmptySDIMenu_Help_Using_Help_ID, ;
        HyperLabel{ #HelpUsingHelp , "&Using Help" , "How to use help" , "Help_UsingHelp" })

    SELF:RegisterItem(IDM_EmptySDIMenu_Help_About_ID, ;
        HyperLabel{ #HelpAbout , "&About..." , "About application" ,  })

    oTB := Toolbar{}

    oTB:ButtonStyle := TB_ICONONLY
    oTB:Flat := TRUE
    oTB:EnableBands(FALSE)

    oTB:AppendItem(IDT_OPEN , IDM_EmptySDIMenu_File_Open_ID)
    oTB:AddTipText(IDT_OPEN , IDM_EmptySDIMenu_File_Open_ID , "Open File")

    oTB:AppendItem(IDT_HELP , IDM_EmptySDIMenu_Help_About_ID)
    oTB:AddTipText(IDT_HELP , IDM_EmptySDIMenu_Help_About_ID , "Help About")


    SELF:ToolBar := oTB
    SELF:Accelerator := EmptySDIMenu_Accelerator{ }

    SELF:PostInit()

    RETURN

END CLASS
PARTIAL CLASS EmptySDIMenu_Accelerator INHERIT Accelerator

CONSTRUCTOR()
    SUPER( ResourceID { "EmptySDIMenu_Accelerator" , _GetInst( ) } )
RETURN


END CLASS
PARTIAL CLASS StandardSDIMenu INHERIT Menu

CONSTRUCTOR( oOwner )

    LOCAL oTB AS Toolbar

    SELF:PreInit()

    SUPER( ResourceID { "StandardSDIMenu" , _GetInst( ) } )

    SELF:RegisterItem(IDM_StandardSDIMenu_File_ID, ;
        HyperLabel{ #File , "&File" ,  , "File" } , SELF:Handle() , 0)

    SELF:RegisterItem(IDM_StandardSDIMenu_File_Open_ID, ;
        HyperLabel{ #FileOpen , "&Open...    Ctrl+O" , "Open a file" , "File_Open" })

    SELF:RegisterItem(IDM_StandardSDIMenu_File_Print_ID, ;
        HyperLabel{ #FilePrint , "&Print" , "Print the window" ,  })

    SELF:RegisterItem(IDM_StandardSDIMenu_File_Print_Setup_ID, ;
        HyperLabel{ #FilePrinterSetup , "P&rint Setup..." , "Setup printer options" , "File_Printer_Setup" })

    SELF:RegisterItem(IDM_StandardSDIMenu_File_Exit_ID, ;
        HyperLabel{ #FileExit , "E&xit    Alt+F4" , "End of application" , "File_Exit" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_ID, ;
        HyperLabel{ #Edit , "&Edit" , "Edit information" , "Edit" } , SELF:Handle() , 1)

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Cut_ID, ;
        HyperLabel{ #Cut , "Cu&t    Ctrl+X" , "Cut to clipboard" , "Cut" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Copy_ID, ;
        HyperLabel{ #Copy , "&Copy    Ctrl+C" , "Copy to clipboard" , "Copy" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Paste_ID, ;
        HyperLabel{ #Paste , "&Paste    Ctrl+V" , "Paste from clipboard" , "Paste" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Insert_Record_ID, ;
        HyperLabel{ #Append , "&Insert Record" , "Insert a new record" , "Append" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Delete_Record_ID, ;
        HyperLabel{ #Delete , "&Delete Record" , "Delete this record" , "Delete" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Go_To_Top_ID, ;
        HyperLabel{ #GoTop , "G&o To Top    Ctrl+Home" , "Go to first record" , "GoTop" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Previous_ID, ;
        HyperLabel{ #SkipPrevious , "Pre&vious" , "Go to previous record" , "SkipPrevious" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Next_ID, ;
        HyperLabel{ #SkipNext , "&Next" , "Go to next record" , "SkipNext" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Edit_Go_To_Bottom_ID, ;
        HyperLabel{ #GoBottom , "Go To &Bottom    Ctrl+End" , "Go to last record" , "GoBottom" })

    SELF:RegisterItem(IDM_StandardSDIMenu_View_ID, ;
        HyperLabel{ #View , "&View" ,  ,  } , SELF:Handle() , 2)

    SELF:RegisterItem(IDM_StandardSDIMenu_View_Form_ID, ;
        HyperLabel{ #ViewForm , "&Form    Shift+F2" , "View a single record as a form" , "ViewForm" })

    SELF:RegisterItem(IDM_StandardSDIMenu_View_Table_ID, ;
        HyperLabel{ #ViewTable , "&Table    F2" , "View several records in a table" , "ViewTable" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Help_ID, ;
        HyperLabel{ #Help , "&Help" ,  ,  } , SELF:Handle() , 3)

    SELF:RegisterItem(IDM_StandardSDIMenu_Help_Index_ID, ;
        HyperLabel{ #HelpIndex , "&Index    F1" , "Index of help" , "Help_Index" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Help_Context_Help_ID, ;
        HyperLabel{ #HelpContext , "&Context Help    Ctrl+F1" , "Context sensitive help" , "Help_ContextHelp" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Help_Using_Help_ID, ;
        HyperLabel{ #HelpUsingHelp , "&Using Help" , "How to use help" , "Help_UsingHelp" })

    SELF:RegisterItem(IDM_StandardSDIMenu_Help_About_ID, ;
        HyperLabel{ #HelpAbout , "&About..." , "About application" ,  })

    oTB := Toolbar{}

    oTB:ButtonStyle := TB_ICONONLY
    oTB:Flat := TRUE
    oTB:EnableBands(FALSE)

    oTB:AppendItem(IDT_OPEN , IDM_StandardSDIMenu_File_Open_ID)
    oTB:AddTipText(IDT_OPEN , IDM_StandardSDIMenu_File_Open_ID , "Open File")

    oTB:AppendItem(IDT_SEPARATOR)

    oTB:AppendItem(IDT_CUT , IDM_StandardSDIMenu_Edit_Cut_ID)
    oTB:AddTipText(IDT_CUT , IDM_StandardSDIMenu_Edit_Cut_ID , "Cut")

    oTB:AppendItem(IDT_COPY , IDM_StandardSDIMenu_Edit_Copy_ID)
    oTB:AddTipText(IDT_COPY , IDM_StandardSDIMenu_Edit_Copy_ID , "Copy")

    oTB:AppendItem(IDT_PASTE , IDM_StandardSDIMenu_Edit_Paste_ID)
    oTB:AddTipText(IDT_PASTE , IDM_StandardSDIMenu_Edit_Paste_ID , "Paste")

    oTB:AppendItem(IDT_SEPARATOR)

    oTB:AppendItem(IDT_STARTREC , IDM_StandardSDIMenu_Edit_Go_To_Top_ID)
    oTB:AddTipText(IDT_STARTREC , IDM_StandardSDIMenu_Edit_Go_To_Top_ID , "Go Top")

    oTB:AppendItem(IDT_PREVREC , IDM_StandardSDIMenu_Edit_Previous_ID)
    oTB:AddTipText(IDT_PREVREC , IDM_StandardSDIMenu_Edit_Previous_ID , "Previous Record")

    oTB:AppendItem(IDT_NEXTREC , IDM_StandardSDIMenu_Edit_Next_ID)
    oTB:AddTipText(IDT_NEXTREC , IDM_StandardSDIMenu_Edit_Next_ID , "Next Record")

    oTB:AppendItem(IDT_ENDREC , IDM_StandardSDIMenu_Edit_Go_To_Bottom_ID)
    oTB:AddTipText(IDT_ENDREC , IDM_StandardSDIMenu_Edit_Go_To_Bottom_ID , "Go Bottom")

    oTB:AppendItem(IDT_SEPARATOR)

    oTB:AppendItem(IDT_VFORM , IDM_StandardSDIMenu_View_Form_ID)
    oTB:AddTipText(IDT_VFORM , IDM_StandardSDIMenu_View_Form_ID , "Form View")

    oTB:AppendItem(IDT_VGBROWSE , IDM_StandardSDIMenu_View_Table_ID)
    oTB:AddTipText(IDT_VGBROWSE , IDM_StandardSDIMenu_View_Table_ID , "Browse View")

    oTB:AppendItem(IDT_SEPARATOR)

    oTB:AppendItem(IDT_HELP , IDM_StandardSDIMenu_Help_About_ID)
    oTB:AddTipText(IDT_HELP , IDM_StandardSDIMenu_Help_About_ID , "Help")


    SELF:ToolBar := oTB
    SELF:Accelerator := StandardSDIMenu_Accelerator{ }

    SELF:PostInit()

    RETURN

END CLASS
PARTIAL CLASS StandardSDIMenu_Accelerator INHERIT Accelerator

CONSTRUCTOR()
    SUPER( ResourceID { "StandardSDIMenu_Accelerator" , _GetInst( ) } )
RETURN


END CLASS
