//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// The defines below are imported from FoxPro.h

************************************************************************************
************************************************************************************
**                                                                                **
** ================                    ===========                                **
**   ==         ===                      ==       ==                              **
**   ==           =                      ==        ==                             **
**   ==                                  ==        ==                             **
**   ==          ===      =====  =====   ==       ==  ===   ====         ===      **
**   =====    ==     ==     ==    ==     =========      == ==   ==    ==     ==   **
**   ==      ==       ==     ==  ==      ==             ===          ==       ==  **
**   ==     ==         ==     ===        ==             ==          ==         == **
**   ==      ==       ==     == ==       ==             ==           ==       ==  **
**   ==       ==     ==     ==   ==      ==             ==            ==     ==   **
** =======       ===      =====  ====  ======         ======             ===      **
**                                                                                **
************************************************************************************
************************************************************************************

************************************************************************************
************************************************************************************
**                                                                                **
** Visual FoxPro Named Constant File                                              **
** This file contains named constants for most Visual FoxPro functions            **
**                                                                                **
************************************************************************************
************************************************************************************

*-- General
*--ZOrder Method
DEFINE ZORDER_BRINGTOFRONT   := 0
DEFINE ZORDER_SENDTOBACK     := 1

*-- TYPE() tags
DEFINE T_CHARACTER    :=  "C"
DEFINE T_NUMERIC      :=  "N"
DEFINE T_DOUBLE       :=  "B"
DEFINE T_DATE         :=  "D"
DEFINE T_DATETIME     :=  "T"
DEFINE T_MEMO         :=  "M"
DEFINE T_GENERAL      :=  "G"
DEFINE T_OBJECT       :=  "O"
DEFINE T_SCREEN       :=  "S"
DEFINE T_LOGICAL      :=  "L"
DEFINE T_CURRENCY     :=  "Y"
DEFINE T_UNDEFINED    :=  "U"
DEFINE T_INTEGER      :=  "N"
DEFINE T_VARCHAR      :=  "C"
DEFINE T_VARBINARY    :=  "Q"
DEFINE T_BLOB         :=  "W"

*-- QueryUnload
DEFINE FORM_CONTROLMENU     := 0
DEFINE FORM_CODE            := 1
DEFINE APP_WINDOWS          := 2
DEFINE APP_TASKMANAGER      := 3
DEFINE FORM_MDIFORM         := 4

*-- Columns in array returned by AVCXCLASSES()
DEFINE AVCX_CLASS            := 1    && OBJNAME field
DEFINE AVCX_BASECLASS        := 2    && BASECLASS field
DEFINE AVCX_PARENTCLASS      := 3    && CLASS field
DEFINE AVCX_PARENTCLASSLIB   := 4    && CLASSLOC field
DEFINE AVCX_CLASSICON        := 5    && RESERVED4 field
DEFINE AVCX_CONTAINERICON    := 6    && RESERVED5 field
DEFINE AVCX_SCALEMODE        := 7    && RESERVED6 field
DEFINE AVCX_CLASSDESCRIPTION := 8    && RESERVED7 field
DEFINE AVCX_INCLUDEFILE      := 9    && RESERVED8 field
DEFINE AVCX_USERINFO         := 10   && USER field
DEFINE AVCX_OLEPUBLIC        := 11   && RESERVED2 field


*-- Active Document
*-- CommandTargetQuery Command Options
DEFINE CMDF_NOTSUPPORTED           := 0  && The command is not supported by this object
DEFINE CMDF_SUPPORTED              := 1  && The command is supported by this object
DEFINE CMDF_ENABLED                := 2  && The command is available and enabled
DEFINE CMDF_LATCHED                := 4  && The command is an on-off toggle and is currently on
DEFINE CMDF_NINCHED                := 8  && The command state is indeterminate

*-- CommandTargetQuery CommandTextFlag values
DEFINE CMDTEXTF_NONE               := 0  && No extra information is requested
DEFINE CMDTEXTF_NAME               := 1  && Object should provide the localized name of the command
DEFINE CMDTEXTF_STATUS             := 2  && Object should provide a localized status string for the command

*-- CommandTarget Command IDs
DEFINE CMDID_OPEN               :=   1  && File menu, Open command
DEFINE CMDID_NEW                :=   2  && File menu, New command
DEFINE CMDID_SAVE               :=   3  && File menu, Save command
DEFINE CMDID_SAVEAS             :=   4  && File menu, Save As command
DEFINE CMDID_SAVECOPYAS         :=   5  && File menu, Save Copy As command
DEFINE CMDID_PRINT              :=   6  && File menu, Print command
DEFINE CMDID_PRINTPREVIEW       :=   7  && File menu, Print Preview command
DEFINE CMDID_PAGESETUP          :=   8  && File menu, Page Setup command
DEFINE CMDID_SPELL              :=   9  && Tools menu, Spelling command
DEFINE CMDID_PROPERTIES         :=  10  && File menu, Properties command
DEFINE CMDID_CUT                :=  11  && Edit menu, Cut command (from Host)
DEFINE CMDID_COPY               :=  12  && Edit menu, Copy command (from Host)
DEFINE CMDID_PASTE              :=  13  && Edit menu, Paste command (from Host)
DEFINE CMDID_PASTESPECIAL       :=  14  && Edit menu, Paste Special command (from Host)
DEFINE CMDID_UNDO               :=  15  && Edit menu, Undo command (from Host)
DEFINE CMDID_REDO               :=  16  && Edit menu, Redo command (from Host)
DEFINE CMDID_SELECTALL          :=  17  && Edit menu, Select All command (from Host)
DEFINE CMDID_CLEARSELECTION     :=  18  && Edit menu, Clear command (from Host)
DEFINE CMDID_ZOOM               :=  19  && View menu, Zoom command
DEFINE CMDID_GETZOOMRANGE       :=  20  && Retrieves zoom range applicable to View Zoom
DEFINE CMDID_UPDATECOMMANDS     :=  21  && Informs the Active Document of state changes
DEFINE CMDID_REFRESH            :=  22  && Asks the Active Document to refresh its display
DEFINE CMDID_STOP               :=  23  && Asks the Active Document to stop all current processing
DEFINE CMDID_HIDETOOLBARS       :=  24  && Asks the Active Document to hide its toolbars
DEFINE CMDID_SETPROGRESSMAX     :=  25  && Sets the maximum value of a progress indicator
DEFINE CMDID_SETPROGRESSPOS     :=  26  && Sets the current value of a progress indicator
DEFINE CMDID_SETPROGRESSTEXT    :=  27  && Sets the text contained in a progress indicator
DEFINE CMDID_SETTITLE           :=  28  && Sets the title bar text
DEFINE CMDID_SETDOWNLOADSTATE   :=  29  && Sent by host when its download state changes
DEFINE CMDID_STOPDOWNLOAD       :=  30  && Stops the download when executed
DEFINE CMDID_ONTOOLBARACTIVATED :=  31  && One of the container's toolbars has received the focus
DEFINE CMDID_ENABLE_INTERACTION :=  36  && Asks the Active Document to either pause or resume multimedia
DEFINE CMDID_ONUNLOAD           :=  37  && Sent by the Active Document host before navigation to another 
                                        && site is initiated or the host is closed

*-- CommantTargetExec nExecOption parameter
DEFINE CMDEXECOPT_DODEFAULT       := 0  && Use the default behavior
DEFINE CMDEXECOPT_PROMPTUSER      := 1  && Execute the command after obtaining user input
DEFINE CMDEXECOPT_DONTPROMPTUSER  := 2  && Execute the command without prompting the user
DEFINE CMDEXECOPT_SHOWHELP        := 3  && Show help for the corresponding command

*-- CommandTargetExec Return Values
DEFINE CMD_OK                     :=  0  && Command handled okay by the Active Document
DEFINE CMD_NOTSUPPORTED           :=  1  && Command is not supported by the Active Document
DEFINE CMD_DISABLED               :=  2  && Command is disabled for the Active Document
DEFINE CMD_NOHELP                 :=  3  && No help is available for the command from the Active Document
DEFINE CMD_CANCELED               :=  4  && The user canceled the execution of the command


*-- Project Hooks
*-- Build Actions
DEFINE BUILDACTION_REBUILD        := 1  && Rebuild Project
DEFINE BUILDACTION_BUILDAPP       := 2  && Build APP
DEFINE BUILDACTION_BUILDEXE       := 3  && Build EXE
DEFINE BUILDACTION_BUILDDLL       := 4  && Build DLL
DEFINE BUILDACTION_BUILDMTDLL     := 5  && Build MTDLL

*-- File Object SCCStatus Property
DEFINE SCCFILE_NOTCONTROLLED      :=  0  && File is not source controlled
DEFINE SCCFILE_NOTCHECKEDOUT      :=  1  && File is controlled but not checked out to anyone
DEFINE SCCFILE_CHECKEDOUTCU       :=  2  && File is checked out to the current user only
DEFINE SCCFILE_CHECKEDOUTOU       :=  3  && File is checked out to someone other than the current user
DEFINE SCCFILE_MERGECONFLICT      :=  4  && File has a merge conflict
DEFINE SCCFILE_MERGE              :=  5  && File has been merged without conflict
DEFINE SCCFILE_CHECKEDOUTMU       :=  6  && File is checked out to multiple users

*-- File Object Type Property
DEFINE FILETYPE_DATABASE        :=  "d"  && Database (.DBC)
DEFINE FILETYPE_FREETABLE       :=  "D"  && Free table (.DBF)
DEFINE FILETYPE_QUERY           :=  "Q"  && Query (.QPR)
DEFINE FILETYPE_FORM            :=  "K"  && Form (.SCX)
DEFINE FILETYPE_REPORT          :=  "R"  && Report (.FRX)
DEFINE FILETYPE_LABEL           :=  "B"  && Label (.LBX)
DEFINE FILETYPE_CLASSLIB        :=  "V"  && Class Library (.VCX)
DEFINE FILETYPE_PROGRAM         :=  "P"  && Program (.PRG)
DEFINE FILETYPE_APILIB          :=  "L"  && API Library (.FLL)
DEFINE FILETYPE_APPLICATION     :=  "Z"  && Application (.APP)
DEFINE FILETYPE_MENU            :=  "M"  && Menu (.MNX)
DEFINE FILETYPE_TEXT            :=  "T"  && Text (.TXT, .H., etc.)
DEFINE FILETYPE_OTHER           :=  "x"  && Other file types not enumerated above

*-- Server Object Instancing Property
DEFINE SERVERINSTANCE_NOTCREATABLE := 0  && Instances creatable only inside Visual FoxPro
DEFINE SERVERINSTANCE_SINGLEUSE    := 1  && Single use server
DEFINE SERVERINSTANCE_MULTIUSE     := 2  && Multi-use server


*-- Drag and Drop
*-- OLE Drag and Drop:  Drop Effects
DEFINE DROPEFFECT_NONE     := 0
DEFINE DROPEFFECT_COPY     := 1
DEFINE DROPEFFECT_MOVE     := 2
DEFINE DROPEFFECT_LINK     := 4

*-- OLE Drag and Drop:  Drop Modes
DEFINE DROP_DISABLED        :=0
DEFINE DROP_ENABLED         :=1
DEFINE DROP_PASSTOCONTAINER :=2

*-- OLE Drag and Drop:  OLEDropHasData settings
DEFINE DROPHASDATA_VFPDETERMINE := -1
DEFINE DROPHASDATA_NOTUSEFUL    :=  0
DEFINE DROPHASDATA_USEFUL       :=  1

*-- Clipboard formats (Global)
DEFINE CF_TEXT             := 1      && Text
DEFINE CF_BITMAP           := 2      && Bitmap
DEFINE CF_METAFILEPICT     := 3      && Handle of a metafile picture format
DEFINE CF_SYLK             := 4      && Microsoft Symbolic Link format
DEFINE CF_DIF              := 5      && Software Arts' Data Interchange Format
DEFINE CF_TIFF             := 6      && Tagged-image file format
DEFINE CF_OEMTEXT          := 7      && Text format containing characters in the OEM character set
DEFINE CF_DIB              := 8      && Device-independent bitmap
DEFINE CF_PALETTE          := 9      && Handle of a color palette
DEFINE CF_PENDATA          := 10     && Data for the pen extensions to the Microsoft Windows for Pen Computing
DEFINE CF_RIFF             := 11     && Represents complex audio data
DEFINE CF_WAVE             := 12     && Audio data in one of the standard wave formats
DEFINE CF_UNICODETEXT      := 13     && Windows NT only: Unicode text format
DEFINE CF_ENHMETAFILE      := 14     && A handle of an enhanced metafile
DEFINE CF_FILES            := 15     && A list of files
DEFINE CF_HDROP            := 15     && A list of files
DEFINE CF_LOCALE           := 16     && A handle to the locale identifier
DEFINE CF_MAX              := 17

*-- Other Miscellaneous Clipboard formats
DEFINE CFSTR_HYPERLINK     := "Hyperlink"               && A Hyperlink
DEFINE CFSTR_BIFF          := "Biff"                    && Microsoft Excel version 2.x
DEFINE CFSTR_BIFF3         := "Biff3"                   && Microsoft Excel version 3.0
DEFINE CFSTR_BIFF4         := "Biff4"                   && Microsoft Excel version 4.0
DEFINE CFSTR_BIFF5         := "Biff5"                   && Microsoft Excel version 5.0
DEFINE CFSTR_BIFF7         := "Biff7"                   && Microsoft Excel version 7.0
DEFINE CFSTR_BIFF8         := "Biff8"                   && Microsoft Excel version 8.0
DEFINE CFSTR_XLTABLE       := "XlTable"                 && Microsoft Excel fast table format.
DEFINE CFSTR_CSV           := "CSV"                     && Comma separated values
DEFINE CFSTR_WK1           := "Wk1"                     && Lotus 1-2-3 Release 2.01 and Release 2.2 format
DEFINE CFSTR_URL           := "UniformResourceLocator"  && URL

DEFINE CFSTR_RTF           := "Rich Text Format"
DEFINE CFSTR_RTFNOOBJS     := "Rich Text Format Without Objects"
DEFINE CFSTR_RETEXTOBJ     := "RichEdit Text and Objects"

*-- Clipboard formats (Private to VFP)
DEFINE CFSTR_OLEVARIANTARRAY :="OLE Variant Array"       && VFP array
DEFINE CFSTR_OLEVARIANT      :="OLE Variant"             && Data in variant form
DEFINE CFSTR_VFPSOURCEOBJECT :="VFP Source Object"       && A reference to the VFP source object

*-- DragMode
DEFINE DRAG_MANUAL          := 0      && 0 - Manual
DEFINE DRAG_AUTOMATIC       := 1      && 1 - Automatic

*-- DragOver
DEFINE DRAG_ENTER          := 0
DEFINE DRAG_LEAVE          := 1
DEFINE DRAG_OVER           := 2

*-- Drag (controls)
DEFINE DRAG_CANCEL          := 0
DEFINE DRAG_BEGIN           := 1
DEFINE DRAG_END             := 2


*-- Properties
*-- Colors
DEFINE COLOR_WHITE       :=16777215
DEFINE COLOR_BLACK       :=       0
DEFINE COLOR_GRAY        :=12632256
DEFINE COLOR_DARK_GRAY   := 8421504
DEFINE COLOR_RED         :=     255
DEFINE COLOR_DARK_BLUE   := 8388608
DEFINE COLOR_CYAN        :=16776960
DEFINE COLOR_DARK_CYAN   := 8421376
DEFINE COLOR_GREEN       :=   65280
DEFINE COLOR_DARK_GREEN  :=   32768
DEFINE COLOR_YELLOW      :=   65535
DEFINE COLOR_DARK_YELLOW :=   32896
DEFINE COLOR_BLUE        :=16711680
DEFINE COLOR_DARK_RED    :=     128
DEFINE COLOR_MAGENTA     :=16711935
DEFINE COLOR_DARK_MAGENTA:= 8388736

*-- MousePointer
DEFINE MOUSE_DEFAULT          := 0       && 0 - Default
DEFINE MOUSE_ARROW            := 1       && 1 - Arrow
DEFINE MOUSE_CROSSHAIR        := 2       && 2 - Cross
DEFINE MOUSE_IBEAM            := 3       && 3 - I-Beam
DEFINE MOUSE_ICON_POINTER     := 4       && 4 - Icon
DEFINE MOUSE_SIZE_POINTER     := 5       && 5 - Size
DEFINE MOUSE_SIZE_NE_SW       := 6       && 6 - Size NE SW
DEFINE MOUSE_SIZE_N_S         := 7       && 7 - Size N S
DEFINE MOUSE_SIZE_NW_SE       := 8       && 8 - Size NW SE
DEFINE MOUSE_SIZE_W_E         := 9       && 9 - Size W E
DEFINE MOUSE_UP_ARROW         := 10      && 10 - Up Arrow
DEFINE MOUSE_HOURGLASS        := 11      && 11 - Hourglass
DEFINE MOUSE_NO_DROP          := 12      && 12 - No drop
DEFINE MOUSE_HIDE_POINTER     := 13      && 13 - Hide Pointer
DEFINE MOUSE_ARROW2           := 14      && 14 - Arrow
DEFINE MOUSE_CUSTOM           := 99      && 99 - Custom

*-- ScrollBars
DEFINE SCROLLBARS_NONE         := 0      && 0 - None (Default)
DEFINE SCROLLBARS_HORIZONTAL   := 1      && 1 - Horizontal
DEFINE SCROLLBARS_VERTICAL     := 2      && 2 - Vertical
DEFINE SCROLLBARS_BOTH         := 3      && 3 - Both

*-- DrawMode
DEFINE DRAWMODE_BLACKNESS     := 1       && 1 - Blackness
DEFINE DRAWMODE_NOT_MERGE_PEN := 2       && 2 - Not Merge Pen
DEFINE DRAWMODE_MASK_NOT_PEN  := 3       && 3 - Mask Not Pen
DEFINE DRAWMODE_NOT_COPY_PEN  := 4       && 4 - Not Copy Pen
DEFINE DRAWMODE_MASK_PEN_NOT  := 5       && 5 - Mask Pen Not
DEFINE DRAWMODE_INVERT        := 6       && 6 - Invert
DEFINE DRAWMODE_XOR_PEN       := 7       && 7 - Xor Pen
DEFINE DRAWMODE_NOT_MASK_PEN  := 8       && 8 - Not Mask Pen
DEFINE DRAWMODE_MASK_PEN      := 9       && 9 - Mask Pen
DEFINE DRAWMODE_NOT_XOR_PEN   := 10      && 10 - Not Xor Pen
DEFINE DRAWMODE_NOP           := 11      && 11 - Nop
DEFINE DRAWMODE_MERGE_NOT_PEN := 12      && 12 - Merge Not Pen
DEFINE DRAWMODE_COPY_PEN      := 13      && 13 - Copy Pen
DEFINE DRAWMODE_MERGE_PEN_NOT := 14      && 14 - Merge Pen Not
DEFINE DRAWMODE_MERGE_PEN     := 15      && 15 - Merge Pen
DEFINE DRAWMODE_WHITENESS     := 16      && 16 - Whiteness

*-- DrawStyle
DEFINE DRAWSTYLE_SOLID         :=  0       && 0 - Solid
DEFINE DRAWSTYLE_DASH          :=  1       && 1 - Dash
DEFINE DRAWSTYLE_DOT           :=  2       && 2 - Dot
DEFINE DRAWSTYLE_DASH_DOT      :=  3       && 3 - Dash-Dot
DEFINE DRAWSTYLE_DASH_DOT_DOT  :=  4       && 4 - Dash-Dot-Dot
DEFINE DRAWSTYLE_INVISIBLE     :=  5       && 5 - Invisible
DEFINE DRAWSTYLE_INSIDE_SOLID  :=  6       && 6 - Inside Solid

*-- FillStyle
DEFINE FILLSTYLE_SOLID               :=    0       && 0 - Solid
DEFINE FILLSTYLE_TRANSPARENT         :=    1       && 1 - Transparent
DEFINE FILLSTYLE_HORIZONTAL_LINE     :=    2       && 2 - Horizontal Line
DEFINE FILLSTYLE_VERTICAL_LINE       :=    3       && 3 - Vertical Line
DEFINE FILLSTYLE_UPWARD_DIAGONAL     :=    4       && 4 - Upward Diagonal
DEFINE FILLSTYLE_DOWNWARD_DIAGONAL   :=    5       && 5 - Downward Diagonal
DEFINE FILLSTYLE_CROSS               :=    6       && 6 - Cross
DEFINE FILLSTYLE_DIAGONAL_CROSS      :=    7       && 7 - Diagonal Cross
                                      
*-- ScaleMode
DEFINE SCALEMODE_PIXELS        :=  3       && 3 - Pixel
DEFINE SCALEMODE_FOXELS        :=  0       && 0 - Foxels

*-- WindowState
DEFINE WINDOWSTATE_NORMAL        :=  0       && 0 - Normal
DEFINE WINDOWSTATE_MINIMIZED     :=  1       && 1 - Minimized
DEFINE WINDOWSTATE_MAXIMIZED     :=  2       && 2 - Maximized

*-- Window Borders
DEFINE BORDER_NONE    := 0
DEFINE BORDER_SINGLE  := 1
DEFINE BORDER_DOUBLE  := 2
DEFINE BORDER_SYSTEM  := 3

*-- Toolbar and Form Docking Positions
DEFINE TOOL_NOTDOCKED:=  -1
DEFINE TOOL_TOP      :=  0
DEFINE TOOL_LEFT     :=  1
DEFINE TOOL_RIGHT    :=  2
DEFINE TOOL_BOTTOM   :=  3
DEFINE TOOL_TAB     :=	4
DEFINE TOOL_LINK     :=	5

*-- Button parameter masks
DEFINE BUTTON_LEFT     :=1
DEFINE BUTTON_RIGHT    :=2
DEFINE BUTTON_MIDDLE   :=4

*-- Function Parameters
*-- MessageBox parameters
DEFINE MB_OK                :=  0       && OK button only
DEFINE MB_OKCANCEL          :=  1       && OK and Cancel buttons
DEFINE MB_ABORTRETRYIGNORE  :=  2       && Abort, Retry, and Ignore buttons
DEFINE MB_YESNOCANCEL       :=  3       && Yes, No, and Cancel buttons
DEFINE MB_YESNO             :=  4       && Yes and No buttons
DEFINE MB_RETRYCANCEL       :=  5       && Retry and Cancel buttons

DEFINE MB_ICONSTOP          :=   16      && Critical message
DEFINE MB_ICONQUESTION      :=   32      && Warning query
DEFINE MB_ICONEXCLAMATION   :=   48      && Warning message
DEFINE MB_ICONINFORMATION   :=   64      && Information message

DEFINE MB_APPLMODAL         :=  0       && Application modal message box
DEFINE MB_DEFBUTTON1        :=  0       && First button is default
DEFINE MB_DEFBUTTON2        :=  256     && Second button is default
DEFINE MB_DEFBUTTON3        :=  512     && Third button is default
DEFINE MB_SYSTEMMODAL       :=  4096    && System Modal

*-- MsgBox return values
DEFINE IDOK          :=1       && OK button pressed
DEFINE IDCANCEL      :=2       && Cancel button pressed
DEFINE IDABORT       :=3       && Abort button pressed
DEFINE IDRETRY       :=4       && Retry button pressed
DEFINE IDIGNORE      :=5       && Ignore button pressed
DEFINE IDYES         :=6       && Yes button pressed
DEFINE IDNO          :=7       && No button pressed
DEFINE IDTIMEOUT 		:=-1		&& Timeout occurs


*-- Low Level File Constants
DEFINE F_READONLY             := 0
DEFINE F_WRITEONLY            := 1
DEFINE F_READWRITE            := 2
DEFINE F_READONLY_UNBUFF      := 10
DEFINE F_WRITEONLY_UNBUFF     := 11
DEFINE F_READWRITE_UNBUFF     := 12

*-- PRTINFO() Constants
*-- PRTINFO() Valid types to pass
DEFINE PRT_ORIENTATION       := 1
DEFINE PRT_PAPERSIZE         := 2
DEFINE PRT_PAPERLENGTH       := 3
DEFINE PRT_PAPERWIDTH        := 4
DEFINE PRT_SCALE             := 5
DEFINE PRT_COPIES            := 6
DEFINE PRT_DEFASOURCE        := 7
DEFINE PRT_PRINTQUAL         := 8
DEFINE PRT_COLOR             := 9
DEFINE PRT_DUPLEX            := 10
DEFINE PRT_YRESOLUTION       := 11
DEFINE PRT_TTOPTION          := 12
DEFINE PRT_COLLATE           := 13

*--PRTINFO() Return types
*-- Paper sizes
DEFINE PRTPAPER_LETTER     := 1       && Letter 8 1/2 x 11 in               
DEFINE PRTPAPER_LETTERSMALL:= 2       && Letter Small 8 1/2 x 11 in         
DEFINE PRTPAPER_TABLOID    := 3       && Tabloid 11 x 17 in                 
DEFINE PRTPAPER_LEDGER     := 4       && Ledger 17 x 11 in                  
DEFINE PRTPAPER_LEGAL      := 5       && Legal 8 1/2 x 14 in                
DEFINE PRTPAPER_STATEMENT  := 6       && Statement 5 1/2 x 8 1/2 in         
DEFINE PRTPAPER_EXECUTIVE  := 7       && Executive 7 1/4 x 10 1/2 in      
DEFINE PRTPAPER_A3         := 8       && A3 297 x 420 mm                    
DEFINE PRTPAPER_A4         := 9       && A4 210 x 297 mm                    
DEFINE PRTPAPER_A4SMALL    := 10      && A4 Small 210 x 297 mm              
DEFINE PRTPAPER_A5         := 11      && A5 148 x 210 mm                    
DEFINE PRTPAPER_B4         := 12      && B4 250 x 354                       
DEFINE PRTPAPER_B5         := 13      && B5 182 x 257 mm                    
DEFINE PRTPAPER_FOLIO      := 14      && Folio 8 1/2 x 13 in                
DEFINE PRTPAPER_QUARTO     := 15      && Quarto 215 x 275 mm                
DEFINE PRTPAPER_10X14      := 16      && 10x14 in                           
DEFINE PRTPAPER_11X17      := 17      && 11x17 in                           
DEFINE PRTPAPER_NOTE       := 18      && Note 8 1/2 x 11 in                 
DEFINE PRTPAPER_ENV_9      := 19      && Envelope #9 3 7/8 x 8 7/8          
DEFINE PRTPAPER_ENV_10     := 20      && Envelope #10 4 1/8 x 9 1/2         
DEFINE PRTPAPER_ENV_11     := 21      && Envelope #11 4 1/2 x 10 3/8        
DEFINE PRTPAPER_ENV_12     := 22      && Envelope #12 4 \276 x 11           
DEFINE PRTPAPER_ENV_14     := 23      && Envelope #14 5 x 11 1/2            
DEFINE PRTPAPER_CSHEET     := 24      && C size sheet                       
DEFINE PRTPAPER_DSHEET     := 25      && D size sheet                       
DEFINE PRTPAPER_ESHEET     := 26      && E size sheet                       
DEFINE PRTPAPER_ENV_DL     := 27      && Envelope DL 110 x 220mm            
DEFINE PRTPAPER_ENV_C5     := 28      && Envelope C5 162 x 229 mm           
DEFINE PRTPAPER_ENV_C3     := 29      && Envelope C3  324 x 458 mm          
DEFINE PRTPAPER_ENV_C4     := 30      && Envelope C4  229 x 324 mm          
DEFINE PRTPAPER_ENV_C6     := 31      && Envelope C6  114 x 162 mm          
DEFINE PRTPAPER_ENV_C65    := 32      && Envelope C65 114 x 229 mm          
DEFINE PRTPAPER_ENV_B4     := 33      && Envelope B4  250 x 353 mm          
DEFINE PRTPAPER_ENV_B5     := 34      && Envelope B5  176 x 250 mm          
DEFINE PRTPAPER_ENV_B6     := 35      && Envelope B6  176 x 125 mm          
DEFINE PRTPAPER_ENV_ITALY  := 36      && Envelope 110 x 230 mm              
DEFINE PRTPAPER_ENV_MONARCH :=37      && Envelope Monarch 3.875 x 7.5 in    
DEFINE PRTPAPER_ENV_PERSONAL:= 38     && 6 3/4 Envelope 3 5/8 x 6 1/2 in    
DEFINE PRTPAPER_FANFOLD_US := 39      && US Std Fanfold 14 7/8 x 11 in      
DEFINE PRTPAPER_FANFOLD_STD_GERMAN := 40 && German Std Fanfold 8 1/2 x 12 in   
DEFINE PRTPAPER_FANFOLD_LGL_GERMAN := 41 && German Legal Fanfold 8 1/2 x 13 in 

*-- Paper bins
DEFINE PRTBIN_UPPER           :=1
DEFINE PRTBIN_ONLYONE         :=1
DEFINE PRTBIN_LOWER           :=2
DEFINE PRTBIN_MIDDLE          :=3
DEFINE PRTBIN_MANUAL          :=4
DEFINE PRTBIN_ENVELOPE        :=5
DEFINE PRTBIN_ENVMANUAL       :=6
DEFINE PRTBIN_AUTO            :=7
DEFINE PRTBIN_TRACTOR         :=8
DEFINE PRTBIN_SMALLFMT        :=9
DEFINE PRTBIN_LARGEFMT        :=10
DEFINE PRTBIN_LARGECAPACITY   :=11
DEFINE PRTBIN_CASSETTE        :=14
DEFINE PRTBIN_AUTOSELECT   	:=15

*-- Print qualities
DEFINE PRTRES_DRAFT      :=  -1
DEFINE PRTRES_LOW        :=  -2
DEFINE PRTRES_MEDIUM     :=  -3
DEFINE PRTRES_HIGH       :=  -4

*-- Color printer
DEFINE PRTCOLOR_MONOCHROME := 1
DEFINE PRTCOLOR_COLOR      := 2

*-- Duplexing
DEFINE PRTDUP_SIMPLEX   :=1
DEFINE PRTDUP_VERTICAL  :=2
DEFINE PRTDUP_HORIZONTAL:=3

*-- True Type fonts
DEFINE PRTTT_BITMAP    := 1  && Print True Type fonts as graphics
DEFINE PRTTT_DOWNLOAD  := 2  && Download True Type fonts as soft fonts
DEFINE PRTTT_SUBDEV    := 3  && Substitute device fonts for True Type

*-- FontMetric()
DEFINE TM_HEIGHT         := 1
DEFINE TM_ASCENT         := 2
DEFINE TM_DESCENT        := 3
DEFINE TM_INTERNALLEADING:= 4
DEFINE TM_EXTERNALLEADING:= 5
DEFINE TM_AVECHARWIDTH   := 6
DEFINE TM_MAXCHARWIDTH   := 7
DEFINE TM_WEIGHT         := 8
DEFINE TM_ITALIC         := 9
DEFINE TM_UNDERLINED     :=10
DEFINE TM_STRUCKOUT      :=11
DEFINE TM_FIRSTCHAR      :=12
DEFINE TM_LASTCHAR       :=13
DEFINE TM_DEFAULTCHAR    :=14
DEFINE TM_BREAKCHAR      :=15
DEFINE TM_PITCHANDFAMILY :=16
DEFINE TM_CHARSET        :=17
DEFINE TM_OVERHANG       :=18
DEFINE TM_ASPECTX        :=19
DEFINE TM_ASPECTY        :=20

*-- Sysmetric() parameter values
DEFINE SYSMETRIC_SCREENWIDTH       := 1 && Screen width
DEFINE SYSMETRIC_SCREENHEIGHT      := 2 && Screen width
DEFINE SYSMETRIC_SIZINGBORDERWIDTH := 3 && Width of the sizing border around a resizable window
DEFINE SYSMETRIC_SIZINGBORDERHEIGHT:= 4 && Height of the sizing border around a resizable window
DEFINE SYSMETRIC_VSCROLLBARWIDTH   := 5 && Width of a vertical scroll bar
DEFINE SYSMETRIC_VSCROLLBARHEIGHT  := 6 && Height of the arrow bitmap on a vertical scroll bar
DEFINE SYSMETRIC_HSCROLLBARWIDTH   := 7 && Width of the arrow bitmap on a horizontal scroll bar
DEFINE SYSMETRIC_HSCROLLBARHEIGHT  := 8 && Height of a horizontal scroll bar
DEFINE SYSMETRIC_WINDOWTITLEHEIGHT := 9 && Height of window title (caption) area
DEFINE SYSMETRIC_WINDOWBORDERWIDTH :=10 && Width of a window border
DEFINE SYSMETRIC_WINDOWBORDERHEIGHT:=11 && Height of a window border
DEFINE SYSMETRIC_WINDOWFRAMEWIDTH  :=12 && Width of the frame around the perimeter of a window
                                        && that has a caption but is not sizable
DEFINE SYSMETRIC_WINDOWFRAMEHEIGHT := 13 && Height of the frame around the perimeter of a window
                                        && that has a caption but is not sizable
DEFINE SYSMETRIC_THUMBBOXWIDTH     := 14 && Width of the thumb box in a horizontal scroll bar
DEFINE SYSMETRIC_THUMBBOXHEIGHT    := 15 && Height of the thumb box in a vertical scroll bar
DEFINE SYSMETRIC_ICONWIDTH         := 16 && Width of an icon
DEFINE SYSMETRIC_ICONHEIGHT        := 17 && Height of an icon
DEFINE SYSMETRIC_CURSORWIDTH       := 18 && Width of a cursor
DEFINE SYSMETRIC_CURSORHEIGHT      := 19 && Height of a cursor
DEFINE SYSMETRIC_MENUBAR           := 20 && Height of a single-line menu bar
DEFINE SYSMETRIC_CLIENTWIDTH       := 21 && Width of the client area for a full-screen window
DEFINE SYSMETRIC_CLIENTHEIGHT      := 22 && Height of the client area for a full-screen window
DEFINE SYSMETRIC_KANJIWINHEIGHT    := 23 && Height of the Kanji window at the bottom of the screen in DBCS versions
DEFINE SYSMETRIC_MINDRAGWIDTH      := 24 && Minimum tracking width of a window. (The user cannot drag the window frame to a size smaller than this)
DEFINE SYSMETRIC_MINDRAGHEIGHT     := 25 && Minimum tracking height of a window. (The user cannot drag the window frame to a size smaller than this)
DEFINE SYSMETRIC_MINWINDOWWIDTH    := 26 && Minimum width of a window
DEFINE SYSMETRIC_MINWINDOWHEIGHT   := 27 && Minimum height of a window
DEFINE SYSMETRIC_TITLEBARBUTTONWIDTH := 28 && Width of a title bar button
DEFINE SYSMETRIC_TITLEBARBUTTONHEIGHT:= 29 && Height of a title bar button
DEFINE SYSMETRIC_MOUSEPRESENT      := 30 && Is mouse present?  1 => mouse is installed, 0 => no mouse is installed
DEFINE SYSMETRIC_DEBUGVERSION      := 31 && Is this a debug version?  1 => debug version, 0 => retail version
DEFINE SYSMETRIC_MOUSEBUTTONSWAP   := 32 && Are mouse buttons swapped?  1 => Yes, 0 => No
DEFINE SYSMETRIC_HALFHEIGHTBUTTONWIDTH := 33 && Width of a button in a half-height title bar
DEFINE SYSMETRIC_HALFHEIGHTBUTTONHEIGHT:= 34 && Height of a button in a half-height title bar

*-- Windows GetSysMetric() defines
DEFINE SM_CXSCREEN          :=  0
DEFINE SM_CYSCREEN          :=  1
DEFINE SM_CXVSCROLL         :=  2
DEFINE SM_CYHSCROLL         :=  3
DEFINE SM_CYCAPTION         :=  4
DEFINE SM_CXBORDER          :=  5
DEFINE SM_CYBORDER          :=  6
DEFINE SM_CXDLGFRAME        :=  7
DEFINE SM_CYDLGFRAME        :=  8
DEFINE SM_CYVTHUMB          :=  9
DEFINE SM_CXHTHUMB          :=  10
DEFINE SM_CXICON            :=  11
DEFINE SM_CYICON            :=  12
DEFINE SM_CXCURSOR          :=  13
DEFINE SM_CYCURSOR          :=  14
DEFINE SM_CYMENU            :=  15
DEFINE SM_CXFULLSCREEN      :=  16
DEFINE SM_CYFULLSCREEN      :=  17
DEFINE SM_CYKANJIWINDOW     :=  18
DEFINE SM_MOUSEPRESENT      :=  19
DEFINE SM_CYVSCROLL         :=  20
DEFINE SM_CXHSCROLL         :=  21
DEFINE SM_DEBUG             :=  22
DEFINE SM_SWAPBUTTON        :=  23
DEFINE SM_RESERVED1         :=  24
DEFINE SM_RESERVED2         :=  25
DEFINE SM_RESERVED3         :=  26
DEFINE SM_RESERVED4         :=  27
DEFINE SM_CXMIN             :=  28
DEFINE SM_CYMIN             :=  29
DEFINE SM_CXSIZE            :=  30
DEFINE SM_CYSIZE            :=  31
DEFINE SM_CXFRAME           :=  32
DEFINE SM_CYFRAME           :=  33
DEFINE SM_CXMINTRACK        :=  34
DEFINE SM_CYMINTRACK        :=  35
DEFINE SM_CMETRICS          :=  36

*-- Cursor buffering modes
DEFINE DB_BUFOFF            :=  1
DEFINE DB_BUFLOCKRECORD     :=  2
DEFINE DB_BUFOPTRECORD      :=  3        
DEFINE DB_BUFLOCKTABLE      :=  4
DEFINE DB_BUFOPTTABLE       :=  5

*-- Update types for views/cursors
DEFINE DB_UPDATE             :=  1
DEFINE DB_DELETEINSERT       :=  2

*-- WHERE clause types for views/cursors
DEFINE DB_KEY                := 1
DEFINE DB_KEYANDUPDATABLE    := 2
DEFINE DB_KEYANDMODIFIED     := 3
DEFINE DB_KEYANDTIMESTAMP    := 4

*-- Remote connection login prompt options
DEFINE DB_PROMPTCOMPLETE     :=  1
DEFINE DB_PROMPTALWAYS       :=  2
DEFINE DB_PROMPTNEVER        :=  3

*-- Remote transaction modes
DEFINE DB_TRANSAUTO          :=  1
DEFINE DB_TRANSMANUAL        :=  2

*-- Source Types for CursorGetProp()
DEFINE DB_SRCLOCALVIEW       :=  1
DEFINE DB_SRCREMOTEVIEW      :=  2
DEFINE DB_SRCTABLE           :=  3

*--  Language IDs.
*
*  A language ID is a 16 bit value which is the combination of a
*  primary language ID and a secondary language ID.  The bits are
*  allocated as follows:
*
*       +-----------------------+-------------------------+
*       |     Sublanguage ID    |   Primary Language ID   |
*       +-----------------------+-------------------------+
*        15                   10 9                       0   bit
*
*
*  The following three combinations of primary language ID and
*  sublanguage ID have special semantics:
*
*    Primary Language ID   Sublanguage ID      Result
*    -------------------   ---------------     ------------------------
*    LANG_NEUTRAL          SUBLANG_NEUTRAL     Language neutral
*    LANG_NEUTRAL          SUBLANG_DEFAULT     User default language
*    LANG_NEUTRAL          SUBLANG_SYS_DEFAULT System default language
*

*
*  Primary language IDs.
*

DEFINE LANG_NEUTRAL                   :=  0x00

DEFINE LANG_AFRIKAANS                 := 0x36
DEFINE LANG_ALBANIAN                  := 0x1c
DEFINE LANG_ARABIC                    := 0x01
DEFINE LANG_BASQUE                    := 0x2d
DEFINE LANG_BELARUSIAN                := 0x23
DEFINE LANG_BULGARIAN                 := 0x02
DEFINE LANG_CATALAN                   := 0x03
DEFINE LANG_CHINESE                   := 0x04
DEFINE LANG_CROATIAN                  := 0x1a
DEFINE LANG_CZECH                     := 0x05
DEFINE LANG_DANISH                    := 0x06
DEFINE LANG_DUTCH                     := 0x13
DEFINE LANG_ENGLISH                   := 0x09
DEFINE LANG_ESTONIAN                  := 0x25
DEFINE LANG_FAEROESE                  := 0x38
DEFINE LANG_FARSI                     := 0x29
DEFINE LANG_FINNISH                   := 0x0b
DEFINE LANG_FRENCH                    := 0x0c
DEFINE LANG_GERMAN                    := 0x07
DEFINE LANG_GREEK                     := 0x08
DEFINE LANG_HEBREW                    := 0x0d
DEFINE LANG_HUNGARIAN                 := 0x0e
DEFINE LANG_ICELANDIC                 := 0x0f
DEFINE LANG_INDONESIAN                := 0x21
DEFINE LANG_ITALIAN                   := 0x10
DEFINE LANG_JAPANESE                  := 0x11
DEFINE LANG_KOREAN                    := 0x12
DEFINE LANG_LATVIAN                   := 0x26
DEFINE LANG_LITHUANIAN                := 0x27
DEFINE LANG_NORWEGIAN                 := 0x14
DEFINE LANG_POLISH                    := 0x15
DEFINE LANG_PORTUGUESE                := 0x16
DEFINE LANG_ROMANIAN                  := 0x18
DEFINE LANG_RUSSIAN                   := 0x19
DEFINE LANG_SERBIAN                   := 0x1a
DEFINE LANG_SLOVAK                    := 0x1b
DEFINE LANG_SLOVENIAN                 := 0x24
DEFINE LANG_SPANISH                   := 0x0a
DEFINE LANG_SWEDISH                   := 0x1d
DEFINE LANG_THAI                      := 0x1e
DEFINE LANG_TURKISH                   := 0x1f
DEFINE LANG_UKRAINIAN                 := 0x22
DEFINE LANG_VIETNAMESE                :=  0x2a

*
*  Sublanguage IDs.
*
*  The name immediately following SUBLANG_ dictates which primary
*  language ID that sublanguage ID can be combined with to form a
*  valid language ID.
*

DEFINE SUBLANG_NEUTRAL                :=  0x0000    && language neutral
DEFINE SUBLANG_DEFAULT                :=  0x0400    && user default
DEFINE SUBLANG_SYS_DEFAULT            :=  0x0800    && system default

DEFINE SUBLANG_ARABIC_SAUDI_ARABIA   :=  0x0400    && Arabic (Saudi Arabia)
DEFINE SUBLANG_ARABIC_IRAQ           :=  0x0800    && Arabic (Iraq)
DEFINE SUBLANG_ARABIC_EGYPT          :=  0x0C00    && Arabic (Egypt)
DEFINE SUBLANG_ARABIC_LIBYA          :=  0x1000    && Arabic (Libya)
DEFINE SUBLANG_ARABIC_ALGERIA        :=  0x1400    && Arabic (Algeria)
DEFINE SUBLANG_ARABIC_MOROCCO        :=  0x1800    && Arabic (Morocco)
DEFINE SUBLANG_ARABIC_TUNISIA        :=  0x1C00    && Arabic (Tunisia)
DEFINE SUBLANG_ARABIC_OMAN           :=  0x2000    && Arabic (Oman)
DEFINE SUBLANG_ARABIC_YEMEN          :=  0x2400    && Arabic (Yemen)
DEFINE SUBLANG_ARABIC_SYRIA          :=  0x2800    && Arabic (Syria)
DEFINE SUBLANG_ARABIC_JORDAN         :=  0x2C00    && Arabic (Jordan)
DEFINE SUBLANG_ARABIC_LEBANON        :=  0x3000    && Arabic (Lebanon)
DEFINE SUBLANG_ARABIC_KUWAIT         :=  0x3400    && Arabic (Kuwait)
DEFINE SUBLANG_ARABIC_UAE            :=  0x3800    && Arabic (U.A.E)
DEFINE SUBLANG_ARABIC_BAHRAIN        :=  0x3C00    && Arabic (Bahrain)
DEFINE SUBLANG_ARABIC_QATAR          :=  0x4000    && Arabic (Qatar)
DEFINE SUBLANG_CHINESE_TRADITIONAL   :=  0x0400    && Chinese (Taiwan)
DEFINE SUBLANG_CHINESE_SIMPLIFIED    :=  0x0800    && Chinese (PR China)
DEFINE SUBLANG_CHINESE_HONGKONG      :=  0x0C00    && Chinese (Hong Kong S.A.R)
DEFINE SUBLANG_CHINESE_SINGAPORE     :=  0x1000    && Chinese (Singapore)
DEFINE SUBLANG_DUTCH                 :=  0x0400    && Dutch
DEFINE SUBLANG_DUTCH_BELGIAN         :=  0x0800    && Dutch (Belgian)
DEFINE SUBLANG_ENGLISH_US            :=  0x0400    && English (USA)
DEFINE SUBLANG_ENGLISH_UK            :=  0x0800    && English (UK)
DEFINE SUBLANG_ENGLISH_AUS           :=  0x0C00    && English (Australian)
DEFINE SUBLANG_ENGLISH_CAN           :=  0x1000    && English (Canadian)
DEFINE SUBLANG_ENGLISH_NZ            :=  0x1400    && English (New Zealand)
DEFINE SUBLANG_ENGLISH_EIRE          :=  0x1800    && English (Irish)
DEFINE SUBLANG_ENGLISH_SOUTH_AFRICA  :=  0x1C00    && English (South Africa)
DEFINE SUBLANG_ENGLISH_JAMAICA       :=  0x2000    && English (Jamaica)
DEFINE SUBLANG_ENGLISH_CARIBBEAN     :=  0x2400    && English (Caribbean)
DEFINE SUBLANG_ENGLISH_BELIZE        :=  0x2800    && English (Belize)
DEFINE SUBLANG_ENGLISH_TRINIDAD      :=  0x2C00    && English (Trinidad)
DEFINE SUBLANG_FRENCH                :=  0x0400    && French
DEFINE SUBLANG_FRENCH_BELGIAN        :=  0x0800    && French (Belgian)
DEFINE SUBLANG_FRENCH_CANADIAN       :=  0x0C00    && French (Canadian)
DEFINE SUBLANG_FRENCH_SWISS          :=  0x1000    && French (Swiss)
DEFINE SUBLANG_FRENCH_LUXEMBOURG     :=  0x1400    && French (Luxembourg)
DEFINE SUBLANG_GERMAN                :=  0x0400    && German
DEFINE SUBLANG_GERMAN_SWISS          := 0x0800    && German (Swiss)
DEFINE SUBLANG_GERMAN_AUSTRIAN       := 0x0C00    && German (Austrian)
DEFINE SUBLANG_GERMAN_LUXEMBOURG     := 0x1000    && German (Luxembourg)
DEFINE SUBLANG_GERMAN_LIECHTENSTEIN  := 0x1400    && German (Liechtenstein)
DEFINE SUBLANG_ITALIAN               := 0x0400    && Italian
DEFINE SUBLANG_ITALIAN_SWISS         := 0x0800    && Italian (Swiss)
DEFINE SUBLANG_KOREAN                := 0x0400    && Korean (Extended Wansung)
DEFINE SUBLANG_KOREAN_JOHAB          := 0x0800    && Korean (Johab)
DEFINE SUBLANG_NORWEGIAN_BOKMAL      := 0x0400    && Norwegian (Bokmal)
DEFINE SUBLANG_NORWEGIAN_NYNORSK     := 0x0800    && Norwegian (Nynorsk)
DEFINE SUBLANG_PORTUGUESE            := 0x0800    && Portuguese
DEFINE SUBLANG_PORTUGUESE_BRAZILIAN  := 0x0400    && Portuguese (Brazilian)
DEFINE SUBLANG_SERBIAN_LATIN         := 0x0800    && Serbian (Latin)
DEFINE SUBLANG_SERBIAN_CYRILLIC      := 0x0C00    && Serbian (Cyrillic)
DEFINE SUBLANG_SPANISH               := 0x0400    && Spanish (Castilian)
DEFINE SUBLANG_SPANISH_MEXICAN       := 0x0800    && Spanish (Mexican)
DEFINE SUBLANG_SPANISH_MODERN        := 0x0C00    && Spanish (Modern)
DEFINE SUBLANG_SPANISH_GUATEMALA     := 0x1000    && Spanish (Guatemala)
DEFINE SUBLANG_SPANISH_COSTA_RICA    := 0x1400    && Spanish (Costa Rica)
DEFINE SUBLANG_SPANISH_PANAMA        :=   0x1800    && Spanish (Panama)
DEFINE SUBLANG_SPANISH_DOMINICAN_REPUBLIC:= 0x1C00  && Spanish (Dominican Republic)
DEFINE SUBLANG_SPANISH_VENEZUELA     :=   0x2000    && Spanish (Venezuela)
DEFINE SUBLANG_SPANISH_COLOMBIA      :=   0x2400    && Spanish (Colombia)
DEFINE SUBLANG_SPANISH_PERU          :=   0x2800    && Spanish (Peru)
DEFINE SUBLANG_SPANISH_ARGENTINA     :=   0x2C00    && Spanish (Argentina)
DEFINE SUBLANG_SPANISH_ECUADOR       :=   0x3000    && Spanish (Ecuador)
DEFINE SUBLANG_SPANISH_CHILE         :=   0x3400    && Spanish (Chile)
DEFINE SUBLANG_SPANISH_URUGUAY       :=   0x3800    && Spanish (Uruguay)
DEFINE SUBLANG_SPANISH_PARAGUAY      :=   0x3C00    && Spanish (Paraguay)
DEFINE SUBLANG_SPANISH_BOLIVIA       :=   0x4000    && Spanish (Bolivia)
DEFINE SUBLANG_SPANISH_EL_SALVADOR   :=   0x4400    && Spanish (El Salvador)
DEFINE SUBLANG_SPANISH_HONDURAS      :=  0x4800    && Spanish (Honduras)
DEFINE SUBLANG_SPANISH_NICARAGUA     :=  0x4C00    && Spanish (Nicaragua)
DEFINE SUBLANG_SPANISH_PUERTO_RICO   :=  0x5000    && Spanish (Puerto Rico)
DEFINE SUBLANG_SWEDISH               :=  0x0400    && Swedish
DEFINE SUBLANG_SWEDISH_FINLAND       :=  0x0800    && Swedish (Finland)

*-- DBC Events constants
DEFINE DBCEVENTS_NULLTYPE:=0
DEFINE DBCEVENTS_DATABASE:=1
DEFINE DBCEVENTS_TABLE		:=2
DEFINE DBCEVENTS_COLUMN	:=	3
DEFINE DBCEVENTS_INDEX		:=4
DEFINE DBCEVENTS_TAG		:=	4
DEFINE DBCEVENTS_RELATION	:=5
DEFINE	DBCEVENTS_CONNECTION:=	6
DEFINE	DBCEVENTS_VIEW		:=7
DEFINE DBCEVENTS_LASTTYPE	:=8
DEFINE DBCEVENTS_STPROCS	:=	9	

*-- AFields() Constants
DEFINE AFIELDS_NAME				:=		1
DEFINE AFIELDS_TYPE				:=		2
DEFINE AFIELDS_WIDTH				:=		3
DEFINE AFIELDS_PRECISION			:=		4
DEFINE AFIELDS_NULL				:=		5
DEFINE AFIELDS_CODEPAGE_XLATE		:=	6
DEFINE AFIELDS_VALID_EXPRESSION	:=		7
DEFINE AFIELDS_VALID_TEXT			:=	8
DEFINE AFIELDS_DEFAULT				:=	9
DEFINE AFIELDS_TABLE_VALID_EXPRESSION:=10
DEFINE AFIELDS_TABLE_VALID_TEXT		:=	11
DEFINE AFIELDS_TABLE_LONG_NAME			:=12
DEFINE AFIELDS_INSERT_TRIGGER			:=13
DEFINE AFIELDS_UPDATE_TRIGGER			:=14
DEFINE AFIELDS_DELETE_TRIGGER			:=15
DEFINE AFIELDS_TABLE_COMMENT			:=	16
DEFINE AFIELDS_AUTOINC_NEXT_VALUE		:=	17
DEFINE AFIELDS_AUTOINC_STEP			:=	18

* UCS Transformation Format byte order marks.
DEFINE ENCODE_UTF16	:=CHR(0xFF) + CHR(0xFE)
DEFINE ENCODE_UTF8		:=""

* COM_Attrib flag settings for Type Library attributes support
DEFINE COMATTRIB_RESTRICTED	:=0x1			&& The property/method should not be accessible from macro languages.
DEFINE COMATTRIB_HIDDEN		:=0x40		&& The property/method should not be displayed to the user, although it exists and is bindable.
DEFINE COMATTRIB_NONBROWSABLE	:=0x400		&& The property/method appears in an object browser, but not in a properties browser.
DEFINE COMATTRIB_READONLY		:=0x100000	&& The property is read-only (applies only to Properties).
DEFINE COMATTRIB_WRITEONLY		:=0x200000	&& The property is write-only (applies only to Properties).

