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
/// <exclude/>
DEFINE ZORDER_BRINGTOFRONT   := 0
/// <exclude/>
DEFINE ZORDER_SENDTOBACK     := 1

*-- TYPE() tags
/// <exclude/>
DEFINE T_CHARACTER    :=  "C"
/// <exclude/>
DEFINE T_NUMERIC      :=  "N"
/// <exclude/>
DEFINE T_DOUBLE       :=  "B"
/// <exclude/>
DEFINE T_DATE         :=  "D"
/// <exclude/>
DEFINE T_DATETIME     :=  "T"
/// <exclude/>
DEFINE T_MEMO         :=  "M"
/// <exclude/>
DEFINE T_GENERAL      :=  "G"
/// <exclude/>
DEFINE T_OBJECT       :=  "O"
/// <exclude/>
DEFINE T_SCREEN       :=  "S"
/// <exclude/>
DEFINE T_LOGICAL      :=  "L"
/// <exclude/>
DEFINE T_CURRENCY     :=  "Y"
/// <exclude/>
DEFINE T_UNDEFINED    :=  "U"
/// <exclude/>
DEFINE T_INTEGER      :=  "N"
/// <exclude/>
DEFINE T_VARCHAR      :=  "C"
/// <exclude/>
DEFINE T_VARBINARY    :=  "Q"
/// <exclude/>
DEFINE T_BLOB         :=  "W"

*-- QueryUnload
/// <exclude/>
DEFINE FORM_CONTROLMENU     := 0
/// <exclude/>
DEFINE FORM_CODE            := 1
/// <exclude/>
DEFINE APP_WINDOWS          := 2
/// <exclude/>
DEFINE APP_TASKMANAGER      := 3
/// <exclude/>
DEFINE FORM_MDIFORM         := 4
/// <exclude/>

*-- Columns in array returned by AVCXCLASSES()
/// <exclude/>
DEFINE AVCX_CLASS            := 1    // OBJNAME field
/// <exclude/>
DEFINE AVCX_BASECLASS        := 2    // BASECLASS field
/// <exclude/>
DEFINE AVCX_PARENTCLASS      := 3    // CLASS field
/// <exclude/>
DEFINE AVCX_PARENTCLASSLIB   := 4    // CLASSLOC field
/// <exclude/>
DEFINE AVCX_CLASSICON        := 5    // RESERVED4 field
/// <exclude/>
DEFINE AVCX_CONTAINERICON    := 6    // RESERVED5 field
/// <exclude/>
DEFINE AVCX_SCALEMODE        := 7    // RESERVED6 field
/// <exclude/>
DEFINE AVCX_CLASSDESCRIPTION := 8    // RESERVED7 field
/// <exclude/>
DEFINE AVCX_INCLUDEFILE      := 9    // RESERVED8 field
/// <exclude/>
DEFINE AVCX_USERINFO         := 10   // USER field
/// <exclude/>
DEFINE AVCX_OLEPUBLIC        := 11   // RESERVED2 field


*-- Active Document
*-- CommandTargetQuery Command Options
/// <exclude/>
DEFINE CMDF_NOTSUPPORTED           := 0  // The command is not supported by this object
/// <exclude/>
DEFINE CMDF_SUPPORTED              := 1  // The command is supported by this object
/// <exclude/>
DEFINE CMDF_ENABLED                := 2  // The command is available and enabled
/// <exclude/>
DEFINE CMDF_LATCHED                := 4  // The command is an on-off toggle and is currently on
/// <exclude/>
DEFINE CMDF_NINCHED                := 8  // The command state is indeterminate

*-- CommandTargetQuery CommandTextFlag values
/// <exclude/>
DEFINE CMDTEXTF_NONE               := 0  // No extra information is requested
/// <exclude/>
DEFINE CMDTEXTF_NAME               := 1  // Object should provide the localized name of the command
/// <exclude/>
DEFINE CMDTEXTF_STATUS             := 2  // Object should provide a localized status string for the command

*-- CommandTarget Command IDs
/// <exclude/>
DEFINE CMDID_OPEN               :=   1  // File menu, Open command
/// <exclude/>
DEFINE CMDID_NEW                :=   2  // File menu, New command
/// <exclude/>
DEFINE CMDID_SAVE               :=   3  // File menu, Save command
/// <exclude/>
DEFINE CMDID_SAVEAS             :=   4  // File menu, Save As command
/// <exclude/>
DEFINE CMDID_SAVECOPYAS         :=   5  // File menu, Save Copy As command
/// <exclude/>
DEFINE CMDID_PRINT              :=   6  // File menu, Print command
/// <exclude/>
DEFINE CMDID_PRINTPREVIEW       :=   7  // File menu, Print Preview command
/// <exclude/>
DEFINE CMDID_PAGESETUP          :=   8  // File menu, Page Setup command
/// <exclude/>
DEFINE CMDID_SPELL              :=   9  // Tools menu, Spelling command
/// <exclude/>
DEFINE CMDID_PROPERTIES         :=  10  // File menu, Properties command
/// <exclude/>
DEFINE CMDID_CUT                :=  11  // Edit menu, Cut command (from Host)
/// <exclude/>
DEFINE CMDID_COPY               :=  12  // Edit menu, Copy command (from Host)
/// <exclude/>
DEFINE CMDID_PASTE              :=  13  // Edit menu, Paste command (from Host)
/// <exclude/>
DEFINE CMDID_PASTESPECIAL       :=  14  // Edit menu, Paste Special command (from Host)
/// <exclude/>
DEFINE CMDID_UNDO               :=  15  // Edit menu, Undo command (from Host)
/// <exclude/>
DEFINE CMDID_REDO               :=  16  // Edit menu, Redo command (from Host)
/// <exclude/>
DEFINE CMDID_SELECTALL          :=  17  // Edit menu, Select All command (from Host)
/// <exclude/>
DEFINE CMDID_CLEARSELECTION     :=  18  // Edit menu, Clear command (from Host)
/// <exclude/>
DEFINE CMDID_ZOOM               :=  19  // View menu, Zoom command
/// <exclude/>
DEFINE CMDID_GETZOOMRANGE       :=  20  // Retrieves zoom range applicable to View Zoom
/// <exclude/>
DEFINE CMDID_UPDATECOMMANDS     :=  21  // Informs the Active Document of state changes
/// <exclude/>
DEFINE CMDID_REFRESH            :=  22  // Asks the Active Document to refresh its display
/// <exclude/>
DEFINE CMDID_STOP               :=  23  // Asks the Active Document to stop all current processing
/// <exclude/>
DEFINE CMDID_HIDETOOLBARS       :=  24  // Asks the Active Document to hide its toolbars
/// <exclude/>
DEFINE CMDID_SETPROGRESSMAX     :=  25  // Sets the maximum value of a progress indicator
/// <exclude/>
DEFINE CMDID_SETPROGRESSPOS     :=  26  // Sets the current value of a progress indicator
/// <exclude/>
DEFINE CMDID_SETPROGRESSTEXT    :=  27  // Sets the text contained in a progress indicator
/// <exclude/>
DEFINE CMDID_SETTITLE           :=  28  // Sets the title bar text
/// <exclude/>
DEFINE CMDID_SETDOWNLOADSTATE   :=  29  // Sent by host when its download state changes
/// <exclude/>
DEFINE CMDID_STOPDOWNLOAD       :=  30  // Stops the download when executed
/// <exclude/>
DEFINE CMDID_ONTOOLBARACTIVATED :=  31  // One of the container's toolbars has received the focus
/// <exclude/>
DEFINE CMDID_ENABLE_INTERACTION :=  36  // Asks the Active Document to either pause or resume multimedia
/// <exclude/>
DEFINE CMDID_ONUNLOAD           :=  37  // Sent by the Active Document host before navigation to another 
                                        // site is initiated or the host is closed

*-- CommantTargetExec nExecOption parameter
/// <exclude/>
DEFINE CMDEXECOPT_DODEFAULT       := 0  // Use the default behavior
/// <exclude/>
DEFINE CMDEXECOPT_PROMPTUSER      := 1  // Execute the command after obtaining user input
/// <exclude/>
DEFINE CMDEXECOPT_DONTPROMPTUSER  := 2  // Execute the command without prompting the user
/// <exclude/>
DEFINE CMDEXECOPT_SHOWHELP        := 3  // Show help for the corresponding command
/// <exclude/>

*-- CommandTargetExec Return Values
/// <exclude/>
DEFINE CMD_OK                     :=  0  // Command handled okay by the Active Document
/// <exclude/>
DEFINE CMD_NOTSUPPORTED           :=  1  // Command is not supported by the Active Document
/// <exclude/>
DEFINE CMD_DISABLED               :=  2  // Command is disabled for the Active Document
/// <exclude/>
DEFINE CMD_NOHELP                 :=  3  // No help is available for the command from the Active Document
/// <exclude/>
DEFINE CMD_CANCELED               :=  4  // The user canceled the execution of the command


*-- Project Hooks
*-- Build Actions
/// <exclude/>
DEFINE BUILDACTION_REBUILD        := 1  // Rebuild Project
/// <exclude/>
DEFINE BUILDACTION_BUILDAPP       := 2  // Build APP
/// <exclude/>
DEFINE BUILDACTION_BUILDEXE       := 3  // Build EXE
/// <exclude/>
DEFINE BUILDACTION_BUILDDLL       := 4  // Build DLL
/// <exclude/>
DEFINE BUILDACTION_BUILDMTDLL     := 5  // Build MTDLL

*-- File Object SCCStatus Property
/// <exclude/>
DEFINE SCCFILE_NOTCONTROLLED      :=  0  // File is not source controlled
/// <exclude/>
DEFINE SCCFILE_NOTCHECKEDOUT      :=  1  // File is controlled but not checked out to anyone
/// <exclude/>
DEFINE SCCFILE_CHECKEDOUTCU       :=  2  // File is checked out to the current user only
/// <exclude/>
DEFINE SCCFILE_CHECKEDOUTOU       :=  3  // File is checked out to someone other than the current user
/// <exclude/>
DEFINE SCCFILE_MERGECONFLICT      :=  4  // File has a merge conflict
/// <exclude/>
DEFINE SCCFILE_MERGE              :=  5  // File has been merged without conflict
/// <exclude/>
DEFINE SCCFILE_CHECKEDOUTMU       :=  6  // File is checked out to multiple users

*-- File Object Type Property
/// <exclude/>
DEFINE FILETYPE_DATABASE        :=  "d"  // Database (.DBC)
/// <exclude/>
DEFINE FILETYPE_FREETABLE       :=  "D"  // Free table (.DBF)
/// <exclude/>
DEFINE FILETYPE_QUERY           :=  "Q"  // Query (.QPR)
/// <exclude/>
DEFINE FILETYPE_FORM            :=  "K"  // Form (.SCX)
/// <exclude/>
DEFINE FILETYPE_REPORT          :=  "R"  // Report (.FRX)
/// <exclude/>
DEFINE FILETYPE_LABEL           :=  "B"  // Label (.LBX)
/// <exclude/>
DEFINE FILETYPE_CLASSLIB        :=  "V"  // Class Library (.VCX)
/// <exclude/>
DEFINE FILETYPE_PROGRAM         :=  "P"  // Program (.PRG)
/// <exclude/>
DEFINE FILETYPE_APILIB          :=  "L"  // API Library (.FLL)
/// <exclude/>
DEFINE FILETYPE_APPLICATION     :=  "Z"  // Application (.APP)
/// <exclude/>
DEFINE FILETYPE_MENU            :=  "M"  // Menu (.MNX)
/// <exclude/>
DEFINE FILETYPE_TEXT            :=  "T"  // Text (.TXT, .H., etc.)
/// <exclude/>
DEFINE FILETYPE_OTHER           :=  "x"  // Other file types not enumerated above

*-- Server Object Instancing Property
/// <exclude/>
DEFINE SERVERINSTANCE_NOTCREATABLE := 0  // Instances creatable only inside Visual FoxPro
/// <exclude/>
DEFINE SERVERINSTANCE_SINGLEUSE    := 1  // Single use server
/// <exclude/>
DEFINE SERVERINSTANCE_MULTIUSE     := 2  // Multi-use server


*-- Drag and Drop
*-- OLE Drag and Drop:  Drop Effects
/// <exclude/>
DEFINE DROPEFFECT_NONE     := 0
/// <exclude/>
DEFINE DROPEFFECT_COPY     := 1
/// <exclude/>
DEFINE DROPEFFECT_MOVE     := 2
/// <exclude/>
DEFINE DROPEFFECT_LINK     := 4

*-- OLE Drag and Drop:  Drop Modes
/// <exclude/>
DEFINE DROP_DISABLED        :=0
/// <exclude/>
DEFINE DROP_ENABLED         :=1
/// <exclude/>
DEFINE DROP_PASSTOCONTAINER :=2

*-- OLE Drag and Drop:  OLEDropHasData settings
/// <exclude/>
DEFINE DROPHASDATA_VFPDETERMINE := -1
/// <exclude/>
DEFINE DROPHASDATA_NOTUSEFUL    :=  0
/// <exclude/>
DEFINE DROPHASDATA_USEFUL       :=  1
/// <exclude/>

*-- Clipboard formats (Global)
/// <exclude/>
DEFINE CF_TEXT             := 1      // Text
/// <exclude/>
DEFINE CF_BITMAP           := 2      // Bitmap
/// <exclude/>
DEFINE CF_METAFILEPICT     := 3      // Handle of a metafile picture format
/// <exclude/>
DEFINE CF_SYLK             := 4      // Microsoft Symbolic Link format
/// <exclude/>
DEFINE CF_DIF              := 5      // Software Arts' Data Interchange Format
/// <exclude/>
DEFINE CF_TIFF             := 6      // Tagged-image file format
/// <exclude/>
DEFINE CF_OEMTEXT          := 7      // Text format containing characters in the OEM character set
/// <exclude/>
DEFINE CF_DIB              := 8      // Device-independent bitmap
/// <exclude/>
DEFINE CF_PALETTE          := 9      // Handle of a color palette
/// <exclude/>
DEFINE CF_PENDATA          := 10     // Data for the pen extensions to the Microsoft Windows for Pen Computing
/// <exclude/>
DEFINE CF_RIFF             := 11     // Represents complex audio data
/// <exclude/>
DEFINE CF_WAVE             := 12     // Audio data in one of the standard wave formats
/// <exclude/>
DEFINE CF_UNICODETEXT      := 13     // Windows NT only: Unicode text format
/// <exclude/>
DEFINE CF_ENHMETAFILE      := 14     // A handle of an enhanced metafile
/// <exclude/>
DEFINE CF_FILES            := 15     // A list of files
/// <exclude/>
DEFINE CF_HDROP            := 15     // A list of files
/// <exclude/>
DEFINE CF_LOCALE           := 16     // A handle to the locale identifier
/// <exclude/>
DEFINE CF_MAX              := 17

*-- Other Miscellaneous Clipboard formats
/// <exclude/>
DEFINE CFSTR_HYPERLINK     := "Hyperlink"               // A Hyperlink
/// <exclude/>
DEFINE CFSTR_BIFF          := "Biff"                    // Microsoft Excel version 2.x
/// <exclude/>
DEFINE CFSTR_BIFF3         := "Biff3"                   // Microsoft Excel version 3.0
/// <exclude/>
DEFINE CFSTR_BIFF4         := "Biff4"                   // Microsoft Excel version 4.0
/// <exclude/>
DEFINE CFSTR_BIFF5         := "Biff5"                   // Microsoft Excel version 5.0
/// <exclude/>
DEFINE CFSTR_BIFF7         := "Biff7"                   // Microsoft Excel version 7.0
/// <exclude/>
DEFINE CFSTR_BIFF8         := "Biff8"                   // Microsoft Excel version 8.0
/// <exclude/>
DEFINE CFSTR_XLTABLE       := "XlTable"                 // Microsoft Excel fast table format.
/// <exclude/>
DEFINE CFSTR_CSV           := "CSV"                     // Comma separated values
/// <exclude/>
DEFINE CFSTR_WK1           := "Wk1"                     // Lotus 1-2-3 Release 2.01 and Release 2.2 format
/// <exclude/>
DEFINE CFSTR_URL           := "UniformResourceLocator"  // URL

/// <exclude/>
DEFINE CFSTR_RTF           := "Rich Text Format"
/// <exclude/>
DEFINE CFSTR_RTFNOOBJS     := "Rich Text Format Without Objects"
/// <exclude/>
DEFINE CFSTR_RETEXTOBJ     := "RichEdit Text and Objects"

*-- Clipboard formats (Private to VFP)
/// <exclude/>
DEFINE CFSTR_OLEVARIANTARRAY :="OLE Variant Array"       // VFP array
/// <exclude/>
DEFINE CFSTR_OLEVARIANT      :="OLE Variant"             // Data in variant form
/// <exclude/>
DEFINE CFSTR_VFPSOURCEOBJECT :="VFP Source Object"       // A reference to the VFP source object

*-- DragMode
/// <exclude/>
DEFINE DRAG_MANUAL          := 0      // 0 - Manual
/// <exclude/>
DEFINE DRAG_AUTOMATIC       := 1      // 1 - Automatic

*-- DragOver
/// <exclude/>
DEFINE DRAG_ENTER          := 0
/// <exclude/>
DEFINE DRAG_LEAVE          := 1
/// <exclude/>
DEFINE DRAG_OVER           := 2

*-- Drag (controls)
/// <exclude/>
DEFINE DRAG_CANCEL          := 0
/// <exclude/>
DEFINE DRAG_BEGIN           := 1
/// <exclude/>
DEFINE DRAG_END             := 2


*-- Properties
*-- Colors
/// <exclude/>
DEFINE COLOR_WHITE       :=16777215
/// <exclude/>
DEFINE COLOR_BLACK       :=       0
/// <exclude/>
DEFINE COLOR_GRAY        :=12632256
/// <exclude/>
DEFINE COLOR_DARK_GRAY   := 8421504
/// <exclude/>
DEFINE COLOR_RED         :=     255
/// <exclude/>
DEFINE COLOR_DARK_BLUE   := 8388608
/// <exclude/>
DEFINE COLOR_CYAN        :=16776960
/// <exclude/>
DEFINE COLOR_DARK_CYAN   := 8421376
/// <exclude/>
DEFINE COLOR_GREEN       :=   65280
/// <exclude/>
DEFINE COLOR_DARK_GREEN  :=   32768
/// <exclude/>
DEFINE COLOR_YELLOW      :=   65535
/// <exclude/>
DEFINE COLOR_DARK_YELLOW :=   32896
/// <exclude/>
DEFINE COLOR_BLUE        :=16711680
/// <exclude/>
DEFINE COLOR_DARK_RED    :=     128
/// <exclude/>
DEFINE COLOR_MAGENTA     :=16711935
/// <exclude/>
DEFINE COLOR_DARK_MAGENTA:= 8388736
/// <exclude/>

*-- MousePointer
/// <exclude/>
DEFINE MOUSE_DEFAULT          := 0       // 0 - Default
/// <exclude/>
DEFINE MOUSE_ARROW            := 1       // 1 - Arrow
/// <exclude/>
DEFINE MOUSE_CROSSHAIR        := 2       // 2 - Cross
/// <exclude/>
DEFINE MOUSE_IBEAM            := 3       // 3 - I-Beam
/// <exclude/>
DEFINE MOUSE_ICON_POINTER     := 4       // 4 - Icon
/// <exclude/>
DEFINE MOUSE_SIZE_POINTER     := 5       // 5 - Size
/// <exclude/>
DEFINE MOUSE_SIZE_NE_SW       := 6       // 6 - Size NE SW
/// <exclude/>
DEFINE MOUSE_SIZE_N_S         := 7       // 7 - Size N S
/// <exclude/>
DEFINE MOUSE_SIZE_NW_SE       := 8       // 8 - Size NW SE
/// <exclude/>
DEFINE MOUSE_SIZE_W_E         := 9       // 9 - Size W E
/// <exclude/>
DEFINE MOUSE_UP_ARROW         := 10      // 10 - Up Arrow
/// <exclude/>
DEFINE MOUSE_HOURGLASS        := 11      // 11 - Hourglass
/// <exclude/>
DEFINE MOUSE_NO_DROP          := 12      // 12 - No drop
/// <exclude/>
DEFINE MOUSE_HIDE_POINTER     := 13      // 13 - Hide Pointer
/// <exclude/>
DEFINE MOUSE_ARROW2           := 14      // 14 - Arrow
/// <exclude/>
DEFINE MOUSE_CUSTOM           := 99      // 99 - Custom

*-- ScrollBars
/// <exclude/>
DEFINE SCROLLBARS_NONE         := 0      // 0 - None (Default)
/// <exclude/>
DEFINE SCROLLBARS_HORIZONTAL   := 1      // 1 - Horizontal
/// <exclude/>
DEFINE SCROLLBARS_VERTICAL     := 2      // 2 - Vertical
/// <exclude/>
DEFINE SCROLLBARS_BOTH         := 3      // 3 - Both

*-- DrawMode
/// <exclude/>
DEFINE DRAWMODE_BLACKNESS     := 1       // 1 - Blackness
/// <exclude/>
DEFINE DRAWMODE_NOT_MERGE_PEN := 2       // 2 - Not Merge Pen
/// <exclude/>
DEFINE DRAWMODE_MASK_NOT_PEN  := 3       // 3 - Mask Not Pen
/// <exclude/>
DEFINE DRAWMODE_NOT_COPY_PEN  := 4       // 4 - Not Copy Pen
/// <exclude/>
DEFINE DRAWMODE_MASK_PEN_NOT  := 5       // 5 - Mask Pen Not
/// <exclude/>
DEFINE DRAWMODE_INVERT        := 6       // 6 - Invert
/// <exclude/>
DEFINE DRAWMODE_XOR_PEN       := 7       // 7 - Xor Pen
/// <exclude/>
DEFINE DRAWMODE_NOT_MASK_PEN  := 8       // 8 - Not Mask Pen
/// <exclude/>
DEFINE DRAWMODE_MASK_PEN      := 9       // 9 - Mask Pen
/// <exclude/>
DEFINE DRAWMODE_NOT_XOR_PEN   := 10      // 10 - Not Xor Pen
/// <exclude/>
DEFINE DRAWMODE_NOP           := 11      // 11 - Nop
/// <exclude/>
DEFINE DRAWMODE_MERGE_NOT_PEN := 12      // 12 - Merge Not Pen
/// <exclude/>
DEFINE DRAWMODE_COPY_PEN      := 13      // 13 - Copy Pen
/// <exclude/>
DEFINE DRAWMODE_MERGE_PEN_NOT := 14      // 14 - Merge Pen Not
/// <exclude/>
DEFINE DRAWMODE_MERGE_PEN     := 15      // 15 - Merge Pen
/// <exclude/>
DEFINE DRAWMODE_WHITENESS     := 16      // 16 - Whiteness

*-- DrawStyle
/// <exclude/>
DEFINE DRAWSTYLE_SOLID         :=  0       // 0 - Solid
/// <exclude/>
DEFINE DRAWSTYLE_DASH          :=  1       // 1 - Dash
/// <exclude/>
DEFINE DRAWSTYLE_DOT           :=  2       // 2 - Dot
/// <exclude/>
DEFINE DRAWSTYLE_DASH_DOT      :=  3       // 3 - Dash-Dot
/// <exclude/>
DEFINE DRAWSTYLE_DASH_DOT_DOT  :=  4       // 4 - Dash-Dot-Dot
/// <exclude/>
DEFINE DRAWSTYLE_INVISIBLE     :=  5       // 5 - Invisible
/// <exclude/>
DEFINE DRAWSTYLE_INSIDE_SOLID  :=  6       // 6 - Inside Solid

*-- FillStyle
/// <exclude/>
DEFINE FILLSTYLE_SOLID               :=    0       // 0 - Solid
/// <exclude/>
DEFINE FILLSTYLE_TRANSPARENT         :=    1       // 1 - Transparent
/// <exclude/>
DEFINE FILLSTYLE_HORIZONTAL_LINE     :=    2       // 2 - Horizontal Line
/// <exclude/>
DEFINE FILLSTYLE_VERTICAL_LINE       :=    3       // 3 - Vertical Line
/// <exclude/>
DEFINE FILLSTYLE_UPWARD_DIAGONAL     :=    4       // 4 - Upward Diagonal
/// <exclude/>
DEFINE FILLSTYLE_DOWNWARD_DIAGONAL   :=    5       // 5 - Downward Diagonal
/// <exclude/>
DEFINE FILLSTYLE_CROSS               :=    6       // 6 - Cross
/// <exclude/>
DEFINE FILLSTYLE_DIAGONAL_CROSS      :=    7       // 7 - Diagonal Cross
                                      
*-- ScaleMode
/// <exclude/>
DEFINE SCALEMODE_PIXELS        :=  3       // 3 - Pixel
/// <exclude/>
DEFINE SCALEMODE_FOXELS        :=  0       // 0 - Foxels

*-- WindowState
/// <exclude/>
DEFINE WINDOWSTATE_NORMAL        :=  0       // 0 - Normal
/// <exclude/>
DEFINE WINDOWSTATE_MINIMIZED     :=  1       // 1 - Minimized
/// <exclude/>
DEFINE WINDOWSTATE_MAXIMIZED     :=  2       // 2 - Maximized

*-- Window Borders
/// <exclude/>
DEFINE BORDER_NONE    := 0
/// <exclude/>
DEFINE BORDER_SINGLE  := 1
/// <exclude/>
DEFINE BORDER_DOUBLE  := 2
/// <exclude/>
DEFINE BORDER_SYSTEM  := 3

*-- Toolbar and Form Docking Positions
/// <exclude/>
DEFINE TOOL_NOTDOCKED:=  -1
/// <exclude/>
DEFINE TOOL_TOP      :=  0
/// <exclude/>
DEFINE TOOL_LEFT     :=  1
/// <exclude/>
DEFINE TOOL_RIGHT    :=  2
/// <exclude/>
DEFINE TOOL_BOTTOM   :=  3
/// <exclude/>
DEFINE TOOL_TAB     :=	4
/// <exclude/>
DEFINE TOOL_LINK     :=	5

*-- Button parameter masks
/// <exclude/>
DEFINE BUTTON_LEFT     :=1
/// <exclude/>
DEFINE BUTTON_RIGHT    :=2
/// <exclude/>
DEFINE BUTTON_MIDDLE   :=4

*-- Function Parameters
*-- MessageBox parameters
/// <exclude/>
DEFINE MB_OK                :=  0       // OK button only
/// <exclude/>
DEFINE MB_OKCANCEL          :=  1       // OK and Cancel buttons
/// <exclude/>
DEFINE MB_ABORTRETRYIGNORE  :=  2       // Abort, Retry, and Ignore buttons
/// <exclude/>
DEFINE MB_YESNOCANCEL       :=  3       // Yes, No, and Cancel buttons
/// <exclude/>
DEFINE MB_YESNO             :=  4       // Yes and No buttons
/// <exclude/>
DEFINE MB_RETRYCANCEL       :=  5       // Retry and Cancel buttons
/// <exclude/>

/// <exclude/>
DEFINE MB_ICONSTOP          :=   16      // Critical message
/// <exclude/>
DEFINE MB_ICONQUESTION      :=   32      // Warning query
/// <exclude/>
DEFINE MB_ICONEXCLAMATION   :=   48      // Warning message
/// <exclude/>
DEFINE MB_ICONINFORMATION   :=   64      // Information message
/// <exclude/>

/// <exclude/>
DEFINE MB_APPLMODAL         :=  0       // Application modal message box
/// <exclude/>
DEFINE MB_DEFBUTTON1        :=  0       // First button is default
/// <exclude/>
DEFINE MB_DEFBUTTON2        :=  256     // Second button is default
/// <exclude/>
DEFINE MB_DEFBUTTON3        :=  512     // Third button is default
/// <exclude/>
DEFINE MB_SYSTEMMODAL       :=  4096    // System Modal
/// <exclude/>

*-- MsgBox return values
/// <exclude/>
DEFINE IDOK          :=1       // OK button pressed
/// <exclude/>
DEFINE IDCANCEL      :=2       // Cancel button pressed
/// <exclude/>
DEFINE IDABORT       :=3       // Abort button pressed
/// <exclude/>
DEFINE IDRETRY       :=4       // Retry button pressed
/// <exclude/>
DEFINE IDIGNORE      :=5       // Ignore button pressed
/// <exclude/>
DEFINE IDYES         :=6       // Yes button pressed
/// <exclude/>
DEFINE IDNO          :=7       // No button pressed
/// <exclude/>
DEFINE IDTIMEOUT 		:=-1		// Timeout occurs
/// <exclude/>


*-- Low Level File Constants
/// <exclude/>
DEFINE F_READONLY             := 0
/// <exclude/>
DEFINE F_WRITEONLY            := 1
/// <exclude/>
DEFINE F_READWRITE            := 2
/// <exclude/>
DEFINE F_READONLY_UNBUFF      := 10
/// <exclude/>
DEFINE F_WRITEONLY_UNBUFF     := 11
/// <exclude/>
DEFINE F_READWRITE_UNBUFF     := 12

*-- PRTINFO() Constants
*-- PRTINFO() Valid types to pass
/// <exclude/>
DEFINE PRT_ORIENTATION       := 1
/// <exclude/>
DEFINE PRT_PAPERSIZE         := 2
/// <exclude/>
DEFINE PRT_PAPERLENGTH       := 3
/// <exclude/>
DEFINE PRT_PAPERWIDTH        := 4
/// <exclude/>
DEFINE PRT_SCALE             := 5
/// <exclude/>
DEFINE PRT_COPIES            := 6
/// <exclude/>
DEFINE PRT_DEFASOURCE        := 7
/// <exclude/>
DEFINE PRT_PRINTQUAL         := 8
/// <exclude/>
DEFINE PRT_COLOR             := 9
/// <exclude/>
DEFINE PRT_DUPLEX            := 10
/// <exclude/>
DEFINE PRT_YRESOLUTION       := 11
/// <exclude/>
DEFINE PRT_TTOPTION          := 12
/// <exclude/>
DEFINE PRT_COLLATE           := 13

*--PRTINFO() Return types
*-- Paper sizes
/// <exclude/>
DEFINE PRTPAPER_LETTER     := 1       // Letter 8 1/2 x 11 in               
/// <exclude/>
DEFINE PRTPAPER_LETTERSMALL:= 2       // Letter Small 8 1/2 x 11 in         
/// <exclude/>
DEFINE PRTPAPER_TABLOID    := 3       // Tabloid 11 x 17 in                 
/// <exclude/>
DEFINE PRTPAPER_LEDGER     := 4       // Ledger 17 x 11 in                  
/// <exclude/>
DEFINE PRTPAPER_LEGAL      := 5       // Legal 8 1/2 x 14 in                
/// <exclude/>
DEFINE PRTPAPER_STATEMENT  := 6       // Statement 5 1/2 x 8 1/2 in         
/// <exclude/>
DEFINE PRTPAPER_EXECUTIVE  := 7       // Executive 7 1/4 x 10 1/2 in      
/// <exclude/>
DEFINE PRTPAPER_A3         := 8       // A3 297 x 420 mm                    
/// <exclude/>
DEFINE PRTPAPER_A4         := 9       // A4 210 x 297 mm                    
/// <exclude/>
DEFINE PRTPAPER_A4SMALL    := 10      // A4 Small 210 x 297 mm              
/// <exclude/>
DEFINE PRTPAPER_A5         := 11      // A5 148 x 210 mm                    
/// <exclude/>
DEFINE PRTPAPER_B4         := 12      // B4 250 x 354                       
/// <exclude/>
DEFINE PRTPAPER_B5         := 13      // B5 182 x 257 mm                    
/// <exclude/>
DEFINE PRTPAPER_FOLIO      := 14      // Folio 8 1/2 x 13 in                
/// <exclude/>
DEFINE PRTPAPER_QUARTO     := 15      // Quarto 215 x 275 mm                
/// <exclude/>
DEFINE PRTPAPER_10X14      := 16      // 10x14 in                           
/// <exclude/>
DEFINE PRTPAPER_11X17      := 17      // 11x17 in                           
/// <exclude/>
DEFINE PRTPAPER_NOTE       := 18      // Note 8 1/2 x 11 in                 
/// <exclude/>
DEFINE PRTPAPER_ENV_9      := 19      // Envelope #9 3 7/8 x 8 7/8          
/// <exclude/>
DEFINE PRTPAPER_ENV_10     := 20      // Envelope #10 4 1/8 x 9 1/2         
/// <exclude/>
DEFINE PRTPAPER_ENV_11     := 21      // Envelope #11 4 1/2 x 10 3/8        
/// <exclude/>
DEFINE PRTPAPER_ENV_12     := 22      // Envelope #12 4 \276 x 11           
/// <exclude/>
DEFINE PRTPAPER_ENV_14     := 23      // Envelope #14 5 x 11 1/2            
/// <exclude/>
DEFINE PRTPAPER_CSHEET     := 24      // C size sheet                       
/// <exclude/>
DEFINE PRTPAPER_DSHEET     := 25      // D size sheet                       
/// <exclude/>
DEFINE PRTPAPER_ESHEET     := 26      // E size sheet                       
/// <exclude/>
DEFINE PRTPAPER_ENV_DL     := 27      // Envelope DL 110 x 220mm            
/// <exclude/>
DEFINE PRTPAPER_ENV_C5     := 28      // Envelope C5 162 x 229 mm           
/// <exclude/>
DEFINE PRTPAPER_ENV_C3     := 29      // Envelope C3  324 x 458 mm          
/// <exclude/>
DEFINE PRTPAPER_ENV_C4     := 30      // Envelope C4  229 x 324 mm          
/// <exclude/>
DEFINE PRTPAPER_ENV_C6     := 31      // Envelope C6  114 x 162 mm          
/// <exclude/>
DEFINE PRTPAPER_ENV_C65    := 32      // Envelope C65 114 x 229 mm          
/// <exclude/>
DEFINE PRTPAPER_ENV_B4     := 33      // Envelope B4  250 x 353 mm          
/// <exclude/>
DEFINE PRTPAPER_ENV_B5     := 34      // Envelope B5  176 x 250 mm          
/// <exclude/>
DEFINE PRTPAPER_ENV_B6     := 35      // Envelope B6  176 x 125 mm          
/// <exclude/>
DEFINE PRTPAPER_ENV_ITALY  := 36      // Envelope 110 x 230 mm              
/// <exclude/>
DEFINE PRTPAPER_ENV_MONARCH :=37      // Envelope Monarch 3.875 x 7.5 in    
/// <exclude/>
DEFINE PRTPAPER_ENV_PERSONAL:= 38     // 6 3/4 Envelope 3 5/8 x 6 1/2 in    
/// <exclude/>
DEFINE PRTPAPER_FANFOLD_US := 39      // US Std Fanfold 14 7/8 x 11 in      
/// <exclude/>
DEFINE PRTPAPER_FANFOLD_STD_GERMAN := 40 // German Std Fanfold 8 1/2 x 12 in   
/// <exclude/>
DEFINE PRTPAPER_FANFOLD_LGL_GERMAN := 41 // German Legal Fanfold 8 1/2 x 13 in 

*-- Paper bins
/// <exclude/>
DEFINE PRTBIN_UPPER           :=1
/// <exclude/>
DEFINE PRTBIN_ONLYONE         :=1
/// <exclude/>
DEFINE PRTBIN_LOWER           :=2
/// <exclude/>
DEFINE PRTBIN_MIDDLE          :=3
/// <exclude/>
DEFINE PRTBIN_MANUAL          :=4
/// <exclude/>
DEFINE PRTBIN_ENVELOPE        :=5
/// <exclude/>
DEFINE PRTBIN_ENVMANUAL       :=6
/// <exclude/>
DEFINE PRTBIN_AUTO            :=7
/// <exclude/>
DEFINE PRTBIN_TRACTOR         :=8
/// <exclude/>
DEFINE PRTBIN_SMALLFMT        :=9
/// <exclude/>
DEFINE PRTBIN_LARGEFMT        :=10
/// <exclude/>
DEFINE PRTBIN_LARGECAPACITY   :=11
/// <exclude/>
DEFINE PRTBIN_CASSETTE        :=14
/// <exclude/>
DEFINE PRTBIN_AUTOSELECT   	:=15
/// <exclude/>

*-- Print qualities
/// <exclude/>
DEFINE PRTRES_DRAFT      :=  -1
/// <exclude/>
DEFINE PRTRES_LOW        :=  -2
/// <exclude/>
DEFINE PRTRES_MEDIUM     :=  -3
/// <exclude/>
DEFINE PRTRES_HIGH       :=  -4

*-- Color printer
/// <exclude/>
DEFINE PRTCOLOR_MONOCHROME := 1
/// <exclude/>
DEFINE PRTCOLOR_COLOR      := 2

*-- Duplexing
/// <exclude/>
DEFINE PRTDUP_SIMPLEX   :=1
/// <exclude/>
DEFINE PRTDUP_VERTICAL  :=2
/// <exclude/>
DEFINE PRTDUP_HORIZONTAL:=3

*-- True Type fonts
/// <exclude/>
DEFINE PRTTT_BITMAP    := 1  // Print True Type fonts as graphics
/// <exclude/>
DEFINE PRTTT_DOWNLOAD  := 2  // Download True Type fonts as soft fonts
/// <exclude/>
DEFINE PRTTT_SUBDEV    := 3  // Substitute device fonts for True Type

*-- FontMetric()
/// <exclude/>
DEFINE TM_HEIGHT         := 1
/// <exclude/>
DEFINE TM_ASCENT         := 2
/// <exclude/>
DEFINE TM_DESCENT        := 3
/// <exclude/>
DEFINE TM_INTERNALLEADING:= 4
/// <exclude/>
DEFINE TM_EXTERNALLEADING:= 5
/// <exclude/>
DEFINE TM_AVECHARWIDTH   := 6
/// <exclude/>
DEFINE TM_MAXCHARWIDTH   := 7
/// <exclude/>
DEFINE TM_WEIGHT         := 8
/// <exclude/>
DEFINE TM_ITALIC         := 9
/// <exclude/>
DEFINE TM_UNDERLINED     :=10
/// <exclude/>
DEFINE TM_STRUCKOUT      :=11
/// <exclude/>
DEFINE TM_FIRSTCHAR      :=12
/// <exclude/>
DEFINE TM_LASTCHAR       :=13
/// <exclude/>
DEFINE TM_DEFAULTCHAR    :=14
/// <exclude/>
DEFINE TM_BREAKCHAR      :=15
/// <exclude/>
DEFINE TM_PITCHANDFAMILY :=16
/// <exclude/>
DEFINE TM_CHARSET        :=17
/// <exclude/>
DEFINE TM_OVERHANG       :=18
/// <exclude/>
DEFINE TM_ASPECTX        :=19
/// <exclude/>
DEFINE TM_ASPECTY        :=20
/// <exclude/>

*-- Sysmetric() parameter values
/// <exclude/>
DEFINE SYSMETRIC_SCREENWIDTH       := 1 // Screen width
/// <exclude/>
DEFINE SYSMETRIC_SCREENHEIGHT      := 2 // Screen width
/// <exclude/>
DEFINE SYSMETRIC_SIZINGBORDERWIDTH := 3 // Width of the sizing border around a resizable window
/// <exclude/>
DEFINE SYSMETRIC_SIZINGBORDERHEIGHT:= 4 // Height of the sizing border around a resizable window
/// <exclude/>
DEFINE SYSMETRIC_VSCROLLBARWIDTH   := 5 // Width of a vertical scroll bar
/// <exclude/>
DEFINE SYSMETRIC_VSCROLLBARHEIGHT  := 6 // Height of the arrow bitmap on a vertical scroll bar
/// <exclude/>
DEFINE SYSMETRIC_HSCROLLBARWIDTH   := 7 // Width of the arrow bitmap on a horizontal scroll bar
/// <exclude/>
DEFINE SYSMETRIC_HSCROLLBARHEIGHT  := 8 // Height of a horizontal scroll bar
/// <exclude/>
DEFINE SYSMETRIC_WINDOWTITLEHEIGHT := 9 // Height of window title (caption) area
/// <exclude/>
DEFINE SYSMETRIC_WINDOWBORDERWIDTH :=10 // Width of a window border
/// <exclude/>
DEFINE SYSMETRIC_WINDOWBORDERHEIGHT:=11 // Height of a window border
/// <exclude/>
DEFINE SYSMETRIC_WINDOWFRAMEWIDTH  :=12 // Width of the frame around the perimeter of a window
/// <exclude/>
                                        // that has a caption but is not sizable
/// <exclude/>
DEFINE SYSMETRIC_WINDOWFRAMEHEIGHT := 13 // Height of the frame around the perimeter of a window
                                        // that has a caption but is not sizable
/// <exclude/>
DEFINE SYSMETRIC_THUMBBOXWIDTH     := 14 // Width of the thumb box in a horizontal scroll bar
/// <exclude/>
DEFINE SYSMETRIC_THUMBBOXHEIGHT    := 15 // Height of the thumb box in a vertical scroll bar
/// <exclude/>
DEFINE SYSMETRIC_ICONWIDTH         := 16 // Width of an icon
/// <exclude/>
DEFINE SYSMETRIC_ICONHEIGHT        := 17 // Height of an icon
/// <exclude/>
DEFINE SYSMETRIC_CURSORWIDTH       := 18 // Width of a cursor
/// <exclude/>
DEFINE SYSMETRIC_CURSORHEIGHT      := 19 // Height of a cursor
/// <exclude/>
DEFINE SYSMETRIC_MENUBAR           := 20 // Height of a single-line menu bar
/// <exclude/>
DEFINE SYSMETRIC_CLIENTWIDTH       := 21 // Width of the client area for a full-screen window
/// <exclude/>
DEFINE SYSMETRIC_CLIENTHEIGHT      := 22 // Height of the client area for a full-screen window
/// <exclude/>
DEFINE SYSMETRIC_KANJIWINHEIGHT    := 23 // Height of the Kanji window at the bottom of the screen in DBCS versions
/// <exclude/>
DEFINE SYSMETRIC_MINDRAGWIDTH      := 24 // Minimum tracking width of a window. (The user cannot drag the window frame to a size smaller than this)
/// <exclude/>
DEFINE SYSMETRIC_MINDRAGHEIGHT     := 25 // Minimum tracking height of a window. (The user cannot drag the window frame to a size smaller than this)
/// <exclude/>
DEFINE SYSMETRIC_MINWINDOWWIDTH    := 26 // Minimum width of a window
/// <exclude/>
DEFINE SYSMETRIC_MINWINDOWHEIGHT   := 27 // Minimum height of a window
/// <exclude/>
DEFINE SYSMETRIC_TITLEBARBUTTONWIDTH := 28 // Width of a title bar button
/// <exclude/>
DEFINE SYSMETRIC_TITLEBARBUTTONHEIGHT:= 29 // Height of a title bar button
/// <exclude/>
DEFINE SYSMETRIC_MOUSEPRESENT      := 30 // Is mouse present?  1 => mouse is installed, 0 => no mouse is installed
/// <exclude/>
DEFINE SYSMETRIC_DEBUGVERSION      := 31 // Is this a debug version?  1 => debug version, 0 => retail version
/// <exclude/>
DEFINE SYSMETRIC_MOUSEBUTTONSWAP   := 32 // Are mouse buttons swapped?  1 => Yes, 0 => No
/// <exclude/>
DEFINE SYSMETRIC_HALFHEIGHTBUTTONWIDTH := 33 // Width of a button in a half-height title bar
/// <exclude/>
DEFINE SYSMETRIC_HALFHEIGHTBUTTONHEIGHT:= 34 // Height of a button in a half-height title bar
/// <exclude/>

*-- Windows GetSysMetric() defines
/// <exclude/>
DEFINE SM_CXSCREEN          :=  0
/// <exclude/>
DEFINE SM_CYSCREEN          :=  1
/// <exclude/>
DEFINE SM_CXVSCROLL         :=  2
/// <exclude/>
DEFINE SM_CYHSCROLL         :=  3
/// <exclude/>
DEFINE SM_CYCAPTION         :=  4
/// <exclude/>
DEFINE SM_CXBORDER          :=  5
/// <exclude/>
DEFINE SM_CYBORDER          :=  6
/// <exclude/>
DEFINE SM_CXDLGFRAME        :=  7
/// <exclude/>
DEFINE SM_CYDLGFRAME        :=  8
/// <exclude/>
DEFINE SM_CYVTHUMB          :=  9
/// <exclude/>
DEFINE SM_CXHTHUMB          :=  10
/// <exclude/>
DEFINE SM_CXICON            :=  11
/// <exclude/>
DEFINE SM_CYICON            :=  12
/// <exclude/>
DEFINE SM_CXCURSOR          :=  13
/// <exclude/>
DEFINE SM_CYCURSOR          :=  14
/// <exclude/>
DEFINE SM_CYMENU            :=  15
/// <exclude/>
DEFINE SM_CXFULLSCREEN      :=  16
/// <exclude/>
DEFINE SM_CYFULLSCREEN      :=  17
/// <exclude/>
DEFINE SM_CYKANJIWINDOW     :=  18
/// <exclude/>
DEFINE SM_MOUSEPRESENT      :=  19
/// <exclude/>
DEFINE SM_CYVSCROLL         :=  20
/// <exclude/>
DEFINE SM_CXHSCROLL         :=  21
/// <exclude/>
DEFINE SM_DEBUG             :=  22
/// <exclude/>
DEFINE SM_SWAPBUTTON        :=  23
/// <exclude/>
DEFINE SM_RESERVED1         :=  24
/// <exclude/>
DEFINE SM_RESERVED2         :=  25
/// <exclude/>
DEFINE SM_RESERVED3         :=  26
/// <exclude/>
DEFINE SM_RESERVED4         :=  27
/// <exclude/>
DEFINE SM_CXMIN             :=  28
/// <exclude/>
DEFINE SM_CYMIN             :=  29
/// <exclude/>
DEFINE SM_CXSIZE            :=  30
/// <exclude/>
DEFINE SM_CYSIZE            :=  31
/// <exclude/>
DEFINE SM_CXFRAME           :=  32
/// <exclude/>
DEFINE SM_CYFRAME           :=  33
/// <exclude/>
DEFINE SM_CXMINTRACK        :=  34
/// <exclude/>
DEFINE SM_CYMINTRACK        :=  35
/// <exclude/>
DEFINE SM_CMETRICS          :=  36
/// <exclude/>

/// <exclude/>
*-- Cursor buffering modes
/// <exclude/>
DEFINE DB_BUFOFF            :=  1
/// <exclude/>
DEFINE DB_BUFLOCKRECORD     :=  2
/// <exclude/>
DEFINE DB_BUFOPTRECORD      :=  3        
/// <exclude/>
DEFINE DB_BUFLOCKTABLE      :=  4
/// <exclude/>
DEFINE DB_BUFOPTTABLE       :=  5
/// <exclude/>

*-- Update types for views/cursors
/// <exclude/>
DEFINE DB_UPDATE             :=  1
/// <exclude/>
DEFINE DB_DELETEINSERT       :=  2

*-- WHERE clause types for views/cursors
/// <exclude/>
DEFINE DB_KEY                := 1
/// <exclude/>
DEFINE DB_KEYANDUPDATABLE    := 2
/// <exclude/>
DEFINE DB_KEYANDMODIFIED     := 3
/// <exclude/>
DEFINE DB_KEYANDTIMESTAMP    := 4

*-- Remote connection login prompt options
/// <exclude/>
DEFINE DB_PROMPTCOMPLETE     :=  1
/// <exclude/>
DEFINE DB_PROMPTALWAYS       :=  2
/// <exclude/>
DEFINE DB_PROMPTNEVER        :=  3

*-- Remote transaction modes
/// <exclude/>
DEFINE DB_TRANSAUTO          :=  1
/// <exclude/>
DEFINE DB_TRANSMANUAL        :=  2

*-- Source Types for CursorGetProp()
/// <exclude/>
DEFINE DB_SRCLOCALVIEW       :=  1
/// <exclude/>
DEFINE DB_SRCREMOTEVIEW      :=  2
/// <exclude/>
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

/// <exclude/>
DEFINE LANG_NEUTRAL                   :=  0x00
/// <exclude/>
DEFINE LANG_AFRIKAANS                 := 0x36
/// <exclude/>
DEFINE LANG_ALBANIAN                  := 0x1c
/// <exclude/>
DEFINE LANG_ARABIC                    := 0x01
/// <exclude/>
DEFINE LANG_BASQUE                    := 0x2d
/// <exclude/>
DEFINE LANG_BELARUSIAN                := 0x23
/// <exclude/>
DEFINE LANG_BULGARIAN                 := 0x02
/// <exclude/>
DEFINE LANG_CATALAN                   := 0x03
/// <exclude/>
DEFINE LANG_CHINESE                   := 0x04
/// <exclude/>
DEFINE LANG_CROATIAN                  := 0x1a
/// <exclude/>
DEFINE LANG_CZECH                     := 0x05
/// <exclude/>
DEFINE LANG_DANISH                    := 0x06
/// <exclude/>
DEFINE LANG_DUTCH                     := 0x13
/// <exclude/>
DEFINE LANG_ENGLISH                   := 0x09
/// <exclude/>
DEFINE LANG_ESTONIAN                  := 0x25
/// <exclude/>
DEFINE LANG_FAEROESE                  := 0x38
/// <exclude/>
DEFINE LANG_FARSI                     := 0x29
/// <exclude/>
DEFINE LANG_FINNISH                   := 0x0b
/// <exclude/>
DEFINE LANG_FRENCH                    := 0x0c
/// <exclude/>
DEFINE LANG_GERMAN                    := 0x07
/// <exclude/>
DEFINE LANG_GREEK                     := 0x08
/// <exclude/>
DEFINE LANG_HEBREW                    := 0x0d
/// <exclude/>
DEFINE LANG_HUNGARIAN                 := 0x0e
/// <exclude/>
DEFINE LANG_ICELANDIC                 := 0x0f
/// <exclude/>
DEFINE LANG_INDONESIAN                := 0x21
/// <exclude/>
DEFINE LANG_ITALIAN                   := 0x10
/// <exclude/>
DEFINE LANG_JAPANESE                  := 0x11
/// <exclude/>
DEFINE LANG_KOREAN                    := 0x12
/// <exclude/>
DEFINE LANG_LATVIAN                   := 0x26
/// <exclude/>
DEFINE LANG_LITHUANIAN                := 0x27
/// <exclude/>
DEFINE LANG_NORWEGIAN                 := 0x14
/// <exclude/>
DEFINE LANG_POLISH                    := 0x15
/// <exclude/>
DEFINE LANG_PORTUGUESE                := 0x16
/// <exclude/>
DEFINE LANG_ROMANIAN                  := 0x18
/// <exclude/>
DEFINE LANG_RUSSIAN                   := 0x19
/// <exclude/>
DEFINE LANG_SERBIAN                   := 0x1a
/// <exclude/>
DEFINE LANG_SLOVAK                    := 0x1b
/// <exclude/>
DEFINE LANG_SLOVENIAN                 := 0x24
/// <exclude/>
DEFINE LANG_SPANISH                   := 0x0a
/// <exclude/>
DEFINE LANG_SWEDISH                   := 0x1d
/// <exclude/>
DEFINE LANG_THAI                      := 0x1e
/// <exclude/>
DEFINE LANG_TURKISH                   := 0x1f
/// <exclude/>
DEFINE LANG_UKRAINIAN                 := 0x22
/// <exclude/>
DEFINE LANG_VIETNAMESE                :=  0x2a
/// <exclude/>

*
*  Sublanguage IDs.
*
*  The name immediately following SUBLANG_ dictates which primary
*  language ID that sublanguage ID can be combined with to form a
*  valid language ID.
*

/// <exclude/>
DEFINE SUBLANG_NEUTRAL                :=  0x0000    // language neutral
/// <exclude/>
DEFINE SUBLANG_DEFAULT                :=  0x0400    // user default
/// <exclude/>
DEFINE SUBLANG_SYS_DEFAULT            :=  0x0800    // system default

/// <exclude/>
DEFINE SUBLANG_ARABIC_SAUDI_ARABIA   :=  0x0400    // Arabic (Saudi Arabia)
/// <exclude/>
DEFINE SUBLANG_ARABIC_IRAQ           :=  0x0800    // Arabic (Iraq)
/// <exclude/>
DEFINE SUBLANG_ARABIC_EGYPT          :=  0x0C00    // Arabic (Egypt)
/// <exclude/>
DEFINE SUBLANG_ARABIC_LIBYA          :=  0x1000    // Arabic (Libya)
/// <exclude/>
DEFINE SUBLANG_ARABIC_ALGERIA        :=  0x1400    // Arabic (Algeria)
/// <exclude/>
DEFINE SUBLANG_ARABIC_MOROCCO        :=  0x1800    // Arabic (Morocco)
/// <exclude/>
DEFINE SUBLANG_ARABIC_TUNISIA        :=  0x1C00    // Arabic (Tunisia)
/// <exclude/>
DEFINE SUBLANG_ARABIC_OMAN           :=  0x2000    // Arabic (Oman)
/// <exclude/>
DEFINE SUBLANG_ARABIC_YEMEN          :=  0x2400    // Arabic (Yemen)
/// <exclude/>
DEFINE SUBLANG_ARABIC_SYRIA          :=  0x2800    // Arabic (Syria)
/// <exclude/>
DEFINE SUBLANG_ARABIC_JORDAN         :=  0x2C00    // Arabic (Jordan)
/// <exclude/>
DEFINE SUBLANG_ARABIC_LEBANON        :=  0x3000    // Arabic (Lebanon)
/// <exclude/>
DEFINE SUBLANG_ARABIC_KUWAIT         :=  0x3400    // Arabic (Kuwait)
/// <exclude/>
DEFINE SUBLANG_ARABIC_UAE            :=  0x3800    // Arabic (U.A.E)
/// <exclude/>
DEFINE SUBLANG_ARABIC_BAHRAIN        :=  0x3C00    // Arabic (Bahrain)
/// <exclude/>
DEFINE SUBLANG_ARABIC_QATAR          :=  0x4000    // Arabic (Qatar)
/// <exclude/>
DEFINE SUBLANG_CHINESE_TRADITIONAL   :=  0x0400    // Chinese (Taiwan)
/// <exclude/>
DEFINE SUBLANG_CHINESE_SIMPLIFIED    :=  0x0800    // Chinese (PR China)
/// <exclude/>
DEFINE SUBLANG_CHINESE_HONGKONG      :=  0x0C00    // Chinese (Hong Kong S.A.R)
/// <exclude/>
DEFINE SUBLANG_CHINESE_SINGAPORE     :=  0x1000    // Chinese (Singapore)
/// <exclude/>
DEFINE SUBLANG_DUTCH                 :=  0x0400    // Dutch
/// <exclude/>
DEFINE SUBLANG_DUTCH_BELGIAN         :=  0x0800    // Dutch (Belgian)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_US            :=  0x0400    // English (USA)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_UK            :=  0x0800    // English (UK)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_AUS           :=  0x0C00    // English (Australian)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_CAN           :=  0x1000    // English (Canadian)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_NZ            :=  0x1400    // English (New Zealand)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_EIRE          :=  0x1800    // English (Irish)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_SOUTH_AFRICA  :=  0x1C00    // English (South Africa)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_JAMAICA       :=  0x2000    // English (Jamaica)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_CARIBBEAN     :=  0x2400    // English (Caribbean)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_BELIZE        :=  0x2800    // English (Belize)
/// <exclude/>
DEFINE SUBLANG_ENGLISH_TRINIDAD      :=  0x2C00    // English (Trinidad)
/// <exclude/>
DEFINE SUBLANG_FRENCH                :=  0x0400    // French
/// <exclude/>
DEFINE SUBLANG_FRENCH_BELGIAN        :=  0x0800    // French (Belgian)
/// <exclude/>
DEFINE SUBLANG_FRENCH_CANADIAN       :=  0x0C00    // French (Canadian)
/// <exclude/>
DEFINE SUBLANG_FRENCH_SWISS          :=  0x1000    // French (Swiss)
/// <exclude/>
DEFINE SUBLANG_FRENCH_LUXEMBOURG     :=  0x1400    // French (Luxembourg)
/// <exclude/>
DEFINE SUBLANG_GERMAN                :=  0x0400    // German
/// <exclude/>
DEFINE SUBLANG_GERMAN_SWISS          := 0x0800    // German (Swiss)
/// <exclude/>
DEFINE SUBLANG_GERMAN_AUSTRIAN       := 0x0C00    // German (Austrian)
/// <exclude/>
DEFINE SUBLANG_GERMAN_LUXEMBOURG     := 0x1000    // German (Luxembourg)
/// <exclude/>
DEFINE SUBLANG_GERMAN_LIECHTENSTEIN  := 0x1400    // German (Liechtenstein)
/// <exclude/>
DEFINE SUBLANG_ITALIAN               := 0x0400    // Italian
/// <exclude/>
DEFINE SUBLANG_ITALIAN_SWISS         := 0x0800    // Italian (Swiss)
/// <exclude/>
DEFINE SUBLANG_KOREAN                := 0x0400    // Korean (Extended Wansung)
/// <exclude/>
DEFINE SUBLANG_KOREAN_JOHAB          := 0x0800    // Korean (Johab)
/// <exclude/>
DEFINE SUBLANG_NORWEGIAN_BOKMAL      := 0x0400    // Norwegian (Bokmal)
/// <exclude/>
DEFINE SUBLANG_NORWEGIAN_NYNORSK     := 0x0800    // Norwegian (Nynorsk)
/// <exclude/>
DEFINE SUBLANG_PORTUGUESE            := 0x0800    // Portuguese
/// <exclude/>
DEFINE SUBLANG_PORTUGUESE_BRAZILIAN  := 0x0400    // Portuguese (Brazilian)
/// <exclude/>
DEFINE SUBLANG_SERBIAN_LATIN         := 0x0800    // Serbian (Latin)
/// <exclude/>
DEFINE SUBLANG_SERBIAN_CYRILLIC      := 0x0C00    // Serbian (Cyrillic)
/// <exclude/>
DEFINE SUBLANG_SPANISH               := 0x0400    // Spanish (Castilian)
/// <exclude/>
DEFINE SUBLANG_SPANISH_MEXICAN       := 0x0800    // Spanish (Mexican)
/// <exclude/>
DEFINE SUBLANG_SPANISH_MODERN        := 0x0C00    // Spanish (Modern)
/// <exclude/>
DEFINE SUBLANG_SPANISH_GUATEMALA     := 0x1000    // Spanish (Guatemala)
/// <exclude/>
DEFINE SUBLANG_SPANISH_COSTA_RICA    := 0x1400    // Spanish (Costa Rica)
/// <exclude/>
DEFINE SUBLANG_SPANISH_PANAMA        :=   0x1800    // Spanish (Panama)
/// <exclude/>
DEFINE SUBLANG_SPANISH_DOMINICAN_REPUBLIC:= 0x1C00  // Spanish (Dominican Republic)
/// <exclude/>
DEFINE SUBLANG_SPANISH_VENEZUELA     :=   0x2000    // Spanish (Venezuela)
/// <exclude/>
DEFINE SUBLANG_SPANISH_COLOMBIA      :=   0x2400    // Spanish (Colombia)
/// <exclude/>
DEFINE SUBLANG_SPANISH_PERU          :=   0x2800    // Spanish (Peru)
/// <exclude/>
DEFINE SUBLANG_SPANISH_ARGENTINA     :=   0x2C00    // Spanish (Argentina)
/// <exclude/>
DEFINE SUBLANG_SPANISH_ECUADOR       :=   0x3000    // Spanish (Ecuador)
/// <exclude/>
DEFINE SUBLANG_SPANISH_CHILE         :=   0x3400    // Spanish (Chile)
/// <exclude/>
DEFINE SUBLANG_SPANISH_URUGUAY       :=   0x3800    // Spanish (Uruguay)
/// <exclude/>
DEFINE SUBLANG_SPANISH_PARAGUAY      :=   0x3C00    // Spanish (Paraguay)
/// <exclude/>
DEFINE SUBLANG_SPANISH_BOLIVIA       :=   0x4000    // Spanish (Bolivia)
/// <exclude/>
DEFINE SUBLANG_SPANISH_EL_SALVADOR   :=   0x4400    // Spanish (El Salvador)
/// <exclude/>
DEFINE SUBLANG_SPANISH_HONDURAS      :=  0x4800    // Spanish (Honduras)
/// <exclude/>
DEFINE SUBLANG_SPANISH_NICARAGUA     :=  0x4C00    // Spanish (Nicaragua)
/// <exclude/>
DEFINE SUBLANG_SPANISH_PUERTO_RICO   :=  0x5000    // Spanish (Puerto Rico)
/// <exclude/>
DEFINE SUBLANG_SWEDISH               :=  0x0400    // Swedish
/// <exclude/>
DEFINE SUBLANG_SWEDISH_FINLAND       :=  0x0800    // Swedish (Finland)

*-- DBC Events constants
/// <exclude/>
DEFINE DBCEVENTS_NULLTYPE:=0
/// <exclude/>
DEFINE DBCEVENTS_DATABASE:=1
/// <exclude/>
DEFINE DBCEVENTS_TABLE		:=2
/// <exclude/>
DEFINE DBCEVENTS_COLUMN	:=	3
/// <exclude/>
DEFINE DBCEVENTS_INDEX		:=4
/// <exclude/>
DEFINE DBCEVENTS_TAG		:=	4
/// <exclude/>
DEFINE DBCEVENTS_RELATION	:=5
/// <exclude/>
DEFINE DBCEVENTS_CONNECTION:=	6
/// <exclude/>
DEFINE	DBCEVENTS_VIEW		:=7
/// <exclude/>
DEFINE DBCEVENTS_LASTTYPE	:=8
/// <exclude/>
DEFINE DBCEVENTS_STPROCS	:=	9	
/// <exclude/>

*-- AFields() Constants
/// <exclude/>
DEFINE AFIELDS_NAME				:=		1
/// <exclude/>
DEFINE AFIELDS_TYPE				:=		2
/// <exclude/>
DEFINE AFIELDS_WIDTH				:=		3
/// <exclude/>
DEFINE AFIELDS_PRECISION			:=		4
/// <exclude/>
DEFINE AFIELDS_NULL				:=		5
/// <exclude/>
DEFINE AFIELDS_CODEPAGE_XLATE		:=	6
/// <exclude/>
DEFINE AFIELDS_VALID_EXPRESSION	:=		7
/// <exclude/>
DEFINE AFIELDS_VALID_TEXT			:=	8
/// <exclude/>
DEFINE AFIELDS_DEFAULT				:=	9
/// <exclude/>
DEFINE AFIELDS_TABLE_VALID_EXPRESSION:=10
/// <exclude/>
DEFINE AFIELDS_TABLE_VALID_TEXT		:=	11
/// <exclude/>
DEFINE AFIELDS_TABLE_LONG_NAME			:=12
/// <exclude/>
DEFINE AFIELDS_INSERT_TRIGGER			:=13
/// <exclude/>
DEFINE AFIELDS_UPDATE_TRIGGER			:=14
/// <exclude/>
DEFINE AFIELDS_DELETE_TRIGGER			:=15
/// <exclude/>
DEFINE AFIELDS_TABLE_COMMENT			:=	16
/// <exclude/>
DEFINE AFIELDS_AUTOINC_NEXT_VALUE		:=	17
/// <exclude/>
DEFINE AFIELDS_AUTOINC_STEP			:=	18

* UCS Transformation Format byte order marks.
/// <exclude/>
DEFINE ENCODE_UTF16	:= e"\uFFFE"
/// <exclude/>
DEFINE ENCODE_UTF8		:=""
/// <exclude/>

* COM_Attrib flag settings for Type Library attributes support
/// <exclude/>
DEFINE COMATTRIB_RESTRICTED	:=0x1			// The property/method should not be accessible from macro languages.
/// <exclude/>
DEFINE COMATTRIB_HIDDEN		:=0x40		// The property/method should not be displayed to the user, although it exists and is bindable.
/// <exclude/>
DEFINE COMATTRIB_NONBROWSABLE	:=0x400		// The property/method appears in an object browser, but not in a properties browser.
/// <exclude/>
DEFINE COMATTRIB_READONLY		:=0x100000	// The property is read-only (applies only to Properties).
/// <exclude/>
DEFINE COMATTRIB_WRITEONLY		:=0x200000	// The property is write-only (applies only to Properties).
/// <exclude/>

