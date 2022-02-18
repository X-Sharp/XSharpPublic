﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

INTERNAL DEFINE BS_PUSHBUTTON             := 0x00000000L
INTERNAL DEFINE BS_DEFPUSHBUTTON      := 0x00000001L
INTERNAL DEFINE BS_CHECKBOX               := 0x00000002L
INTERNAL DEFINE BS_AUTOCHECKBOX       := 0x00000003L
INTERNAL DEFINE BS_RADIOBUTTON        := 0x00000004L
INTERNAL DEFINE BS_3STATE                     := 0x00000005L
INTERNAL DEFINE BS_AUTO3STATE             := 0x00000006L
INTERNAL DEFINE BS_GROUPBOX               := 0x00000007L
INTERNAL DEFINE BS_USERBUTTON             := 0x00000008L
INTERNAL DEFINE BS_AUTORADIOBUTTON  := 0x00000009L
INTERNAL DEFINE BS_PUSHBOX          := 0x0000000AL
INTERNAL DEFINE BS_OWNERDRAW              := 0x0000000BL
INTERNAL DEFINE BS_TYPEMASK         := 0x0000000FL
INTERNAL DEFINE BS_LEFTTEXT               := 0x00000020L
INTERNAL DEFINE BS_TEXT                       := 0x00000000L
INTERNAL DEFINE BS_ICON                       := 0x00000040L
INTERNAL DEFINE BS_BITMAP                     := 0x00000080L
INTERNAL DEFINE BS_LEFT                       := 0x00000100L
INTERNAL DEFINE BS_RIGHT                      := 0x00000200L
INTERNAL DEFINE BS_CENTER                     := 0x00000300L
INTERNAL DEFINE BS_TOP                        := 0x00000400L
INTERNAL DEFINE BS_BOTTOM                     := 0x00000800L
INTERNAL DEFINE BS_VCENTER                := 0x00000C00L
INTERNAL DEFINE BS_PUSHLIKE               := 0x00001000L
INTERNAL DEFINE BS_MULTILINE              := 0x00002000L
INTERNAL DEFINE BS_NOTIFY                     := 0x00004000L
INTERNAL DEFINE BS_FLAT                       := 0x00008000L
INTERNAL DEFINE BS_RIGHTBUTTON        := BS_LEFTTEXT
INTERNAL DEFINE CBS_SIMPLE                    := 0x0001L
INTERNAL DEFINE CBS_DROPDOWN                  := 0x0002L
INTERNAL DEFINE CBS_DROPDOWNLIST          := 0x0003L
INTERNAL DEFINE CBS_OWNERDRAWFIXED    := 0x0010L
INTERNAL DEFINE CBS_OWNERDRAWVARIABLE := 0x0020L
INTERNAL DEFINE CBS_AUTOHSCROLL           := 0x0040L
INTERNAL DEFINE CBS_OEMCONVERT            := 0x0080L
INTERNAL DEFINE CBS_SORT                          := 0x0100L
INTERNAL DEFINE CBS_HASSTRINGS            := 0x0200L
INTERNAL DEFINE CBS_NOINTEGRALHEIGHT  := 0x0400L
INTERNAL DEFINE CBS_DISABLENOSCROLL   := 0x0800L
INTERNAL DEFINE CBS_UPPERCASE                     := 0x2000L
INTERNAL DEFINE CBS_LOWERCASE                     := 0x4000L

INTERNAL DEFINE DS_ABSALIGN               := 0x01L
INTERNAL DEFINE DS_SYSMODAL               := 0x02L
INTERNAL DEFINE DS_LOCALEDIT              := 0x20L
INTERNAL DEFINE DS_SETFONT                := 0x40L
INTERNAL DEFINE DS_MODALFRAME             := 0x80L
INTERNAL DEFINE DS_NOIDLEMSG              := 0x100L
INTERNAL DEFINE DS_SETFOREGROUND      := 0x200L
INTERNAL DEFINE DS_3DLOOK                     := 0x0004L
INTERNAL DEFINE DS_FIXEDSYS               := 0x0008L
INTERNAL DEFINE DS_NOFAILCREATE       := 0x0010L
INTERNAL DEFINE DS_CONTROL                := 0x0400L
INTERNAL DEFINE DS_CENTER                     := 0x0800L
INTERNAL DEFINE DS_CENTERMOUSE        := 0x1000L
INTERNAL DEFINE DS_CONTEXTHELP        := 0x2000L
INTERNAL DEFINE DS_SHELLFONT        := DS_SETFONT | DS_FIXEDSYS
INTERNAL DEFINE DS_USEPIXELS        := 0x8000L

INTERNAL DEFINE ES_AUTOVSCROLL        := 0x0040L
INTERNAL DEFINE ES_AUTOHSCROLL        := 0x0080L
INTERNAL DEFINE ES_NOHIDESEL              := 0x0100L
INTERNAL DEFINE ES_OEMCONVERT             := 0x0400L
INTERNAL DEFINE ES_READONLY               := 0x0800L
INTERNAL DEFINE ES_WANTRETURN             := 0x1000L
INTERNAL DEFINE ES_NUMBER                    := 0x2000L
INTERNAL DEFINE ES_LEFT                       := 0x0000L
INTERNAL DEFINE ES_CENTER                     := 0x0001L
INTERNAL DEFINE ES_RIGHT                      := 0x0002L
INTERNAL DEFINE ES_MULTILINE              := 0x0004L
INTERNAL DEFINE ES_UPPERCASE              := 0x0008L
INTERNAL DEFINE ES_LOWERCASE              := 0x0010L
INTERNAL DEFINE ES_PASSWORD               := 0x0020L

INTERNAL DEFINE LBS_NOTIFY                    := 0x0001L
INTERNAL DEFINE LBS_SORT                          := 0x0002L
INTERNAL DEFINE LBS_NOREDRAW                  := 0x0004L
INTERNAL DEFINE LBS_MULTIPLESEL           := 0x0008L
INTERNAL DEFINE LBS_OWNERDRAWFIXED    := 0x0010L
INTERNAL DEFINE LBS_OWNERDRAWVARIABLE := 0x0020L
INTERNAL DEFINE LBS_HASSTRINGS            := 0x0040L
INTERNAL DEFINE LBS_USETABSTOPS           := 0x0080L
INTERNAL DEFINE LBS_NOINTEGRALHEIGHT  := 0x0100L
INTERNAL DEFINE LBS_MULTICOLUMN           := 0x0200L
INTERNAL DEFINE LBS_WANTKEYBOARDINPUT := 0x0400L
INTERNAL DEFINE LBS_EXTENDEDSEL           := 0x0800L
INTERNAL DEFINE LBS_DISABLENOSCROLL   := 0x1000L
INTERNAL DEFINE LBS_NODATA                    := 0x2000L
INTERNAL DEFINE LBS_NOSEL                         := 0x4000L
INTERNAL DEFINE LBS_COMBOBOX          :=0x8000L
INTERNAL DEFINE LBS_STANDARD          := (LBS_NOTIFY | LBS_SORT | WS_VSCROLL | WS_BORDER)

INTERNAL DEFINE SBS_HORZ                                      := 0x0000L
INTERNAL DEFINE SBS_VERT                                      := 0x0001L
INTERNAL DEFINE SBS_TOPALIGN                              := 0x0002L
INTERNAL DEFINE SBS_LEFTALIGN                             := 0x0002L
INTERNAL DEFINE SBS_BOTTOMALIGN                       := 0x0004L
INTERNAL DEFINE SBS_RIGHTALIGN                        := 0x0004L
INTERNAL DEFINE SBS_SIZEBOXTOPLEFTALIGN       := 0x0002L
INTERNAL DEFINE SBS_SIZEBOXBOTTOMRIGHTALIGN := 0x0004L
INTERNAL DEFINE SBS_SIZEBOX                               := 0x0008L
INTERNAL DEFINE SBS_SIZEGRIP                              := 0x0010L

INTERNAL DEFINE SS_LEFT                       := 0x00000000L
INTERNAL DEFINE SS_CENTER                     := 0x00000001L
INTERNAL DEFINE SS_RIGHT                      := 0x00000002L
INTERNAL DEFINE SS_ICON                       := 0x00000003L
INTERNAL DEFINE SS_BLACKRECT              := 0x00000004L
INTERNAL DEFINE SS_GRAYRECT               := 0x00000005L
INTERNAL DEFINE SS_WHITERECT              := 0x00000006L
INTERNAL DEFINE SS_BLACKFRAME             := 0x00000007L
INTERNAL DEFINE SS_GRAYFRAME              := 0x00000008L
INTERNAL DEFINE SS_WHITEFRAME             := 0x00000009L
INTERNAL DEFINE SS_USERITEM               := 0x0000000AL
INTERNAL DEFINE SS_SIMPLE                     := 0x0000000BL
INTERNAL DEFINE SS_LEFTNOWORDWRAP     := 0x0000000CL
INTERNAL DEFINE SS_OWNERDRAW              := 0x0000000DL
INTERNAL DEFINE SS_BITMAP                     := 0x0000000EL
INTERNAL DEFINE SS_ENHMETAFILE        := 0x0000000FL
INTERNAL DEFINE SS_ETCHEDHORZ             := 0x00000010L
INTERNAL DEFINE SS_ETCHEDVERT             := 0x00000011L
INTERNAL DEFINE SS_ETCHEDFRAME        := 0x00000012L
INTERNAL DEFINE SS_TYPEMASK               := 0x0000001FL
INTERNAL DEFINE SS_REALSIZECONTROL  :=0x00000040L
INTERNAL DEFINE SS_NOPREFIX               := 0x00000080L
INTERNAL DEFINE SS_NOTIFY                     := 0x00000100L
INTERNAL DEFINE SS_CENTERIMAGE        := 0x00000200L
INTERNAL DEFINE SS_RIGHTJUST              := 0x00000400L
INTERNAL DEFINE SS_REALSIZEIMAGE      := 0x00000800L
INTERNAL DEFINE SS_SUNKEN                     := 0x00001000L
INTERNAL DEFINE SS_EDITCONTROL      :=0x00002000L
INTERNAL DEFINE SS_ENDELLIPSIS      :=0x00004000L
INTERNAL DEFINE SS_PATHELLIPSIS     :=0x00008000L
INTERNAL DEFINE SS_WORDELLIPSIS     :=0x0000C000L
INTERNAL DEFINE SS_ELLIPSISMASK     :=0x0000C000L

INTERNAL DEFINE WS_OVERLAPPED          := 0x00000000L
INTERNAL DEFINE WS_POPUP               := 0x80000000L
INTERNAL DEFINE WS_CHILD               := 0x40000000L
INTERNAL DEFINE WS_MINIMIZE            := 0x20000000L
INTERNAL DEFINE WS_VISIBLE             := 0x10000000L
INTERNAL DEFINE WS_DISABLED            := 0x08000000L
INTERNAL DEFINE WS_CLIPSIBLINGS        := 0x04000000L
INTERNAL DEFINE WS_CLIPCHILDREN        := 0x02000000L
INTERNAL DEFINE WS_MAXIMIZE            := 0x01000000L
INTERNAL DEFINE WS_CAPTION             := 0x00C00000L
INTERNAL DEFINE WS_BORDER              := 0x00800000L
INTERNAL DEFINE WS_DLGFRAME            := 0x00400000L
INTERNAL DEFINE WS_VSCROLL             := 0x00200000L
INTERNAL DEFINE WS_HSCROLL             := 0x00100000L
INTERNAL DEFINE WS_SYSMENU             := 0x00080000L
INTERNAL DEFINE WS_THICKFRAME          := 0x00040000L
INTERNAL DEFINE WS_GROUP               := 0x00020000L
INTERNAL DEFINE WS_TABSTOP             := 0x00010000L
INTERNAL DEFINE WS_MINIMIZEBOX        := 0x00020000L
INTERNAL DEFINE WS_MAXIMIZEBOX        := 0x00010000L
INTERNAL DEFINE WS_TILED                      := WS_OVERLAPPED
INTERNAL DEFINE WS_ICONIC                     := WS_MINIMIZE
INTERNAL DEFINE WS_SIZEBOX                := WS_THICKFRAME
INTERNAL DEFINE WS_TILEDWINDOW        := WS_OVERLAPPEDWINDOW
INTERNAL DEFINE WS_OVERLAPPEDWINDOW := 0x00CF0000L
INTERNAL DEFINE WS_POPUPWINDOW        :=0X80880000L
INTERNAL DEFINE WS_CHILDWINDOW        := (WS_CHILD)
INTERNAL DEFINE WS_EX_DLGMODALFRAME  := 0x00000001L
INTERNAL DEFINE WS_EX_NOPARENTNOTIFY := 0x00000004L
INTERNAL DEFINE WS_EX_TOPMOST                := 0x00000008L
INTERNAL DEFINE WS_EX_ACCEPTFILES        := 0x00000010L
INTERNAL DEFINE WS_EX_TRANSPARENT        := 0x00000020L
INTERNAL DEFINE WS_EX_MDICHILD                := 0x00000040L
INTERNAL DEFINE WS_EX_TOOLWINDOW              := 0x00000080L
INTERNAL DEFINE WS_EX_WINDOWEDGE              := 0x00000100L
INTERNAL DEFINE WS_EX_CLIENTEDGE              := 0x00000200L
INTERNAL DEFINE WS_EX_CONTEXTHELP             := 0x00000400L
INTERNAL DEFINE WS_EX_RIGHT                       := 0x00001000L
INTERNAL DEFINE WS_EX_LEFT                        := 0x00000000L
INTERNAL DEFINE WS_EX_RTLREADING              := 0x00002000L
INTERNAL DEFINE WS_EX_LTRREADING              := 0x00000000L
INTERNAL DEFINE WS_EX_LEFTSCROLLBAR       := 0x00004000L
INTERNAL DEFINE WS_EX_RIGHTSCROLLBAR      := 0x00000000L
INTERNAL DEFINE WS_EX_CONTROLPARENT       := 0x00010000L
INTERNAL DEFINE WS_EX_STATICEDGE              := 0x00020000L
INTERNAL DEFINE WS_EX_APPWINDOW               := 0x00040000L
INTERNAL DEFINE WS_EX_OVERLAPPEDWINDOW := 0x00000300L
INTERNAL DEFINE WS_EX_PALETTEWINDOW      := 0x00000188L
INTERNAL DEFINE WS_EX_LAYERED           := 0x00080000
INTERNAL DEFINE WS_EX_NOINHERITLAYOUT   := 0x00100000L // Disable inheritence of mirroring by children
INTERNAL DEFINE WS_EX_LAYOUTRTL         := 0x00400000L // Right to left mirroring

INTERNAL DEFINE ACS_CENTER 						 := 0x0001
INTERNAL DEFINE ACS_TRANSPARENT				 := 0x0002
INTERNAL DEFINE ACS_AUTOPLAY 					 := 0x0004

INTERNAL DEFINE DTS_UPDOWN          := 0x0001 // use UPDOWN instead of MONTHCAL
INTERNAL DEFINE DTS_SHOWNONE        := 0x0002 // allow a NONE selection
INTERNAL DEFINE DTS_SHORTDATEFORMAT := 0x0000 // use the short date format (app must forward WM_winINICHANGE messages)
INTERNAL DEFINE DTS_LONGDATEFORMAT  := 0x0004 // use the long date format (app must forward WM_winINICHANGE messages)
INTERNAL DEFINE DTS_TIMEFORMAT      := 0x0009 // use the time format (app must forward WM_winINICHANGE messages)
INTERNAL DEFINE DTS_APPCANPARSE     := 0x0010 // allow user entered strings (app MUST respond to DTN_USERSTRING)
INTERNAL DEFINE DTS_RIGHTALIGN      := 0x0020 // right-align popup instead of left-align it


INTERNAL DEFINE ES_SAVESEL						:= 0x00008000
INTERNAL DEFINE ES_SUNKEN			:= 0x00004000
INTERNAL DEFINE ES_DISABLENOSCROLL		:= 0x00002000
// Same as WS_MAXIMIZE, but that doesn't make sense so we re-use the value 
INTERNAL DEFINE ES_SELECTIONBAR			:= 0x01000000
// Same as ES_UPPERCASE, but re-used to completely disable OLE drag'n'drop 
INTERNAL DEFINE ES_NOOLEDRAGDROP			:= 0x00000008
INTERNAL DEFINE ES_EX_NOCALLOLEINIT		:= 0x01000000
// These flags are used in FE Windows 
INTERNAL DEFINE ES_VERTICAL			:= 0x00400000
INTERNAL DEFINE ES_NOIME			:= 0x00080000
INTERNAL DEFINE ES_SELFIME			:= 0x00040000



INTERNAL DEFINE LVS_ICON 							 := 0x0000
INTERNAL DEFINE LVS_REPORT 						 := 0x0001
INTERNAL DEFINE LVS_SMALLICON					 := 0x0002
INTERNAL DEFINE LVS_LIST 							 := 0x0003
INTERNAL DEFINE LVS_TYPEMASK 					 := 0x0003
INTERNAL DEFINE LVS_SINGLESEL					 := 0x0004
INTERNAL DEFINE LVS_SHOWSELALWAYS			 := 0x0008
INTERNAL DEFINE LVS_SORTASCENDING			 := 0x0010
INTERNAL DEFINE LVS_SORTDESCENDING 		 := 0x0020
INTERNAL DEFINE LVS_SHAREIMAGELISTS		 := 0x0040
INTERNAL DEFINE LVS_NOLABELWRAP				 := 0x0080
INTERNAL DEFINE LVS_AUTOARRANGE				 := 0x0100
INTERNAL DEFINE LVS_EDITLABELS 				 := 0x0200
INTERNAL DEFINE LVS_OWNERDATA           := 0x1000
INTERNAL DEFINE LVS_NOSCROLL 					 := 0x2000
INTERNAL DEFINE LVS_TYPESTYLEMASK			 := 0xfc00
INTERNAL DEFINE LVS_ALIGNTOP 					 := 0x0000
INTERNAL DEFINE LVS_ALIGNLEFT					 := 0x0800
INTERNAL DEFINE LVS_ALIGNMASK					 := 0x0c00
INTERNAL DEFINE LVS_OWNERDRAWFIXED 		 := 0x0400
INTERNAL DEFINE LVS_NOCOLUMNHEADER 		 := 0x4000
INTERNAL DEFINE LVS_NOSORTHEADER 			 := 0x8000


INTERNAL DEFINE MCS_DAYSTATE        := 0x0001
INTERNAL DEFINE MCS_MULTISELECT     := 0x0002
INTERNAL DEFINE MCS_WEEKNUMBERS     := 0x0004
INTERNAL DEFINE MCS_NOTODAYCIRCLE   := 0x0008
INTERNAL DEFINE MCS_NOTODAY         := 0x0010


INTERNAL DEFINE PBS_SMOOTH              := 0x01
INTERNAL DEFINE PBS_VERTICAL            := 0x04


INTERNAL DEFINE TBS_AUTOTICKS					 := 0x0001
INTERNAL DEFINE TBS_VERT 							 := 0x0002
INTERNAL DEFINE TBS_HORZ 							 := 0x0000
INTERNAL DEFINE TBS_TOP								 := 0x0004
INTERNAL DEFINE TBS_BOTTOM 						 := 0x0000
INTERNAL DEFINE TBS_LEFT 							 := 0x0004
INTERNAL DEFINE TBS_RIGHT							 := 0x0000
INTERNAL DEFINE TBS_BOTH 							 := 0x0008
INTERNAL DEFINE TBS_NOTICKS						 := 0x0010
INTERNAL DEFINE TBS_ENABLESELRANGE 		 := 0x0020
INTERNAL DEFINE TBS_FIXEDLENGTH				 := 0x0040
INTERNAL DEFINE TBS_NOTHUMB						 := 0x0080
INTERNAL DEFINE TBS_TOOLTIPS            := 0x0100
INTERNAL DEFINE TBM_GETPOS 						 := (WM_USER)
INTERNAL DEFINE TBM_GETRANGEMIN				 := (WM_USER+1)
INTERNAL DEFINE TBM_GETRANGEMAX				 := (WM_USER+2)
INTERNAL DEFINE TBM_GETTIC 						 := (WM_USER+3)
INTERNAL DEFINE TBM_SETTIC 						 := (WM_USER+4)
INTERNAL DEFINE TBM_SETPOS 						 := (WM_USER+5)
INTERNAL DEFINE TBM_SETRANGE 					 := (WM_USER+6)
INTERNAL DEFINE TBM_SETRANGEMIN				 := (WM_USER+7)
INTERNAL DEFINE TBM_SETRANGEMAX				 := (WM_USER+8)
INTERNAL DEFINE TBM_CLEARTICS					 := (WM_USER+9)
INTERNAL DEFINE TBM_SETSEL 						 := (WM_USER+10)
INTERNAL DEFINE TBM_SETSELSTART				 := (WM_USER+11)
INTERNAL DEFINE TBM_SETSELEND					 := (WM_USER+12)
INTERNAL DEFINE TBM_GETPTICS 					 := (WM_USER+14)
INTERNAL DEFINE TBM_GETTICPOS					 := (WM_USER+15)
INTERNAL DEFINE TBM_GETNUMTICS 				 := (WM_USER+16)
INTERNAL DEFINE TBM_GETSELSTART				 := (WM_USER+17)
INTERNAL DEFINE TBM_GETSELEND					 := (WM_USER+18)
INTERNAL DEFINE TBM_CLEARSEL 					 := (WM_USER+19)
INTERNAL DEFINE TBM_SETTICFREQ 				 := (WM_USER+20)
INTERNAL DEFINE TBM_SETPAGESIZE				 := (WM_USER+21)
INTERNAL DEFINE TBM_GETPAGESIZE				 := (WM_USER+22)
INTERNAL DEFINE TBM_SETLINESIZE				 := (WM_USER+23)
INTERNAL DEFINE TBM_GETLINESIZE				 := (WM_USER+24)
INTERNAL DEFINE TBM_GETTHUMBRECT 			 := (WM_USER+25)
INTERNAL DEFINE TBM_GETCHANNELRECT 		 := (WM_USER+26)
INTERNAL DEFINE TBM_SETTHUMBLENGTH 		 := (WM_USER+27)
INTERNAL DEFINE TBM_GETTHUMBLENGTH 		 := (WM_USER+28)
INTERNAL DEFINE TB_LINEUP							 := 0
INTERNAL DEFINE TB_LINEDOWN						 := 1
INTERNAL DEFINE TB_PAGEUP							 := 2
INTERNAL DEFINE TB_PAGEDOWN						 := 3
INTERNAL DEFINE TB_THUMBPOSITION 			 := 4
INTERNAL DEFINE TB_THUMBTRACK					 := 5
INTERNAL DEFINE TB_TOP 								 := 6
INTERNAL DEFINE TB_BOTTOM							 := 7
INTERNAL DEFINE TB_ENDTRACK						 := 8

INTERNAL DEFINE TCS_SCROLLOPPOSITE      := 0x0001   // assumes multiline tab
INTERNAL DEFINE TCS_BOTTOM              := 0x0002
INTERNAL DEFINE TCS_RIGHT               := 0x0002
INTERNAL DEFINE TCS_MULTISELECT         := 0x0004  // allow multi-select in button mode
INTERNAL DEFINE TCS_FORCEICONLEFT       := 0x0010
INTERNAL DEFINE TCS_FORCELABELLEFT      := 0x0020
INTERNAL DEFINE TCS_HOTTRACK            := 0x0040
INTERNAL DEFINE TCS_VERTICAL            := 0x0080
INTERNAL DEFINE TCS_TABS                := 0x0000
INTERNAL DEFINE TCS_BUTTONS             := 0x0100
INTERNAL DEFINE TCS_SINGLELINE          := 0x0000
INTERNAL DEFINE TCS_MULTILINE           := 0x0200
INTERNAL DEFINE TCS_RIGHTJUSTIFY        := 0x0000
INTERNAL DEFINE TCS_FIXEDWIDTH          := 0x0400
INTERNAL DEFINE TCS_RAGGEDRIGHT         := 0x0800
INTERNAL DEFINE TCS_FOCUSONBUTTONDOWN   := 0x1000
INTERNAL DEFINE TCS_OWNERDRAWFIXED      := 0x2000
INTERNAL DEFINE TCS_TOOLTIPS            := 0x4000
INTERNAL DEFINE TCS_FOCUSNEVER          := 0x8000


INTERNAL DEFINE TVS_HASBUTTONS 				 := 0x0001
INTERNAL DEFINE TVS_HASLINES 					 := 0x0002
INTERNAL DEFINE TVS_LINESATROOT				 := 0x0004
INTERNAL DEFINE TVS_EDITLABELS 				 := 0x0008
INTERNAL DEFINE TVS_DISABLEDRAGDROP		 := 0x0010
INTERNAL DEFINE TVS_SHOWSELALWAYS			 := 0x0020


INTERNAL DEFINE UDS_WRAP 							 := 0x0001
INTERNAL DEFINE UDS_SETBUDDYINT				 := 0x0002
INTERNAL DEFINE UDS_ALIGNRIGHT 				 := 0x0004
INTERNAL DEFINE UDS_ALIGNLEFT					 := 0x0008
INTERNAL DEFINE UDS_AUTOBUDDY					 := 0x0010
INTERNAL DEFINE UDS_ARROWKEYS					 := 0x0020
INTERNAL DEFINE UDS_HORZ 							 := 0x0040
INTERNAL DEFINE UDS_NOTHOUSANDS				 := 0x0080


INTERNAL DEFINE WM_SETFOCUS                                       := 0x0007
INTERNAL DEFINE WM_SETCURSOR                                      := 0x0020


INTERNAL DEFINE WM_NCHITTEST                    := 0x0084
INTERNAL DEFINE WM_NCMOUSEMOVE                  := 0x00A0
INTERNAL DEFINE WM_NCLBUTTONDOWN                := 0x00A1
INTERNAL DEFINE WM_NCLBUTTONUP                  := 0x00A2
INTERNAL DEFINE WM_NCLBUTTONDBLCLK              := 0x00A3
INTERNAL DEFINE WM_NCRBUTTONDOWN                := 0x00A4
INTERNAL DEFINE WM_NCRBUTTONUP                  := 0x00A5
INTERNAL DEFINE WM_NCRBUTTONDBLCLK              := 0x00A6
INTERNAL DEFINE WM_NCMBUTTONDOWN                := 0x00A7
INTERNAL DEFINE WM_NCMBUTTONUP                  := 0x00A8
INTERNAL DEFINE WM_NCMBUTTONDBLCLK              := 0x00A9
INTERNAL DEFINE WM_NCXBUTTONDOWN                := 0x00AB
INTERNAL DEFINE WM_NCXBUTTONUP                  := 0x00AC
INTERNAL DEFINE WM_NCXBUTTONDBLCLK              := 0x00AD
INTERNAL DEFINE WM_USER                         := 0x0400
INTERNAL DEFINE WM_LBUTTONDOWN                  := 0x0201

