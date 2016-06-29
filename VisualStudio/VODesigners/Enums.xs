//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
[Flags];
ENUM Direction
	MEMBER None := 0
	MEMBER Left := 1
	MEMBER Right := 2
	MEMBER Up := 4
	MEMBER Down := 8
END ENUM

ENUM WEDAction
	MEMBER None
	MEMBER Create
	MEMBER Resize
	MEMBER Move
	MEMBER Select
END ENUM

ENUM DesignerActionType
	
	MEMBER Select
	MEMBER SelectAdd
	MEMBER SelectAll
	MEMBER SelectAllChildren
	MEMBER SelectDefault
	MEMBER DeSelect
	MEMBER DeSelectAll
	MEMBER RemoveSelected
	
	MEMBER AlignLeft
	MEMBER AlignRight
	MEMBER AlignTop
	MEMBER AlignBottom
	MEMBER AlignCenterHorz
	MEMBER AlignCenterVert
	MEMBER ResizeAlignLeft
	MEMBER ResizeAlignRight
	MEMBER ResizeAlignTop
	MEMBER ResizeAlignBottom
	MEMBER SameSize
	MEMBER SameHorSize
	MEMBER SameVerSize

	MEMBER CenterHorz
	MEMBER CenterVert

	MEMBER SpacingVertInc
	MEMBER SpacingVertDec
	MEMBER SpacingVertRem
	MEMBER SpacingVertEqual

	MEMBER SpacingHorzInc
	MEMBER SpacingHorzDec
	MEMBER SpacingHorzRem
	MEMBER SpacingHorzEqual
	
	MEMBER Cut
	MEMBER Copy
	MEMBER Paste

	MEMBER Undo
	MEMBER Redo

	MEMBER ToggleLock
	MEMBER BringToFront
	MEMBER SendToBack

	MEMBER AddPage
	MEMBER DeletePage
	MEMBER AddColumn
	
	MEMBER Properties
	MEMBER AttachToolStripDesigner
	MEMBER AttachDataGridViewDesigner
	
	MEMBER TestDesigner

	MEMBER Promote
	MEMBER Demote
	MEMBER Add
	MEMBER AddSub
	MEMBER Insert
	MEMBER Edit
	MEMBER MoveUp
	MEMBER MoveDown

	MEMBER ComponentMainMenu
	MEMBER ComponentContextMenu

	MEMBER ComponentToolStrip
	MEMBER ComponentMenuStrip
	MEMBER ComponentContextMenuStrip
	MEMBER ComponentStatusStrip
	MEMBER ComponentBindingNavigator

	MEMBER ComponentImageList
	
END ENUM

ENUM DesignerBasicActionType
	MEMBER Create
	MEMBER Remove
	MEMBER SetParent
	MEMBER SetIndex
	MEMBER SetProperty

	MEMBER Promote
	MEMBER Demote
END ENUM

ENUM PropertyType
	MEMBER None
	MEMBER Numeric
	MEMBER Text
//	MEMBER Boolean
	MEMBER Enumerated
	MEMBER Type
	MEMBER CODE
	MEMBER @@Callback
END ENUM

ENUM VOStyle
	MEMBER None
	MEMBER Style
	MEMBER ExStyle
END ENUM

[flags];
ENUM PropertyStyles
	MEMBER None := 0
	MEMBER ReadOnly := 1
	MEMBER NoNULL := 2
	MEMBER NoAuto := 4
	MEMBER NoVisual := 8
	MEMBER NoCF := 16
	MEMBER Track := 32
	MEMBER NoCode := 64
END ENUM

ENUM MethodType
	MEMBER @@Method
	MEMBER @@Access
	MEMBER @@Assign
END ENUM

INTERNAL ENUM CavoWedInfSection
	MEMBER None
	MEMBER Control
	MEMBER StdProp
	MEMBER AssignMap
	MEMBER Supplemental
	MEMBER options
END ENUM

ENUM EditorStreamType
	MEMBER None
	MEMBER Module
	MEMBER File
END ENUM

ENUM DBServerItemType
	MEMBER DBServer
	MEMBER @@Field
	MEMBER Index
	MEMBER Order
END ENUM

ENUM ViewMode
	MEMBER Auto
	MEMBER Form
	MEMBER Browse
END ENUM

ENUM EntityType AS Int32
	MEMBER _None := -1

	MEMBER _Class //:= ElementType.Class
	MEMBER _Structure //:= ElementType.Structure
	MEMBER _Interface //:= ElementType.Interface
	MEMBER _VOStruct //:= ElementType.VoStruct
	MEMBER _Union //:= ElementType.VoStruct
	MEMBER _Enum //:= ElementType.Enum
	MEMBER _Delegate //:= ElementType.Delegate

	MEMBER _Constructor //:= ElementType.Constructor
	MEMBER _Destructor //:= ElementType.Destructor

	MEMBER _IVar //:= ElementType.IVar
	MEMBER _Method //:= ElementType.Method
	MEMBER _Access //:= ElementType.Access
	MEMBER _Assign //:= ElementType.Assign
	MEMBER _Event //:= ElementType.Event

	MEMBER _Function //:= ElementType.Function
	MEMBER _Global //:= ElementType.Global
END ENUM

ENUM AccessLevel
	MEMBER @@Hidden
	MEMBER @@Internal
	MEMBER @@Protected
	MEMBER @@Public
	MEMBER @@Static
END ENUM


ENUM LexerStep
	MEMBER None
	MEMBER Quote
	MEMBER DoubleQuote
	MEMBER Comment
	MEMBER BlockComment
END ENUM

ENUM ParseStep
	MEMBER None
	MEMBER AfterAs
	MEMBER AfterInherit
	MEMBER AfterEnd
	MEMBER AfterBegin
	MEMBER AfterBeginNamespace
	MEMBER AfterSharp
	MEMBER AfterDefine
END ENUM


ENUM FillUsingType
	MEMBER UseArray
	MEMBER UseMethod
	MEMBER UseServer
END ENUM
