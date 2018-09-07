
USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.VODesigners
BEGIN NAMESPACE XSharp.VOEditors

	/// <summary>
    /// The Class1 class.
    /// </summary>
	ENUM Actions
		MEMBER AlignLeft		:=DesignerActionType.AlignLeft
		MEMBER AlignRight		:= DesignerActionType.AlignRight
		MEMBER AlignCenterVert	:= DesignerActionType.AlignCenterVert
		MEMBER AlignTop			:= DesignerActionType.AlignTop
		MEMBER AlignBottom		:= DesignerActionType.AlignBottom
		MEMBER AlignCenterHorz	:= DesignerActionType.AlignCenterHorz
		MEMBER SameSize			:= DesignerActionType.SameSize
		MEMBER SameHorSize		:= DesignerActionType.SameHorSize
		MEMBER SameVerSize		:= DesignerActionType.SameVerSize
		MEMBER SpacingHorzEqual	:= DesignerActionType.SpacingHorzEqual
		MEMBER SpacingHorzInc	:= DesignerActionType.SpacingHorzInc	
		MEMBER SpacingHorzDec	:= DesignerActionType.SpacingHorzDec
		MEMBER SpacingHorzRem	:= DesignerActionType.SpacingHorzRem	
		MEMBER SpacingVertEqual	:= DesignerActionType.SpacingVertEqual
		MEMBER SpacingVertInc	:= DesignerActionType.SpacingVertInc
		MEMBER SpacingVertDec	:= DesignerActionType.SpacingVertDec
		MEMBER SpacingVertRem	:= DesignerActionType.SpacingVertRem
		MEMBER CenterVert		:= DesignerActionType.CenterVert
		MEMBER CenterHorz		:= DesignerActionType.CenterHorz
		MEMBER Redo				:= DesignerActionType.Redo
		MEMBER Undo				:= DesignerActionType.Undo
		MEMBER Copy				:= DesignerActionType.Copy
		MEMBER Cut				:= DesignerActionType.Cut
		MEMBER Paste			:= DesignerActionType.Paste
		// Selection
		MEMBER SelectAll		:= DesignerActionType.SelectAll
		MEMBER RemoveSelected	:= DesignerActionType.RemoveSelected
		
	END ENUM
END NAMESPACE // XSharp.VOEditors