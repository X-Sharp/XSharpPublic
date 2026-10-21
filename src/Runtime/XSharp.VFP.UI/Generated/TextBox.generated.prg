USING System.ComponentModel

// Class TextBox  BaseClass   Textbox  Class  Textbox
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS TextBox IMPLEMENTS IVFPControl, IVFPText, IVFPEditable
#include "VFPControl.xh"
		PROPERTY AutoComplete AS LONG AUTO
		PROPERTY AutoCompSource AS STRING AUTO
		PROPERTY AutoCompTable AS STRING AUTO
		PROPERTY BorderColor AS System.Drawing.Color AUTO
		PROPERTY Century AS LONG AUTO
		PROPERTY DateFormat AS LONG AUTO
        PROPERTY DateMark AS STRING AUTO

        PROPERTY DisabledBackColor AS System.Drawing.Color AUTO

        PROPERTY DisabledForeColor AS System.Drawing.Color AUTO

		PROPERTY EnableHyperLinks AS LOGIC AUTO
		PROPERTY FontOutline AS LOGIC AUTO
		PROPERTY FontShadow AS LOGIC AUTO
		PROPERTY Hours AS LONG AUTO
		PROPERTY IntegralHeight AS LONG AUTO
        [Obsolete];
            [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY MemoWindow AS USUAL AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY OLEDropTextInsertion AS LONG AUTO
        [Obsolete];
            [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY OpenWindow AS USUAL AUTO
		PROPERTY Seconds AS LONG AUTO
		PROPERTY SelectedBackColor AS System.Drawing.Color AUTO
		PROPERTY SelectedForeColor AS System.Drawing.Color AUTO
		PROPERTY StrictDateEntry AS LONG AUTO
			//PROPERTY Style AS LONG AUTO



		END CLASS
	END NAMESPACE      
