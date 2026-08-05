
// Class Column  BaseClass   Column  Class  Column
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Column  IMPLEMENTS IVFPOwner,IVFPEditable
#include "VFPContainer.xh"
#include "VFPPropertiesDynamic.xh"
		PROPERTY ActiveControl AS USUAL AUTO

		METHOD AutoFit AS LOGIC CLIPPER
			RETURN FALSE
        PROPERTY Bound AS USUAL AUTO

		// DynamicBackColor / DynamicForeColor implemented with real logic in Column.prg
		// ColumnOrder implemented with real logic in Column.prg
		PROPERTY DynamicAlignment AS LONG AUTO
		PROPERTY DynamicCurrentControl AS STRING AUTO
		PROPERTY DynamicFontBold AS LOGIC AUTO
		PROPERTY DynamicFontItalic AS LOGIC AUTO
		PROPERTY DynamicFontName  AS STRING AUTO
		PROPERTY DynamicFontOutline AS LOGIC AUTO
		PROPERTY DynamicFontShadow  AS LOGIC AUTO
		PROPERTY DynamicFontSize AS FLOAT AUTO
		PROPERTY DynamicFontStrikethru AS LOGIC AUTO
		PROPERTY DynamicFontUnderline AS LOGIC AUTO
		PROPERTY DynamicInputMask  AS STRING AUTO
		PROPERTY HeaderClass AS STRING AUTO
		PROPERTY HeaderClassLibrary AS STRING AUTO
		PROPERTY MouseIcon AS STRING AUTO
		PROPERTY Movable  AS LOGIC AUTO

	END CLASS
	END NAMESPACE
