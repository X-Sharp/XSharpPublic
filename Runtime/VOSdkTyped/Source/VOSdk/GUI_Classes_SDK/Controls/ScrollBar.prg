/// <include file="Gui.xml" path="doc/HorizontalScrollBar/*" />
CLASS HorizontalScrollBar INHERIT ScrollBar
   /// <inheritdoc />
    PROPERTY ControlType AS ControlType GET ControlType.HorizontalScrollBar

/// <include file="Gui.xml" path="doc/HorizontalScrollBar.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
		SUPER(oOwner,xID,oPoint,oDimension)
		RETURN

END CLASS

/// <include file="Gui.xml" path="doc/ScrollBar/*" />
ABSTRACT CLASS ScrollBar INHERIT Control

	ACCESS __ScrollBar as System.Windows.Forms.ScrollBar
		RETURN (System.Windows.Forms.ScrollBar) oCtrl

	[Obsolete];
	METHOD __SetColors(_hDc AS IntPtr) AS IntPtr STRICT
		RETURN IntPtr.Zero

 /// <exclude />
	ASSIGN __Value(nValue AS USUAL)  STRICT
		IF IsNil(nValue)
			nValue := 0
		ELSEIF IsString(nValue)
			nValue := Val(nValue)
		ENDIF

		SELF:ThumbPosition := LONGINT(Round(nValue, 0))
		RETURN

/// <include file="Gui.xml" path="doc/ScrollBar.BlockSize/*" />
	PROPERTY BlockSize AS LONGINT GET __ScrollBar:LargeChange SET __ScrollBar:LargeChange := Value

	//METHOD Create()

	//	IF (SUPER:Create() != NULL_PTR)
	//		//SE-051114
	//		SELF:SetInfo(oRange)

	//	ENDIF

	//	RETURN hWnd


/// <include file="Gui.xml" path="doc/ScrollBar.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware)


		Default(@lDataAware, TRUE)
		IF !IsInstanceOfUsual(xID,#ResourceID)
			SUPER(oOwner,xID,oPoint,oDimension,"ScrollBar",,lDataAware)
		ELSE
			SUPER(oOwner,xID,oPoint,oDimension,,,lDataAware)
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ScrollBar.PageSize/*" />
	PROPERTY PageSize AS LONGINT GET __ScrollBar:LargeChange SET __ScrollBar:LargeChange := Value

/// <include file="Gui.xml" path="doc/ScrollBar.Range/*" />
	ACCESS Range as Range
		RETURN Range{__ScrollBar:Minimum, __ScrollBar:Maximum}

/// <include file="Gui.xml" path="doc/ScrollBar.Range/*" />
	ASSIGN Range(oScrollRange as Range)
		__ScrollBar:Minimum := oScrollRange:Min
		__ScrollBar:Maximum := oScrollRange:Max
		RETURN

/// <include file="Gui.xml" path="doc/ScrollBar.SetInfo/*" />
	METHOD SetInfo(oScrollRange, nThumbPosition, nPageSize, lDisableNoScroll)
		RETURN 0

/// <include file="Gui.xml" path="doc/ScrollBar.SetThumbPosition/*" />
	METHOD SetThumbPosition(nPosition, lNotifyOwner)
		__ScrollBar:Value := nPosition
		IF lNotifyOwner
			//Todo Notify Owner
		ENDIF
		RETURN NIL


/// <include file="Gui.xml" path="doc/ScrollBar.TextValue/*" />
	ACCESS TextValue AS STRING
		RETURN AllTrim(AsString(SELF:ThumbPosition))

	ASSIGN TextValue(cNewPos  AS STRING)
		LOCAL wOldValue AS LONGINT
		wOldValue 			:= SELF:ThumbPosition
		SELF:ThumbPosition 	:= Val(cNewPos)
		SELF:Modified 		:= TRUE
		SELF:ValueChanged 	:= !wOldValue == SELF:ThumbPosition

		RETURN
/// <include file="Gui.xml" path="doc/ScrollBar.ThumbPosition/*" />
	PROPERTY ThumbPosition AS INT GET __ScrollBar:Value SET __ScrollBar:Value := Value
/// <include file="Gui.xml" path="doc/ScrollBar.UnitSize/*" />
	PROPERTY UnitSize AS INT  GET __ScrollBar:SmallChange SET __ScrollBar:SmallChange :=Value

/// <include file="Gui.xml" path="doc/ScrollBar.Value/*" />
	ACCESS Value
		RETURN SELF:ThumbPosition

/// <include file="Gui.xml" path="doc/ScrollBar.Value/*" />
	ASSIGN Value(nValue)
		LOCAL iOldValue AS INT

		iOldValue := SELF:ThumbPosition
		SELF:__Value := nValue
		SELF:Modified := TRUE
		SELF:ValueChanged := iOldValue != SELF:ThumbPosition
		RETURN

END CLASS

/// <include file="Gui.xml" path="doc/VerticalScrollBar/*" />
CLASS VerticalScrollBar INHERIT ScrollBar
/// <include file="Gui.xml" path="doc/ScrollBar.UnitSize/*" />

    PROPERTY ControlType AS ControlType GET ControlType.VerticalScrollBar

/// <include file="Gui.xml" path="doc/VerticalScrollBar.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension)

		SUPER(oOwner,xID,oPoint,oDimension)
		RETURN

END CLASS

