;
; syntax of templates:
;
;	[<section name>]
;	<code>
;
;  where <section name> is the name of the section. This name can be used
;						in CODE:<section name> syntax of description file
;
;		 <code> 		consists of code. Use \t for a tab character. Other
;						substitution keywords are:
;
;					%CONTROL:<property>%   for a control property
;					%FORM:<property>%	   for a form property
;
;						ie: %CONTROL:NAME%		for control's name
;							%CONTROL:CLASSNAME% for control's class name
;
;         %INITPARAMS%            for the parameters of the INIT method
;
;  Note: semicolon (';') in first column designates a comment line


[ClassDeclaration]
class %FORM:NAME% inherit %FORM:CLASSNAME%

[Activate]
method Activate(oEvent) class %FORM:NAME%
\tsuper:Activate(oEvent)
\t//Put your changes here
\treturn NIL

[ButtonClick]
method ButtonClick(oControlEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
\tsuper:ButtonClick(oControlEvent)
\t//Put your changes here
\treturn NIL

[ButtonDoubleClick]
method ButtonDoubleClick(oControlEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
\tsuper:ButtonDoubleClick(oControlEvent)
\t//Put your changes here
\treturn NIL

[Close]
method Close(oEvent) class %FORM:NAME%
\tsuper:Close(oEvent)
\t//Put your changes here
\treturn NIL

[Deactivate]
method Deactivate(oEvent) class %FORM:NAME%
\tsuper:Deactivate(oEvent)
\t//Put your changes here
\treturn NIL

[Draw]
method Draw(oDrawObject) class %FORM:NAME%
\tsuper:Draw(oDrawObject)
\t//Put your changes here
return NIL

[EditChange]
method EditChange(oControlEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
\tsuper:EditChange(oControlEvent)
\t//Put your changes here
\treturn NIL\r\n

[EditFocusChange]
method EditFocusChange(oEditFocusChangeEvent) class %FORM:NAME%
\tlocal oControl as Control
\tlocal lGotFocus as logic
\toControl := IIf(oEditFocusChangeEvent == NULL_OBJECT, NULL_OBJECT, oEditFocusChangeEvent:Control)
\tlGotFocus := IIf(oEditFocusChangeEvent == NULL_OBJECT, FALSE, oEditFocusChangeEvent:GotFocus)
\tsuper:EditFocusChange(oEditFocusChangeEvent)
\t//Put your changes here
\treturn NIL

[EditScroll]
method EditScroll(oControlEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
\tsuper:EditScroll(oControlEvent)
\t//Put your changes here
\treturn NIL

[Expose]
method Expose(oExposeEvent) class %FORM:NAME%
\tlocal oBoundingBox as BoundingBox
\toBoundingBox := IIf(oExposeEvent == NULL_OBJECT, NULL_OBJECT, oExposeEvent:ExposedArea)
\tsuper:Expose(oExposeEvent)
\t//Put your changes here
\treturn NIL\r\n

[FocusChange]
method FocusChange(oFocusChangeEvent) class %FORM:NAME%
\tlocal lGotFocus as logic
\tlGotFocus := IIf(oFocusChangeEvent == NULL_OBJECT, FALSE, oFocusChangeEvent:GotFocus)
\tsuper:FocusChange(oFocusChangeEvent)
\t//Put your changes here
\treturn NIL

[PreInit]
method PreInit(%INITPARAMS%) class %FORM:NAME%
\t//Put your PreInit additions here
\treturn NIL

[PostInit]
method PostInit(%INITPARAMS%) class %FORM:NAME%
\t//Put your PostInit additions here
\treturn NIL

[PreInitCall]
; this section is inserted at the end of the INIT method of the form
; use %INITPARAMS% to insert the parameters of INIT
self:PreInit(%INITPARAMS%)

[PostInitCall]
; this section is inserted at the end of the INIT method the form
; use %INITPARAMS% to insert the parameters of INIT
self:PostInit(%INITPARAMS%)

[HelpRequest]
method HelpRequest(oHelpRequestEvent) class %FORM:NAME%
\tsuper:HelpRequest(oHelpRequestEvent)
\t//Put your changes here
\treturn NIL

[HorizontalScroll]
method HorizontalScroll(oScrollEvent) class %FORM:NAME%
\tsuper:HorizontalScroll(oScrollEvent)
\t//Put your changes here
\treturn NIL

[Keydown]
method KeyDown(oKeyEvent) class %FORM:NAME%
\tlocal nKeyCode as int
\tnKeyCode := IIf(oKeyEvent = NULL_OBJECT, 0, IIf(IsNil(oKeyEvent:ASCIIChar), oKeyEvent:KeyCode, oKeyEvent:ASCIIChar))
\tsuper:KeyDown(oKeyEvent)
\t//Put your changes here
\treturn NIL

[Keyup]
method KeyUp(oKeyEvent) class %FORM:NAME%
\tlocal nKeyCode as int
\tnKeyCode := IIf(oKeyEvent = NULL_OBJECT, 0, IIf(IsNil(oKeyEvent:ASCIIChar), oKeyEvent:KeyCode, oKeyEvent:ASCIIChar))
\tsuper:KeyUp(oKeyEvent)
\t//Put your changes here
\treturn NIL

[ListBoxClick]
method ListBoxClick(oControlEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
\tsuper:ListBoxClick(oControlEvent)
\t//Put your changes here
\treturn NIL

[ListBoxSelect]
method ListBoxSelect(oControlEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
\tsuper:ListBoxSelect(oControlEvent)
\t//Put your changes here
\treturn NIL

[MenuCommand]
method MenuCommand(oMenuCommandEvent) class %FORM:NAME%
\tsuper:MenuCommand(oMenuCommandEvent)
\t//Put your changes here
\treturn NIL

[MenuInit]
method MenuInit(oMenuInitEvent) class %FORM:NAME%
\tsuper:MenuInit(oMenuInitEvent)
\t//Put your changes here
\treturn NIL

[MenuSelect]
method MenuSelect(oMenuSelectEvent) class %FORM:NAME%
\tsuper:MenuSelect(oMenuSelectEvent)
\t//Put your changes here
\treturn NIL

[MouseButtonDoubleClick]
method MouseButtonDoubleClick(oMouseEvent) class %FORM:NAME%
\tlocal nButtonID as int
\tnButtonID := IIf(oMouseEvent == NULL_OBJECT, 0, oMouseEvent:ButtonID)
\tsuper:MouseButtonDoubleClick(oMouseEvent)
\t//Put your changes here
\treturn NIL

[MouseButtonDown]
method MouseButtonDown(oMouseEvent) class %FORM:NAME%
\tlocal nButtonID as int
\tnButtonID := IIf(oMouseEvent == NULL_OBJECT, 0, oMouseEvent:ButtonID)
\tsuper:MouseButtonDown(oMouseEvent)
\t//Put your changes here
\treturn NIL

[MouseButtonUp]
method MouseButtonUp(oMouseEvent) class %FORM:NAME%
\tlocal nButtonID as int
\tnButtonID := IIf(oMouseEvent == NULL_OBJECT, 0, oMouseEvent:ButtonID)
\tsuper:MouseButtonUp(oMouseEvent)
\t//Put your changes here
\treturn NIL

[MouseDrag]
method MouseDrag(oMouseEvent) class %FORM:NAME%
\tlocal nButtonID as int
\tnButtonID := IIf(oMouseEvent == NULL_OBJECT, 0, oMouseEvent:ButtonID)
\tsuper:MouseDrag(oMouseEvent)
\t//Put your changes here
\treturn NIL

[MouseMove]
method MouseMove(oMouseEvent) class %FORM:NAME%
\tlocal nButtonID as int
\tnButtonID := IIf(oMouseEvent == NULL_OBJECT, 0, oMouseEvent:ButtonID)
\tsuper:MouseMove(oMouseEvent)
\t//Put your changes here
\treturn NIL

[Move]
method Move(oMoveEvent) class %FORM:NAME%
\tsuper:Move(oMoveEvent)
\t//Put your changes here
\treturn NIL

[Notify]
method Notify(kNotifyName, uDescription) class %FORM:NAME%
\tlocal uValue as usual
\tuValue := super:Notify(kNotifyName, uDescription)
\t//Put your changes here
\treturn uValue

[PreValidate]
method PreValidate() class %FORM:NAME%
\tsuper:PreValidate()
\t//Put your changes here
\treturn NIL

[QueryClose]
method QueryClose(oEvent) class %FORM:NAME%
\tlocal lAllowClose as logic
\tlAllowClose := super:QueryClose(oEvent)
\t//Put your changes here
\treturn lAllowClose

[Resize]
method Resize(oResizeEvent) class %FORM:NAME%
\tsuper:Resize(oResizeEvent)
\t//Put your changes here
\treturn NIL

[VerticalScroll]
method VerticalScroll(oScrollEvent) class %FORM:NAME%
\tsuper:VerticalScroll(oScrollEvent)
\t//Put your changes here
\treturn NIL

[HorizontalSlide]
method HorizontalSlide(oSlideEvent) class %FORM:NAME%
\tsuper:HorizontalSlide(oSlideEvent)
\t//Put your changes here
\treturn NIL

[VerticalSlide]
method VerticalSlide(oSlideEvent) class %FORM:NAME%
\tsuper:VerticalSlide(oSlideEvent)
\t//Put your changes here
\treturn NIL

[HorizontalSpin]
method HorizontalSpin(oSpinEvent) class %FORM:NAME%
\tsuper:HorizontalSpin(oSpinEvent)
\t//Put your changes here
\treturn NIL

[VerticalSpin]
method VerticalSpin(oSpinEvent) class %FORM:NAME%
\tsuper:VerticalSpin(oSpinEvent)
\t//Put your changes here
\treturn NIL

[AnimationStart]
method AnimationStart(oControlNotifyEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlNotifyEvent == NULL_OBJECT, NULL_OBJECT, oControlNotifyEvent:Control)
\tsuper:AnimationStart(oControlNotifyEvent)
\t//Put your changes here
\treturn NIL

[AnimationStop]
method AnimationStop(oControlNotifyEvent) class %FORM:NAME%
\tlocal oControl as Control
\toControl := IIf(oControlNotifyEvent == NULL_OBJECT, NULL_OBJECT, oControlNotifyEvent:Control)
\tsuper:AnimationStop(oControlNotifyEvent)
\t//Put your changes here
\treturn NIL

[RichEditDropFiles]
method RichEditDropFiles(oRichEditDropEvent) class %FORM:NAME%
\tsuper:RichEditDropFiles(oRichEditDropEvent)
\t//Put your changes here
\treturn NIL

[RichEditProtected]
method RichEditProtected(oRichEditProtectEvent) class %FORM:NAME%
\tsuper:RichEditProtected(oRichEditProtectEvent)
\t//Put your changes here
\treturn NIL

[RichEditSelectionChange]
method RichEditSelectionChange(oRichEditSelectionEvent) class %FORM:NAME%
\tsuper:RichEditSelectionChange(oRichEditSelectionEvent)
\t//Put your changes here
\treturn NIL

[RichEditUndoLost]
method RichEditUndoLost(oControlNotifyEvent) class %FORM:NAME%
\tsuper:RichEditUndoLost(oControlNotifyEvent)
\t//Put your changes here
\treturn NIL

[ListViewItemDrag]
method ListViewItemDrag(oListViewDragEvent) class %FORM:NAME%
\tsuper:ListViewItemDrag(oListViewDragEvent)
\t//Put your changes here
\treturn NIL

[ListViewItemEdit]
method ListViewItemEdit(oListViewEditEvent) class %FORM:NAME%
\tsuper:ListViewItemEdit(oListViewEditEvent)
\t//Put your changes here
\treturn NIL

[ListViewColumnClick]
method ListViewColumnClick(oListViewColumnClickEvent) class %FORM:NAME%
\tsuper:ListViewColumnClick(oListViewColumnClickEvent)
\t//Put your changes here
\treturn NIL

[ListViewItemDelete]
method ListViewItemDelete(oListViewDeleteEvent) class %FORM:NAME%
\tsuper:ListViewItemDelete(oListViewDeleteEvent)
\t//Put your changes here
\treturn NIL

[ListViewKeyDown]
method ListViewKeyDown(oListViewKeyEvent) class %FORM:NAME%
\tsuper:ListViewKeyDown(oListViewKeyEvent)
\t//Put your changes here
\treturn NIL

[TreeViewMouseButtonDown]
method TreeViewMouseButtonDown(oTreeViewMouseEvent) class %FORM:NAME%
\tsuper:TreeViewMouseButtonDown(oTreeViewMouseEvent)
\t//Put your changes here
\treturn NIL

[ListViewMouseButtonDown]
method ListViewMouseButtonDown(oListViewMouseEvent) class %FORM:NAME%
\tsuper:ListViewMouseButtonDown(oListViewMouseEvent)
\t//Put your changes here
\treturn NIL

[TreeViewMouseButtonDoubleClick]
method TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent) class %FORM:NAME%
\tsuper:TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent)
\t//Put your changes here
\treturn NIL

[ListViewMouseButtonDoubleClick]
method ListViewMouseButtonDoubleClick(oListViewMouseEvent) class %FORM:NAME%
\tsuper:ListViewMouseButtonDoubleClick(oListViewMouseEvent)
\t//Put your changes here
\treturn NIL

[TabSelect]
method TabSelect(oControlNotifyEvent) class %FORM:NAME%
\tsuper:TabSelect(oControlNotifyEvent)
\t//Put your changes here
\treturn NIL

[TabSelectionChanging]
method TabSelectionChanging(oControlNotifyEvent) class %FORM:NAME%
\tsuper:TabSelectionChanging(oControlNotifyEvent)
\t//Put your changes here
\treturn NIL

[TabKeyDown]
method TabKeyDown(oControlNotifyEvent) class %FORM:NAME%
\tsuper:TabKeyDown(oControlNotifyEvent)
\t//Put your changes here
\treturn NIL

[TreeViewItemDrag]
method TreeViewItemDrag(oTreeViewDragEvent) class %FORM:NAME%
\tsuper:TreeViewItemDrag(oTreeViewDragEvent)
\t//Put your changes here
\treturn NIL

[TreeViewItemEdit]
method TreeViewItemEdit(oTreeViewEditEvent) class %FORM:NAME%
\tsuper:TreeViewItemEdit(oTreeViewEditEvent)
\t//Put your changes here
\treturn NIL

[TreeViewItemDelete]
method TreeViewItemDelete(oTreeViewDeleteEvent) class %FORM:NAME%
\tsuper:TreeViewItemDelete(oTreeViewDeleteEvent)
\t//Put your changes here
\treturn NIL

[TreeViewItemExpanded]
method TreeViewItemExpanded(oTreeViewExpandedEvent) class %FORM:NAME%
\tsuper:TreeViewItemExpanded(oTreeViewExpandedEvent)
\t//Put your changes here
\treturn NIL

[TreeViewItemExpanding]
method TreeViewItemExpanding(oTreeViewExpandingEvent) class %FORM:NAME%
\tsuper:TreeViewItemExpanding(oTreeViewExpandingEvent)
\t//Put your changes here
\treturn NIL

[TreeViewKeyDown]
method TreeViewKeyDown(oTreeViewKeyEvent) class %FORM:NAME%
\tsuper:TreeViewKeyDown(oTreeViewKeyEvent)
\t//Put your changes here
\treturn NIL

[TreeViewSelectionChanged]
method TreeViewSelectionChanged(oTreeViewSelectionEvent) class %FORM:NAME%
\tsuper:TreeViewSelectionChanged(oTreeViewSelectionEvent)
\t//Put your changes here
\treturn NIL

[TreeViewSelectionChanging]
method TreeViewSelectionChanging(oTreeViewSelectionEvent) class %FORM:NAME%
\tsuper:TreeViewSelectionChanging(oTreeViewSelectionEvent)
\t//Put your changes here
\treturn NIL

[MonthCalSelectionChanged]
method MonthCalSelectionChanged(oMonthCalSelectionEvent) class %FORM:NAME%
\tsuper:MonthCalSelectionChanged(oMonthCalSelectionEvent)
\t//Put your changes here
\treturn NIL

[DateTimeSelectionChanged]
method DateTimeSelectionChanged(oDateTimeSelectionEvent) class %FORM:NAME%
\tsuper:DateTimeSelectionChanged(oDateTimeSelectionEvent)
\t//Put your changes here
\treturn NIL

