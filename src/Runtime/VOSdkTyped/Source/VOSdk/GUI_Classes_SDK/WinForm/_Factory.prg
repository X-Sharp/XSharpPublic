//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING VOSDK := XSharp.VO.SDK

CLASS GuiFactory
    PUBLIC STATIC PROPERTY Instance AS GuiFactory AUTO
    STATIC CONSTRUCTOR()
        @@Instance := GuiFactory{}

    METHOD CreateControl(type AS ControlType, owner AS VOSDK.Control, liStyle AS LONG, liExStyle AS LONG) AS OBJECT
        local oRes as IVOControl
        SWITCH type

        CASE ControlType.Label
            oRes := VOLabel{owner, liStyle, liExStyle}

        CASE ControlType.Mle
            oRes := VOMLETextBox{owner, liStyle, liExStyle}

        CASE ControlType.Sle
            oRes := VOTextBox{owner, liStyle, liExStyle}

        CASE ControlType.Hotkey
            oRes := VOHotKeyTextBox{owner, liStyle, liExStyle}

        CASE ControlType.Button
            oRes := VOButton{owner, liStyle, liExStyle}

        CASE ControlType.CheckBox
    		oRes := VOCheckBox{owner, liStyle, liExStyle}

        CASE ControlType.RadioButton
    		oRes := VORadioButton{owner, liStyle, liExStyle}

        CASE ControlType.DateTimePicker
    		oRes := VODateTimePicker{owner, liStyle, liExStyle}

        CASE ControlType.SpinnerTextBox
            oRes := VOSpinnerTextBox{owner, liStyle, liExStyle}

        CASE ControlType.FixedText
            oRes := VOOwnerDrawnLabel{owner, liStyle, liExStyle}

        CASE ControlType.Panel
            oRes := VOPanel{owner, liStyle, liExStyle}

        CASE ControlType.IPAddress
            oRes := VOIPAddressTextBox{owner, liStyle, liExStyle}

        CASE ControlType.ListBox
            oRes := VOListBox{owner, liStyle, liExStyle}

        CASE ControlType.ComboBox
            oRes := VOComboBox{owner, liStyle, liExStyle}

        CASE ControlType.ListView
            oRes := VOListView{owner, liStyle, liExStyle}

        CASE ControlType.GroupBox
            oRes := VOGroupBox{owner, liStyle, liExStyle}

        CASE ControlType.HorizontalScrollBar
            oRes := VOHScrollBar{owner, liStyle, liExStyle}

        CASE ControlType.VerticalScrollBar
            oRes := VOVScrollBar{owner, liStyle, liExStyle}

        CASE ControlType.VerticalSpinner
            oRes := VOVSpinner{owner, liStyle, liExStyle}

        CASE ControlType.HorizontalSpinner
            oRes := VOHSpinner{owner, liStyle, liExStyle}

        CASE ControlType.Slider
		    oRes := VOSlider{owner, liStyle, liExStyle}

        CASE ControlType.StatusBar
		    oRes := VOStatusBar{owner, liStyle, liExStyle}

        CASE ControlType.SysLink
            oRes := VOLinkLabel{owner, liStyle, liExStyle}

        CASE ControlType.TabControl
            oRes := VOTabControl{owner, liStyle, liExStyle}

        CASE ControlType.TreeView
            oRes := VOTreeView{owner, liStyle, liExStyle}

        CASE ControlType.FixedImage
            oRes := VOImageLabel{owner, liStyle, liExStyle}

        CASE ControlType.ToolBar
            oRes := VOToolBar{owner, liStyle, liExStyle}

        CASE ControlType.ProgressBar
            oRes := VOProgressBar{owner, liStyle, liExStyle}

        CASE ControlType.RichEdit
            oRes := VORichTextBox{owner, liStyle, liExStyle}

        CASE ControlType.DataBrowser
            oRes := VODataGridView{owner, liStyle, liExStyle}

        CASE ControlType.DataListView
            oRes := VODataListView{owner, liStyle, liExStyle}

        CASE ControlType.MonthCalendar
            oRes := VOMonthCalendar{owner, liStyle, liExStyle}

        CASE ControlType.SplitContainer
            oRes := VOSplitContainer{owner, liStyle, liExStyle}

        OTHERWISE
            THROW Exception{"Controltype "+type:ToString() +" not implemented"}
        END SWITCH
        owner:OnControlCreated(oRes)
        RETURN oRes

        METHOD CreateDataForm(oWindow AS Window, oOwner AS System.Windows.Forms.Form, oRes AS ResourceDialog) AS VODataForm
            RETURN VODataForm{oWindow , oOwner, oRes}

        METHOD CreateSurfacePanel(oWindow AS Window) AS VOSurfacePanel
            RETURN VOSurfacePanel{oWindow}

        METHOD CreateSurfacePanel(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG) AS VOSurfacePanel
            RETURN VOSurfacePanel{Owner, dwStyle, dwExStyle}


        method CreateFramePanel(oOwner as VODataForm, oWindow as Window) as VOFramePanel
            RETURN VOFramePanel{oOwner, oWindow}

        METHOD CreateDialogWindow(oWindow AS Window, oRes AS ResourceDialog) AS VoDialogForm
            RETURN VoDialogForm{oWindow, oRes}

        METHOD CreateChildAppWindow(oWindow AS Window, oOwner AS System.Windows.Forms.Form) AS VOChildAppForm
            RETURN VOChildAppForm{oWindow, oOwner}

        METHOD CreateShellWindow(oWindow AS Window) AS VOShellForm
            RETURN VOShellForm{oWindow}

        METHOD CreateTopAppWindow(oWindow AS Window) AS VOTopAppForm
            RETURN VOTopAppForm{oWindow}

        METHOD CreateAppWindow(oWindow AS Window) AS VOAppForm
            RETURN VOAppForm{oWindow}

        METHOD CreateWindow(oWindow AS Window) AS VOForm
            RETURN VOForm{oWindow}

        METHOD CreateListViewElement(type AS ControlType, owner AS OBJECT) AS OBJECT
            SWITCH type
            CASE ControlType.ListViewItem
                return VoListViewItem{(ListViewItem) owner}
            CASE ControlType.ListViewColumn
                return VOColumnHeader{(ListViewColumn) owner}
            END SWITCH
            RETURN NULL

END CLASS


ENUM ControlType
    MEMBER Control  := 0
    MEMBER Mle
    MEMBER SLe
    MEMBER HotKey
    MEMBER Label
    MEMBER Button
    MEMBER TextControl
    MEMBER FixedText
    MEMBER ListBox
    MEMBER ComboBox
    MEMBER CheckBox
    MEMBER RadioButton
    MEMBER GroupBox
    MEMBER TabControl
    MEMBER ListView
    MEMBER ListViewItem
    MEMBER ListViewColumn
    MEMBER ListViewGroup
    MEMBER TreeView
    MEMBER TreeViewItem
    MEMBER DateTimePicker
    MEMBER SpinnerTextBox
    MEMBER Panel
    MEMBER IPAddress
    MEMBER HorizontalScrollBar
    MEMBER VerticalScrollBar
    MEMBER HorizontalSpinner
    MEMBER VerticalSpinner
    MEMBER Slider
    MEMBER StatusBar
    MEMBER SysLink
    MEMBER FixedImage
    MEMBER Toolbar
    MEMBER RichEdit
    MEMBER ProgressBar
    MEMBER DataBrowser
    MEMBER DataListView
    MEMBER MonthCalendar
    member SplitContainer
end enum

