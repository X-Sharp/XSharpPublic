//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// textBox.prg
// This file contains subclasses Windows.Forms controls that are used in the
// XSharp GUI Classes, in particular several TextBox subclasses
//
// Also some On..() methods have been implemented that call the event handlers on the VO Window
// class that owns the control
using SWF := System.Windows.Forms
using System.Windows.Forms
using VOSDK := XSharp.VO.SDK

class VOTextBox inherit SWF.TextBox   implements IVOControlProperties
    property oEdit		as VOSDK.Edit get (VOSDK.Edit) self:Control
#undef DEFAULTVISUALSTYLE
#include "PropControl.xh"
    method Initialize() as void strict
        self:AutoSize			:= false
        self:oProperties:OnWndProc += OnWndProc
        return

    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
        super()
        oProperties := VOControlProperties{self, Owner, dwStyle, dwExStyle}
        self:Initialize()
        self:SetVisualStyle()


    method SetVisualStyle as void strict
        if self:oProperties != null_object
            local dwStyle as long
            dwStyle					    := _and(oProperties:Style , _not(oProperties:NotStyle))
            self:TabStop			    := _and(dwStyle, WS_TABSTOP) == WS_TABSTOP
            self:UseSystemPasswordChar	:= _and(dwStyle, ES_PASSWORD) == ES_PASSWORD
            self:AcceptsReturn		    := _and(dwStyle, ES_WANTRETURN) == ES_WANTRETURN
            self:Multiline			    := _and(dwStyle, ES_MULTILINE) == ES_MULTILINE
        endif

    virtual property Text as string get super:Text set super:Text := value

#region Event Handlers
    virtual protect method OnTextChanged(e as EventArgs) as void
        local oWindow as Window
        local oEvent as ControlEvent
        super:OnTextChanged(e)
        if oProperties != null_object .and. self:oEdit != null_object
            oEvent := ControlEvent{self:oEdit}
            oWindow := (Window) self:oEdit:Owner
            if oWindow != null_object
                oWindow:EditChange(oEvent)
            endif
        endif
        return

    virtual protect method OnLeave(e as EventArgs) as void
        local oWindow as Window
        local oEvent as EditFocusChangeEvent
        super:OnLeave(e)
        if oProperties != null_object .and. self:oEdit != null_object
            oEvent := EditFocusChangeEvent{self:oEdit, false}
            oWindow := (Window) self:oEdit:Owner
            if oWindow != null_object
                oWindow:EditFocusChange(oEvent)
            endif
        endif
        return


    virtual protect method OnEnter(e as EventArgs) as void
        local oWindow as Window
        local oEvent as EditFocusChangeEvent
        //Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
        super:OnEnter(e)
        if oProperties != null_object .and. self:oEdit != null_object
            oEvent := EditFocusChangeEvent{self:oEdit, true}
            oWindow := (Window) self:oEdit:Owner
            if oWindow != null_object
                oWindow:EditFocusChange(oEvent)
            endif
        endif
        return

    protect _lInPaint as logic

    method OnWndProc(msg ref SWF.Message) as void
        if msg:Msg == WM_PASTE .and. self:oEdit != null_object
            if IsInstanceOf(oEdit, #SingleLineEdit)
                local oSle as SingleLineEdit
                oSle := (SingleLineEdit) oEdit
                if oSle:__EditString != null_object
                    oSle:Paste(null)
                endif
            endif
        endif
        return
#endregion

end class

class VOHotKeyTextBox inherit VOTextBox
    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
        super(Owner, dwStyle, dwExStyle)

    override protected property CreateParams as SWF.CreateParams
        get
            local implied result := super:CreateParams
            result:ClassName := HOTKEY_CLASS
            return result
        end get
    end property
end class

class VOMLETextBox inherit VOTextBox
    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
        super(Owner,dwStyle,dwExStyle )
        self:Multiline := true

    override protected property CreateParams as SWF.CreateParams
        get
            local implied result := super:CreateParams
            result:style |= (long)WS_VSCROLL
            return result
        end get
    end property

    protected method OnKeyDown (e as SWF.KeyEventArgs) as void strict
        // Suppress Escape. Was in VO in MultiLineEdit:Dispatch()
        if e:KeyCode != SWF.Keys.Escape
            super:OnKeyDown(e)
        endif

end class



class VOIPAddressTextBox inherit VOTextBox
    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
        super(Owner,dwStyle,dwExStyle )

    override protected property CreateParams as SWF.CreateParams
        get
            local implied result := super:CreateParams
            result:ClassName := "SysIPAddress32"
            return result
        end get
    end property
end class


class VORichTextBox inherit SWF.RichTextBox implements IVOControlInitialize
#include "PropControlStyle.xh"
    method Initialize() as void strict
        self:AutoSize			:= false
        return


    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
        oProperties := VOControlProperties{self, Owner, dwStyle, dwExStyle}
        super()
        self:Initialize()
        self:SetVisualStyle()



end class


class VOSpinnerTextBox inherit SWF.NumericUpDown implements IVOControlProperties
    property oEdit		as VOSDK.SpinnerEdit get (VOSDK.SpinnerEdit) self:Control
#include "PropControlStyle.xh"

    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
        oProperties := VOControlProperties{self, Owner, dwStyle, dwExStyle}
        super()
        self:Minimum := 0
        self:Maximum := System.Int32.MaxValue
        self:SetVisualStyle()


    virtual protect method OnTextChanged(e as EventArgs) as void
        local oWindow as Window
        local oEvent as ControlEvent
        super:OnTextChanged(e)
        if oProperties != null_object .and. self:oEdit != null_object
            oEvent := ControlEvent{self:oEdit}
            oWindow := (Window) self:oEdit:Owner
            if oWindow != null_object
                oWindow:EditChange(oEvent)
            endif
        endif

    virtual protect method OnEnter(e as EventArgs) as void
        local oWindow as Window
        local oEvent as EditFocusChangeEvent
        super:OnEnter(e)
        if oProperties != null_object
            oEvent := EditFocusChangeEvent{self:oEdit, false}
            oWindow := (Window) self:oEdit:Owner
            if oWindow != null_object
                oWindow:EditFocusChange(oEvent)
            endif
        endif
        return


    virtual protect method OnLeave(e as EventArgs) as void
        local oWindow as Window
        local oEvent as EditFocusChangeEvent
        //Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
        super:OnLeave(e)
        if oProperties != null_object
            oEvent := EditFocusChangeEvent{self:oEdit, true}
            oWindow := (Window) self:oEdit:Owner
            if oWindow != null_object
                oWindow:EditFocusChange(oEvent)
            endif
        endif
        return
    property Text as string
        get
            return super:Text
        end get
        set
            if STRING.IsNullOrWhiteSpace(value)
                value := self:Minimum:ToString()
            endif
            super:Text := value
        end set
    end property
end class


