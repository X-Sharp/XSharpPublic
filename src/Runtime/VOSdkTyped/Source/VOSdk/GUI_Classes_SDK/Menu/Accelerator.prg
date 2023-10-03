//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System.Collections.Generic

/// <include file="Gui.xml" path="doc/AcceleratorKey/*" />
class AcceleratorKey
    internal Flags	as word
    internal ASCII	as word
    internal ID		as word
    /// <include file="Gui.xml" path="doc/AcceleratorKey.ctor/*" />
    constructor(wFlags as word, wASCII as word, wID as word)
        Flags := wFlags
        ASCII := wASCII
        ID	  := wID
    /// <include file="Gui.xml" path="doc/AcceleratorKey.ShortCut/*" />
    access Shortcut as System.Windows.Forms.Shortcut
        local iKey as long
        if _and(Flags, FALT) == FALT
            iKey += System.Windows.Forms.Keys.Alt
        endif
        if _and(Flags, FCONTROL) == FCONTROL
            iKey += System.Windows.Forms.Keys.Control
        endif
        if _and(Flags, FSHIFT) == FSHIFT
            iKey += System.Windows.Forms.Keys.Shift
        endif
        if _and(Flags, FVIRTKEY) == FVIRTKEY
            iKey += ASCII
        else
            iKey += ASCII
        endif
        return (System.Windows.Forms.Shortcut) iKey



end class

/// <include file="Gui.xml" path="doc/Accelerator/*" />
class Accelerator inherit VObject
    protect hAccel  as IntPtr
    protect aKeys	as List<AcceleratorKey>


    /// <exclude />
    method __AddKeys(hTable as IntPtr) as logic
        local dwI     as long
        local dwCount as long
        local pAccel  as _winAccel
        local pMem    as _winAccel
        if hTable != null_ptr

            dwCount := GuiWin32.CopyAcceleratorTable(hTable, null_ptr, 0l)
            if dwCount >0
                if (pMem := MemAlloc(_sizeof(_winAccel)*dwCount)) != null_ptr
                    GuiWin32.CopyAcceleratorTable(hTable, pMem, dwCount)
                    pAccel := pMem
                    for dwI := 1 upto dwCount
                        aKeys:Add(AcceleratorKey{pAccel:fVirt, pAccel:key, pAccel:cmd})
                        pAccel += 1
                    next //dwI
                    MemFree(pMem)
                endif
                return true
            endif
        endif
        return false

    /// <include file="Gui.xml" path="doc/Accelerator.Keys/*" />
    access Keys as IList<AcceleratorKey>
        if aKeys == null_object
            aKeys :=  List<AcceleratorKey>{}
        endif
        if aKeys:Count == 0
            self:__AddKeys(hAccel)
        endif
        return aKeys
    /// <include file="Gui.xml" path="doc/Accelerator.AddAccelerator/*" />

    method AddAccelerator(oAccelerator as Accelerator) as logic
        if hAccel == null_ptr
            self:__AddKeys(oAccelerator:Handle())
            return true
        endif
        return false

    /// <include file="Gui.xml" path="doc/Accelerator.AddKey/*" />
    method AddKey(nMenuItemId as dword, xKeyId := nil as usual, lCtrl:= false as logic, lAlt:= false as logic, lShift:= false as logic)
        local fVirt as dword
        local wKey  as dword
        local wCmd  as dword


        if hAccel == null_ptr

            fVirt := 0

            if lAlt
                fVirt := _or(fVirt, FALT)
            endif

            if lCtrl
                fVirt := _or(fVirt, FCONTROL)
            endif

            if lShift
                fVirt := _or(fVirt, FSHIFT)
            endif

            if IsNumeric(xKeyId)
                fVirt := _or(fVirt, FVIRTKEY)
                wKey  := xKeyId
            else
                if fVirt != 0
                    fVirt := _or(fVirt, FVIRTKEY)
                    wKey  := word(Asc(Upper(xKeyId)))
                else
                    wKey  := word(Asc(xKeyId))
                endif
            endif

            wCmd := nMenuItemId

            aKeys:Add(AcceleratorKey{(word) fVirt, (word) wKey, (word) wCmd})
            return true
        endif

        return false

    /// <include file="Gui.xml" path="doc/Accelerator.Create/*" />
    method Create() strict
        local dwCount as long
        local pAccel  as _winAccel
        local pMem    as _winAccel


        if hAccel == null_ptr
            if (dwCount := aKeys:Count) > 0
                if (pMem := MemAlloc(_sizeof(_winAccel)*dwCount)) != null_ptr
                    pAccel := pMem
                    foreach implied oKey in aKeys
                        pAccel:fVirt := (byte) oKey:Flags
                        pAccel:key   := oKey:ASCII
                        pAccel:cmd   := oKey:ID
                        pAccel += 1
                    next //dwI
                    hAccel := GuiWin32.CreateAcceleratorTable(pMem, int(_cast,dwCount))
                    MemFree(pMem)
                endif
                aKeys:Clear()
            endif
        endif

        return nil

    /// <include file="Gui.xml" path="doc/Accelerator.Destroy/*" />
    method Destroy() as usual CLIPPER
        if hAccel != null_ptr
            GuiWin32.DestroyAcceleratorTable(hAccel)
            hAccel := null_ptr
        endif

        super:Destroy()

        return nil


    /// <include file="Gui.xml" path="doc/Accelerator.Handle/*" />
    method Handle() as IntPtr strict
        if hAccel == null_ptr
            self:Create()
        endif

        return hAccel

    /// <include file="Gui.xml" path="doc/Accelerator.ctor/*" />
    constructor(xResourceID as usual)
        local hInst as ptr
        local lpTableName as ptr
        local oResourceId as ResourceId

        super()

        if IsNumeric(xResourceID) .or. IsSymbol(xResourceID) .or. IsString(xResourceID)
            xResourceID := ResourceID{xResourceID}
        elseif xResourceID is ResourceID
            // Ok
            nop
        elseif IsNil(xResourceID) .or. xResourceID is Accelerator
            aKeys := List<AcceleratorKey>{}
            hAccel  := null_ptr
            if xResourceID is Accelerator var oAcc
                self:AddAccelerator(oAcc)
            endif
            return
        else
            WCError{#Init, #Accelerator, __WCSTypeError, xResourceID, 1}:Throw()
        endif

        aKeys := null_object
        oResourceId := xResourceID
        hInst := oResourceId:Handle()
        lpTableName := oResourceId:Address()

        hAccel := GuiWin32.LoadAccelerators(hInst, lpTableName)

        return

end class


