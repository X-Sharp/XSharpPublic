//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING VOSDK := XSharp.VO.SDK

/// <include file="Gui.xml" path="doc/OpenDialog/*" />
CLASS OpenDialog INHERIT StandardFileDialog
    protect oOpen as System.Windows.Forms.OpenFileDialog

    /// <include file="Gui.xml" path="doc/OpenDialog.ctor/*" />
    constructor(oOwnWnd := null as Window, cInitPath := "" as string) strict
        super(oOwnWnd,cInitPath)
        oDlg := oOpen := System.Windows.Forms.OpenFileDialog{}

        IsOpen := TRUE
        oOpen:CheckFileExists:= TRUE
        oOpen:CheckPathExists := TRUE
        Flags := _OR(OFN_EXPLORER, OFN_ALLOWMULTISELECT, OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST,OFN_ENABLEHOOK)
        SELF:PostInit()
        RETURN

    /// <include file="Gui.xml" path="doc/OpenDialog.Destroy/*" />
    METHOD Destroy() AS USUAL CLIPPER
        oOpen := NULL_OBJECT
        RETURN SUPER:Destroy()

    /// <include file="Gui.xml" path="doc/OpenDialog.Show/*" />
    METHOD Show() AS LOGIC STRICT
        oOpen:ReadOnlyChecked	:= _AND(Flags, OFN_READONLY) != 0
        oOpen:Multiselect		:= _AND(Flags , OFN_ALLOWMULTISELECT) != 0
        oOpen:ShowReadOnly		:= _AND(Flags , OFN_HIDEREADONLY ) != 0

        RETURN SUPER:Show()

END CLASS
/// <include file="Gui.xml" path="doc/PaletteDialog/*" />
CLASS PaletteDialog INHERIT StandardColorDialog

    /// <include file="Gui.xml" path="doc/PaletteDialog.ctor/*" />
    constructor(uOwner := null as Window,oColor := null as Color)
        super(oColor)
        self:oOwner := uOwner
        return

END CLASS

/// <include file="Gui.xml" path="doc/SaveAsDialog/*" />
CLASS SaveAsDialog INHERIT StandardFileDialog
    PROTECT oSave AS System.Windows.Forms.SaveFileDialog

    /// <include file="Gui.xml" path="doc/SaveAsDialog.ctor/*" />
    constructor(oOwnWnd:= null as Window, cInitPath := "" as string)
        SUPER(oOwnWnd,cInitPath)
        oDlg := oSave := System.Windows.Forms.SaveFileDialog{}
        SELF:IsOpen := FALSE
        oSave:CheckPathExists := TRUE
        oSave:OverwritePrompt := TRUE
        oSave:CreatePrompt := FALSE
        Flags := _OR(OFN_EXPLORER, OFN_PATHMUSTEXIST, OFN_HIDEREADONLY, OFN_ENABLESIZING, OFN_ENABLEHOOK)
        SELF:PostInit()
        RETURN

    /// <include file="Gui.xml" path="doc/SaveAsDialog.Destroy/*" />
    METHOD Destroy() AS USUAL CLIPPER
        oSave := NULL_OBJECT
        RETURN SUPER:Destroy()

END CLASS

/// <include file="Gui.xml" path="doc/SelectDialog/*" />
CLASS SelectDialog INHERIT StandardColorDialog

    /// <include file="Gui.xml" path="doc/SelectDialog.ctor/*" />
    constructor(uOwner := null as Window,oColor := null as Color)
        super(oColor)
        self:oOwner := uOwner
        return

END CLASS


/// <include file="Gui.xml" path="doc/StandardColorDialog/*" />
CLASS StandardColorDialog INHERIT StandardDialog
    PROTECT liFlags AS LONGINT
    PROTECT oDefColor AS Color
    PROTECT oOwner AS Window

    CLASS VOCOlorDialog INHERIT System.Windows.Forms.ColorDialog
        CONSTRUCTOR() STRICT
            SUPER()
    END CLASS

    STATIC PROPERTY CustomColors AS INT[] AUTO
    /// <include file="Gui.xml" path="doc/StandardColorDialog.Color/*" />

    METHOD Color() AS Color STRICT
        RETURN oDefColor

        //METHOD Destroy() AS USUAL STRICT
        //RETURN SELF

    /// <include file="Gui.xml" path="doc/StandardColorDialog.ctor/*" />
    constructor(oColor := null as Color)
        if oColor == null
            oColor := Color{COLORBLACK}
        endif
        super()
        oDefColor := oColor
        return

    /// <include file="Gui.xml" path="doc/StandardColorDialog.Show/*" />
    METHOD Show() AS LOGIC STRICT
        LOCAL lRet AS LOGIC
        LOCAL oColorDlg as VOColorDialog
        LOCAL oRes AS System.Windows.Forms.DialogResult
        oColorDlg := VOColorDialog{}
        oColorDlg:Color := oDefColor
        oColorDlg:CustomColors := CustomColors
        if _and(liFlags, CC_PREVENTFULLOPEN) != 0
            oColorDlg:AllowFullOpen := FALSE
        ELSE
            oColorDlg:AllowFullOpen := TRUE
        ENDIF

        if _and(liFlags, CC_RGBINIT) != 0
            oColorDlg:AnyColor := FALSE
        ELSE
            oColorDlg:AnyColor := TRUE
        ENDIF

        IF oOwner != NULL_OBJECT
            oRes := oColorDlg:ShowDialog(oOwner:__Form)
        ELSE
            oRes := oColorDlg:ShowDialog()
        ENDIF

        lRet := (oRes ==System.Windows.Forms.DialogResult.OK)
        IF lRet
            CustomColors := oColorDlg:CustomColors
            oDefColor := oColorDlg:Color
        ENDIF

        RETURN lRet

END CLASS

/// <include file="Gui.xml" path="doc/StandardDialog/*" />
CLASS StandardDialog INHERIT VObject

    /// <include file="Gui.xml" path="doc/StandardDialog.ctor/*" />
    CONSTRUCTOR() STRICT
        SUPER()
        RETURN
END CLASS

/// <include file="Gui.xml" path="doc/StandardFileDialog/*" />
CLASS StandardFileDialog INHERIT StandardDialog
    PROTECT cInitPath	AS STRING
    PROTECT Flags		AS DWORD
    PROTECT FlagsEx		AS DWORD
    PROTECT IsOpen		AS LOGIC
    PROTECT oOwner		AS Window
    PROTECT oDlg		AS System.Windows.Forms.FileDialog

    /// <exclude />
    METHOD __AddFilter(sFilter AS STRING, sFilterDesc AS STRING) AS VOID STRICT
        LOCAL sOldFilter as STRING
        sOldFilter := oDlg:Filter
        IF !STRING.IsNullOrEmpty(sOldFilter) .and. !sOldFilter:EndsWith("|")
            sOldFilter += "|"
        ENDIF
        oDlg:Filter := sOldFilter+sFilterDesc+"|"+sFilter
        RETURN

    /// <exclude />
    METHOD __ClearFilters() AS VOID STRICT
        oDlg:Filter := String.Empty
        RETURN

    /// <exclude />
    ACCESS __Flags AS DWORD STRICT
        RETURN Flags

    /// <include file="Gui.xml" path="doc/StandardFileDialog.Caption/*" />
    PROPERTY Caption  AS STRING GET oDlg:Title SET oDlg:Title := value

    /// <include file="Gui.xml" path="doc/StandardFileDialog.DefExt/*" />
    PROPERTY DefExt AS STRING GET oDlg:DefaultExt SET oDlg:DefaultExt := value

    /// <include file="Gui.xml" path="doc/StandardFileDialog.Destroy/*" />
    METHOD Destroy() AS USUAL CLIPPER
        IF oDlg != null_OBJECT
            oDlg:Dispose()
            oDlg := NULL_OBJECT
        ENDIF
        RETURN SELF

    /// <include file="Gui.xml" path="doc/StandardFileDialog.Dispatch/*" />
    METHOD Dispatch(oEvt, hDlg)
        RETURN 0L

    /// <include file="Gui.xml" path="doc/StandardFileDialog.DlgStyle/*" />
    ASSIGN DlgStyle(flag as LOGIC)
        IF flag
            Flags := _OR(Flags, DWORD(_CAST, OFN_EXPLORER))
        ELSE
            Flags := _AND(Flags, DWORD(_CAST, _NOT(OFN_EXPLORER)))
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.FileName/*" />
    PROPERTY FileName AS STRING GET oDlg:FileName

    /// <include file="Gui.xml" path="doc/StandardFileDialog.FilterIndex/*" />
    PROPERTY FilterIndex AS LONG GET oDlg:FilterIndex SET oDlg:FilterIndex := value

    /// <include file="Gui.xml" path="doc/StandardFileDialog.help/*" />
    METHOD Help()
        RETURN NIL

    METHOD HelpRequest(oObject as Object, e as System.EventArgs) as VOID
        SELF:Help()
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.HideReadOnly/*" />
    ASSIGN HideReadOnly(flag AS LOGIC)
        IF flag
            Flags := _OR(Flags, OFN_HIDEREADONLY)
        ELSE
            Flags := _AND(Flags, _NOT(OFN_HIDEREADONLY))
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.ctor/*" />
    constructor(uOwner := null as Window, cInitPath := "" as string) strict
        super()
        SELF:cInitPath := cInitPath
        IsOpen := FALSE

    METHOD PostInit() AS VOID STRICT
        LOCAL iPos AS INT
        LOCAL cTest, cRest, cAllFiles AS STRING
        LOCAL lHasWildCard := FALSE AS LOGIC
        LOCAL sFilters AS STRING
        LOCAL Result as STRING
        // needed for changing dialog's style (by OFN_EXPLORER)
        Flags := OFN_ALLOWMULTISELECT

        //cAllFiles := ResourceString{__WCSAllFiles}:Value
        cAllFiles := "All Files (*.*)|*.*"
        Result := IIF(IsNil(cInitPath), "*.*", cInitPath)

        // Find default extension
        iPos := INT(_CAST, RAt2(".", Result))
        cTest := CharPos(Result, 2)

        IF (iPos # 0 .AND. cTest # NULL_STRING)
            cRest := SubStr(Result, ++iPos) // skip '.'
            IF InStr("*", cRest) .OR. InStr("?", cRest)
                lHasWildCard := FALSE
            ENDIF

            // Set as default extension if there are no wildcards
            IF !lHasWildCard
                SELF:DefExt := cRest
            ENDIF

            // Set up filter string
            IF !(cRest == "*") // its not the standard wildcard
                cTest := "*." + cRest
                sFilters := cTest + "|" + cTest + "|" + cAllFiles
            ELSE
                sFilters := cAllFiles
            ENDIF
        ELSE // Set up standard filter string
            sFilters := cAllFiles
        ENDIF
        oDlg:Filter := sFilters
        oDlg:InitialDirectory := System.IO.Path.GetDirectoryName(cInitPath)
        oDlg:AddExtension := TRUE
        oDlg:HelpRequest += HelpRequest
        oDlg:AutoUpgradeEnabled := TRUE
        oDlg:RestoreDirectory := TRUE
        oDlg:FileName := cInitPath

        RETURN
    /// <include file="Gui.xml" path="doc/StandardFileDialog.InitialDirectory/*" />

    ACCESS InitialDirectory AS STRING
        RETURN oDlg:InitialDirectory
    /// <include file="Gui.xml" path="doc/StandardFileDialog.InitialDirectory/*" />

    ASSIGN InitialDirectory(cNewDir AS STRING)
        IF ! empty(cNewDir)
            IF !cNewDir:EndsWith("\")
                cNewDir += "\"
            ENDIF
            oDlg:InitialDirectory := cNewDir
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.NoPlacesBar/*" />
    ACCESS NoPlacesBar AS LOGIC
        RETURN _AND(FlagsEx, OFN_EX_NOPLACESBAR) == OFN_EX_NOPLACESBAR

    /// <include file="Gui.xml" path="doc/StandardFileDialog.NoPlacesBar/*" />
    ASSIGN NoPlacesBar(flag AS LOGIC)
        IF flag
            FlagsEx := _OR(FlagsEx, OFN_EX_NOPLACESBAR)
        ELSE
            FlagsEx := _AND(FlagsEx, _NOT(OFN_EX_NOPLACESBAR))
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.ReadOnly/*" />
    ACCESS ReadOnly AS LOGIC
        RETURN _AND(Flags, OFN_READONLY) == OFN_READONLY

    /// <include file="Gui.xml" path="doc/StandardFileDialog.ReadOnly/*" />
    ASSIGN ReadOnly(flag AS LOGIC)
        IF flag
            Flags := _OR(Flags, OFN_READONLY)
        ELSE
            Flags := _AND(Flags, _NOT(OFN_READONLY))
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.SetFilter/*" />
    METHOD SetFilter(uFilter, uFilterDesc, nIndex)
        LOCAL i AS INT

        IF IsString(uFilter) .AND. IsString(uFilterDesc)
            SELF:__ClearFilters()
            SELF:__AddFilter(uFilter, uFilterDesc)
        ELSEIF IsArray(uFilter) .AND. IsArray(uFilterDesc)
            SELF:__ClearFilters()
            FOR i:=1 TO Math.Min(ALen(uFilter), ALen(uFilterDesc))
                SELF:__AddFilter(AsString(uFilter[i]), AsString(uFilterDesc[i]))
            NEXT
        ENDIF
        IF IsLong(nIndex)
            oDlg:FilterIndex := nIndex
        ENDIF

        RETURN NIL

    /// <include file="Gui.xml" path="doc/StandardFileDialog.SetStyle/*" />
    METHOD SetStyle(kStyle AS LONG, lOnOff := TRUE AS LOGIC) AS VOID
        IF (lOnOff)
            Flags := _OR(Flags, DWORD(kStyle))
        ELSE
            Flags := _AND(Flags, _NOT(DWORD(kStyle)))
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.SetStyleEx/*" />
    METHOD SetStyleEx(kStyle AS LONG, lOnOff := TRUE AS LOGIC) AS VOID
        IF (lOnOff)
            FlagsEx := _OR(Flags, DWORD(kStyle))
        ELSE
            FlagsEx := _AND(Flags, _NOT(DWORD(kStyle)))
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFileDialog.Show/*" />
    METHOD Show() AS LOGIC STRICT
        LOCAL oRes AS System.Windows.Forms.DialogResult
        oDlg:CheckFileExists := _AND(Flags, OFN_FILEMUSTEXIST)  != 0
        oDlg:CheckPathExists := _AND(Flags, OFN_PATHMUSTEXIST) != 0

        IF oOwner != NULL_OBJECT
            oRes := oDlg:ShowDialog(oOwner:__Form)
        ELSE
            oRes := oDlg:ShowDialog()
        ENDIF
        IF oRes == System.Windows.Forms.DialogResult.Cancel
            oDlg:FileName := ""
        ENDIF
        RETURN oRes == System.Windows.Forms.DialogResult.OK

END CLASS

/// <include file="Gui.xml" path="doc/StandardFolderDialog/*" />
CLASS StandardFolderDialog INHERIT StandardDialog
    PROTECT oOwner as Window
    PROTECT sTitle AS STRING
    PROTECT sStart AS STRING
    PROTECT dwType AS DWORD
    PROTECT sResult AS STRING

    /// <exclude />
    ACCESS __StartFolder AS STRING STRICT
        RETURN sStart

    /// <include file="Gui.xml" path="doc/StandardFolderDialog.FolderName/*" />
    PROPERTY FolderName AS STRING GET sResult

    /// <include file="Gui.xml" path="doc/StandardFolderDialog.ctor/*" />
    CONSTRUCTOR(oOwner, sCaption, sStartFolder, kType)
        SUPER()
        DEFAULT( REF kType, BIF_RETURNONLYFSDIRS)
        DEFAULT( REF sCaption, "Browser Folder")
        DEFAULT( REF sStartFolder, "")


        if oOwner is Window 
            SELF:oOwner := oOwner
        ENDIF

        sTitle := sCaption
        sStart := sStartFolder
        IF IsNumeric(dwType)
            dwType := kType
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFolderDialog.Result/*" />
    PROPERTY Result AS STRING GET sResult

    /// <include file="Gui.xml" path="doc/StandardFolderDialog.Show/*" />
    METHOD Show() AS LOGIC STRICT
        LOCAL oDlg as System.Windows.Forms.FolderBrowserDialog
        LOCAL oRes as System.Windows.Forms.DialogResult
        LOCAL lRet as LOGIC
        oDlg := System.Windows.Forms.FolderBrowserDialog{}
        oDlg:Description := SELF:sTitle
        IF ! String.IsNullOrEmpty(sStart)
            oDlg:SelectedPath := SELF:sStart
        ENDIF
        oDlg:RootFolder  := Environment.SpecialFolder.MyComputer
        oDlg:ShowNewFolderButton := TRUE
        IF oOwner != NULL_OBJECT
            oRes := oDlg:ShowDialog(oOwner:__Form)
        ELSE
            oRes := oDlg:ShowDialog()
        ENDIF

        lRet := oRes == System.Windows.Forms.DialogResult.OK
        IF lRet
            SELF:sResult := oDlg:SelectedPath
        ENDIF
        RETURN lRet

END CLASS

/// <include file="Gui.xml" path="doc/StandardFontDialog/*" />
CLASS StandardFontDialog INHERIT StandardDialog
    PROTECT lFlags AS LONGINT
    PROTECT lFixPitchFlag AS LONGINT
    PROTECT lTTYFlag AS LONGINT
    PROTECT lEffectFlag AS LONGINT
    PROTECT lANSIFlag AS LONGINT
    PROTECT lOldFlags AS LONGINT
    PROTECT oColor AS VOSDK.Color
    PROTECT oFont AS VOSDK.Font
    PROTECT iFamily AS INT
        //PROTECT oPrinter AS printer
    PROTECT oOwner as Window

    /// <include file="Gui.xml" path="doc/StandardFontDialog.EnableANSI/*" />
    METHOD EnableANSI(bOnOff := TRUE AS LOGIC) AS VOID
        IF bOnOff
            // lANSIFlag := CF_ANSIONLY // obsolete (see Win32 SDK Help file)
            lANSIFlag := CF_SCRIPTSONLY		// tk new
        ELSE
            lANSIFlag := 0
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFontDialog.EnableEffects/*" />
    METHOD EnableEffects(bOnOff:= TRUE AS LOGIC) AS VOID
        IF bOnOff
            lEffectFlag := CF_EFFECTS
        ELSE
            lEffectFlag := 0
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/StandardFontDialog.EnableFixedPitch/*" />
    METHOD EnableFixedPitch(bOnOff:= TRUE AS LOGIC) AS VOID
        IF bOnOff
            lFixPitchFlag := CF_FIXEDPITCHONLY
        ELSE
            lFixPitchFlag := 0
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFontDialog.EnableTrueType/*" />
    METHOD EnableTrueType(bOnOff:= TRUE AS LOGIC) AS VOID
        IF bOnOff
            lTTYFlag := CF_TTONLY
        ELSE
            lTTYFlag := 0
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFontDialog.Flags/*" />
    ASSIGN Flags(lInt as LONG)
        lFlags := _OR(lFlags, LONGINT(_CAST, lInt))
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFontDialog.Font/*" />
    PROPERTY Font		AS VOSDK.Font	GET oFont	SET oFont	:= VALUE
    /// <include file="Gui.xml" path="doc/StandardFontDialog.FontColor/*" />
    PROPERTY FontColor  AS VOSDK.Color  GET oColor	SET oColor	:= VALUE

    /// <include file="Gui.xml" path="doc/StandardFontDialog.ctor/*" />
    CONSTRUCTOR(uOwner := NIL AS USUAL)
        IF !IsNil(uOwner)
            IF uOwner IS Window
                oOwner := uOwner
                lFlags := CF_SCREENFONTS
            ELSEIF uOwner IS Printer
                lFlags := CF_PRINTERFONTS
            ELSE
                WCError{#Init,#StandardFontDialog,__WCSTypeError,uOwner,1}:Throw()
            ENDIF
        ENDIF
        SUPER()

        oColor := Color{COLORBLACK}
        SELF:EnableEffects(TRUE)
        RETURN

    /// <include file="Gui.xml" path="doc/StandardFontDialog.Show/*" />
    METHOD Show() AS LOGIC STRICT
        LOCAL oDlg as System.Windows.Forms.FontDialog
        LOCAL oRes as System.Windows.Forms.DialogResult
        LOCAL lRet as LOGIC
        oDlg := System.Windows.Forms.FontDialog{}

        IF (oFont != NULL_OBJECT)
            oDlg:Font := oFont
        ENDIF
        oDlg:FixedPitchOnly := lFixPitchFlag != 0
        oDlg:ShowEffects := lEffectFlag != 0
        oDlg:ScriptsOnly := lANSIFlag		 != 0
        oDlg:AllowVectorFonts := lTTYFlag != 0
        oDlg:AllowScriptChange := TRUE
        oDlg:AllowSimulations := TRUE
        oDlg:FontMustExist := TRUE
        oDlg:ShowColor := TRUE
        oDlg:Color := oColor
        IF oOwner != NULL_OBJECT
            oRes := oDlg:ShowDialog(oOwner:__Form)
        ELSE
            oRes := oDlg:ShowDialog()
        ENDIF
        lRet := oRes == System.Windows.Forms.DialogResult.OK
        IF lRet
            oFont  := oDlg:Font
            oColor := oDlg:Color
        ENDIF
        RETURN lRet

END CLASS






#region defines
DEFINE BFFM_ENABLEOK := (WM_USER + 101)
DEFINE BFFM_INITIALIZED := 1
DEFINE BFFM_SELCHANGED := 2
DEFINE BFFM_SETSELECTION := (WM_USER + 102)
DEFINE BFFM_SETSTATUSTEXT := (WM_USER + 100)
DEFINE BIF_BROWSEFORCOMPUTER := 0x1000
DEFINE BIF_BROWSEFORPRINTER := 0x2000
DEFINE BIF_BROWSEINCLUDEFILES := 0x4000
DEFINE BIF_DONTGOBELOWDOMAIN := 0x0002
DEFINE BIF_RETURNFSANCESTORS := 0x0008
DEFINE BIF_RETURNONLYFSDIRS := 0x0001
DEFINE BIF_STATUSTEXT := 0x0004
#endregion

