

USING VOSDK := XSharp.VO.SDK

CLASS OpenDialog INHERIT StandardFileDialog
	
	PROTECT oOpen AS System.Windows.Forms.OpenFileDialog
	
	CONSTRUCTOR(oOwnWnd,cInitPath,dwFlag)
		SUPER(oOwnWnd,cInitPath,dwFlag)
		oDlg := oOpen := System.Windows.Forms.OpenFileDialog{}

		IsOpen := TRUE
		oOpen:CheckFileExists:= TRUE				
		oOpen:CheckPathExists := TRUE
		Flags := _OR(OFN_EXPLORER, OFN_ALLOWMULTISELECT, OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST,OFN_ENABLEHOOK)
		SELF:PostInit()
		RETURN 

	METHOD Destroy() AS USUAL CLIPPER
		oOpen := NULL_OBJECT
		RETURN SUPER:Destroy()

	METHOD Show() 
		oOpen:ReadOnlyChecked	:= _AND(Flags, OFN_READONLY) != 0
		oOpen:Multiselect		:= _AND(Flags , OFN_ALLOWMULTISELECT) != 0
		oOpen:ShowReadOnly		:= _AND(Flags , OFN_HIDEREADONLY ) != 0

		RETURN SUPER:Show()

END CLASS
CLASS PaletteDialog INHERIT StandardColorDialog

	CONSTRUCTOR(uOwner,oColor) 


		IF !IsNil(uOwner)
			IF !IsInstanceOfUsual(uOwner,#Window)
				WCError{#Init,#PaletteDialog,__WCSTypeError,uOwner,1}:@@Throw()
			ENDIF
		ENDIF
		SUPER(oColor)

		IF IsInstanceOfUsual(uOwner, #Window)
			SELF:oOwner := uOwner
		ENDIF




		RETURN 

END CLASS

CLASS SaveAsDialog INHERIT StandardFileDialog


	PROTECT oSave AS System.Windows.Forms.SaveFileDialog

	CONSTRUCTOR(oOwnWnd, cInitPath, dwFlag) 

		SUPER(oOwnWnd,cInitPath, dwFlag)
		oDlg := oSave := System.Windows.Forms.SaveFileDialog{}
		SELF:IsOpen := FALSE
		oSave:CheckPathExists := TRUE
		oSave:OverwritePrompt := TRUE
		oSave:CreatePrompt := FALSE
		Flags := _OR(OFN_EXPLORER, OFN_PATHMUSTEXIST, OFN_HIDEREADONLY, OFN_ENABLESIZING, OFN_ENABLEHOOK)
		SELF:PostInit()
		RETURN 

	METHOD Destroy() AS USUAL CLIPPER
		oSave := NULL_OBJECT
		RETURN SUPER:Destroy()



END CLASS

CLASS SelectDialog INHERIT StandardColorDialog

	CONSTRUCTOR(uOwner,oColor) 


		IF !IsNil(uOwner)
			IF !IsInstanceOfUsual(uOwner,#Window)
				WCError{#Init,#SelectDialog,__WCSTypeError,uOwner,1}:@@Throw()
			ENDIF
		ENDIF
		SUPER(oColor)

		IF IsInstanceOfUsual(uOwner, #Window)
			SELF:oOwner := uOwner
		ENDIF

	
		RETURN 

END CLASS


CLASS StandardColorDialog INHERIT StandardDialog
	PROTECT liFlags AS LONGINT
	PROTECT oDefColor AS Color
	PROTECT oOwner AS Window
	
	CLASS VOCOlorDialog INHERIT System.Windows.Forms.ColorDialog
		CONSTRUCTOR() STRICT
			SUPER()
	END CLASS
	
	STATIC PROPERTY CustomColors AS INT[] AUTO

	METHOD Color() 
		RETURN oDefColor

	//METHOD Destroy() AS USUAL STRICT
		//RETURN SELF

	CONSTRUCTOR(oColor) 

		SUPER()
		oDefColor := Color{COLORBLACK}

		IF !IsNil(oColor)
			IF !IsInstanceOfUsual(oColor,#Color)
				WCError{#Init,#StandardColorDialog,__WCSTypeError,oColor,1}:@@Throw()
			ENDIF
			oDefColor := oColor
		ENDIF

		RETURN 

	METHOD Show() 
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

CLASS StandardDialog INHERIT VObject

	CONSTRUCTOR() 
		SUPER()
		RETURN 
END CLASS

CLASS StandardFileDialog INHERIT StandardDialog
	PROTECT cInitPath	AS STRING
	PROTECT Flags		AS DWORD
	PROTECT FlagsEx		AS DWORD
	PROTECT IsOpen		AS LOGIC
	PROTECT oOwner		AS Window
	PROTECT oDlg		AS System.Windows.Forms.FileDialog

	METHOD __AddFilter(sFilter AS STRING, sFilterDesc AS STRING) AS VOID STRICT 
		LOCAL sOldFilter as STRING
		sOldFilter := oDlg:Filter
		IF !STRING.IsNullOrEmpty(sOldFilter) .and. !sOldFilter:EndsWith("|")
			sOldFilter += "|" 
		ENDIF
		oDlg:Filter := sOldFilter+sFilterDesc+"|"+sFilter
		RETURN

	METHOD __ClearFilters() AS VOID STRICT 
		oDlg:Filter := String.Empty		
		RETURN

	ACCESS __Flags AS DWORD STRICT 
		RETURN Flags

	ACCESS Caption  AS STRING
		RETURN oDlg:Title

	ASSIGN Caption(cNewCaption AS STRING) 
		oDlg:Title := cNewCaption
		RETURN 

	ACCESS DefExt AS STRING
		RETURN oDlg:DefaultExt

	ASSIGN DefExt( cNew  AS STRING) 
		//PP-040101
		oDlg:DefaultExt := cNew

	METHOD Destroy() AS USUAL CLIPPER
		IF oDlg != null_OBJECT
			oDlg:Dispose()
			oDlg := NULL_OBJECT
		ENDIF		
		RETURN SELF

	METHOD Dispatch(oEvt, hDlg) 
		RETURN 0L

	ASSIGN DlgStyle(flag as LOGIC) 
		IF flag
			Flags := _OR(Flags, DWORD(_CAST, OFN_EXPLORER))
		ELSE
			Flags := _AND(Flags, DWORD(_CAST, _NOT(OFN_EXPLORER)))
		ENDIF

		RETURN 

	ACCESS FileName AS STRING
		RETURN oDlg:FileName

	ACCESS FilterIndex AS LONG
		RETURN oDlg:FilterIndex

	ASSIGN  FilterIndex (nIndex AS LONG)
		oDlg:FilterIndex := nIndex
	
	METHOD Help() 
		RETURN NIL

	METHOD HelpRequest(oObject as Object, e as System.EventArgs) as VOID
		SELF:Help()
		RETURN
		
	ASSIGN HideReadOnly(flag AS LOGIC) 
		IF flag
			Flags := _OR(Flags, OFN_HIDEREADONLY)
		ELSE
			Flags := _AND(Flags, _NOT(OFN_HIDEREADONLY))
		ENDIF


		RETURN 

	CONSTRUCTOR(uOwner, uInitPath) 


		SUPER()
		IF !IsNil(uOwner)
			IF !IsInstanceOfUsual(uOwner,#Window)
				WCError{#Init,#TextBox,__WCSTypeError,uOwner,1}:@@Throw()
			ELSE
				oOwner:=uOwner
			ENDIF
		ENDIF
		IF IsString(uInitPath)
			cInitPath := uInitPath
		ENDIF
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

	ACCESS InitialDirectory AS STRING
		RETURN oDlg:InitialDirectory

	ASSIGN InitialDirectory(cNewDir AS STRING) 
		IF ! empty(cNewDir)
			IF !cNewDir:EndsWith("\")
				cNewDir += "\"
			ENDIF
			oDlg:InitialDirectory := cNewDir
		ENDIF
		RETURN 

	ACCESS NoPlacesBar AS LOGIC
		RETURN LOGIC(_CAST, _AND(FlagsEx, OFN_EX_NOPLACESBAR))

	ASSIGN NoPlacesBar(flag AS LOGIC) 
		

		IF flag
			FlagsEx := _OR(FlagsEx, OFN_EX_NOPLACESBAR)
		ELSE
			FlagsEx := _AND(FlagsEx, _NOT(OFN_EX_NOPLACESBAR))
		ENDIF

		RETURN 

	ACCESS ReadOnly AS LOGIC
		RETURN LOGIC(_CAST, _AND(Flags, OFN_READONLY))
	
	ASSIGN ReadOnly(flag AS LOGIC) 
		IF flag
			Flags := _OR(Flags, OFN_READONLY)
		ELSE
			Flags := _AND(Flags, _NOT(OFN_READONLY))
		ENDIF
		RETURN 

	METHOD SetFilter(uFilter, uFilterDesc, nIndex) 
		LOCAL i AS INT

		IF IsString(uFilter) .AND. IsString(uFilterDesc)
			SELF:__ClearFilters()
			SELF:__AddFilter(uFilter, uFilterDesc)
		ELSEIF IsArray(uFilter) .AND. IsArray(uFilterDesc)
			SELF:__ClearFilters()
			FOR i:=1 TO Min(ALen(uFilter), ALen(uFilterDesc))
				SELF:__AddFilter(AsString(uFilter[i]), AsString(uFilterDesc[i]))
			NEXT
		ENDIF
		IF IsLong(nIndex)
			oDlg:FilterIndex := nIndex
		ENDIF

		RETURN NIL

	METHOD SetStyle(kStyle AS LONG, lOnOff := TRUE AS LOGIC) AS VOID
		IF (lOnOff)
			Flags := _OR(Flags, DWORD(kStyle))
		ELSE
			Flags := _AND(Flags, _NOT(DWORD(kStyle)))
		ENDIF
		RETURN 

	METHOD SetStyleEx(kStyle AS LONG, lOnOff := TRUE AS LOGIC) AS VOID
		IF (lOnOff)
			FlagsEx := _OR(Flags, DWORD(kStyle))
		ELSE
			FlagsEx := _AND(Flags, _NOT(DWORD(kStyle)))
		ENDIF
		RETURN 

	METHOD Show() 
		LOCAL oRes AS System.Windows.Forms.DialogResult
		oDlg:CheckFileExists := _AND(Flags, OFN_FILEMUSTEXIST)  != 0
		oDlg:CheckPathExists := _AND(Flags, OFN_PATHMUSTEXIST) != 0
		
		IF oOwner != NULL_OBJECT
			oRes := oDlg:ShowDialog(oOwner:__Form)
		ELSE
			oRes := oDlg:ShowDialog()
		ENDIF
		RETURN oRes == System.Windows.Forms.DialogResult.OK

END CLASS

CLASS StandardFolderDialog INHERIT StandardDialog
	PROTECT oOwner as Window
	PROTECT sTitle AS STRING
	PROTECT sStart AS STRING
	PROTECT dwType AS DWORD
	PROTECT sResult AS STRING

	ACCESS __StartFolder AS STRING STRICT 
		RETURN sStart


	ACCESS FolderName AS STRING
		RETURN sResult

	CONSTRUCTOR(uOwner, sCaption, sStartFolder, kType) 
		SUPER()
		Default(@kType, BIF_RETURNONLYFSDIRS)
		Default(@sCaption, "Browser Folder")
		Default(@sStartFolder, "")


		IF IsInstanceOf(uOwner, #Window)
			oOwner := uOwner
		ENDIF

		sTitle := sCaption
		sStart := sStartFolder
		IF IsNumeric(dwType)
			dwType := kType
		ENDIF
		RETURN 

	ACCESS Result AS STRING
		RETURN sResult

	METHOD Show() 
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

	METHOD EnableANSI(bOnOff := TRUE AS LOGIC) AS VOID
		IF bOnOff
			// lANSIFlag := CF_ANSIONLY // obsolete (see Win32 SDK Help file)
			lANSIFlag := CF_SCRIPTSONLY		// tk new
		ELSE
			lANSIFlag := 0
		ENDIF
		RETURN 

	METHOD EnableEffects(bOnOff:= TRUE AS LOGIC) AS VOID
		IF bOnOff
			lEffectFlag := CF_EFFECTS
		ELSE
			lEffectFlag := 0
		ENDIF

		RETURN 

	METHOD EnableFixedPitch(bOnOff:= TRUE AS LOGIC) AS VOID
		IF bOnOff
			lFixPitchFlag := CF_FIXEDPITCHONLY
		ELSE
			lFixPitchFlag := 0
		ENDIF
		RETURN 

	METHOD EnableTrueType(bOnOff:= TRUE AS LOGIC) AS VOID
		IF bOnOff
			lTTYFlag := CF_TTONLY
		ELSE
			lTTYFlag := 0
		ENDIF
		RETURN 

	ASSIGN Flags(lInt as LONG) 
		lFlags := _OR(lFlags, LONGINT(_CAST, lInt))
		RETURN 

	PROPERTY Font		AS VOSDK.Font	GET oFont	SET oFont	:= VALUE
	PROPERTY FontColor  AS VOSDK.Color  GET oColor	SET oColor	:= VALUE

	CONSTRUCTOR(uOwner) 
		IF !IsNil(uOwner)
			IF !IsInstanceOfUsual(uOwner,#Window) .AND. IsInstanceOfUsual( uOwner,#Printer)
				WCError{#Init,#StandardFontDialog,__WCSTypeError,uOwner,1}:@@Throw()
			ENDIF
			IF IsInstanceOf(uOwner, #Window)
				oOwner := uOwner
			ENDIF

		ENDIF

		SUPER()

		oColor := Color{COLORBLACK}

		IF IsInstanceOfUsual(uOwner, #Printer)
			lFlags := CF_PRINTERFONTS
			//oPrinter := uOwner
		ELSE
			lFlags := CF_SCREENFONTS
		ENDIF

		SELF:EnableEffects(TRUE)

		RETURN 

	METHOD Show() 
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
