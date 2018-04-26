#using System.Windows.Forms
#using System.Drawing
#using System.Collections.Generic
#using System.Collections

#include "VOWin32APILibrary.vh"

CLASS DesignerClipboard
	PROTECT aEntries AS ArrayList
	CONSTRUCTOR()
		SUPER()
		SELF:aEntries := ArrayList{}
	RETURN
	ACCESS Count() AS INT
	RETURN SELF:aEntries:Count
	METHOD GetEntry(n AS INT) AS DesignerClipboardEntry
	RETURN (DesignerClipboardEntry)SELF:aEntries[n]
	METHOD AddEntry(oEntry AS DesignerClipboardEntry) AS VOID
		SELF:aEntries:Add(oEntry)
	RETURN
	METHOD Clear() AS VOID
		SELF:aEntries:Clear()
	RETURN


END CLASS

CLASS DesignerClipboardEntry
	EXPORT cClass AS STRING
	EXPORT cGuid AS STRING
	EXPORT cName AS STRING
	EXPORT x,y AS INT
	EXPORT aProperties AS NameValueCollection
	EXPORT nPasted AS INT
	EXPORT cParent AS STRING
	EXPORT aSubEntries AS ArrayList
	CONSTRUCTOR()
		SELF:aProperties := NameValueCollection{}
		SELF:aSubEntries := ArrayList{}
	RETURN
END CLASS


STRUCTURE ActionData
	EXPORT cGuid AS STRING
	EXPORT cData AS STRING
	EXPORT oData AS OBJECT
	EXPORT aData AS ICollection
	CONSTRUCTOR(_cGuid AS STRING)
		SELF:cGuid := _cGuid
		SELF:cData := NULL
		SELF:oData := NULL
		SELF:aData := NULL
	RETURN
	CONSTRUCTOR(_cGuid AS STRING , _cData AS STRING)
		SELF:cGuid := _cGuid
		SELF:cData := _cData
		SELF:oData := NULL
		SELF:aData := NULL
	RETURN
	CONSTRUCTOR(_cGuid AS STRING , _cData AS STRING , _oData AS OBJECT)
		SELF:cGuid := _cGuid
		SELF:cData := _cData
		SELF:oData := _oData
		SELF:aData := NULL
	RETURN
	CONSTRUCTOR(_cGuid AS STRING , _cData AS STRING , _oData AS OBJECT , _aData AS ICollection)
		SELF:cGuid := _cGuid
		SELF:cData := _cData
		SELF:oData := _oData
		SELF:aData := _aData
	RETURN
END STRUCTURE



CLASS WindowDesignerOptions
	EXPORT lUseGrid AS LOGIC
	EXPORT lShowGrid AS LOGIC
	EXPORT oGridSize AS Size

	EXPORT oColorSelected AS Color
	EXPORT oColorDefault AS Color
	EXPORT oColorLocked AS Color
	CONSTRUCTOR()
		SELF:Default()
	RETURN
	METHOD Default() AS VOID
		SELF:lUseGrid := TRUE
		SELF:lShowGrid := TRUE
		SELF:oGridSize := Size{8 , 8}

		SELF:oColorSelected := Color.Blue
		SELF:oColorDefault := Color.DarkBlue
		SELF:oColorLocked := Color.Gray
	RETURN

END CLASS

CLASS SelectorBitmap INHERIT PictureBox
	EXPORT nGonia AS INT
	EXPORT oDesign AS DesignWindowItem
	CONSTRUCTOR(_oDesign AS DesignWindowItem , _nGonia AS INT)
		SUPER()
		SELF:oDesign := _oDesign
		SELF:nGonia := _nGonia
		SELF:Size := Size{4,4}
		SELF:TabIndex := 0
		SELF:TabStop := FALSE
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		LOCAL oPoint AS Point
		DO CASE
		CASE m:Msg == WM_LBUTTONDOWN
			oPoint := WindowDesignerBase.IntPtrToPoint(m:LParam)
			oPoint := SELF:PointToScreen(oPoint)
			SELF:oDesign:GoniaClicked(SELF , MouseEventArgs{MouseButtons.Left,0,oPoint:X,oPoint:Y,0})
			RETURN
		END CASE
		SUPER:WndProc(REF m)
	RETURN
END CLASS


CLASS CodeContents
	EXPORT aDefines AS List<STRING>
	EXPORT aDefineValues AS List<STRING>
	EXPORT aResource AS List<STRING>
	EXPORT aClass AS List<STRING>
	EXPORT aConstructor AS List<STRING>
	EXPORT aAccessAssign AS List<STRING>
	EXPORT aAdditional AS List< List<STRING> >

	EXPORT aAccelClass AS List<STRING>
	EXPORT aAccelConstructor AS List<STRING>
	EXPORT aAccelResource AS List<STRING>

	EXPORT aFieldDesc AS List<STRING>
	EXPORT aIndexList AS List<STRING>

	CONSTRUCTOR()
		SUPER()
		aDefines := List<STRING>{}
		aDefineValues := List<STRING>{}
		aResource := List<STRING>{}
		aClass := List<STRING>{}
		aConstructor := List<STRING>{}
		aAccessAssign := List<STRING>{}
		aAdditional := List< List<STRING> >{}

		aAccelClass := List<STRING>{}
		aAccelConstructor := List<STRING>{}
		aAccelResource := List<STRING>{}

		aFieldDesc := List<STRING>{}
		aIndexList := List<STRING>{}
	RETURN
END CLASS



ENUM EditorStreamType
	MEMBER None
	MEMBER Module
	MEMBER File
END ENUM

CLASS EditorStream
	PROTECT eType AS EditorStreamType
	PROTECT oStream AS System.IO.FileStream
	PROTECT oEditor AS CodeEditor
	PROTECT oEncoding AS System.Text.Encoding

	CONSTRUCTOR()
		SELF:eType := EditorStreamType.None
	RETURN

	VIRTUAL METHOD Load(cFileName AS STRING) AS VOID
	RETURN

	ACCESS IsValid AS LOGIC
	RETURN SELF:eType != EditorStreamType.None

	ACCESS Editor AS CodeEditor
	RETURN SELF:oEditor

	METHOD Save() AS LOGIC
/*		LOCAL oWriter AS System.IO.StreamWriter
		LOCAL oEnumerator AS IEnumerator

		DO CASE

		CASE SELF:eType == EditorStreamType.Module
			LOCAL nLine AS INT
			LOCAL sb    AS System.Text.StringBuilder
			sb := System.Text.StringBuilder{}
			oEnumerator := SELF:oEditor:GetEnumerator()
			DO WHILE oEnumerator:MoveNext()
				nLine ++
				sb:AppendLine((STRING)oEnumerator:Current )
			END DO
			SELF:oModule:DeleteLines(1 , oModule:GetLineCount() )
			oModule:InsertLine(1, sb:ToString())

		CASE SELF:eType == EditorStreamType.File
			SELF:oStream:SetLength(0)
			oWriter := System.IO.StreamWriter{SELF:oStream , SELF:oEncoding}
			oEnumerator := SELF:oEditor:GetEnumerator()
			DO WHILE oEnumerator:MoveNext()
				oWriter:WriteLine((STRING)oEnumerator:Current)
			END DO
			oWriter:Flush()
			SELF:oStream:Close()

		END CASE*/
	RETURN TRUE

	METHOD Close() AS VOID
		IF SELF:eType == EditorStreamType.File
			IF SELF:oStream != NULL
				SELF:oStream:Dispose()
				SELF:oStream := NULL
			END IF
		END IF
	RETURN

	DESTRUCTOR()
		SELF:Close()
	RETURN

END CLASS

CLASS ProgressBarForm INHERIT System.Windows.Forms.Form
	PROTECT oProgressBar AS System.Windows.Forms.ProgressBar
	PROTECT lCanceled AS LOGIC
	PROTECT lAllowClose AS LOGIC

	CONSTRUCTOR(cTitle AS STRING , nMaximum AS INT)

		SUPER()
		
		LOCAL oCancelButton AS System.Windows.Forms.Button
		LOCAL oGroupBox1 AS System.Windows.Forms.GroupBox
	
		oCancelButton := System.Windows.Forms.Button{}
		oGroupBox1 := System.Windows.Forms.GroupBox{}
		SELF:oProgressBar := System.Windows.Forms.ProgressBar{}
	
		SELF:SuspendLayout()
	
		SELF:ClientSize := System.Drawing.Size{384 , 96}
		SELF:FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog
		SELF:Location := System.Drawing.Point{100 , 100}
		SELF:MaximizeBox := FALSE
		SELF:MinimizeBox := FALSE
	
		oCancelButton:Click += System.EventHandler{ SELF , @CancelButton_Click() }
		oCancelButton:Location := System.Drawing.Point{294 , 69}
		oCancelButton:Size := System.Drawing.Size{85 , 23}
		oCancelButton:TabIndex := 1
		oCancelButton:Text := "&Cancel"
		SELF:Controls:Add(oCancelButton)
		
		oGroupBox1:SuspendLayout()
		oGroupBox1:Location := System.Drawing.Point{8 , 0}
		oGroupBox1:Name := "GroupBox1"
		oGroupBox1:Size := System.Drawing.Size{368 , 64}
		oGroupBox1:TabIndex := 0
		SELF:Controls:Add(oGroupBox1)
		
	
		SELF:oProgressBar:Location := System.Drawing.Point{8 , 22}
		SELF:oProgressBar:Name := "ProgressBar"
		SELF:oProgressBar:Size := System.Drawing.Size{352 , 23}
		SELF:oProgressBar:TabIndex := 0
		oGroupBox1:Controls:Add(SELF:oProgressBar)
		
		oGroupBox1:ResumeLayout()
		SELF:ResumeLayout()
	
		SELF:Text := cTitle
		SELF:oProgressBar:Minimum := 0
		SELF:oProgressBar:Maximum := nMaximum
		SELF:StartPosition := FormStartPosition.CenterParent
		SELF:TopMost := TRUE
		SELF:ShowInTaskbar := FALSE
	
	RETURN
	PROTECTED METHOD OnClosing(e AS System.ComponentModel.CancelEventArgs) AS VOID
		SUPER:OnClosing(e)
		e:Cancel := .not. SELF:lAllowClose
	RETURN
	METHOD DoClose() AS VOID
		SELF:lAllowClose := TRUE
		SELF:Close()
	RETURN
	METHOD Position(nPos AS INT) AS VOID
		IF nPos >= 0 .and. nPos <= SELF:oProgressBar:Maximum
			SELF:oProgressBar:Value := nPos
		END IF
	RETURN
	ACCESS Canceled AS LOGIC
	RETURN SELF:lCanceled
	METHOD CancelButton_Click(sender AS System.Object , e AS System.EventArgs) AS System.Void
		SELF:lCanceled := TRUE
	RETURN
END CLASS

