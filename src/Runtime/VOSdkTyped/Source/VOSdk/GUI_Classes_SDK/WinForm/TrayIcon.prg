//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms
CLASS VOTrayIcon INHERIT VObject
	PROTECT Window  AS Window
	PROTECT oIcon   AS NotifyIcon
	PROTECT nID     AS DWORD

	PROPERTY ID		 AS DWORD   GET nID
	PROPERTY Text	 AS STRING GET oIcon:Text SET oIcon:Text := Value
	PROPERTY Image   AS System.Drawing.Icon GET oIcon:Icon SET oIcon:Icon := Value

	CONSTRUCTOR(oWin AS Window , lID AS DWORD)
		SUPER()
		SELF:Window		:= oWin
		SELF:nID		:= lID
		oIcon := NotifyIcon{}
		oIcon:MouseClick		+= OnMouseClick
		oIcon:MouseDoubleClick	+= OnMouseDoubleClick
		oIcon:BalloonTipClicked += OnBalloonTipClicked
		oIcon:BalloonTipShown   += OnBalloonShown
		oIcon:BalloonTipClosed  += OnBalloonClosed

	METHOD Destroy() AS USUAL CLIPPER
		oIcon:Visible := FALSE
		oIcon:Dispose()
		RETURN SELF

#region Windows Forms Event Handlers

	PROTECTED METHOD OnMouseClick(s AS OBJECT, e AS MouseEventArgs) AS VOID
		SELF:Window:TrayIconClicked(SELF:ID, e:Button == MouseButtons.Right, FALSE)
		RETURN


	PROTECTED METHOD OnMouseDoubleClick(s AS OBJECT, e AS MouseEventArgs) AS VOID
		SELF:Window:TrayIconClicked(SELF:ID, e:Button == MouseButtons.Right, TRUE)
		RETURN


	PROTECTED METHOD OnBalloonClosed(s AS OBJECT, e AS EventArgs) AS VOID
		SELF:Window:TrayIconBalloonTimeOut(SELF:ID)
		RETURN

	PROTECTED METHOD OnBalloonShown(s AS OBJECT, e AS EventArgs) AS VOID
		SELF:Window:TrayIconBalloonShown(SELF:ID)
		RETURN

	PROTECTED METHOD OnBalloonTipClicked(s AS OBJECT, e AS EventArgs) AS VOID
		SELF:Window:TrayIconBalloonClicked(SELF:ID)
		RETURN

#endregion

	METHOD Show() AS VOID STRICT
		SELF:oIcon:Visible := TRUE

	METHOD ShowBalloonTip(nTimeOut AS LONG) AS VOID
		SELF:oIcon:Visible := TRUE
		SELF:oIcon:ShowBalloonTip(nTimeOut)


	METHOD ShowBalloonTip(nTimeOut AS LONG, cTitle AS STRING, cText AS STRING, nIcon AS LONG) AS VOID
		SELF:oIcon:Visible := TRUE
		SELF:oIcon:ShowBalloonTip(nTimeOut, cTitle, cText, (ToolTipIcon) nIcon)


END CLASS
