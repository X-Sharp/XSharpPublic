//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms
PUBLIC PARTIAL CLASS frmXporter ;
    INHERIT System.Windows.Forms.Form
    
    PUBLIC CONSTRUCTOR()
        SUPER()
        SELF:InitializeComponent()
        SELF:Icon := XPorter.Properties.Resources.XSharp
RETURN
    PUBLIC VIRTUAL METHOD OKButtonClick(sender AS OBJECT, e AS System.EventArgs) AS VOID
        LOCAL oDlg AS frmProgress
    oDlg := frmProgress{}
    oDlg:Show()
    oDlg:Start()
    SELF:oOKButton:Enabled := FALSE
    IF SELF:orbSolution:Checked
        SolutionConverter.Convert(SELF:otbFileName:Text, oDlg, self:adjustReferences:Checked)
    ELSE
        ProjectConverter.Convert(SELF:otbFileName:Text, oDlg)
    ENDIF
    oDlg:Stop()
    SELF:oOKButton:Enabled := TRUE
RETURN
    PUBLIC VIRTUAL METHOD rbSolutionClick(sender AS OBJECT, e AS EventArgs) AS VOID
        SELF:SetDescription()
RETURN                             
    PUBLIC VIRTUAL METHOD EnableButtons() AS VOID
        IF String.IsNullOrEmpty(otbFileName:Text)
        SELF:oOKButton:ENabled := FALSE
    ELSEIF System.IO.File.Exists(otbFileName:Text)
        SELF:oOKButton:Enabled := TRUE
    ELSE
        SELF:oOKButton:Enabled := FALSE
    ENDIF
    PUBLIC VIRTUAL METHOD SetDescription() AS VOID
        IF SELF:orbSolution:Checked
        SELF:oDescription:Text := e"This will create a new solution where the Vulcan.NET projects will be replaced "+;
                                  e"with X# projects. \"-XS\" will be appended to the new solution name."+;
                                  e"\r\nEach project will be stored in the original folder with the a different (.xsproj) extension. "+ ;
                                  e"\r\nSource files will be shared between the original project "+;
                                  e"and the X# project."
        SELF:adjustReferences:Enabled := true
    ELSE
        SELF:oDescription:Text :=     e"This will create a new X# project where the source files will be the same as the "+;
                                    e"Vulcan.NET source files and where the compilation options will match the "+;
                                    e"compilation options from the original project. "+;
                                    e"\r\nThe project will be stored in the original folder with the a different (.xsproj) extension. "+ ;
                                    e"\r\nSource files will be shared between the original project and the X# project"
        SELF:adjustReferences:Enabled := false
    ENDIF
    PUBLIC VIRTUAL METHOD rbClick(sender AS OBJECT, e AS System.EventArgs) AS VOID
        SELF:SetDescription()    
RETURN
    PUBLIC VIRTUAL METHOD FileButtonClick(sender AS OBJECT, e AS System.EventArgs) AS VOID
        LOCAL oDlg AS OpenFileDialog
    oDlg := OpenFileDialog{}
    oDlg:FileName := SELF:oTbFileName:Text   
    IF SELF:orbSolution:Checked
        oDlg:Filter := "Visual Studio Solution Files|*.sln"
    ELSE
        oDlg:Filter := "Vulcan.NET Project FIles|*.vnproj"
    ENDIF
    VAR result := oDlg:ShowDialog()
    IF result == DialogResult.OK
        SELF:oTbFileName:Text := oDlg:FileName
    ENDIF
RETURN
    PUBLIC VIRTUAL METHOD BasicFormShown(sender AS OBJECT, e AS System.EventArgs) AS VOID
        SELF:SetDescription()
    SELF:EnableButtons()
RETURN
    PUBLIC VIRTUAL METHOD CancelButtonClick(sender AS OBJECT, e AS System.EventArgs) AS VOID
        SELF:Close()
RETURN
    PUBLIC VIRTUAL METHOD tbFileNameTextChanged(sender AS OBJECT, e AS System.EventArgs) AS VOID
        SELF:EnableButtons()
RETURN
END CLASS
