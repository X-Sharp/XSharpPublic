//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Linq
USING System.Reflection
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.Debugger

	/// <summary>
    /// The MemVars Window shows the Public and private memory variables for the current thread.
    /// </summary>
    /// <remarks>
	/// It uses the public API in the XSharp Runtime to list these variables.
    /// </remarks>
	/// <seealso cref='M:XSharp.RT.Debugger.Functions.DbgShowMemvars' />

	CLASS MemVarsWindow INHERIT VariablesWindow

    CONSTRUCTOR()
         SELF:Text := "XSharp Runtime State - Dynamic Memory Variables for Current Thread"
         RETURN
    OVERRIDE PROTECTED METHOD LoadValues() AS VOID
        LOCAL oRT := NULL AS Assembly
        FOREACH asm AS Assembly IN  AppDomain.CurrentDomain.GetAssemblies()
            IF asm:GetName():Name == "XSharp.RT"
                oRT := asm
                EXIT
            ENDIF
        NEXT
        IF oRT == NULL
            VAR oItem := ListViewItem{}
            oItem:Text := "No memory variables available - XSharp.RT is not loaded"
            SELF:variablesListView:Items:Add(oItem)
        ELSE
            VAR oGroupPublics   := ListViewGroup{"Publics"}
            VAR oGroupPrivates  := ListViewGroup{"Privates"}
            SELF:variablesListView:Groups:Add(oGroupPublics)
            SELF:variablesListView:Groups:Add(oGroupPrivates)
            LOCAL varName AS STRING
            LOCAL oMIFirst AS MethodInfo
            LOCAL oMINext  AS MethodInfo
            LOCAL oMIGetValue AS MethodInfo
            LOCAL oType     AS System.Type
            oType := oRT:GetType("XSharp.MemVar")
            IF oType != NULL_OBJECT
                oMIFirst    := (MethodInfo) oType:GetMember("DbgPublicsFirst"):First()
                oMINext     := (MethodInfo) oType:GetMember("DbgPublicsNext"):First()
                oMIGetValue := (MethodInfo) oType:GetMember("DbgGetVar"):First()
                IF oMIFirst != NULL .AND. oMINext != NULL .AND. oMIGetValue != NULL
                    varName :=  (STRING) oMIFirst:Invoke(NULL,NULL)
                    DO WHILE ! String.IsNullOrEmpty(varName)
                        VAR oValue := oMIGetValue:Invoke(NULL, <OBJECT>{varName})
                        LOCAL sValue AS STRING
                        IF oValue != NULL
                            sValue := oValue:ToString()
                        ELSE
                            sValue := "NIL"
                        ENDIF
                        VAR oItem := ListViewItem{}
                        oItem:Text := varName
                        oItem:SubItems:Add(sValue)
                        oItem:Group := oGroupPublics
                        varName :=  (STRING) oMINext:Invoke(NULL,NULL)
                        SELF:variablesListView:Items:Add(oItem)
                    ENDDO
                ENDIF
                oMIFirst := (MethodInfo) oType:GetMember("DbgPrivatesFirst"):First()
                oMINext  := (MethodInfo) oType:GetMember("DbgPrivatesNext"):First()
                IF oMIFirst != NULL .AND. oMINext != NULL .AND. oMIGetValue != NULL
                    varName := (STRING) oMIFirst:Invoke(NULL,NULL)
                    DO WHILE ! String.IsNullOrEmpty(varName)
                        VAR oValue := oMIGetValue:Invoke(NULL, <OBJECT>{varName})
                        LOCAL sValue AS STRING
                        IF oValue != NULL
                            sValue := oValue:ToString()
                        ELSE
                            sValue := "NIL"
                        ENDIF
                        VAR oItem := ListViewItem{}
                        oItem:Text := varName
                        oItem:SubItems:Add(sValue)
                        oItem:Group := oGroupPrivates
                        varName := (STRING) oMINext:Invoke(NULL,NULL)
                        SELF:variablesListView:Items:Add(oItem)
                    ENDDO
                 ENDIF
             ENDIF
        ENDIF
        IF SELF:variablesListView:Items:Count == 0
            VAR oItem := ListViewItem{}
            oItem:Text := "No memory variables found"
            SELF:variablesListView:Items:Add(oItem)
            SELF:variablesListView:ShowGroups := FALSE
        ENDIF
        SELF:variablesListView:Sorting := SortOrder.Ascending
        SELF:variablesListView:Sort()
        SELF:variablesListView:AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent)
        SELF:variablesListView:AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize)

        RETURN

	END CLASS
END NAMESPACE // XSharp.Debugger
