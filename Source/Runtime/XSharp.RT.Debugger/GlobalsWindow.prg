//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Reflection
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.Debugger

	/// <summary>
    /// The Globals Window shows the globals in all the loaded assemblies that were compiled with X#.
	/// </summary>
	/// <remarks>
	/// The Globals are detected by looking at the existence of the ClassLibraryAttribute for the assembly.
	/// The window lists all the public fields that are not Literals or Readonly.
    /// </remarks>
	/// <seealso cref='M:XSharp.RT.Debugger.Functions.DbgShowGlobals' />
	/// <seealso cref='T:XSharp.Internal.ClassLibraryAttribute' />

	CLASS GlobalsWindow INHERIT VariablesWindow

    CONSTRUCTOR()
        SELF:Text := "XSharp Runtime State - Globals"
         RETURN
    OVERRIDE PROTECTED METHOD LoadValues() AS VOID
        local globals := XSharp.Globals.GetAllGlobals() AS IList<FieldInfo>
        local aGroups as Dictionary<Assembly, ListViewGroup>
        aGroups := Dictionary<Assembly, ListViewGroup>{}
        foreach oFld as FieldInfo in globals
            LOCAL asm := oFld:DeclaringType:Assembly as Assembly
            LOCAL oGroup  := NULL AS ListViewGroup
            if ! aGroups:ContainsKey(asm)
                LOCAL name := asm:GetName() as AssemblyName
                oGroup := ListViewGroup{name:Name}
                SELF:variablesListView:Groups:Add(oGroup)
                aGroups:Add(asm, oGroup)
            ELSE
                oGroup := aGroups[asm]
            endif
            LOCAL sValue AS STRING
            TRY
                VAR oValue := oFld:GetValue(NULL)
                IF oValue != NULL
                    sValue := oValue:ToString()
                ELSE
                    sValue := "<empty>"
                ENDIF
                VAR oItem := ListViewItem{}
                oItem:Text := oFld:Name
                oItem:SubItems:Add(sValue)
                SELF:variablesListView:Items:Add(oItem)
                oItem:Group := oGroup
            CATCH
                NOP
            END TRY
        NEXT
       IF SELF:variablesListView:Items:Count == 0
            VAR oItem := ListViewItem{}
            oItem:Text := "No Globals found"
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
