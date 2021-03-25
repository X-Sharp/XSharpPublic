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
        FOREACH asm AS Assembly IN  AppDomain.CurrentDomain.GetAssemblies()
            VAR att := TYPEOF( XSharp.Internal.ClassLibraryAttribute )
            IF asm:IsDefined(  att, FALSE )
                LOCAL cFunctionClass AS STRING
                FOREACH VAR attribute IN asm:GetCustomAttributes(att,FALSE)
                    VAR cla := (XSharp.Internal.ClassLibraryAttribute) attribute
                    IF !String.IsNullOrEmpty(cla:GlobalClassName)
                        cFunctionClass := cla:GlobalClassName
                        LOCAL oType := asm:GetType(cFunctionClass,FALSE, TRUE) AS System.Type
                        IF oType != NULL
                            LOCAL aFields := oType:GetFields() AS FieldInfo[]
                            LOCAL oGroup  := NULL AS ListViewGroup
                            FOREACH oFld AS FieldInfo IN aFields
                                IF oFld:IsStatic .AND. ;
                                    ! oFld:Attributes:HasFlag(FieldAttributes.Literal) .AND. ; 
                                    ! oFld:Attributes:HasFlag(FieldAttributes.InitOnly) .AND. ;
                                    oFld:IsPublic
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
                                        IF oGroup == NULL
                                            VAR name := asm:GetName()
                                            oGroup := ListViewGroup{name:Name}
                                            SELF:variablesListView:Groups:Add(oGroup)
                                        ENDIF
                                        SELF:variablesListView:Items:Add(oItem)
                                        oItem:Group := oGroup 
                                    CATCH
                                        NOP
                                    END TRY
                                ENDIF
                            NEXT
                        ENDIF
                    ENDIF
                NEXT
            ENDIF
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
