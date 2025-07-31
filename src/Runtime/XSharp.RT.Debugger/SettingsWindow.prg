USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
using System.Linq
USING System.Text

USING System.Windows.Forms

BEGIN NAMESPACE XSharp.Debugger

	/// <summary>
    /// The Settings Window shows the global settings for the current thread.
    /// </summary>
    /// <remarks>
	/// These settings are stored in the Settings field in the the XSharp RuntimeState object.
    /// </remarks>
	/// <seealso cref='M:XSharp.RT.Debugger.Functions.DbgShowSettings' />
	/// <seealso cref='P:XSharp.RuntimeState.Settings' />

    PUBLIC PARTIAL CLASS SettingsWindow ;
        INHERIT VariablesWindow

    CONSTRUCTOR()
         SELF:Text := "XSharp Runtime State - Settings for Current Thread"
         RETURN
        OVERRIDE PROTECTED METHOD LoadValues() AS VOID
            VAR state := XSharp.RuntimeState.GetInstance()
            VAR list  := List<XSharp.Set>{}
            FOREACH nSet AS XSharp.Set IN System.Enum.GetValues(TYPEOF(XSharp.Set))
                IF state:Settings:ContainsKey(nSet)
                    IF list:Contains(nSet)  // There are some duplicates
                        LOOP
                    ENDIF
                    list:Add(nSet)
                    VAR oValue := state:Settings[nSet]
                    VAR oItem := ListViewItem{}
                    oItem:Text := nSet:ToString()
                    IF oValue != NULL
                        if oValue is string[] var aValue
                            var strResult := ""
                            foreach var strValue in aValue
                                if strResult != ""
                                    strResult += ";"
                                endif
                                strResult += strValue
                            next
                            oItem:SubItems:Add(strResult)
                        else
                            oItem:SubItems:Add(oValue:ToString())
                        endif
                    ELSE
                        oItem:SubItems:Add("<no value>")
                    ENDIF

                    SELF:variablesListView:Items:Add(oItem)
                 ENDIF
            NEXT
            SELF:variablesListView:Sorting := SortOrder.Ascending
            SELF:variablesListView:Sort()
            SELF:variablesListView:AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent)
            SELF:variablesListView:AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize)

    END CLASS
END NAMESPACE
