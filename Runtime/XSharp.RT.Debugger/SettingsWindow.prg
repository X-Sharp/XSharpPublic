USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text

USING System.Windows.Forms

BEGIN NAMESPACE XSharp.Debugger

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
                        oItem:SubItems:Add(oValue:ToString())
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
