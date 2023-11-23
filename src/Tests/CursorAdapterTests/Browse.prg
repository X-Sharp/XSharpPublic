USING System.Windows.Forms
USING System.Drawing

CLASS MyBrowseForm INHERIT Form
    PROTECT oGrid AS DataGridView
    CONSTRUCTOR(oTable as Object)
        SUPER()
        //oForm:Text := "Browse table "+oTable:Name
        oGrid := DataGridView{}
        SELF:Controls:Add(oGrid)
        oGrid:Location := System.Drawing.Point{0,0}
        oGrid:Size     := System.Drawing.Size{SELF:Size:Width, SELF:Size:Height - 100}
        oGrid:Dock     := DockStyle.Fill
        oGrid:DataSource := oTable

        FOREACH oCol AS DataGridViewColumn IN oGrid:Columns
            IF oCol:ValueType == typeof(LONG)
                oCol:DefaultCellStyle:Alignment := DataGridViewContentAlignment.TopRight
            ELSEIF oCol:ValueType == typeof(SHORT)
                oCol:DefaultCellStyle:Alignment := DataGridViewContentAlignment.TopRight
            ELSEIF oCol:ValueType == typeof(REAL8)
                oCol:DefaultCellStyle:Alignment := DataGridViewContentAlignment.TopRight
            ELSEIF oCol:ValueType == typeof(System.Decimal)
                oCol:DefaultCellStyle:Alignment := DataGridViewContentAlignment.TopRight
            ELSE
                oCol:DefaultCellStyle:Alignment := DataGridViewContentAlignment.TopLeft
            ENDIF
            oCol:AutoSizeMode := DataGridViewAutoSizeColumnMode.AllCells
        NEXT
        oGrid:AutoSizeRowsMode := DataGridViewAutoSizeRowsMode.None
        oGrid:AllowUserToOrderColumns := TRUE
        oGrid:AllowUserToResizeColumns := TRUE
        oGrid:AllowUserToResizeRows := TRUE
       // oGrid:RowsDefaultCellStyle:BackColor := Color.LightGray
       // oGrid:AlternatingRowsDefaultCellStyle:BackColor := Color.White

        oGrid:KeyDown   += MyOnKeyDown
        oGrid:DataError += MyDataError
        SELF:Size := System.Drawing.Size{SystemInformation.PrimaryMonitorSize:Width/2, SystemInformation.PrimaryMonitorSize:Height/2}
        RETURN
    METHOD MyOnKeyDown(sender AS OBJECT, e AS KeyEventArgs) AS VOID
        IF e:KeyData == Keys.Escape
            LOCAL grid := (DataGridView) sender AS DataGridView
            LOCAL Form := (Form) grid:Parent AS Form
            Form:Close()
        ENDIF
        RETURN
    METHOD MyDataError(sender AS OBJECT, e AS DataGridViewDataErrorEventArgs) AS VOID
        LOCAL ex AS Exception
        ex := e:Exception
        MessageBox(ex:Message,,"Invalid data")
        RETURN


end class

Function Browse(oTable as DbDataSource)
    local oForm as MyBrowseForm
    oForm  := MyBrowseForm{oTable}
    oForm:Text := "Browse "+oTable:Name
    oForm:ShowDialog()
    RETURN



FUNCTION Browse() AS VOID STRICT
    LOCAL oSource AS DbDataSource
    oSource := DbDataSource()
    oSource:ShowDeleted := FALSE
    oSource:ShowRecno  := FALSE
    Browse(oSource)
    RETURN

