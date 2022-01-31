BEGIN NAMESPACE XSharp.VFP
    PUBLIC CLASS AssertDialog ;
        INHERIT System.Windows.Forms.Form
        PRIVATE cmdDebug AS System.Windows.Forms.Button
        PRIVATE cmdCancel AS System.Windows.Forms.Button
        PRIVATE cmdIgnore AS System.Windows.Forms.Button
        PRIVATE cmdIgnoreAll AS System.Windows.Forms.Button
        PRIVATE lblMessage AS System.Windows.Forms.Label
        PRIVATE pictureBox1 AS System.Windows.Forms.PictureBox
        PRIVATE components := NULL AS System.ComponentModel.IContainer
    PROPERTY Result as AssertResult AUTO
    PROPERTY Message as STRING GET lblMessage:Text SET lblMessage:Text := value









        
    #region Windows Form Designer generated code
    
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    public constructor() strict
        SELF:InitializeComponent()
        SELF:cmdDebug:Enabled := System.Diagnostics.Debugger.IsAttached
        SELF:Result := AssertResult.None
        RETURN
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override method Dispose(disposing as logic) as void strict
    
        if (disposing .and. (components != null))
            components:Dispose()
        endif
        Super:Dispose(disposing)
        return
        
    PUBLIC VIRTUAL METHOD InitializeComponent() AS VOID STRICT
        LOCAL resources := System.ComponentModel.ComponentResourceManager{typeof(AssertDialog)} AS System.ComponentModel.ComponentResourceManager
        SELF:cmdDebug := System.Windows.Forms.Button{}
        SELF:cmdCancel := System.Windows.Forms.Button{}
        SELF:cmdIgnore := System.Windows.Forms.Button{}
        SELF:cmdIgnoreAll := System.Windows.Forms.Button{}
        SELF:lblMessage := System.Windows.Forms.Label{}
        SELF:pictureBox1 := System.Windows.Forms.PictureBox{}
        ((System.ComponentModel.ISupportInitialize)(SELF:pictureBox1)):BeginInit()
        SELF:SuspendLayout()
        // 
        // cmdDebug
        // 
        SELF:cmdDebug:Location := System.Drawing.Point{48, 140}
        SELF:cmdDebug:Name := "cmdDebug"
        SELF:cmdDebug:Size := System.Drawing.Size{80, 25}
        SELF:cmdDebug:TabIndex := 0
        SELF:cmdDebug:Text := "&Debug"
        SELF:cmdDebug:UseVisualStyleBackColor := true
        SELF:cmdDebug:Click += System.EventHandler{ SELF, @cmdDebug_Click() }
        // 
        // cmdCancel
        // 
        SELF:cmdCancel:Location := System.Drawing.Point{203, 140}
        SELF:cmdCancel:Name := "cmdCancel"
        SELF:cmdCancel:Size := System.Drawing.Size{80, 25}
        SELF:cmdCancel:TabIndex := 1
        SELF:cmdCancel:Text := "&Quit"
        SELF:cmdCancel:UseVisualStyleBackColor := true
        SELF:cmdCancel:Click += System.EventHandler{ SELF, @cmdCancel_Click() }
        // 
        // cmdIgnore
        // 
        SELF:cmdIgnore:Location := System.Drawing.Point{358, 140}
        SELF:cmdIgnore:Name := "cmdIgnore"
        SELF:cmdIgnore:Size := System.Drawing.Size{80, 25}
        SELF:cmdIgnore:TabIndex := 2
        SELF:cmdIgnore:Text := "&Ignore"
        SELF:cmdIgnore:UseVisualStyleBackColor := true
        SELF:cmdIgnore:Click += System.EventHandler{ SELF, @cmdIgnore_Click() }
        // 
        // cmdIgnoreAll
        // 
        SELF:cmdIgnoreAll:Location := System.Drawing.Point{513, 140}
        SELF:cmdIgnoreAll:Name := "cmdIgnoreAll"
        SELF:cmdIgnoreAll:Size := System.Drawing.Size{80, 25}
        SELF:cmdIgnoreAll:TabIndex := 3
        SELF:cmdIgnoreAll:Text := "Ignore &All"
        SELF:cmdIgnoreAll:UseVisualStyleBackColor := true
        SELF:cmdIgnoreAll:Click += System.EventHandler{ SELF, @cmdIgnoreAll_Click() }
        // 
        // lblMessage
        // 
        SELF:lblMessage:BackColor := System.Drawing.SystemColors.Control
        SELF:lblMessage:Location := System.Drawing.Point{126, 24}
        SELF:lblMessage:Name := "lblMessage"
        SELF:lblMessage:Size := System.Drawing.Size{469, 100}
        SELF:lblMessage:TabIndex := 4
        SELF:lblMessage:Text := "Default Message"
        SELF:lblMessage:TextAlign := System.Drawing.ContentAlignment.MiddleCenter
        // 
        // pictureBox1
        // 
        SELF:pictureBox1:ErrorImage := NULL
        SELF:pictureBox1:Image := ((System.Drawing.Image)(resources:GetObject("pictureBox1.Image")))
        SELF:pictureBox1:InitialImage := NULL
        SELF:pictureBox1:Location := System.Drawing.Point{25, 25}
        SELF:pictureBox1:Name := "pictureBox1"
        SELF:pictureBox1:Size := System.Drawing.Size{95, 109}
        SELF:pictureBox1:SizeMode := System.Windows.Forms.PictureBoxSizeMode.Zoom
        SELF:pictureBox1:TabIndex := 5
        SELF:pictureBox1:TabStop := false
        // 
        // AssertDialog
        // 
        SELF:AutoScaleDimensions := System.Drawing.SizeF{6, 13}
        SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
        SELF:ClientSize := System.Drawing.Size{634, 211}
        SELF:Controls:Add(SELF:pictureBox1)
        SELF:Controls:Add(SELF:lblMessage)
        SELF:Controls:Add(SELF:cmdIgnoreAll)
        SELF:Controls:Add(SELF:cmdIgnore)
        SELF:Controls:Add(SELF:cmdCancel)
        SELF:Controls:Add(SELF:cmdDebug)
        SELF:FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedSingle
        SELF:MaximizeBox := false
        SELF:MinimizeBox := false
        SELF:Name := "AssertDialog"
        SELF:ShowIcon := false
        SELF:StartPosition := System.Windows.Forms.FormStartPosition.CenterParent
        SELF:Text := "Assert Dialog"
        ((System.ComponentModel.ISupportInitialize)(SELF:pictureBox1)):EndInit()
        SELF:ResumeLayout(false)
    
    
        PRIVATE METHOD cmdDebug_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
            SELF:Result := AssertResult.Debug
            SELF:Close()
        RETURN
        PRIVATE METHOD cmdCancel_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
            SELF:Result := AssertResult.Cancel
            SELF:Close()
        RETURN
        PRIVATE METHOD cmdIgnore_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
            SELF:Result := AssertResult.Ignore
            SELF:Close()
        RETURN
        PRIVATE METHOD cmdIgnoreAll_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
            SELF:Result := AssertResult.IgnoreAll
            SELF:Close()
        RETURN


        
    #endregion
END CLASS 
END NAMESPACE
