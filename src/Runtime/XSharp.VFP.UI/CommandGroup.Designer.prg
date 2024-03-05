BEGIN NAMESPACE XSharp.VFP.UI

    PUBLIC PARTIAL CLASS CommandGroup
        
    PRIVATE gBox AS System.Windows.Forms.GroupBox
 
       /// <summary>
       /// Required designer variable.
       /// </summary>
       PRIVATE INSTANCE components := NULL AS System.ComponentModel.IContainer
    
       /// <summary>
       /// Clean up any resources being used.
       /// </summary>
       /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
       PROTECTED METHOD Dispose( disposing AS LOGIC ) AS VOID STRICT
          IF disposing .AND. components != null
             components:Dispose()
          ENDIF
          SUPER:Dispose( disposing )
          RETURN
    
       /// <summary>
       /// Required method for Designer support - do not modify
       /// the contents of this method with the code editor.
       /// </summary>
       PRIVATE METHOD InitializeComponent() AS VOID STRICT
            SELF:gBox := System.Windows.Forms.GroupBox{}
            SELF:SuspendLayout()
            // 
            // gBox
            // 
            SELF:gBox:Dock := System.Windows.Forms.DockStyle.Fill
            SELF:gBox:Location := System.Drawing.Point{0, 0}
            SELF:gBox:Name := "gBox"
            SELF:gBox:Size := System.Drawing.Size{91, 67}
            SELF:gBox:TabIndex := 0
            SELF:gBox:TabStop := false
            // 
            // VFPOptionGroup
            // 
            SELF:AutoScaleDimensions := System.Drawing.SizeF{8, 16}
            SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
				SUPER:Controls:Add(SELF:gBox)
            SELF:Name := "VFPCommandGroup"
            SELF:Size := System.Drawing.Size{91, 67}
            SELF:ResumeLayout(false)
    
    END CLASS 
END NAMESPACE
