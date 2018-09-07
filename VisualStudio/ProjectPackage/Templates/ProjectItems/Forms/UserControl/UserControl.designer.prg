begin namespace $rootnamespace$

    PARTIAL CLASS $safeitemrootname$ INHERIT System.Windows.Forms.UserControl
       /// <summary>
       /// Required designer variable.
       /// </summary>
       PRIVATE INSTANCE components := NULL AS System.ComponentModel.IContainer
    
       /// <summary>
       /// Clean up any resources being used.
       /// </summary>
       /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
       PROTECTED METHOD Dispose( disposing AS LOGIC ) AS VOID STRICT
          IF disposing && components != NULL
             components:Dispose()
          ENDIF
          SUPER:Dispose( disposing )
          RETURN
    
       /// <summary>
       /// Required method for Designer support - do not modify
       /// the contents of this method with the code editor.
       /// </summary>
       PRIVATE METHOD InitializeComponent() AS VOID STRICT
          SELF:components := System.ComponentModel.Container{}
          SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
          SELF:Text := "$safeitemname$"
          RETURN
    
    END CLASS
end namespace