BEGIN NAMESPACE $safeprojectname$
   PARTIAL CLASS Form1 INHERIT System.Windows.Forms.Form
    
      /// <summary>
      /// Required designer variable.
      /// </summary>
      PRIVATE INSTANCE components AS System.ComponentModel.IContainer
        
      /// <summary>
      /// Clean up any resources being used.
      /// </summary>
      /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
      PROTECTED NEW METHOD Dispose( disposing AS LOGIC ) AS VOID
         IF disposing && components != NULL
            components:Dispose()
         ENDIF
         SUPER:Dispose( disposing )
         RETURN
    
      /// <summary>
      /// Required method for Designer support - do not modify
      /// the contents of this method with the code editor.
      /// </summary>
      PRIVATE METHOD InitializeComponent() AS VOID
         SELF:components := System.ComponentModel.Container{}
         SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
         SELF:Text := "Form1"
         RETURN
    
   END CLASS
END NAMESPACE   
