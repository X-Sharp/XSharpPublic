begin namespace $rootnamespace$

    partial class $safeitemrootname$ inherit System.Windows.Forms.UserControl
       /// <summary>
       /// Required designer variable.
       /// </summary>
       private instance components := null as System.ComponentModel.IContainer
    
       /// <summary>
       /// Clean up any resources being used.
       /// </summary>
       /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
       protected method Dispose( disposing as logic ) as void strict
          if disposing && components != null
             components:Dispose()
          endif
          super:Dispose( disposing )
          return
    
       /// <summary>
       /// Required method for Designer support - do not modify
       /// the contents of this method with the code editor.
       /// </summary>
       private method InitializeComponent() as void strict
          self:components := System.ComponentModel.Container{}
          self:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
          self:Text := "$safeitemname$"
          return
    
    end class
end namespace