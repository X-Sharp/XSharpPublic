begin namespace Company.Namespace1

    partial class UserControl1
       /// <summary>
       /// Required designer variable.
       /// </summary>
       private instance components := null as System.ComponentModel.IContainer

       /// <summary>
       /// Clean up any resources being used.
       /// </summary>
       /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
       protected override method Dispose( disposing as logic ) as void strict
          IF disposing .AND. components != null
             components:Dispose()
          endif
          super:Dispose( disposing )
          return
       end method
       /// <summary>
       /// Required method for Designer support - do not modify
       /// the contents of this method with the code editor.
       /// </summary>
       private method InitializeComponent() as void strict
          self:components := System.ComponentModel.Container{}
          self:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
          self:Text := "UserControl"
          return
       end method
    end class
end namespace
