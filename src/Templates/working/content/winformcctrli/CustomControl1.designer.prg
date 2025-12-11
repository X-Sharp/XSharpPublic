begin namespace XSharp.WinFormsCCtrlI

    partial class CustomControl1

        /// <summary>
        /// Required designer variable.
        /// </summary>
        private components := NULL as System.ComponentModel.IContainer

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override method Dispose(disposing as logic) as void  STRICT

            if (disposing .AND. (components != null))
                components:Dispose()
            endif
            Super:Dispose(disposing)
			return
        end method
        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private method InitializeComponent() as void  STRICT
            self:components := System.ComponentModel.Container{}
			return
        end method
        #endregion
    end class

END NAMESPACE
