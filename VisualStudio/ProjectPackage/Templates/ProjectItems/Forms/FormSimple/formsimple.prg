using System
using System.Collections.Generic
using System.ComponentModel
using System.Data
using System.Drawing
$if$ ($targetframeworkversion$ >= 3.5)using System.Linq
$endif$
using System.Text
$if$ ($targetframeworkversion$ >= 4.5)using System.Threading.Tasks
$endif$
using System.Windows.Forms

begin namespace $rootnamespace$

    public partial class $safeitemrootname$ inherit System.Windows.Forms.Form

        public CONSTRUCTOR() STRICT //$safeitemrootname$
            InitializeComponent()
			return

		/// <summary>
        /// Required designer variable.
        /// </summary>
        private components := NULL as System.ComponentModel.IContainer

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected method Dispose(disposing as logic) as void STRICT

            if (disposing .AND. (components != null))
                components:Dispose()
            endif
            Super:Dispose(disposing)
			return

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private method InitializeComponent() as void STRICT
            self:components := System.ComponentModel.Container{}
            self:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
            self:Text := "$safeitemrootname$"
			return

        #endregion
    end class
end namespace
