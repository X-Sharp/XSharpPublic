using System
using System.Collections.Generic
using System.ComponentModel
using System.Data
using System.Drawing
using System.Linq

using System.Text
using System.Threading.Tasks

using System.Windows.Forms

begin namespace Company.Namespace1

    public partial class Windows Forms Simple Form inherit System.Windows.Forms.Form

        public constructor() strict
            SELF:InitializeComponent()
            return
        end constructor
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private components := null as System.ComponentModel.IContainer

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
        end method
        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private method InitializeComponent() as void strict
            self:components := System.ComponentModel.Container{}
            self:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
            self:Text := "Windows Forms Simple Form"
            return
        end method

        #endregion
    end class
end namespace
