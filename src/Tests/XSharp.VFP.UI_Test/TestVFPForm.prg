using System
using System.Collections.Generic
using System.ComponentModel
using System.Data
using System.Drawing

using System.Text

using System.Windows.Forms

begin namespace XSharp.VFP.UI_Test

    public partial class TestVFPForm inherit XSharp.VFP.UI.Form

        public constructor() strict
            InitializeComponent()
            SELF:mDIForm := True
            return
        end constructor
    end class
end namespace
