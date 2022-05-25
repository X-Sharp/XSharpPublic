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

    partial class $safeitemrootname$ inherit System.Windows.Forms.UserControl

        public constructor() strict
            InitializeComponent()
			return
        end constructor


    end class
end namespace
