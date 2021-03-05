// 767. error XS0121: The call is ambiguous between the following methods or properties: 
// 'System.Windows.Forms.ToolStripMenuItem.ToolStripMenuItem(string, System.Drawing.Image, System.EventHandler)' and 
// 'System.Windows.Forms.ToolStripMenuItem.ToolStripMenuItem(string, System.Drawing.Image, params System.Windows.Forms.ToolStripItem[])'
// Please note that this example works fine when compiled in the Core dialect but does not work in the VO dialect

USING System.Windows.Forms
USING System.Drawing

// Problem happens only in VO/Vulcan etc dialect
FUNCTION Start( ) AS VOID

/*
Those report an error:

error XS0121: The call is ambiguous between the following methods or properties: 
'System.Windows.Forms.ToolStripMenuItem.ToolStripMenuItem(string, System.Drawing.Image, System.EventHandler)' 
and 
'System.Windows.Forms.ToolStripMenuItem.ToolStripMenuItem(string, System.Drawing.Image, params System.Windows.Forms.ToolStripItem[])'
*/

ToolStripMenuItem{"test" , NULL , {o,e => } }
ToolStripMenuItem{"test" , Bitmap{10,10} , {o,e => } }
ToolStripMenuItem{"test" , Bitmap{10,10} , {o,e => test_handler(o,e) } }

// those work ok:
ToolStripMenuItem{"test" , NULL , test_handler }
ToolStripMenuItem{"test" , NULL , eventhandler{ NULL , @test_handler() } }


FUNCTION test_handler(sender AS OBJECT , e AS EventArgs) AS VOID

