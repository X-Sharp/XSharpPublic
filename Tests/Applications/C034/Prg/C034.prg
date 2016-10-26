// 34. vulcan compiles this without errors. xsharp:  error XS0119: 'TestClass.MessageBox()' is a method, which is not valid in the given context
#using System.Windows.Forms
CLASS TestClass
	METHOD MessageBox() AS VOID
		MessageBox.Show("test") // System.Windows.Forms.MessageBox()
	RETURN
END CLASS

