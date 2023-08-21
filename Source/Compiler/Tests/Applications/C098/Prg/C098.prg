// 98. error XS0263: Partial declarations of 'test' must not specify different base classes

// file 1
PARTIAL CLASS test
END CLASS

// file 2
PARTIAL CLASS test INHERIT System.Windows.Forms.Form
END CLASS
