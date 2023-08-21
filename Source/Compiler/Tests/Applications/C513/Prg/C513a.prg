// 513. error XS0246: The type or namespace name 'AssemblyTitleAttribute' could not be found (are you missing a using directive or an assembly reference?)
// Note that the order of prgs is important, issue does not exist if contents of the 2 files are swaped

// Also note that the file information is missing from the error message
PARTIAL CLASS SomeTest
// without PARTIAL it compiles ok
END CLASS

