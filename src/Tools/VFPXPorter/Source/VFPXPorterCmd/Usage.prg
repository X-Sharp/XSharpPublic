// Usage.prg

USING System

FUNCTION Usage AS VOID
	Console.WriteLine("VFPXPorterCmd — VFP to X#/WinForms code generator")
	Console.WriteLine()
	Console.WriteLine("Usage:")
	Console.WriteLine("  VFPXPorterCmd -o:<output> [-f:<file> ...] [-p:<project>] [-b] [-l:<logfile>]")
	Console.WriteLine()
	Console.WriteLine("Options:")
	Console.WriteLine("  -f:<path>   SCX (form) or VCX (class library) file to convert.")
	Console.WriteLine("              Can be repeated to process multiple files in one run.")
	Console.WriteLine("  -p:<path>   PJX (VFP project) file to convert entirely")
	Console.WriteLine("              (forms, libraries, menus, programs).")
	Console.WriteLine("  -o:<path>   Output folder. Created automatically if it does not exist.")
	Console.WriteLine("  -b          Backup source structure to XML files before converting.")
	Console.WriteLine("  -l:<path>   Write log output to this file (in addition to the console).")
	Console.WriteLine()
	Console.WriteLine("Examples:")
	Console.WriteLine(e"  VFPXPorterCmd -f:\"C:\\MyApp\\myform.scx\" -o:\"C:\\Output\"")
	Console.WriteLine(e"  VFPXPorterCmd -p:\"C:\\MyApp\\myapp.pjx\" -o:\"C:\\Output\" -b -l:convert.log")
	Console.WriteLine()
	Console.WriteLine("Exit codes:  0 = success,  1 = error")
	RETURN
