// 72. compiler crash in both lines with "Parameters"
CLASS Test
PROTECT Parameters AS System.Collections.ArrayList
CONSTRUCTOR()
? .not. Parameters:Contains(1) 
END CLASS

