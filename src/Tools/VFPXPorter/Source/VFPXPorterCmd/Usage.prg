// Usage.prg
// Created by    : fabri
// Creation Date : 9/23/2020 11:57:54 AM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

FUNCTION Usage AS VOID
	Console.WriteLine("FabVFPXPorter Usage :" )
	//-f:"TestDataSet\form1.scx" -o:"VFPXPorter" -b
	Console.WriteLine(" -f:<fullpath to file>		Set the file to process")
	Console.WriteLine(" -o:<output folder>			Set the Folder where generated files must be placed")
	Console.WriteLine(" -b							Indicate to backup all content to XML files")
	Console.WriteLine()
	Console.WriteLine( "Example :")
	Console.WriteLine( e"FabVFPXPorterCmd -f:\"TestDataSet\\form1.scx\" -o:\"VFPXPorter\" -b" )
    RETURN