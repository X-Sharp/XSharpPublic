// Helpers.prg
// Created by    : Fabrice
// Creation Date : 
// Created for   : 
// WorkStation   : 

// !!! WARNING !!!
// This file is copy as-is in the destination project


USING System
USING System.Collections.Generic
USING System.Text


/// <summary>
/// Replace the Std RGB() function to ease migration
/// </summary>
/// <param name="r"></param>
/// <param name="g"></param>
/// <param name="b"></param>
/// <returns>A System.Drawing.Color as expected by Windows Forms</returns>
FUNCTION RGB( r AS INT, g AS INT, b AS INT ) AS System.Drawing.Color
    RETURN System.Drawing.Color.FromARGB( r, g, b )


[Obsolete("Return an empty Path")];
FUNCTION Home( nLocation ) CLIPPER
RETURN ""

