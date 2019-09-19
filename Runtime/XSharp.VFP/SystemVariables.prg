//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

///<summary>Specifies the number of columns in an ASCII text file created with the ASCII keyword in the REPORT FORM command.</summary>
GLOBAL _ASCIICOLS := 0 AS LONG
///<summary>Specifies the number of rows in an ASCII text file created with the ASCII keyword in the REPORT FORM command.</summary>
GLOBAL _ASCIIROWS := 0 AS LONG
///<summary>Contains the memo field offset for the MLINE( ) function.</summary>
GLOBAL _MLINE     := 0 AS LONG
///<summary>Contains the current page number.</summary>
GLOBAL _PAGENO    := 1 AS LONG
///<summary>Contains the total number of pages in a report.</summary>
GLOBAL _PAGETOTAL := 0 AS LONG
///<summary>Specifies a character expression to preface text merge lines.</summary>
GLOBAL _PRETEXT   := "" AS STRING
///<summary>Contains the number of records processed by the most recently executed table command.</summary>
GLOBAL _TALLY    := 0 AS LONG
///<summary>File handle for the output file for the TEXT .. ENDTEXT and \ and \\ commands.</summary>
/// <seealso cref='M:XSharp.VFP.Functions.SetTextFile(System.String)' />
GLOBAL _TEXT     := -1 AS INT64
///<summary>Contains a numeric value indicating the current trigger procedure nesting level.</summary>
GLOBAL _TRIGGERLEVEL := 0 AS LONG
///<summary></summary>

