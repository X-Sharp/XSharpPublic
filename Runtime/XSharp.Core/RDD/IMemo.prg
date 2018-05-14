//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>This interface defines the mimimum methods that a RDD that supports memos should implement.</summary>	
INTERFACE XSharp.RDD.IMemo
	
	// Read & Write	
	/// <summary>Get a value from a memo file for the specified column.</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be retrieved.</param>
	METHOD GetValue(nFldPos AS INT) AS OBJECT
	/// <summary>Get the value for a column and write (export) it to an external file.</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be retrieved.</param>
	/// <param name="fileName">Name of the file that needs to be written to.</param>
	METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	/// <summary>Get the length of the for the specified column.</summary>	
	/// <param name="nFldPos">1 based column number for which the length should be retrieved.</param>
	METHOD GetValueLength(nFldPos AS INT) AS INT
	/// <summary>Write a value to a memo file for a specified column</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be written.</param>
	/// <param name="oValue">New value that needs to written to the memo file for this column.</param>
	METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
	/// <summary>Read (Import) a value from an external file and write it to the specified column.</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be written.</param>
	/// <param name="fileName">Name of the file that needs to be read from.</param>
	METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
	/// <summary>Flush the changes to the memo file </summary>	
	METHOD Flush() 			AS LOGIC

	// Memo File Access 
	/// <summary>Close the memo file</summary>	
	METHOD CloseMemFile() 	AS LOGIC    
	/// <summary>Create the memo file</summary>	
	/// <param name="info">object describing the file to create.</param>
	METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
	/// <summary>Open the memo file </summary>	
	/// <param name="info">object describing the file to open.</param>
	METHOD OpenMemFile(info AS DbOpenInfo) 	AS LOGIC   


			
END INTERFACE	
