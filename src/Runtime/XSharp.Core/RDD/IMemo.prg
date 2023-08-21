//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support
/// <summary>This interface defines the mimimum methods that a RDD that supports memos should implement.</summary>
INTERFACE XSharp.RDD.IMemo

#region Properties
    PROPERTY FullPath as STRING GET
#endregion
#region Read/Write
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
    /// <summary>Physically remove all memos from the memofile.</summary>
    METHOD Zap() AS LOGIC
#endregion
#region Memo File Access
    /// <summary>Close the memo file</summary>
    METHOD CloseMemFile() 	AS LOGIC
    /// <summary>Create the memo file</summary>
    /// <param name="info">object describing the file to create.</param>
    METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
    /// <summary>Open the memo file </summary>
    /// <param name="info">object describing the file to open.</param>
    METHOD OpenMemFile(info AS DbOpenInfo) 	AS LOGIC
#endregion

END INTERFACE

/// <summary>
/// An RDD that implements this interface will return the data for memo fields as BYTE[] when
/// the ReturnRawData property is set TRUE
/// </summary>
INTERFACE XSharp.RDD.IRawData
    /// <summary>
    /// Return Memo values as BYTE[] ?
    /// </summary>
    PROPERTY ReturnRawData AS LOGIC GET SET
END INTERFACE

/// <summary>
/// Interface that is used for BLOB operations for memo files
/// </summary>
INTERFACE XSharp.RDD.IBlobData
    /// <summary>
    /// Pointer for which to write or return the data
    /// </summary>
    PROPERTY Pointer   as INT GET SET
    /// <summary>
    /// Start of the data to write or return. Defaults to 0
    /// </summary>
    PROPERTY Start     AS INT GET SET
    /// <summary>
    /// Length of the data to write or return. Defaults to all data
    /// </summary>
    PROPERTY Length    AS INT GET SET
    /// <summary>
    /// Data to write to the blob file. This can be a BYTE[] but also other values.
    /// At this moment the FPT RDD supports BYTE[], String, Logic
    /// Other value types may follow later
    /// </summary>
    PROPERTY Data      AS OBJECT GET SET
END INTERFACE

