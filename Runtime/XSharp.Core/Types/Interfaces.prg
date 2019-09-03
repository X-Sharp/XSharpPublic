//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp

	/// <summary>
	/// This interface defines Date values
	/// </summary>
	/// <seealso cref="T:XSharp.__Date"/>
	/// <seealso cref="T:XSharp.RDD.DbDate"/>
	INTERFACE IDate
		/// <summary>Year part of the date. A number between 0 and 9999</summary>
		/// <returns>Integer value</returns>
		PROPERTY Year		AS INT GET
		/// <summary>Month part of the date. A number between 0 an 12</summary>
		/// <returns>Integer value</returns>
		PROPERTY Month		AS INT GET
		/// <summary>Day part of the date. A number between 0 an 31</summary>
		/// <returns>Integer value</returns>
		PROPERTY Day		AS INT GET
		/// <summary>Date as System.DateTime structure</summary>
		/// <returns>System.DateTime value</returns>
		PROPERTY @@Value		AS DateTime GET 
		/// <summary>Is the date empty (NULL_DATE)</summary>
		/// <returns>Logical value</returns>
		PROPERTY IsEmpty	AS LOGIC GET
	END INTERFACE

	/// <summary>
	/// This interface defines FLOAT values
	/// </summary>
	/// <seealso cref="T:XSharp.__Float"/>
	/// <seealso cref="T:XSharp.RDD.DbFloat"/>
	INTERFACE IFloat
		/// <summary>Double value of the Float</summary>
		/// <returns>Integer value</returns>
		PROPERTY @@Value    AS REAL8 GET
		/// <summary>Number of digits (includes the optional decimal separator and decimals).</summary>
		/// <returns>Integer value</returns>
		PROPERTY Digits	  AS INT  GET 
		/// <summary>Number of decimals.</summary>
		/// <returns>Integer value</returns>
		PROPERTY Decimals AS INT  GET 
	END INTERFACE

	/// <summary>
	/// This interface must be implemented by objects that register themselves for DB Notifications
	/// </summary>
    INTERFACE IDbNotify
        METHOD Notify(nEvent AS LONG, nNotification AS LONG) AS VOID
    END INTERFACE 
END NAMESPACE
