//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.RDD
	#region functions
	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION DbcsNext(pszSource AS PSZ) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <param name="pszCurr"></param>
	/// <returns>
	/// </returns>
	FUNCTION DbcsPrev(pszSource AS PSZ,pszCurr AS PSZ) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Return the name of the alias.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION DBF() AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Convert double-byte kana characters in a string to their single-byte equivalents.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION DBTOSB(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	#endregion
end namespace