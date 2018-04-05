/// <summary>
/// </summary>
/// <param name="cSubKey"></param>
/// <returns>
/// </returns>
using XSharp
FUNCTION QueryRTRegArray(cSubKey AS STRING) AS Array
	/// THROW NotImplementedException{}
RETURN NULL_ARRAY   

/// <summary>
/// Retrieve a numeric value from the Registry.
/// </summary>
/// <param name="cSubKey"></param>
/// <param name="cKeyName"></param>
/// <returns>
/// </returns>
FUNCTION QueryRTRegInt(cSubKey AS STRING,cKeyName AS STRING) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// Retrieve a string value from the Registry.
/// </summary>
/// <param name="cSubKey"></param>
/// <param name="cKeyName"></param>
/// <returns>
/// </returns>
FUNCTION QueryRTRegString(cSubKey AS STRING,cKeyName AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   
