// Extensions.prg
// Created by    : fabri
// Creation Date : 10/17/2020 7:12:25 PM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Linq

// Read all Lines of Source Code
FUNCTION ReadSource( source AS STRING ) AS List<STRING>
	LOCAL strReader AS StringReader
	LOCAL lines AS List<STRING>
	LOCAL oneLine AS STRING
	// Get Lines, one by one
	lines := List<STRING>{}
	TRY
		strReader := StringReader{ source }
		//
		REPEAT
			oneLine := strReader:ReadLine()
			IF ( oneLine != NULL )
				lines:Add( oneLine )
			ENDIF
		UNTIL oneLine == NULL
		// remove all empty lines from End
		VAR pos := 0
		REPEAT
			pos := lines:Count
			IF pos > 0
				oneLine := lines[pos-1]
				IF String.IsNullOrEmpty( oneLine )
					lines:RemoveAt(pos-1)
					LOOP
				ELSE
					EXIT
				ENDIF
			ENDIF
		UNTIL pos == 0
    CATCH
        nop
	END TRY
	//
	RETURN lines


FUNCTION GetRelativePath( relativeTo AS STRING, path AS STRING ) AS STRING
	VAR relDirs := relativeTo:Split('\\')
	VAR pathDirs := path:Split('\\')

	// Get the shortest of the two paths
	LOCAL len AS INT
	IF relDirs:Length < pathDirs:Length
		len := relDirs:Length
	ELSE
		len := pathDirs:Length
	ENDIF

	// Use to determine where in the loop we exited
	LOCAL lastCommonRoot := -1 AS INT
	LOCAL index AS INT

	// Find common root
	FOR index := 1 TO len
		IF (relDirs[index] == pathDirs[index])
			lastCommonRoot := index
		ELSE
			EXIT
		ENDIF
	NEXT

	// If we didn't find a common prefix then throw
	IF (lastCommonRoot == -1)
		THROW ArgumentException{"Paths do not have a common base"}
	ENDIF

	// Build up the relative path
	LOCAL relativePath := StringBuilder{} AS StringBuilder

	// Add on the ..
	FOR index := lastCommonRoot + 1 TO relDirs:Length
		IF (relDirs[index]:Length > 0)
			relativePath:Append("..\")
		ENDIF
	NEXT

	// Add on the folders
	FOR index := lastCommonRoot + 1 TO pathDirs:Length - 1
		relativePath:Append(pathDirs[index] + "\")
	NEXT
	relativePath:Append(pathDirs[pathDirs:Length ])

	RETURN relativePath:ToString()

FUNCTION IsFullPath( checkedPath AS STRING ) AS LOGIC

	IF (String.IsNullOrWhiteSpace(checkedPath) .OR. checkedPath.IndexOfAny(Path.GetInvalidPathChars()) != -1 .OR. !Path.IsPathRooted(checkedPath))
		RETURN FALSE
	ENDIF

	VAR pathRoot := Path.GetPathRoot(checkedPath)
	IF (pathRoot.Length <= 2 .AND. pathRoot != "/") // Accepts X:\ and \\UNC\PATH, rejects empty string, \ and X:, but accepts / to support Linux
		RETURN FALSE
	ENDIF

	IF (pathRoot[0] != '\\' .OR. pathRoot[1] != '\\')
		RETURN TRUE // Rooted and not a UNC path
	ENDIF

	RETURN pathRoot:Trim('\\'):IndexOf('\\') != -1 // A UNC server name without a share name (e.g "\\NAME" or "\\NAME\") is invalid



STATIC CLASS FabStringExtensions

	STATIC METHOD ReplaceEx( SELF str AS StringBuilder, oldValue AS STRING, newValue AS STRING,  ignoreCase AS LOGIC ) AS StringBuilder
		// Oops
		IF String.IsNullOrEmpty(oldValue)
			THROW ArgumentNullException{"oldValue"}
		ENDIF
		IF String.IsNullOrEmpty(newValue)
			THROW ArgumentNullException{"newValue"}
		ENDIF
		//
		VAR sb := StringBuilder{}
		VAR previousIndex := 0
		VAR index := str:IndexOf(oldValue, ignoreCase)
		//
		DO WHILE (index != -1)
			sb:Append(str:Substring(previousIndex, index - previousIndex))
			sb:Append(newValue)
			index += oldValue.Length

			previousIndex := index
			index := str:IndexOf(oldValue, index, ignoreCase)
		ENDDO
		sb:Append(str:Substring(previousIndex))
		//
		str:Clear()
		str:Append( sb )
		RETURN sb

	STATIC METHOD IndexOf( SELF sb AS StringBuilder, toSearch AS STRING) AS INT
		IF (String.IsNullOrEmpty(toSearch))
			RETURN 0
		ENDIF
		RETURN IndexOf(sb, toSearch, 0, FALSE)

	STATIC METHOD IndexOf( SELF sb AS StringBuilder, toSearch AS STRING, ignoreCase AS LOGIC) AS INT
		IF (String.IsNullOrEmpty(toSearch))
			RETURN 0
		ENDIF
		RETURN IndexOf(sb, toSearch, 0, ignoreCase)

	STATIC METHOD IndexOf( SELF sb AS StringBuilder, toSearch AS STRING, startIndex AS INT, ignoreCase AS LOGIC) AS INT
		// Ooops
		IF (startIndex > sb.Length)
			THROW  ArgumentOutOfRangeException{"startIndex"}
		ENDIF
		IF (sb.Length == 0) .OR. (toSearch == NULL)
			RETURN -1
		ENDIF
		IF (toSearch == String.Empty)
			RETURN startIndex
		ENDIF
		//
		LOCAL index AS INT
		LOCAL leftChar, rightChar AS CHAR
		VAR searchLength := toSearch:Length
		VAR count := (sb:Length - searchLength) + 1
		//
		FOR VAR i := startIndex TO count-1
			IF !ignoreCase
				leftChar := sb[i]
				rightChar := toSearch[0]
			ELSE
				leftChar := Char.ToLower(sb[i])
				rightChar := Char.ToLower(toSearch[0])
			ENDIF
			IF leftChar != rightChar
				LOOP
			ENDIF
			index := 1
			DO WHILE index < searchLength
				IF !ignoreCase
					leftChar := sb[i + index]
					rightChar := toSearch[index]
				ELSE
					leftChar := Char.ToLower(sb[i + index])
					rightChar := Char.ToLower(toSearch[index])
				ENDIF
				IF leftChar != rightChar
					EXIT
				ENDIF
				index++
			ENDDO
			IF (index == searchLength)
				RETURN i
			ENDIF
		NEXT
		RETURN -1


	STATIC METHOD Substring( SELF sb AS StringBuilder, startIndex AS INT ) AS STRING
		RETURN sb:ToString(startIndex, sb:Length - startIndex)

	STATIC METHOD Substring( SELF sb AS StringBuilder, startIndex AS INT, length AS INT ) AS STRING
		RETURN sb:ToString(startIndex, length )


END CLASS


STATIC CLASS FabHashSetHelper

    PUBLIC STATIC METHOD AddRange<T>( SELF hash AS HashSet<T>, hashRange AS HashSet<T> ) AS VOID
        hashRange?:ForEach( { x => hash:Add(x) } )
END CLASS




STATIC CLASS FabDictionaryHelper

	PUBLIC STATIC METHOD AddRangeOverride<TKey, TValue>( SELF dic AS IDictionary<TKey, TValue>, dicToAdd AS IDictionary<TKey, TValue> ) AS VOID
		dicToAdd?:ForEach( { x => dic[x:Key] := x:Value } )
	END METHOD

	PUBLIC STATIC METHOD AddRangeNewOnly<TKey, TValue>( SELF dic AS IDictionary<TKey, TValue>, dicToAdd AS IDictionary<TKey, TValue> ) AS VOID
		dicToAdd?:ForEach( { x =>
		IF !dic:ContainsKey(x:Key)
			dic:Add(x.Key, x.Value)
		ENDIF
		})
	END METHOD

	PUBLIC STATIC METHOD AddRange<TKey, TValue>(SELF dic AS IDictionary<TKey, TValue>, dicToAdd AS IDictionary<TKey, TValue> ) AS VOID
		dicToAdd?:ForEach( { x => dic:Add(x:Key, x:Value) } )
	END METHOD

	PUBLIC STATIC METHOD ContainsKeys<TKey, TValue>(SELF dic AS IDictionary<TKey, TValue>, keys AS IEnumerable<TKey> ) AS LOGIC
		VAR result := FALSE
		keys:ForEachOrExit( { x =>
		result := dic:ContainsKey(x)
		RETURN result
		})
		RETURN result
	END METHOD

	PUBLIC STATIC METHOD ForEach<T>(SELF source AS IEnumerable<T>, action AS Action<T>) AS VOID
		FOREACH VAR item IN source
			action(item)
		NEXT
	END METHOD

	PUBLIC STATIC METHOD ForEachOrExit<T>(SELF source AS IEnumerable<T>, newFunc AS System.Func<T, logic>) AS VOID
		FOREACH VAR item IN source
			VAR result := newFunc(item)
			IF (result)
				EXIT
			ENDIF
		NEXT
	END METHOD
END CLASS