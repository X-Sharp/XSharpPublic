// CodeConverter.prg
// Created by    : fabri
// Creation Date : 1/11/2021 2:06:32 PM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
	/// The CodeConverter class.
	/// Provide several methods to convert some VFP code to XSharp
	/// </summary>
	CLASS CodeConverter

		PRIVATE _keepOriginal AS LOGIC
		PRIVATE _convertThisObject AS LOGIC
		PRIVATE _convertStatement AS LOGIC
		PRIVATE _convertStatementOnlyIfLast AS LOGIC
		PRIVATE _lineContent AS SortedDictionary<INT,STRING>


		PROPERTY Statements AS List<STRING> AUTO

		PROPERTY VFPElements AS Dictionary<STRING,STRING> AUTO

		PROPERTY ColorProperties AS List<STRING> AUTO


		CONSTRUCTOR( ko AS LOGIC, cvtThisObject AS LOGIC, cvtStatement AS LOGIC, cvtOnlyIfLast AS LOGIC )
			SELF:_keepOriginal := ko
			SELF:_convertThisObject := cvtThisObject
			SELF:_convertStatement := cvtStatement
			SELF:_convertStatementOnlyIfLast := cvtOnlyIfLast
			// We will store the line Number where we need to add comments
			// These are sorted in Reverse Order ( Greatest -> Lowest )
			SELF:_lineContent := SortedDictionary<INT,STRING>{ ReverseInt{} }
			SELF:Statements := List<String>{}
			SELF:VFPElements := Dictionary<STRING,STRING>{}
			SELF:ColorProperties := List<STRING>{}

			// The lines of source code that will handle that Code
		PROPERTY Source AS List<STRING> AUTO

		/// <summary>
		///
		/// </summary>
		/// <param name="cdeBlock"></param>
		PUBLIC METHOD ProcessEvent( cdeBlock AS EventCode ) AS VOID
			LOCAL Name AS STRING
			//
			Name := ""
			SELF:Source := cdeBlock:Source
			IF cdeBlock:Owner != NULL
				Name := cdeBlock:Owner:Name
				IF cdeBlock:Owner:Owner != NULL
					IF cdeBlock:Owner:Owner:IsInLibrary
						Name := ""
					ENDIF
				ENDIF
			ENDIF
			SELF:_lineContent:Clear()
			//
			IF SELF:_convertStatement
				SELF:ChangeStatement()
			ENDIF
			SELF:ChangeColorProperties()
			IF SELF:_convertThisObject
				IF cdeBlock:Owner != NULL
					SELF:ChangeThisObject( cdeBlock:Owner:FoxClassName )
				ENDIF
			ENDIF
			// Now, add the original line as Comment, just before the changed one
			IF SELF:_keepOriginal .AND. _lineContent:Count > 0
				FOREACH VAR lineInfo IN _lineContent
					SELF:Source:Insert( lineInfo:Key, "** VFPXPorter -=>" + lineInfo:Value)
				NEXT
			ENDIF
			//
			cdeBlock:Source := SELF:Source

		PUBLIC METHOD ProcessProcedure( sourceCode AS STRING, procedureName AS STRING ) AS VOID
			SELF:Source := ReadSource(sourceCode)
			SELF:CheckForProcedureName(procedureName)
			IF SELF:_convertStatement
				SELF:ChangeStatement()
			ENDIF
			SELF:ChangeColorProperties()

		// Converts a block of menu handler code: applies statement→method-call conversions
		// and VFPElements substitutions (THISFORM./THISFORMSET.) without the form-perspective
		// logic that ProcessEvent uses. Caller sets Source on return.
		PUBLIC METHOD ProcessMenuCode( sourceCode AS STRING ) AS VOID
			SELF:Source := ReadSource(sourceCode)
			IF SELF:_convertStatement
				SELF:ChangeStatement()
			ENDIF
			SELF:ChangeColorProperties()
			IF SELF:_convertThisObject
				SELF:ChangeThisObject()
			ENDIF



			// We will enumerate the lines of source code
			// and change the "perspective" : FoxPro handler is from the Control perspective; .NET are from the Form perspective
			// -> change this. to thisObject.
			// The VAR thisObject is injected during export
		PRIVATE METHOD ChangeThisObject( FoxClassName := "" AS STRING ) AS VOID
			LOCAL line AS STRING
			LOCAL startLine := 0 AS INT
			LOCAL toBeContinued := FALSE AS LOGIC
			// First, process Parent Call
			FOR VAR i := 0 TO SELF:Source:Count-1
				line := SELF:Source[i]
				// TODO : Try to do this only once...will be three times faster...
				IF SELF:_keepOriginal
					IF line:TrimEnd():EndsWith(";")
						toBeContinued := TRUE
					ELSE
						IF toBeContinued
							toBeContinued := FALSE
						ELSE
							startLine := i
						ENDIF
					ENDIF
				ENDIF
				// Change this. to thisObject.
				//line := SELF:SearchAndReplace( startLine, line, "this.", "thisObject.", null )
				// Change Parent. to _Parent.
				//line := SELF:SearchAndReplace( startLine, line, "Parent.", "_Parent.", null )
				// Change VFP Elts To WinForms Elts
				FOREACH VAR elt IN SELF:VFPElements
					line := SELF:SearchAndReplace( startLine, line, elt:Key, elt:Value, null )
				NEXT
				//
				SELF:Source[i] := line
			NEXT
			RETURN
		END METHOD

		// We will enumerate the lines of source code
		PRIVATE METHOD ChangeStatement( ) AS VOID
			LOCAL line AS STRING
			LOCAL startLine := 0 AS INT
			LOCAL toBeContinued := FALSE AS LOGIC
			// First, process Parent Call

			FOR VAR i := 0 TO SELF:Source:Count-1
				line := SELF:Source[i]
				// TODO : Try to do this only once...will be three times faster...
				IF SELF:_keepOriginal
					IF line:TrimEnd():EndsWith(";")
						toBeContinued := TRUE
					ELSE
						IF toBeContinued
							toBeContinued := FALSE
						ELSE
							startLine := i
						ENDIF
					ENDIF
				ENDIF
				// Change Statement to Statement
				FOREACH VAR statement IN SELF:Statements
					line := SELF:SearchAndReplace( startLine, line, statement, statement+"()", null, true, SELF:_convertStatementOnlyIfLast )
				NEXT
				//
				SELF:Source[i] := line
			NEXT

			RETURN
		END METHOD

		/// <summary>
		/// Search for a String and replace it by something else.
		/// Take care we don't have a letter or an underscore before the searched string
		/// and that it doesn't ends with an open parenthesis.
		/// (May be we could do that with a RegEx ??)
		/// </summary>
		/// <param name="lineNumber">The LineNumber in the Original code source List of String</param>
		/// <param name="line">The source itself</param>
		/// <param name="search">What we search</param>
		/// <param name="replace">What it will be replaced by</param>
		/// <param name="extra"></param>
		/// <param name="strictSize">Check that after searched string we don't have a (, or char, or a digit, or a _</param>
		/// <param name="onlyIfLast">Replace only if Statement is the last thing on Line</param>
		/// <returns></returns>
		PRIVATE METHOD SearchAndReplace( lineNumber AS INT, line AS STRING, search AS STRING, replace AS STRING, extra AS STRING, strictSize := FALSE AS LOGIC, onlyIfLast := FALSE AS LOGIC ) AS STRING
			LOCAL useExtra AS LOGIC
			LOCAL c AS Char
			LOCAL amp := false AS LOGIC
			LOCAL inSearch AS INT
			// quoteChar tracks the delimiter of the current open string literal (\0 = not in string).
			// VFP supports three string delimiters: "...", '...' and [...] (closes with ]).
			// Using a single matching-delimiter approach prevents apostrophes inside "it's" from
			// prematurely closing the string, which the old toggle-on-any-quote logic got wrong.
			LOCAL quoteChar := (CHAR)0 AS CHAR
			// Check for line starting with * or &&
			VAR tmp := line:TrimStart()
			IF tmp:StartsWith("*") .OR. tmp:StartsWith("&&")
				RETURN line
			ENDIF
			// Save current line
			VAR org := line
			TRY
				useExtra := !String.IsNullOrEmpty( extra )
				inSearch := 0
				//
				VAR pos := -1
				WHILE TRUE
					pos++
					IF pos >= line:Length
						EXIT
					ENDIF
					// Get one char
					c := line[ pos ]
					// String literal detection: track opening delimiter and only close on its match.
					// '[' opens a bracket string that closes with ']'.
					IF c:CompareTo('"')==0 .OR. (INT)c==39 .OR. c:CompareTo('[')==0 .OR. c:CompareTo(']')==0
						IF quoteChar == (CHAR)0
							IF c:CompareTo(']') != 0  // ']' alone cannot open a string
								quoteChar := IIF( c:CompareTo('[')==0, ']', c )
							ENDIF
						ELSEIF c == quoteChar
							quoteChar := (CHAR)0
						ENDIF
					ENDIF
					IF quoteChar != (CHAR)0
						LOOP
					ENDIF
					// Start of Comment ?
					IF ( c:CompareTo('&')==0 )
						IF !amp
							amp := true
						else
							// Comment, bye
							EXIT
						endif
					else
						amp := false
					endif
					//
					IF Char.ToUpperInvariant(c) != Char.ToUpperInvariant( search[ inSearch ] )
						inSearch := 0
						LOOP
					ENDIF
					// pos == 3
					// inSearch == 3
					// 012345
					// abcdef
					//  bcd
					inSearch++
					IF inSearch == search:Length
						// We found the Search String
						// Check we don't have a Character (a-z) in front of "this.", to avoid var like "moveThis."
						// and for the same reason check for underscore
						VAR startPos := pos - (inSearch-1)
						IF ( startPos > 0 )
							IF search[0]:CompareTo('.')!=0 .AND. ( Char.IsLetter(line[startPos-1]) .OR. line[startPos-1]:CompareTo('_')==0 )
								inSearch := 0
								LOOP
							ENDIF
						ENDIF
						IF strictSize
							IF pos+1 < line:Length
								IF line[pos+1]:CompareTo('(')==0 .OR. ;
								   Char.IsLetterOrDigit(line[pos+1]) .OR. ;
								   line[pos+1]:CompareTo('_')==0
									inSearch := 0
									loop
								ENDIF
							ENDIF
						ENDIF
						// and replace
						VAR eol := String.Empty
						IF pos+1 < line:Length
							eol := line:Substring( pos+1 )
						ENDIF
						IF (!onlyIfLast) .OR. (onlyIfLast .AND. String.IsNullOrWhiteSpace(eol))
							line := line.Substring(0,startPos) + replace
							IF useExtra
								line += extra
							ENDIF
							pos := line:Length
							line += eol
						ENDIF
						// and reset
						inSearch := 0
					ENDIF
				ENDDO
			CATCH e AS Exception
				XPorterLogger.Instance:Warning("SearchAndReplace: Failed to convert line, using original")
				XPorterLogger.Instance:Verbose("Original line: " + org)
				XPorterLogger.Instance:Verbose("Exception: " + e:Message)
				line := org
			END TRY
			//
			RETURN line


		PRIVATE METHOD CheckForProcedureName(procedureName AS STRING) AS VOID
			LOCAL isComment := FALSE AS LOGIC
			LOCAL needPrototype := TRUE AS LOGIC
			//LOCAL viewCode := FALSE AS LOGIC
			//
			FOREACH line AS STRING IN SELF:Source
				IF String.IsNullOrEmpty( line )
					LOOP
				ENDIF
				VAR currentLine := line:Trim()
				IF currentLine:StartsWith("*")
					IF currentLine:EndsWith(";")
						isComment := TRUE
					ENDIF
					LOOP
				ELSEIF currentLine:StartsWith("#")
					LOOP
				ENDIF
				IF isComment
					IF currentLine:EndsWith(";")
						isComment := TRUE
					ELSE
						isComment := FALSE
					ENDIF
					LOOP
				ENDIF
				IF currentLine:StartsWith("PROCEDURE ") .OR. currentLine:StartsWith("FUNCTION ")
					needPrototype := FALSE
				ELSEIF currentLine:StartsWith("DEFINE ")
					needPrototype := FALSE
				ENDIF
				EXIT
			NEXT
			//
			IF needPrototype
				SELF:Source:Insert( 0, "** VFPXPorter -=> Add procedure name." )
				SELF:Source:Insert( 0, "PROCEDURE " + procedureName )
			ENDIF

		// Returns the index of the first && comment that is not inside a string literal,
		// starting search at startPos. Returns -1 if none found.
		PRIVATE METHOD FindComment( line AS STRING, startPos AS INT ) AS INT
			LOCAL quoteChar := (CHAR)0 AS CHAR
			LOCAL i AS INT
			i := startPos
			DO WHILE i < line:Length
				LOCAL c := line[i] AS CHAR
				IF quoteChar == (CHAR)0
					IF c == '"' .OR. (INT)c == 39 .OR. c == '['
						quoteChar := IIF( c == '[', ']', c )
					ELSEIF c == '&' .AND. i + 1 < line:Length .AND. line[i+1] == '&'
						RETURN i
					ENDIF
				ELSEIF c == quoteChar
					quoteChar := (CHAR)0
				ENDIF
				i++
			ENDDO
			RETURN -1

		// Scans the line for a pattern: .<colorPropName><spaces>=<not=> outside strings/comments.
		// Returns the index of the first character of the RHS expression (after the =), or -1.
		// Also returns the index of the dot via dotPos.
		PRIVATE METHOD FindColorAssignment( line AS STRING, dotPos REF INT ) AS INT
			LOCAL quoteChar := (CHAR)0 AS CHAR
			LOCAL amp := FALSE AS LOGIC
			LOCAL i AS INT
			i := 0
			DO WHILE i < line:Length
				LOCAL c := line[i] AS CHAR
				// track string literals
				IF quoteChar == (CHAR)0
					IF c == '"' .OR. (INT)c == 39 .OR. c == '['
						quoteChar := IIF( c == '[', ']', c )
						i++ ; LOOP
					ENDIF
				ELSE
					IF c == quoteChar
						quoteChar := (CHAR)0
					ENDIF
					i++ ; LOOP
				ENDIF
				// track comments
				IF c == '&'
					IF amp
						RETURN -1   // rest of line is comment
					ENDIF
					amp := TRUE
                    i++ ; LOOP
				ELSE
					amp := FALSE
				ENDIF
				// look for dot
				IF c != '.'
					i++ ; LOOP
				ENDIF
				// found a dot — check if followed by a known color property
				LOCAL propStart := i + 1 AS INT
				LOCAL matched := "" AS STRING
				FOREACH prop AS STRING IN SELF:ColorProperties
					LOCAL propEnd := propStart + prop:Length AS INT
					IF propEnd <= line:Length
						LOCAL candidate := line:Substring( propStart, prop:Length ):ToLower() AS STRING
						IF candidate == prop:ToLower()
							// make sure it's not part of a longer identifier
							IF propEnd < line:Length
								LOCAL nextClr := line[propEnd] AS CHAR
								IF Char.IsLetterOrDigit(nextClr) .OR. nextClr == '_'
									LOOP
								ENDIF
							ENDIF
							matched := prop
							EXIT
						ENDIF
					ENDIF
				NEXT
				IF String.IsNullOrEmpty(matched)
					i++ ; LOOP
				ENDIF
				// found the property — skip spaces, expect = but not ==
				LOCAL eqIdx := propStart + matched:Length AS INT
				DO WHILE eqIdx < line:Length .AND. line[eqIdx] == ' '
					eqIdx++
				ENDDO
				IF eqIdx >= line:Length .OR. line[eqIdx] != '='
					i++ ; LOOP
				ENDIF
				IF eqIdx + 1 < line:Length .AND. line[eqIdx+1] == '='
					i++ ; LOOP   // == comparison, not assignment
				ENDIF
				// also skip if RHS already starts with VFPTools.ColorFromVFP
				LOCAL rhsStart := eqIdx + 1 AS INT
				DO WHILE rhsStart < line:Length .AND. line[rhsStart] == ' '
					rhsStart++
				ENDDO
				IF rhsStart < line:Length
					LOCAL remaining := line:Substring(rhsStart) AS STRING
					IF remaining:ToLower():StartsWith("vfptools.colorFromvfp(")
						RETURN -1
					ENDIF
				ENDIF
				dotPos := i
				RETURN rhsStart
			ENDDO
			RETURN -1

		// Converts assignments to color properties in the current Source lines:
		//   obj.BackColor = expr  →  obj.BackColor = VFPTools.ColorFromVFP(expr)
		PRIVATE METHOD ChangeColorProperties() AS VOID
			FOR VAR i := 0 TO SELF:Source:Count - 1
				LOCAL line := SELF:Source[i] AS STRING
				// skip empty lines and full-line comments
				LOCAL trimmed := line:TrimStart() AS STRING
				IF String.IsNullOrEmpty(trimmed) .OR. trimmed:StartsWith("*") .OR. trimmed:StartsWith("&&")
					LOOP
				ENDIF
				LOCAL dotPos := 0 AS INT
				LOCAL rhsStart := SELF:FindColorAssignment(line, REF dotPos) AS INT
				IF rhsStart < 0
					LOOP
				ENDIF
				// separate RHS expression from trailing comment
				LOCAL commentIdx := SELF:FindComment(line, rhsStart) AS INT
				LOCAL rhs AS STRING
				LOCAL comment AS STRING
				IF commentIdx >= 0
					rhs     := line:Substring(rhsStart, commentIdx - rhsStart):TrimEnd()
					comment := " " + line:Substring(commentIdx)
				ELSE
					rhs     := line:Substring(rhsStart):TrimEnd()
					comment := String.Empty
				ENDIF
				IF String.IsNullOrWhiteSpace(rhs)
					LOOP
				ENDIF
				// rebuild: prefix + VFPTools.ColorFromVFP(rhs) + comment
				LOCAL prefix := line:Substring(0, rhsStart) AS STRING
				SELF:Source[i] := prefix + "VFPTools.ColorFromVFP(" + rhs + ")" + comment
			NEXT

		METHOD ToString() AS STRING
			VAR code := StringBuilder{ }
			//code:AppendLine( SELF:Definition )
			FOREACH VAR line IN Source
				code:AppendLine( line )
			NEXT
			RETURN code:ToString()


	END CLASS

	CLASS ReverseInt IMPLEMENTS IComparer<INT>

		PUBLIC METHOD Compare( x AS INT, y AS INT ) AS INT
			IF ( x > y )
				RETURN -1
			ELSE
				IF ( x < y )
					RETURN 1
				ENDIF
			ENDIF
			RETURN 0
	END CLASS


END NAMESPACE // VFPXPorterLib
