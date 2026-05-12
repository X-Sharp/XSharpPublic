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
		PRIVATE _convertThisParent AS LOGIC
		PRIVATE _convertThisObject AS LOGIC
		PRIVATE _convertStatement AS LOGIC
		PRIVATE _convertStatementOnlyIfLast AS LOGIC
		PRIVATE _lineContent AS SortedDictionary<INT,STRING>
			
		PROPERTY Statements AS List<STRING> AUTO
			
		PROPERTY VFPElements AS Dictionary<STRING,STRING> AUTO
			
			
		CONSTRUCTOR( ko AS LOGIC, cvtThisParent AS LOGIC, cvtThisObject AS LOGIC, cvtStatement AS LOGIC, cvtOnlyIfLast AS LOGIC )
			SELF:_keepOriginal := ko
			SELF:_convertThisParent := cvtThisParent
			SELF:_convertThisObject := cvtThisObject
			SELF:_convertStatement := cvtStatement
			SELF:_convertStatementOnlyIfLast := cvtOnlyIfLast
			// We will store the line Number where we need to add comments
			// These are sorted in Reverse Order ( Greatest -> Lowest )
			SELF:_lineContent := SortedDictionary<INT,STRING>{ ReverseInt{} }
			SELF:Statements := List<String>{}
			SELF:VFPElements := Dictionary<STRING,STRING>{}
			
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
			// Convert ThisObject has priority over ThisParent
			IF SELF:_convertThisObject
				IF cdeBlock:Owner != NULL
					SELF:ChangeThisObject( cdeBlock:Owner:FoxClassName )
				ENDIF
			ELSE
				IF SELF:_convertThisParent
					IF cdeBlock:Owner != NULL
						SELF:ChangeThisAndParent( cdeBlock:Owner:FoxClassName, Name )
					ENDIF
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
			//
			
			// We will enumerate the lines of source code
			// and change the "perspective" : FoxPro handler is from the Control perspective; .NET are from the Form perspective
			// -> change this.Parent to this.
			// -> this. to this.<theNameOfTheControl> if needed
			// -> ThisForm has to be Property on the new Form Class that points to SELF
			// WARNING !! This will be done for EventHandlers that are referring to OwnerControl instead of Form
			// Try to avoid using it, better to support these through NEW PROPERTY etc
		PRIVATE METHOD ChangeThisAndParent( FoxClassName := "" AS STRING, Name := "" AS STRING ) AS VOID
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
				// Change this.Parent to an impossible element, $$XSHARP$$
				line := SELF:SearchAndReplace( startLine, line, "this.parent.", "$$XSHARP$$.", null )
				// Now, Change this to $$XSHARP$$.<NameOfTheControl>
				IF (String.Compare( FoxClassName,"form",TRUE)!=0) .AND. !String.IsNullOrEmpty( Name )
					line := SELF:SearchAndReplace( startLine, line, "thisform.", "$$XSHARP$$.", Name+".")
				ELSE
					line := SELF:SearchAndReplace( startLine, line, "thisform.", "$$XSHARP$$.", null )
				ENDIF
				//
				SELF:Source[i] := line
			NEXT
			// Ok, now ... Brute-Force ;)
			FOR VAR i := 0 TO SELF:Source:Count-1
				line := SELF:Source[i]
				//
				line := line:Replace( "$$XSHARP$$.", "THIS." )
				//
				SELF:Source[i] := line
			NEXT			
			RETURN
			
			
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
			LOCAL ignore := false AS LOGIC
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
					// Simple Quote, Double Quotes ?? 34/39
					IF c:CompareTo('"')==0 .OR. (int)c==39 // '"' .OR. "'"
						if !ignore
							ignore := true
						else
							ignore := false
						endif
					ENDIF
					IF ignore
						loop
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
			CATCH
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