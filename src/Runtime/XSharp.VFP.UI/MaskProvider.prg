// MaskProvider.prg
// Created by    : fabri
// Creation Date : 4/27/2023 8:32:04 AM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
// For DllImport
USING System.Runtime.InteropServices
USING System.Text
USING XSharp.RT

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The MaskProvider class, used in TextBox when applying an InputMask
	/// </summary>
	INTERNAL CLASS MaskProvider
		// Possible Mask chars "!#$,.9AHLNUWXY"
		// ",." removed from the list
		CONST INTERNAL MaskChar := "!#$9AHLNUWXY" AS STRING


		// The attached control
		PRIVATE _owner AS TextBox
			// The current InputMask
		PRIVATE _mask AS STRING
		PROPERTY Mask AS STRING GET _mask SET _mask := VALUE

			// The current Format ( like the Functions in VO )
		PRIVATE _format AS STRING
		PROPERTY Format AS STRING GET _format SET _format := VALUE

			// The expected type of Value property
		PRIVATE _type AS STRING

			// Value of the VFPTextBox
		PRIVATE _value AS USUAL
		PROPERTY Value AS USUAL
			GET
				RETURN _value
			END GET
			SET
				_value := VALUE
				IF !IsNil( _value )
					IF  !String.IsNullOrEmpty(SELF:_mask )
						SELF:_textValue := Transform( _value, SELF:_mask )
					ELSE
						SELF:_textValue := ((OBJECT)_value):ToString()
					ENDIF
				ELSE
					SELF:_textValue := SELF:EmptyValue
				ENDIF
				SELF:UpdateOwnerText()
			END SET
		END PROPERTY

		PRIVATE _textValue AS STRING
		PROPERTY TextValue AS STRING GET _textValue

		PROPERTY EmptyValue AS STRING
			GET
				VAR sb := StringBuilder{}
				IF SELF:_mask != NULL
					VAR isNumeric := ( SELF:_type == "N" )
					//
					FOR VAR i := 1 TO SELF:_mask:Length
						VAR maskChar := GetMask( i )
						IF SELF:IsMaskChar( maskChar )
							sb:Append(' ')
						ELSE
							IF maskChar == c'.' .AND. isNumeric
								sb:Append( SELF:_decimalSeparator )
							ELSEIF maskChar == c',' .AND. isNumeric
								sb:Append( SELF:_thousandSeparator )
							ELSE
								sb:Append( maskChar )
							ENDIF
						ENDIF
					NEXT
				ENDIF
				//
				RETURN sb:ToString()
			END GET
		END PROPERTY

		PRIVATE _decimalSeparator AS STRING
		PRIVATE _thousandSeparator AS STRING
		PRIVATE _currency AS STRING
		PROTECTED _currencyPosition AS INT

		CONSTRUCTOR( oOwner AS TextBox, mask AS STRING, format AS STRING, type := "C" AS STRING )
			SELF:_owner := oOwner
			SELF:_mask := mask
			SELF:_format := format
			IF type == NULL
				type := "C"
			ENDIF
			SELF:_type := type
			// TODO : Maybe we should read the SET xxx definitions here ?
			VAR numberFormat := System.Globalization.NumberFormatInfo.CurrentInfo
			SELF:_decimalSeparator := numberFormat:NumberDecimalSeparator
			SELF:_thousandSeparator := numberFormat:NumberGroupSeparator
			SELF:_currency := numberFormat:CurrencySymbol
			SELF:_currencyPosition := numberFormat:CurrencyPositivePattern // Where is located the $/€ sign ?
			//
			SELF:_textValue := SELF:EmptyValue
			//

			RETURN


		METHOD MoveToFirstEditingPosition() AS VOID
			LOCAL currentPos AS INT
			currentPos := 1
			IF !SELF:IsMaskChar( SELF:GetMask( currentPos ) )
				currentPos := SELF:NextEditingPosition( currentPos )
				SELF:_owner:CursorPos := currentPos
			ENDIF
		END METHOD

		/// <summary>
		/// Search the next position editable in the InputMask.
		/// </summary>
		/// <param name="currentPos">Where to start. One-based position</param>
		/// <returns>The new position. 0 if none</returns>
		METHOD NextEditingPosition( currentPos := -1 AS INT) AS INT
			//
			IF (currentPos == -1)
				currentPos := SELF:_owner:CursorPos+1
			ENDIF
			//
			VAR isNumeric := ( SELF:_type == "N" )
			FOR VAR i := currentPos TO SELF:_mask:Length
				// "Standard" letters
				IF IsMaskChar( SELF:GetMask( i ) )
					RETURN i
				ELSEIF isNumeric
					IF SELF:GetMask( i ) == c'.' .OR. SELF:GetMask( i ) == c','
						NOP
						//RETURN i
                    NOP
					ENDIF
				ENDIF
			NEXT
			RETURN currentPos
		END METHOD

		METHOD PreviousEditingPosition( currentPos := -1 AS INT ) AS INT
			//
			IF (currentPos == -1)
				currentPos := SELF:_owner:CursorPos-1
			ENDIF
			//
			VAR isNumeric := ( SELF:_type == "N" )
			FOR VAR i := currentPos DOWNTO 1
				// "Standard" letters
				IF IsMaskChar( SELF:GetMask( i ) )
					RETURN i
				ELSEIF isNumeric
					IF SELF:GetMask( i )== c'.' .OR. SELF:GetMask( i )== c','
						RETURN i
					ENDIF
				ENDIF
			NEXT
			RETURN 0
		END METHOD

		METHOD OnKeyDown( e AS KeyEventArgs ) AS VOID
			// Shift + Left/Right/Home/End : Selection of Text
			IF ! (e:Shift .AND. ( e:KeyCode==Keys.Left .OR. e:KeyCode==Keys.Right .OR. e:KeyCode==Keys.Home .OR. e:KeyCode==Keys.End ) )
				SWITCH e:KeyCode
				CASE Keys.Left
					SELF:_owner:CursorPos := SELF:PreviousEditingPosition()
				CASE Keys.Right
					SELF:_owner:CursorPos := SELF:NextEditingPosition()
				CASE Keys.Home
					SELF:_owner:CursorPos := SELF:NextEditingPosition(1)
				CASE Keys.End
					SELF:_owner:CursorPos := SELF:PreviousEditingPosition( SELF:_mask:Length )
				CASE Keys.Delete
				CASE Keys.Back
					IF e:KeyCode == Keys.Delete .OR. SELF:_owner:SelectionLength != 0
						// Delete selection
						SELF:DeleteSelection( SELF:_owner:SelectionStart+1, SELF:_owner:SelectionStart + 1 + SELF:_owner:SelectionLength )
					ELSE
						SELF:_owner:CursorPos := SELF:PreviousEditingPosition()
						SELF:DeleteChar( SELF:_owner:CursorPos, FALSE, TRUE )
					ENDIF
				END SWITCH
				e:Handled := TRUE
			ENDIF
		END METHOD

		METHOD OnKeyPress( e AS KeyPressEventArgs ) AS VOID

			SWITCH e:KeyChar
			CASE Keys.Oemcomma
			CASE Keys.OemPeriod
			CASE Keys.Decimal
				IF SELF:_owner:SelectionLength != 0
					// Delete Selection
                    NOP
				ENDIF
				// Move to the Decimal point position
				//SELF:_owner:CursorPos :=
			OTHERWISE
				IF SELF:_owner:SelectionLength != 0
					// Delete Selection
                    NOP
				ENDIF
				IF SELF:ProcessKeyChar( e )
					e:Handled := TRUE
				ENDIF
			END SWITCH
		END METHOD

		PROTECTED METHOD ProcessKeyChar( e AS KeyPressEventArgs ) AS LOGIC
			LOCAL processed AS LOGIC
			//
			processed := TRUE
			//
			VAR curPos := SELF:_owner:CursorPos
			VAR keyChar := e:KeyChar
			VAR maskChar := GetMask( curPos )
			IF SELF:IsValidChar( keyChar, maskChar, FALSE )
				SWITCH maskChar
				CASE c'!'; CASE c'U'; CASE c'u'
					keyChar := Char.ToUpper( keyChar )
				CASE c'W'
				CASE c'w'
					keyChar := Char.ToLower( keyChar )
				END SWITCH
				// Insert Mode ?
				IF System.Windows.Forms.Control.IsKeyLocked( Keys.Insert )
					SELF:PutChar( keyChar, curPos )
				ELSE
					SELF:InsertChar( keyChar, curPos )
				ENDIF
				SELF:UpdateOwnerText()
				// Move the Cursor/Caret
				curPos := SELF:NextEditingPosition( curPos+1 )
				IF ( (curPos > SELF:_mask:Length .OR. curPos==0 ).AND. SELF:_type != "N")
					SELF:_owner:Parent:SelectNextControl( SELF:_owner, TRUE, TRUE, TRUE, TRUE )
				ELSE
					SELF:_owner:CursorPos := curPos
				ENDIF
			ENDIF
			RETURN processed
		END METHOD

		/// <summary>
		/// Write char in a specific position
		/// </summary>
		/// <param name="newChar"></param>
		/// <param name="position"></param>
		METHOD PutChar( newChar AS CHAR, position AS INT ) AS VOID
			IF position > 0 .AND. position <= SELF:_textValue:Length .AND. newChar != c'\x0'
				VAR newText := StringBuilder{ SELF:_textValue }
				newText[ position - 1 ] := newChar
				SELF:_textValue := newText:ToString()
			ENDIF
		END METHOD

		METHOD GetChar( position AS INT, def := c'\x0' AS CHAR ) AS CHAR
			LOCAL gtChar AS CHAR
			gtChar := def
			IF position > 0 .AND. position <= SELF:_textValue:Length
				gtChar := SELF:_textValue[ position - 1 ]
			ENDIF
			RETURN gtChar
		END METHOD

		METHOD GetMask( position AS INT, def := c'\x0' AS CHAR ) AS CHAR
			LOCAL gtChar AS CHAR
			gtChar := def
			IF position > 0 .AND. position <= SELF:_mask:Length
				gtChar := SELF:_mask[ position - 1 ]
			ENDIF
			RETURN gtChar
		END METHOD

		METHOD InsertChar( newChar AS CHAR, position AS INT ) AS VOID
			VAR curPos := SELF:_owner:CursorPos
			VAR isNumeric := ( SELF:_type == "N" )
			//
			VAR maskChar := SELF:GetMask( curPos )
			// Editing position ??
			IF !SELF:IsMaskChar( maskChar )
				// No
				IF isNumeric .AND. SELF:GetChar( curPos ) == SELF:_decimalSeparator[0]
					// Move to the nextPos
					VAR moveTo := SELF:NextEditingPosition( curPos )
					IF moveTo == curPos
						RETURN
					ENDIF
					curPos := moveTo
					SELF:_owner:CursorPos := moveTo
				ENDIF
			ENDIF
			// Get the next editing position, after the current position
			VAR newPos := SELF:NextEditingPosition( curPos +1 )
			// And the current char
			VAR curChar := SELF:GetChar( curPos )
			// Put the new one here
			SELF:PutChar( newChar, curPos )
			// And move the rest of the text
			WHILE curPos > 0 .AND. newPos > 0

				IF Math.Abs( newPos - curPos ) != 1
					EXIT
				ENDIF

				IF isNumeric .AND. curPos <= SELF:_textValue:Length .AND. SELF:GetChar( curPos ) == SELF:_decimalSeparator[0]
					EXIT
				ENDIF

				// Future position
				IF SELF:IsValidChar( curChar, newPos, FALSE )
					// Ok, it works, next char
					VAR tmpChar := SELF:GetChar( newPos, c' ' )
					SELF:PutChar( curChar, newPos )
					curChar := tmpChar
				ELSE
					// No, Bye
					EXIT
				ENDIF
				// Move to the next char
				curPos := newPos
				newPos := SELF:NextEditingPosition( curPos +1 )
			END WHILE
		END METHOD

		METHOD DeleteChar( curPos AS INT, invertProcess AS LOGIC, updateOwner AS LOGIC ) AS VOID
			// Editing position ??
			IF !SELF:IsEditingPosition( curPos )
				RETURN
			ENDIF
			VAR newPos := 0
			IF !invertProcess
				newPos := SELF:NextEditingPosition( curPos + 1 )
			ELSE
				newPos := SELF:PreviousEditingPosition( curPos  - 1 )
			ENDIF
			WHILE curPos > 0 .AND. newPos > 0
				VAR tmpChar := SELF:GetChar( newPos )
				IF SELF:IsValidChar( tmpChar, curPos, TRUE )
					IF SELF:IsEditingPosition( newPos )
						SELF:PutChar( tmpChar, curPos )
						SELF:PutChar( c' ', newPos )
					ELSE
						SELF:PutChar( c' ', curPos )
					ENDIF
				ELSE
					EXIT
				ENDIF
				curPos := newPos
				IF !invertProcess
					newPos := SELF:NextEditingPosition( curPos + 1 )
				ELSE
					newPos := SELF:PreviousEditingPosition( curPos  - 1 )
				ENDIF
			END WHILE
			IF SELF:IsEditingPosition( curPos )
				SELF:PutChar( c' ', curPos )
			ENDIF
			IF updateOwner
				SELF:UpdateOwnerText()
			ENDIF
		END METHOD


		METHOD DeleteSelection( selStart AS INT, selEnd AS INT ) AS VOID

			IF selEnd <= selStart
				selEnd := selStart
			ELSE
				selEnd--
			ENDIF
			VAR isNumeric := ( SELF:_type == "N" )
			VAR invertDel := FALSE
			VAR decPos := SELF:_textValue:IndexOf( SELF:_decimalSeparator ) + 1

			IF isNumeric .AND. decPos > selStart .AND. decPos < selEnd
				SELF:DeleteSelection( selStart, decPos )
				SELF:DeleteSelection(decPos +1, selEnd+1)
			ELSE
				decPos := SELF:_textValue:IndexOf( SELF:_decimalSeparator, selEnd ) + 1
				invertDel := isNumeric .AND. decPos > 0
				IF !invertDel
					FOR VAR i := selStart TO selEnd
						IF SELF:IsEditingPosition( i )
							SELF:DeleteChar( i, FALSE, FALSE )
						ENDIF
					NEXT
				ELSE
					IF isNumeric
						VAR signPos := SELF:_textValue:IndexOf( c'-' ) + 1
						IF signPos > 0
							SELF:PutChar( c' ', signPos )
						ENDIF
					ENDIF
					FOR VAR i := selEnd DOWNTO selStart
						IF SELF:IsEditingPosition( i )
							SELF:DeleteChar( i, FALSE, FALSE )
						ENDIF
					NEXT
				ENDIF
				SELF:UpdateOwnerText()

				IF invertDel
					//SELF:_owner:CursorPos := SELF:PreviousEditingPosition( selStart + 1 )
                    NOP
				ENDIF
			ENDIF
		END METHOD


		/// <summary>
		/// Verify that the Char fits with the corresponding maskChar
		/// </summary>
		/// <param name="keyChar"></param>
		/// <param name="maskChar"></param>
		/// <returns></returns>
		PROTECTED METHOD IsValidChar( keyChar AS CHAR, maskPos AS INT, ignoreBlank AS LOGIC ) AS LOGIC
			IF maskPos <= SELF:_mask:Length
				VAR maskChar := SELF:GetMask( maskPos )
				RETURN IsValidChar( keyChar, maskChar, ignoreBlank )
			ENDIF
			RETURN FALSE
		END METHOD

		PROTECTED METHOD IsValidChar( keyChar AS CHAR, maskChar AS CHAR, ignoreBlank AS LOGIC  ) AS LOGIC
			VAR isOk := FALSE
			VAR isNumeric := ( SELF:_type == "N" )
			// When the expected type is Numeric, the "." in not in MaskChars
			IF isNumeric .AND. !SELF:IsMaskChar( maskChar )
				// It could be a dot
				IF !( maskChar == c'.' .OR. maskChar == c',' )
					RETURN FALSE
				ENDIF
			ENDIF
			// Any char or Upper (so anychar)
			IF maskChar == c'X' .OR. maskChar == c'!'
				RETURN TRUE
			ENDIF

			IF keyChar == c' ' .AND. ignoreBlank
				RETURN TRUE
			ENDIF

			// Should we define MaskChars as an Enum with Members named to there usage ?
			SWITCH maskChar
			CASE c'A'
			CASE c'U'
			CASE c'W'
				isOk := Char.IsLetter( keyChar )
			CASE c'N'
				isOk := Char.IsLetterOrDigit( keyChar )
			CASE c'9'
			CASE c'.'
			CASE c','
				isOk := Char.IsDigit( keyChar ) .OR. ( isNumeric .AND. ( keyChar == c'+' .OR. keyChar == c'-' ) )
			CASE c'#'
				isOk := Char.IsDigit( keyChar ) .OR. keyChar == c'+' .OR. keyChar == c'-'
			CASE c'Y'
				isOk := keyChar == c'Y' .OR. keyChar == c'y' .OR. keyChar == c'N' .OR. keyChar == c'n'
			CASE c'L'
				isOk := keyChar == c'Y' .OR. keyChar == c'y' .OR. keyChar == c'N' .OR. keyChar == c'n' .OR. keyChar == c'T' .OR. keyChar == c'F'
			END SWITCH


			RETURN isOk
		END METHOD

		PROPERTY DecimalPosition AS INT
			GET
				RETURN SELF:_mask:IndexOf( c'.')+1
			END GET
		END PROPERTY

		PROPERTY ThousandPosition AS INT
			GET
				RETURN SELF:_mask:IndexOf( c',')+1
			END GET
		END PROPERTY

		PROTECTED METHOD IsEditingPosition( curPos AS INT ) AS LOGIC
			IF curPos > 0 .AND. curPos <= SELF:_mask:Length
				VAR maskChar := SELF:GetMask( curPos )
				// Editing position ??
				RETURN SELF:IsMaskChar( maskChar )
			ENDIF
			RETURN FALSE
		END METHOD

		PROTECTED METHOD IsMaskChar( charToCheck AS CHAR ) AS LOGIC
			LOCAL found AS LOGIC
			found := FALSE
			FOR VAR i := 1 TO MaskProvider.MaskChar:Length
				IF MaskProvider.MaskChar[i-1] == charToCheck
					found := TRUE
					EXIT
				ENDIF
			NEXT
			RETURN found
		END METHOD

		INTERNAL METHOD UpdateValue() AS VOID
			SELF:_owner:_uValue := Unformat( SELF:TextValue, SELF:Mask, SELF:_type )
		END METHOD


		// Direct write, no Event
		PROTECTED METHOD UpdateOwnerText() AS VOID
			TRY
				VAR curPos := SELF:_owner:CursorPos
				//
				SetWindowText( SELF:_owner:Handle, SELF:TextValue )
				//
				SELF:_owner:CursorPos := curPos
			CATCH
				SELF:_owner:Text := SELF:TextValue
			END TRY
		END METHOD
		[DllImport("User32.dll", CharSet:=CharSet.Unicode, EntryPoint := "SetWindowTextW", SetLastError := TRUE)];
		STATIC METHOD SetWindowText(hwnd AS IntPtr, sText AS STRING) AS LOGIC

	END CLASS




END NAMESPACE // XSharp.VFP.UI