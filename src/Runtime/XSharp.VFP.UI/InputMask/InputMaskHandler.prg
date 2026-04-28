USING System
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// Coordinates InputMask validation, formatting, and event handling
    /// This is the main orchestrator that handles keyboard input with masks
    /// </summary>
    PUBLIC CLASS InputMaskHandler

        PRIVATE _pattern AS InputMaskPattern
        PRIVATE _validator AS InputMaskValidator
        PRIVATE _formatter AS InputMaskFormatter
        PRIVATE _isUpdatingMask AS LOGIC  // Prevent recursion when updating display

        /// <summary>
        /// The parsed InputMask pattern
        /// </summary>
        PUBLIC PROPERTY Pattern AS InputMaskPattern
            GET
                RETURN SELF:_pattern
            END GET
            SET
                SELF:_pattern := VALUE
            END SET
        END PROPERTY

        /// <summary>
        /// Constructor
        /// </summary>
        PUBLIC CONSTRUCTOR()
            SELF:_validator := InputMaskValidator{}
            SELF:_formatter := InputMaskFormatter{}
            SELF:_isUpdatingMask := FALSE
        END CONSTRUCTOR

        /// <summary>
        /// Set pattern and parse mask string
        /// Example: SetPattern("(999) 999-9999")
        /// </summary>
        PUBLIC METHOD SetPattern(maskString AS STRING) AS VOID
            IF String.IsNullOrEmpty(maskString)
                SELF:_pattern := NIL
                RETURN
            ENDIF

            SELF:_pattern := InputMaskPattern{}
            SELF:_pattern:Parse(maskString)
        END METHOD

        /// <summary>
        /// Handle keyboard input on KeyPress event.
        /// Validates characters and prevents invalid input before it appears.
        /// </summary>
        /// <param name="e">KeyPress event args; sets <c>e.Handled = TRUE</c> to suppress invalid input.</param>
        /// <param name="textBox">The text box being edited.
        /// <c>textBox.SelectionStart</c> is a 0-based character offset, consistent with
        /// the 0-based <see cref="InputMaskPattern.Positions"/> index.</param>
        PUBLIC METHOD HandleKeyPress(e AS KeyPressEventArgs, textBox AS TextBox) AS VOID
            IF SELF:_pattern == NIL .OR. !SELF:_pattern:IsValid()
                RETURN
            ENDIF

            // Prevent recursion if we're already updating
            IF SELF:_isUpdatingMask
                e:Handled := TRUE
                RETURN
            ENDIF

            VAR keyChar := e:KeyChar

            // ===================================================================
            // Handle special control keys
            // ===================================================================

            // Backspace key (ASCII 8)
            IF keyChar == (CHAR)8
                SELF:HandleBackspace(textBox)
                e:Handled := TRUE
                RETURN
            ENDIF

            // Delete key
            IF e:KeyCode == Keys.Delete
                SELF:HandleDelete(textBox)
                e:Handled := TRUE
                RETURN
            ENDIF

            // Tab key - move to next data position
            IF e:KeyCode == Keys.Tab
                SELF:MoveToNextDataPosition(textBox)
                e:Handled := FALSE  // Allow tab to propagate
                RETURN
            ENDIF

            // Enter key - allow it
            IF keyChar == (CHAR)13
                e:Handled := FALSE
                RETURN
            ENDIF

            // Arrow keys - allow navigation
            IF e:KeyCode == Keys.Left .OR. e:KeyCode == Keys.Right .OR. _
               e:KeyCode == Keys.Up .OR. e:KeyCode == Keys.Down .OR. _
               e:KeyCode == Keys.Home .OR. e:KeyCode == Keys.End
                SELF:HandleArrowKey(textBox, e:KeyCode)
                e:Handled := FALSE
                RETURN
            ENDIF

            // ===================================================================
            // Handle regular character input
            // ===================================================================

            VAR position := textBox:SelectionStart

            // Skip to next data position if cursor is on a literal
            WHILE position < SELF:_pattern:Positions:Count .AND. _
                  (SELF:_pattern:IsLiteralPosition(position) .OR. SELF:_pattern:IsModifierPosition(position))
                position := position + 1
            END WHILE

            // Check if we're within the mask bounds
            IF position >= SELF:_pattern:Positions:Count
                // Beyond the mask - don't allow input
                e:Handled := TRUE
                System.Media.SystemSounds.Beep:Play()
                RETURN
            ENDIF

            // Validate the character for this position
            IF SELF:_validator:IsValidCharacter(SELF:_pattern, position, keyChar:ToString())
                // Valid character - allow it, we'll format in TextChanged
                e:Handled := FALSE
                RETURN
            ELSE
                // Invalid character - reject it
                System.Media.SystemSounds.Beep:Play()
                e:Handled := TRUE
                RETURN
            ENDIF

        END METHOD

        /// <summary>
        /// Handle text changed event - format the display after each edit.
        /// </summary>
        /// <param name="textBox">The text box whose content changed.
        /// Cursor restoration uses 0-based <c>SelectionStart</c> offsets.</param>
        PUBLIC METHOD HandleTextChanged(textBox AS TextBox) AS VOID
            IF SELF:_pattern == NIL .OR. !SELF:_pattern:IsValid()
                RETURN
            ENDIF

            IF SELF:_isUpdatingMask
                RETURN  // Prevent recursion
            ENDIF

            SELF:_isUpdatingMask := TRUE
            TRY
                // Get clean data and reformat
                VAR cleanData := SELF:_formatter:ExtractDataValue(SELF:_pattern, textBox:Text)
                VAR formatted := SELF:_formatter:FormatValue(SELF:_pattern, cleanData)

                // Only update if text changed
                IF formatted != textBox:Text
                    VAR oldSelection := textBox:SelectionStart
                    textBox:Text := formatted

                    // Restore cursor position, but move past literals
                    IF oldSelection < formatted:Length
                        // Move cursor to next data position if on literal
                        WHILE oldSelection < SELF:_pattern:Positions:Count .AND. _
                              SELF:_pattern:IsLiteralPosition(oldSelection)
                            oldSelection := oldSelection + 1
                        END WHILE
                        textBox:SelectionStart := oldSelection
                    ENDIF
                ENDIF
            FINALLY
                SELF:_isUpdatingMask := FALSE
            END TRY
        END METHOD

        /// <summary>
        /// Handle backspace key - delete character before cursor.
        /// After the operation <c>textBox.SelectionStart</c> is restored to the
        /// 0-based position of the cleared slot.
        /// </summary>
        PUBLIC METHOD HandleBackspace(textBox AS TextBox) AS VOID
            IF SELF:_isUpdatingMask
                RETURN
            ENDIF

            SELF:_isUpdatingMask := TRUE
            TRY
                IF textBox:SelectionLength > 0
                    // Delete selected text
                    VAR startS := textBox:SelectionStart
                    VAR endS := start + textBox:SelectionLength

                    VAR text := textBox:Text
                    VAR before := Text:Substring(0, startS)
                    VAR after := Text:Substring(endS)

                    textBox:Text := before + after
                    textBox:SelectionStart := start
                ELSE
                    // Delete character before cursor
                    VAR pos := textBox:SelectionStart
                    IF pos > 0
                        // Skip back over literals
                        pos := pos - 1
                        WHILE pos > 0 .AND. SELF:_pattern:IsLiteralPosition(pos)
                            pos := pos - 1
                        END WHILE

                        VAR text := textBox:Text
                        IF pos < text:Length
                            VAR sb := StringBuilder{text}
                            sb[pos] := '_'

                            // Rebuild formatted value
                            VAR rebuilt := SELF:_formatter:RebuildFormattedValue(SELF:_pattern, sb:ToString())
                            textBox:Text := rebuilt
                            textBox:SelectionStart := pos
                        ENDIF
                    ENDIF
                ENDIF
            FINALLY
                SELF:_isUpdatingMask := FALSE
            END TRY
        END METHOD

        /// <summary>
        /// Handle delete key - delete character at cursor.
        /// After the operation <c>textBox.SelectionStart</c> is restored to the
        /// 0-based position of the cleared slot.
        /// </summary>
        PUBLIC METHOD HandleDelete(textBox AS TextBox) AS VOID
            IF SELF:_isUpdatingMask
                RETURN
            ENDIF

            SELF:_isUpdatingMask := TRUE
            TRY
                IF textBox:SelectionLength > 0
                    // Delete selected text
                    VAR start := textBox:SelectionStart
                    VAR text := textBox:Text
                    VAR before := text:Substring(0, start)
                    VAR after := text:Substring(start + textBox:SelectionLength)

                    textBox:Text := before + after
                    textBox:SelectionStart := start
                ELSE
                    // Delete character at cursor
                    VAR pos := textBox:SelectionStart
                    VAR text := textBox:Text

                    // Skip over literals to find next data position
                    WHILE pos < SELF:_pattern:Positions:Count .AND. _
                          SELF:_pattern:IsLiteralPosition(pos)
                        pos := pos + 1
                    END WHILE

                    IF pos < text:Length
                        VAR sb := StringBuilder{text}
                        sb[pos] := '_'

                        // Rebuild formatted value
                        VAR rebuilt := SELF:_formatter:RebuildFormattedValue(SELF:_pattern, sb:ToString())
                        textBox:Text := rebuilt
                        textBox:SelectionStart := pos
                    ENDIF
                ENDIF
            FINALLY
                SELF:_isUpdatingMask := FALSE
            END TRY
        END METHOD

        /// <summary>
        /// Handle arrow key navigation.
        /// Updates <c>textBox.SelectionStart</c> using 0-based offsets,
        /// skipping literal positions automatically.
        /// </summary>
        PUBLIC METHOD HandleArrowKey(textBox AS TextBox, keyCode AS Keys) AS VOID
            VAR pos := textBox:SelectionStart

            SWITCH keyCode
                CASE Keys.Left
                    // Move left, skip over literals
                    IF pos > 0
                        pos := pos - 1
                        WHILE pos > 0 .AND. SELF:_pattern:IsLiteralPosition(pos)
                            pos := pos - 1
                        END WHILE
                        textBox:SelectionStart := pos
                    ENDIF

                CASE Keys.Right
                    // Move right, skip over literals
                    IF pos < textBox:Text:Length
                        pos := pos + 1
                        WHILE pos < SELF:_pattern:Positions:Count .AND. _
                              SELF:_pattern:IsLiteralPosition(pos)
                            pos := pos + 1
                        END WHILE
                        IF pos <= textBox:Text:Length
                            textBox:SelectionStart := pos
                        ENDIF
                    ENDIF

                CASE Keys.Home
                    // Move to first data position
                    VAR firstData := SELF:_pattern:GetFirstDataPosition()
                    textBox:SelectionStart := firstData

                CASE Keys.End
                    // Move to end
                    textBox:SelectionStart := textBox:Text:Length
            END SWITCH
        END METHOD

        /// <summary>
        /// Move to next data position.
        /// Sets <c>textBox.SelectionStart</c> to the 0-based index returned by
        /// <see cref="InputMaskPattern.GetNextDataPosition"/>.
        /// </summary>
        PUBLIC METHOD MoveToNextDataPosition(textBox AS TextBox) AS VOID
            VAR pos := textBox:SelectionStart
            VAR nextPos := SELF:_pattern:GetNextDataPosition(pos)

            IF nextPos >= 0
                textBox:SelectionStart := nextPos
            ELSE
                // No more data positions, move to end
                textBox:SelectionStart := textBox:Text:Length
            ENDIF
        END METHOD

        /// <summary>
        /// Move to previous data position.
        /// Sets <c>textBox.SelectionStart</c> to the 0-based index returned by
        /// <see cref="InputMaskPattern.GetPreviousDataPosition"/>.
        /// </summary>
        PUBLIC METHOD MoveToPreviousDataPosition(textBox AS TextBox) AS VOID
            VAR pos := textBox:SelectionStart
            VAR prevPos := SELF:_pattern:GetPreviousDataPosition(pos)

            IF prevPos >= 0
                textBox:SelectionStart := prevPos
            ELSE
                // No previous position, move to first
                VAR firstData := SELF:_pattern:GetFirstDataPosition()
                textBox:SelectionStart := firstData
            ENDIF
        END METHOD

        /// <summary>
        /// Initialize TextBox with mask - show placeholder.
        /// Sets <c>textBox.SelectionStart</c> to the 0-based first data position.
        /// </summary>
        PUBLIC METHOD InitializeTextBox(textBox AS TextBox) AS VOID
            IF SELF:_pattern == NIL .OR. !SELF:_pattern:IsValid()
                RETURN
            ENDIF

            IF String.IsNullOrEmpty(textBox:Text)
                textBox:Text := SELF:_pattern:PlaceholderDisplay
                textBox:SelectionStart := SELF:_pattern:GetFirstDataPosition()
            ENDIF
        END METHOD

        /// <summary>
        /// Get the clean data value (without mask formatting)
        /// </summary>
        PUBLIC METHOD GetDataValue(maskedValue AS STRING) AS STRING
            IF SELF:_pattern == NIL
                RETURN maskedValue
            ENDIF
            RETURN SELF:_formatter:ExtractDataValue(SELF:_pattern, maskedValue)
        END METHOD

    END CLASS

END NAMESPACE
