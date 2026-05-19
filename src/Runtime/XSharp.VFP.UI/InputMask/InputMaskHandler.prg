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

        PUBLIC PROPERTY RangeMin     AS USUAL AUTO
        PUBLIC PROPERTY RangeMax     AS USUAL AUTO
        PUBLIC PROPERTY RangeMessage AS STRING AUTO

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
                SELF:_pattern := NULL_OBJECT
                RETURN
            ENDIF

            SELF:_pattern := InputMaskPattern{}
            SELF:_pattern:Parse(maskString)
        END METHOD

        /// <summary>
        /// Handle keyboard input on KeyPress event.
        /// Validates printable characters and prevents invalid input before it appears.
        /// Special keys (Delete, Tab, Arrow) must be handled separately via HandleKeyDown.
        /// </summary>
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
            // Handle special control keys available in KeyPressEventArgs
            // ===================================================================

            // Backspace key (ASCII 8)
            IF keyChar == (CHAR)8
                SELF:HandleBackspace(textBox)
                e:Handled := TRUE
                RETURN
            ENDIF

            // Enter key - allow it
            IF keyChar == (CHAR)13
                e:Handled := FALSE
                RETURN
            ENDIF

            // ===================================================================
            // Handle regular character input
            // ===================================================================

            VAR position := textBox:SelectionStart

            // Skip to next data position if cursor is on a literal
            WHILE position < SELF:_pattern:Positions:Count .AND. ;
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
        /// Handle keyboard input on KeyDown event.
        /// Handles Delete, Tab, and Arrow key navigation within the mask.
        /// Wire this to the TextBox KeyDown event alongside HandleKeyPress on KeyPress.
        /// </summary>
        PUBLIC METHOD HandleKeyDown(e AS KeyEventArgs, textBox AS TextBox) AS VOID
            IF SELF:_pattern == NIL .OR. !SELF:_pattern:IsValid()
                RETURN
            ENDIF

            SWITCH e:KeyCode
                CASE Keys.Delete
                    SELF:HandleDelete(textBox)
                    e:Handled := TRUE

                CASE Keys.Tab
                    SELF:MoveToNextDataPosition(textBox)
                    e:Handled := FALSE  // Allow tab to propagate to next control

                CASE Keys.Left
                CASE Keys.Right
                CASE Keys.Up
                CASE Keys.Down
                CASE Keys.Home
                CASE Keys.End
                    SELF:HandleArrowKey(textBox, e:KeyCode)
                    e:Handled := FALSE
            END SWITCH

        END METHOD

        /// <summary>
        /// Handle text changed event - format the display after each edit
        /// </summary>
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
                        WHILE oldSelection < SELF:_pattern:Positions:Count .AND. ;
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
        /// Handle backspace key - delete character before cursor
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
                    VAR endS := startS + textBox:SelectionLength

                    VAR text := textBox:Text
                    VAR before := text:Substring(0, startS)
                    VAR after := text:Substring(endS)

                    textBox:Text := before + after
                    textBox:SelectionStart := startS
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
                            VAR sb := System.Text.StringBuilder{text}
                            sb[pos] := Char.Parse("_")

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
        /// Handle delete key - delete character at cursor
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
                    WHILE pos < SELF:_pattern:Positions:Count .AND. ;
                          SELF:_pattern:IsLiteralPosition(pos)
                        pos := pos + 1
                    END WHILE

                    IF pos < text:Length
                        VAR sb := System.Text.StringBuilder{text}
                        sb[pos] := Char.Parse("_")

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
        /// Handle arrow key navigation
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
                        WHILE pos < SELF:_pattern:Positions:Count .AND. ;
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
        /// Move to next data position
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
        /// Move to previous data position
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
        /// Initialize TextBox with mask - show placeholder
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

        /// <summary>
        /// Validate the current value against RangeMin / RangeMax.
        /// dateFormatPattern is the .NET format string used for date-typed bounds (e.g. "MM/dd/yyyy").
        /// Returns TRUE when no bounds are set, the field is empty, or the value is in range.
        /// Shows a warning and returns FALSE when the value is out of range.
        /// </summary>
        PUBLIC METHOD CheckRange(textBox AS TextBox, dateFormatPattern AS STRING) AS LOGIC
            IF IsNil(SELF:RangeMin) .AND. IsNil(SELF:RangeMax)
                RETURN TRUE
            ENDIF
            VAR strVal := SELF:GetDataValue(textBox:Text):Trim()
            IF String.IsNullOrEmpty(strVal)
                RETURN TRUE  // empty field passes — required-field logic belongs in VALID
            ENDIF
            // ── Date range ───────────────────────────────────────────────────────
            IF (!IsNil(SELF:RangeMin) .AND. IsDate(SELF:RangeMin)) .OR. ;
               (!IsNil(SELF:RangeMax) .AND. IsDate(SELF:RangeMax))
                RETURN SELF:CheckDateRange(strVal, dateFormatPattern)
            ENDIF
            // ── Numeric range ────────────────────────────────────────────────────
            LOCAL d AS Decimal
            IF !Decimal.TryParse(strVal, System.Globalization.NumberStyles.Any, ;
                                 System.Globalization.CultureInfo.InvariantCulture, OUT d)
                RETURN TRUE  // non-numeric — let VALID handle type errors
            ENDIF
            LOCAL ok AS LOGIC
            ok := TRUE
            IF !IsNil(SELF:RangeMin)
                TRY
                    ok := ok .AND. d >= (Decimal)(REAL8) SELF:RangeMin
                CATCH
                    NOP
                END TRY
            ENDIF
            IF !IsNil(SELF:RangeMax)
                TRY
                    ok := ok .AND. d <= (Decimal)(REAL8) SELF:RangeMax
                CATCH
                    NOP
                END TRY
            ENDIF
            IF !ok
                LOCAL msg AS STRING
                msg := IIF(!String.IsNullOrEmpty(SELF:RangeMessage), SELF:RangeMessage, ;
                           "Value must be in range " + SELF:RangeMin:ToString() + " to " + SELF:RangeMax:ToString())
                MessageBox.Show(msg, "", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            ENDIF
            RETURN ok
        END METHOD

        // Date-specific range check. Parses strVal using dateFormatPattern, with a
        // 2-digit-year fallback and a lenient DateTime.TryParse last resort.
        PRIVATE METHOD CheckDateRange(strVal AS STRING, dateFormatPattern AS STRING) AS LOGIC
            LOCAL dt AS DateTime
            LOCAL culture := System.Globalization.CultureInfo.InvariantCulture AS System.Globalization.CultureInfo
            LOCAL styles  := System.Globalization.DateTimeStyles.None AS System.Globalization.DateTimeStyles
            IF !DateTime.TryParseExact(strVal, dateFormatPattern, culture, styles, OUT dt)
                VAR fmt2 := dateFormatPattern:Replace("yyyy", "yy")
                IF !DateTime.TryParseExact(strVal, fmt2, culture, styles, OUT dt)
                    IF !DateTime.TryParse(strVal, OUT dt)
                        RETURN TRUE  // unparseable — let VALID handle it
                    ENDIF
                ENDIF
            ENDIF
            LOCAL ok AS LOGIC
            ok := TRUE
            IF !IsNil(SELF:RangeMin) .AND. IsDate(SELF:RangeMin)
                TRY
                    ok := ok .AND. dt >= (DateTime)(DATE) SELF:RangeMin
                CATCH
                    NOP
                END TRY
            ENDIF
            IF !IsNil(SELF:RangeMax) .AND. IsDate(SELF:RangeMax)
                TRY
                    ok := ok .AND. dt <= (DateTime)(DATE) SELF:RangeMax
                CATCH
                    NOP
                END TRY
            ENDIF
            IF !ok
                LOCAL msg AS STRING
                IF !String.IsNullOrEmpty(SELF:RangeMessage)
                    msg := SELF:RangeMessage
                ELSEIF !IsNil(SELF:RangeMin) .AND. IsDate(SELF:RangeMin) .AND. ;
                       !IsNil(SELF:RangeMax) .AND. IsDate(SELF:RangeMax)
                    msg := "Date must be in range " + DToC((DATE) SELF:RangeMin) + " to " + DToC((DATE) SELF:RangeMax)
                ELSEIF !IsNil(SELF:RangeMin) .AND. IsDate(SELF:RangeMin)
                    msg := "Date must be on or after " + DToC((DATE) SELF:RangeMin)
                ELSE
                    msg := "Date must be on or before " + DToC((DATE) SELF:RangeMax)
                ENDIF
                MessageBox.Show(msg, "", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            ENDIF
            RETURN ok
        END METHOD

        /// <summary>
        /// Validate that the displayed text is a real calendar date.
        /// dateFormatPattern is a .NET DateTime format string (e.g. "MM/dd/yyyy").
        /// Returns TRUE when the field is empty, partially filled, or the date parses correctly.
        /// Shows a warning and returns FALSE when the date is syntactically complete but invalid.
        /// </summary>
        PUBLIC METHOD CheckDateSemantics(textBox AS TextBox, dateFormatPattern AS STRING) AS LOGIC
            VAR displayVal := textBox:Text:Trim()
            IF String.IsNullOrEmpty(displayVal) .OR. displayVal:Contains("_")
                RETURN TRUE
            ENDIF
            VAR culture := System.Globalization.CultureInfo.InvariantCulture
            VAR styles  := System.Globalization.DateTimeStyles.None
            LOCAL dt AS DateTime
            IF DateTime.TryParseExact(displayVal, dateFormatPattern, culture, styles, OUT dt)
                RETURN TRUE
            ENDIF
            // 2-digit year fallback
            VAR fmt2 := dateFormatPattern:Replace("yyyy", "yy")
            IF DateTime.TryParseExact(displayVal, fmt2, culture, styles, OUT dt)
                RETURN TRUE
            ENDIF
            MessageBox.Show("** Invalid date **", "", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            RETURN FALSE
        END METHOD

    END CLASS

END NAMESPACE
