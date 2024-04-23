// VFPTextBox.prg
USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFPTextBox class.
	/// </summary>
	PARTIAL CLASS TextBox INHERIT System.Windows.Forms.TextBox

		// Common properties that all VFP Objects support
		#include "Headers\VFPObject.xh"

		#include "XSharp\VFPProperties.xh"

		//
		PRIVATE _format AS STRING
		PROPERTY Format AS STRING
			GET
				IF SELF:DesignMode
					RETURN _format
				ELSE
					IF SELF:maskCheck != NULL
						RETURN SELF:maskCheck:Format
					ENDIF
				ENDIF
				RETURN _format
			END GET
			SET
				_format := Upper( VALUE )
				IF !SELF:DesignMode
					IF SELF:maskCheck == NULL
						IF SELF:_format != NULL
							SELF:maskCheck := MaskProvider{ SELF, SELF:_inputMask, SELF:_format, SELF:_valueType }
						ENDIF
					ELSE
						IF SELF:_inputMask == NULL .AND. SELF:_format == NULL
							SELF:maskCheck := NULL
						ELSE
							VAR savedValue := SELF:Value
							SELF:maskCheck:Format := SELF:_format
							SELF:maskCheck:Value := savedValue
						ENDIF
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		// Todo
		PRIVATE _inputMask AS STRING
		PROPERTY InputMask AS STRING
			GET
				IF SELF:DesignMode
					RETURN _inputMask
				ELSE
					IF SELF:maskCheck != NULL
						RETURN SELF:maskCheck:Mask
					ENDIF
				ENDIF
				RETURN _inputMask
			END GET

			SET
				_inputMask := VALUE
				IF !SELF:DesignMode
					VAR savedValue := SELF:Value
					IF SELF:maskCheck == NULL .AND. ( !String.IsNullOrEmpty(SELF:_inputMask) .OR. !String.IsNullOrEmpty(SELF:_format) )
						SELF:maskCheck := MaskProvider{ SELF, SELF:_inputMask, SELF:_format, SELF:_valueType }
						IF !IsNil( savedValue )
							SELF:maskCheck:Value := savedValue
						ENDIF
					ELSE
						IF String.IsNullOrEmpty(SELF:_inputMask) .AND. String.IsNullOrEmpty(SELF:_format)
							SELF:maskCheck := NULL
						ELSE
							SELF:maskCheck := MaskProvider{ SELF, SELF:_inputMask, SELF:_format, SELF:_valueType }
							IF !IsNil( savedValue )
								SELF:maskCheck:Value := savedValue
							ENDIF
						ENDIF
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		INTERNAL PROPERTY maskCheck AS MaskProvider AUTO




		CONSTRUCTOR(  ) STRICT
			SUPER()

			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent
			// Default work
			SELF:_format := NULL
			SELF:_inputMask := NULL
            SELF:_valueType := "C"
            SELF:Size := Size{100,21}


		OVERRIDE PROTECTED METHOD OnKeyDown( e AS KeyEventArgs ) AS VOID
			IF SELF:maskCheck == NULL
				SUPER:OnKeyDown(e)
				RETURN
			ENDIF
			//
			SELF:maskCheck:OnKeyDown(e)

		OVERRIDE PROTECTED METHOD OnKeyUp( e AS KeyEventArgs ) AS VOID
			IF SELF:maskCheck == NULL
				SUPER:OnKeyUp(e)
				RETURN
			ENDIF
			//

        OVERRIDE PROTECTED METHOD OnKeyPress( e AS KeyPressEventArgs) AS VOID
            // First call our VPF handling
            SELF:OnVFPKeyPress( SELF, e )
            //
            // TODO : Don't forget that if NODEFAULT has been calledd previously we should mark the Event has handled and return
            // .....
            //
			IF SELF:maskCheck == NULL
                SUPER:OnKeyPress(e)
            ELSE
				IF !SELF:ReadOnly
					// First position ?
					IF SELF:CursorPos == 1
						SELF:maskCheck:MoveToFirstEditingPosition()
					ENDIF
					// we have a valid picture, apply the mask
					SELF:maskCheck:OnKeyPress( e )
				ELSE
					e:Handled := TRUE
				ENDIF
            ENDIF
            // How do we ignore a KeyPress in VFP ???
            SELF:OnVFPKeyPress( SELF, e )


		OVERRIDE PROTECTED METHOD OnTextChanged ( e AS EventArgs) AS VOID
			// TRUE is changed by the user
			IF SELF:Modified
				// VFP fallback Code ??
                SELF:OnVFPInteractiveChange( SELF, e )
			ENDIF
			//
			RETURN

		PROPERTY Alignment AS INT
			GET
				RETURN (INT)SELF:TextAlign
			END GET
			SET
				IF VALUE >= 0 .AND. VALUE < 3
					SELF:TextAlign := (HorizontalAlignment) VALUE
				ENDIF
			END SET
		END PROPERTY

		// RvdH Value in VFP may have ANY type!
		// Therefore store the value in its own slot
		INTERNAL _uValue AS USUAL
		PRIVATE _valueType AS STRING

			[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
				[EditorBrowsable(EditorBrowsableState.Never)];
				[Bindable(FALSE)];
				[Browsable(FALSE)];
		PROPERTY Value AS USUAL
			GET
				RETURN _uValue
			END GET
			SET
				IF !IsNil( VALUE )
					// Set uValue first, as setting Text will call TextChanged
					_uValue := VALUE
					_valueType := ValType(_uValue )
					IF SELF:maskCheck != NULL
						SELF:maskCheck:Value := _uValue
					ELSE
						SELF:Text := ((OBJECT)VALUE):ToString()
					ENDIF
				ENDIF
			END SET
		END PROPERTY

				[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
			[EditorBrowsable(EditorBrowsableState.Never)];
			[Bindable(FALSE)];
			[Browsable(FALSE)];
		PROPERTY SelStart AS INT GET SELF:SelectionStart SET SELF:SelectionStart := VALUE

			// Don't Care
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
			[EditorBrowsable(EditorBrowsableState.Never)];
			[Bindable(FALSE)];
			[Browsable(FALSE)];
		PROPERTY Style AS INT AUTO

			// TODO
		PROPERTY SelectOnEntry AS LOGIC AUTO

			// This will include a call to VFPAfterWhen() if When() is True, and before VFPGotFocus()
			// It has to be set BEFORE the #include "TextControlProperties.xh"
		#define VFPAfterWhenCall
		PRIVATE METHOD VFPAfterWhen( sender AS OBJECT, e AS System.EventArgs) AS VOID
			//
			IF SELF:SelectOnEntry
				SELF:SelectAll()
			ENDIF
			RETURN

		/// <summary>
		/// Position of the Cursor in the edit zone of the TextBox. Value is One-Based
		/// </summary>
		/// <value></value>
				[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
			[EditorBrowsable(EditorBrowsableState.Never)];
			[Bindable(FALSE)];
			[Browsable(FALSE)];
		PROPERTY CursorPos AS LONGINT
			GET
				LOCAL liStart AS LONGINT
				liStart := SELF:SelectionStart+1
				RETURN liStart
			END GET

			SET
				IF (VALUE > 0)
					SELF:SelectionStart := VALUE-1
				ELSE
					SELF:SelectionStart := 0
				ENDIF
				SELF:SelectionLength := 0
			END SET

		END PROPERTY

		OVERRIDE PROTECTED METHOD OnLostFocus( e AS EventArgs ) AS VOID
			IF SELF:maskCheck != NULL
				SELF:maskCheck:UpdateValue()
			ELSE
				SELF:_uValue := SELF:Text
			ENDIF
			SUPER:OnLostFocus( e )
		END METHOD


		#include ".\Headers\TextControlProperties.xh"

		#include ".\Headers\FontProperties.xh"

		#include ".\Headers\ControlSource.xh"

	END CLASS

END NAMESPACE
