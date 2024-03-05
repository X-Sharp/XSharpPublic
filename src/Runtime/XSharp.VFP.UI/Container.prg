// VFPContainer.prg
USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel


BEGIN NAMESPACE XSharp.VFP.UI


	// TODO Check IDynamicProperties -> XSharp.RT

	/// <summary>
	/// The VFPContainer class.
	/// </summary>
	PARTIAL CLASS Container INHERIT System.Windows.Forms.UserControl



         #include ".\XSharp\VFPProperties.xh"
		 #include ".\Headers\ControlProperties.xh"

		 #include ".\Headers\Tooltips.xh"

			// This is a fake property, just here to ease Code Generation
			//PROPERTY AutoScaleMode AS System.Windows.Forms.AutoScaleMode AUTO

		CONSTRUCTOR( ) STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, true)
            SELF:BackColor := Color.Transparent
            SELF:Size := Size{75,75}
			RETURN


        PROPERTY BorderColor AS System.Drawing.Color AUTO

		PRIVATE _backStyle := 1 AS INT
		[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
		PROPERTY BackStyle AS INT
			GET
				RETURN _backStyle
			END GET
			SET
				_backStyle := VALUE
				IF (VALUE == 0 )
					// Todo No Transparency on Panel
					SELF:BackColor := System.Drawing.Color.Transparent
				ENDIF
			END SET
		END PROPERTY



		// Todo : BorderColor Currently ... do nothing
		//PROPERTY BorderColor AS System.Drawing.Color AUTO

		/*
		All this is still experimental.....
		// Capture the Parent call
		// With the Attribute, during compilation the generated code will receive the name of the Method in which this code is called.
		// So, we will check if it is in form of <objectName>_<Event>
		// So first, extract the objectName, search it in the Properties list
		NEW PROPERTY Parent[ [System.Runtime.CompilerServices.CallerMemberName]memberName := "" AS STRING] AS OBJECT
		GET
		// We are trying to check from which Method the Parent property was called.
		// If it is in form of <objectName>_<Event>, it might be some VFPXPorter generated method that contains the original code
		// In VFP, the code was "contained" by the control, where in WinForms it is "contained" by the Container (Panel/Form/...)

		// Normal behaviour
		IF !memberName:Contains("_")
		RETURN Super:Parent
		ELSE
		VAR underScore := memberName:IndexOf("_")
		if memberName:Length == underScore +1
		RETURN Super:Parent
		ENDIF
		// Now, check who is calling the Parent.
		//
		VAR objectName := memberName:SubString( 0, underScore )
		VAR eventName := memberName:SubString(underScore+1)
		// Do we already have a list of Controls here ?
		if SELF:_ControlDict == NULL
		SELF:_ControlDict := HashSet<String>{}
		VAR topParent := ScrollableControl{}:GetType()
		FOREACH VAR fieldInfo IN SELF:GetType():GetFields( System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic )
		// We have the Field
		// IsAssignable check if the parameter is derived either directly or indirectly from the current instance.
		// So we check if the FieldType is not derived from ScrollableControl, which is the Parent of Panel, Form, ... but not Control

		if !topParent:IsAssignableFrom( fieldInfo:FieldType )
		// TODO !! Warning we are Case-Insensitive here
		_ControlDict:Add( fieldInfo:Name:ToLower() )
		ENDIF
		NEXT
		ENDIF
		ENDIF
		RETURN NULL
		END GET
		END PROPERTY


		// This Dictionary contains the Controls
		PRIVATE _ControlDict AS HashSet<String>

		*/


	END CLASS
END NAMESPACE // XSharp.VFP.UI
