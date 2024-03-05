

		PROPERTY BindingDefinition AS System.Collections.Generic.Dictionary<Control,STRING> AUTO

		METHOD SetBinding( sender AS Control, bindingInfo AS STRING ) AS VOID
			//
			IF SELF:BindingDefinition == NULL
				SELF:BindingDefinition := System.Collections.Generic.Dictionary<Control,STRING>{}
			ENDIF
			SELF:BindingDefinition:Add( sender, bindingInfo )
