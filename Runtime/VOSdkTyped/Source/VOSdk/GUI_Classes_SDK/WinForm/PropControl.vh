// PropControl.vh
	
	PROTECTED oProperties AS VOControlProperties
	PROPERTY ControlProperties AS VOControlProperties GET oProperties
	PROPERTY Control AS XSharp.VO.Control 
		GET 
			IF oProperties=NULL
				RETURN NULL
			ELSE
				RETURN oProperties:Control
			ENDIF
		END GET
	END PROPERTY

	CONSTRUCTOR() STRICT
		SUPER()
		SELF:SetVisualStyle()
		IF SELF is IVOControlInitialize
			LOCAL oIni AS IVOControlInitialize
			oIni := (IVOControlInitialize) (OBJECT) SELF
			oIni:Initialize()
		ENDIF

	METHOD SetOwner(Owner AS XSharp.VO.Control) AS VOID 
		oProperties              := VOControlProperties{SELF, Owner}		
		oProperties:StyleChanged += SetVisualStyle	
		IF SELF:TabStop
			Owner:SetStyle(WS_TABSTOP, TRUE)
		ENDIF

	PUBLIC METHOD RevertEventPosition(p AS System.Drawing.Point) AS System.Drawing.Point
		RETURN SELF:oProperties:RevertEventPosition(p)

	VIRTUAL PROTECT METHOD WndProc(m REF Message) AS VOID
		SUPER:WndProc(REF m)
      SELF:oProperties:Dispatch(REF m)
      RETURN
        