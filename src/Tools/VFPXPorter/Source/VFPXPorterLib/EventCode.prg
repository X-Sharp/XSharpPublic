USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
	/// Store the Code attached to an Event
	/// and give some informations about the EventHandler
	/// </summary>
	CLASS EventCode INHERIT BlockCode

		// These are .NET EventName and EventHandler
		PROPERTY EventName AS STRING AUTO
		PROPERTY EventHandler AS STRING AUTO
		PROPERTY EventHandlerPrototype AS STRING AUTO
		/// <summary>
		/// Indicate if this EventCode belongs to a control inside a Container
		/// </summary>
		/// <value></value>
		PROPERTY IsChild AS LOGIC AUTO
		/// <summary>
		/// The Event Type : System.EventHandler, ...
		/// </summary>
		/// <value></value>
        PROPERTY EventType AS STRING AUTO

		// Info about the Owner of this EventHandler, usually (!?) a control
		PROPERTY Owner AS ItemCode AUTO

		PROPERTY Help AS STRING AUTO

		PROPERTY IsUserDef AS LOGIC AUTO

		/// <summary>
		/// Indicate if this Event is handled in EventRules.json
		/// </summary>
		/// <value></value>
		PROPERTY IsHandled AS LOGIC AUTO

		// XmlSerialize Constructor
		CONSTRUCTOR(  )

			// Copy Constructor
		CONSTRUCTOR( evtCde AS EventCode )
			SUPER( evtCde )
			SELF:EventName := evtCde:EventName
			SELF:EventHandler := evtCde:EventHandler
			SELF:EventHandlerPrototype := evtCde:EventHandlerPrototype
            SELF:EventType := evtCde:EventType
			SELF:IsChild := evtCde:IsChild
			SELF:IsUserDef := evtCde:IsUserDef
			SELF:IsHandled := evtCde:IsHandled
			SELF:Help := evtCde:Help

			// Standard Constructor
		CONSTRUCTOR(  name AS STRING, def AS STRING )
			SUPER(  name, def )

			// Copy Constructor
		CONSTRUCTOR( code AS BlockCode )
			SUPER( code )
			SELF:EventName := ""
			SELF:EventHandler := ""
			SELF:EventHandlerPrototype := ""
            SELF:EventType := ""
			SELF:IsChild := FALSE
			SELF:IsUserDef := FALSE
			SELF:IsHandled := FALSE

			// Copy Constructor
		CONSTRUCTOR( code AS BlockCode, ownedBy AS ItemCode )
			SUPER( code )
			SELF:EventName := ""
			SELF:EventHandler := ""
			SELF:EventHandlerPrototype := ""
            SELF:EventType := ""
			SELF:Owner := ownedBy
			SELF:IsChild := FALSE
			SELF:IsHandled := FALSE
			//
			SELF:IsUserDef := FALSE
			FOREACH VAR uDef IN ownedBy:Owner:UserDefItems
				IF String.Compare( uDef:Name, SELF:Name, true ) == 0
					SELF:IsUserDef := TRUE
					EXIT
				ENDIF
			NEXT


	END CLASS

END NAMESPACE