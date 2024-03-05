USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml.Serialization

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
	/// The SCXVCXItem class : The Form design and attached event.
	/// </summary>
	CLASS SCXVCXItem INHERIT BaseItem

		/// <summary>
		/// A list of User-Defined elements : Properties and Procedures (Methods)
		/// </summary>
		PROPERTY UserDefItems AS List<UserDefinition> AUTO

		PROPERTY IsContainer AS LOGIC AUTO
		PROPERTY IsTopLevel AS LOGIC AUTO

		PROPERTY AddToControls AS LOGIC AUTO

		/// <summary>
		/// Default Constructor
		/// </summary>
		CONSTRUCTOR()
			SUPER()
			SELF:UserDefItems := List<UserDefinition>{}

		/// <summary>
		/// Copy Constructor
		/// </summary>
		CONSTRUCTOR( itemToCopy AS BaseItem )
			SUPER( itemToCopy )
			// This Class Attributes...
			SELF:UserDefItems := List<UserDefinition>{}
			// Do we have some UserDefined Items ?
			IF itemToCopy IS SCXVCXItem
				VAR oneItem := (SCXVCXItem) itemToCopy
				SELF:IsContainer := oneItem:IsContainer
				SELF:IsTopLevel := oneItem:IsTopLevel
				SELF:AddToControls := oneItem:AddToControls
				// Copy the User Defined Items
				FOREACH VAR userDef IN oneItem:UserDefItems
					SELF:UserDefItems:Add( UserDefinition{ userDef } )
				NEXT
			ENDIF
			RETURN

		/// <summary>
		/// DBF Reading Constructor
		/// </summary>
		CONSTRUCTOR( lFillWithDB AS LOGIC )
			SUPER( lFillWithDB )
			SELF:InitPropertiesDict( SELF:PROPERTIES )
			SELF:UserDefItems := List<UserDefinition>{}


		/// <summary>
		/// Initialize the dictionary of Properties
		/// <param name="cTextBlock">
		/// A text block containing lines, each one with "propname=value"
		/// </param>
		/// </summary>
		METHOD InitPropertiesDict( cTextBlock AS STRING ) AS VOID
			// Assume it is "list" of lines, each one with "propname=value"
			VAR lines := cTextBlock:Split( <CHAR>{'\r', '\n'} )
			//
			FOREACH VAR textLine IN lines
				//				VAR elements := textLine:Split( <CHAR>{'='} )
				//				//
				//				IF elements:Length >= 2
				//					SELF:PropertiesDict:Add( elements[1]:Trim(), IIF( String.IsNullOrEmpty(elements[2]:Trim()), "Nil", elements[2]:Trim()))
				//				ENDIF
				//
				VAR eqPos := textLine:IndexOf('=')
				IF eqPos > -1
					VAR propName := textLine:Substring(0, eqPos ):Trim()
					VAR propValue := textLine:Substring( eqPos + 1 ):Trim()
					SELF:PropertiesDict:Add( propName, IIF( String.IsNullOrEmpty(propValue), "Nil", propValue ) )
				ENDIF
			NEXT

		/// <summary>
		/// Add to the List of Properties some special ones, like _currentobject_ as SELF
		/// </summary>
		/// <param name="ConversionList"></param>
		/// <param name="asChild"></param>
		PROTECTED METHOD AddSpecialProperties( ConversionList AS Dictionary<STRING,STRING>, asChild := FALSE AS LOGIC ) AS VOID
			//
			TRY
				// Checking if we are in the Form (We should check if we are a parent..??)
				//IF (String.Compare( SELF:BaseClassName, "form", TRUE ) == 0 )
				IF !asChild
					ConversionList:Add( "_currentobject_", "SELF" )
				ELSE
					ConversionList:Add( "_currentobject_", "SELF:" + SELF:Name )
				ENDIF
            CATCH
                nop
			END TRY

		/// <summary>
		/// Convert the dictionary of Properties, applying all replacement rules.
		/// - place-holders between markers will be replaced by their content value
		/// - properties with ?? with have Quotes if needed
		/// </summary>
		/// <param name="ConversionList">A dictionnary of conversion "rules"</param>
		METHOD ConvertProperties( ConversionList AS Dictionary<STRING,STRING>, defaultValues AS Dictionary<STRING,STRING>, asChild := FALSE AS LOGIC ) AS VOID
			// TODO : Maybe rewrite this method, because currently having a replacement (with "<@") cannot be used with "^xx99" rules
			// Get the Current PropList
			LOCAL propList AS SerializableDictionary<STRING,STRING>
			LOCAL prop AS STRING
			// Get the current the Properties List : The "real"/"current" Properties list Key->Value
			propList := SELF:PropertiesDict
			// Add Special Properties ( CurrentObject, .... )
			SELF:AddSpecialProperties( propList, asChild )
			//  Create a new "empty" Properties list
			SELF:PropertiesDict := SerializableDictionary<STRING,STRING>{StringComparer.InvariantCultureIgnoreCase}
			// Get one Rule and Convert
			FOREACH conversion AS KeyValuePair<STRING, STRING> IN ConversionList
				// We need to replace something
				IF conversion:Value:Contains("<@" )
					LOCAL newProp := conversion:Value AS STRING
					// Create a list of Element to replace
					VAR tokenList := SELF:BuildTokenList( newProp )
					VAR Found := FALSE
					// Search the item in the List of existing Properties
					FOREACH token AS STRING IN tokenList
						IF propList:ContainsKey( token )
							// Get the value of this "Fox" Property
							VAR data := propList:Item[ token ]
							// and put that value at the replaceable position
							// If newProp start with a quotation mark, and Data contains a quotation mark, remove it from newProp
							IF data:Contains(e"\"")
								IF newProp:StartsWith(e"\"") .AND. newProp:EndsWith(e"\"")
									// Remove the quote
									newProp := newProp:Substring(1)
									newProp := newProp:Substring(0,newProp:Length-1)
								ENDIF
							ENDIF
							newProp := ReplaceCaseInsensitive( newProp, "<@"+token+"@>", data)
							Found := TRUE
						ENDIF
					NEXT
					// Sorry, we are missing informations
					IF newProp:Contains("<@" )
						// But we had at least one, so get others from the Default Values
						IF Found
							// Recreate the TokenList for missing items
							tokenList := SELF:BuildTokenList( newProp )
							Found := FALSE
							FOREACH token AS STRING IN tokenList
								IF defaultValues:ContainsKey( token )
									// Get the value of this "Fox" Property
									VAR data := propList:Item[ token ]
									// and put that value at the replaceable position
									newProp := ReplaceCaseInsensitive( newProp, "<@"+token+"@>", data)
									Found := TRUE
								ENDIF
							NEXT
							// Still missing ?
							IF newProp:Contains("<@" )
								// Forget it
								Found := FALSE
							ENDIF
						ENDIF
					ENDIF
					IF Found
						// Create a new Property after replacing the elements
						// If the new prop start with a double Question Mark, we must check that the result is a string with quotation marks
						VAR qmOpen := newProp:IndexOf("??")
						VAR qmClose := newProp:LastIndexOf("??")
						IF (qmOpen > -1) .AND. (qmOpen != qmClose)
							VAR prefix := newProp:Substring(0, qmOpen)
							VAR postfix := newProp:Substring(qmClose+2)
							// Extract the newProp
							newProp := newProp:Substring(qmOpen+2, qmClose-(qmOpen+2))
							// Quotation Mark inside ?
							IF newProp:IndexOf('"') == -1
								newProp := e"\"" + newProp + e"\""
							ENDIF
							newProp := prefix + newProp + postfix
						ENDIF
						propList:Item[ conversion:Key ] := newProp
					ENDIF
				ELSE
					VAR conversionKey := conversion:Key
					VAR forceUsual := false
					// Hot!! Hot!!
					// We don't want to change the Property or its value, we just want to cast it to Usual
					IF conversion:Key:StartsWith("!")
						// Remove the !
						conversionKey := conversionKey:Substring(1)
						forceUsual := true
					ENDIF
					// Just a simple one-to-one property replacement
					IF propList:ContainsKey( conversionKey )
						// For this element, retrieve the current FoxPro value
						prop := propList:Item[ conversionKey ]
						// Remove the current "Fox" Property
						propList:Remove( conversionKey )
						// If conversion:Value is Empty, remove the FoxPro one
						// Don't add the special properties (Like _CurrentObject)
						IF !String.IsNullOrEmpty( conversion:Value ) .AND. !conversion:Value:StartsWith("_")
							// and replace with the "new" Property
							// Special mark : Add Quotes only if needed
							IF prop:StartsWith("??")
								prop := prop:Substring(2)
								IF prop:IndexOf('"') == -1
									prop := e"\"" + prop + e"\""
								ENDIF
							ENDIF
							//
							IF conversion:Value:EndsWith(")") .OR. conversion:Value:EndsWith("}")
								// Oh !! The Property has been replaced by a Constructor or Method Call !
								// Ok, recreate the Property, but set the "new" value
								propList:Add( conversionKey, conversion:Value )
							ELSE
								// We simply replace Key with the new one, and keep the actual value
								IF forceUsual
									propList:Add( "!"+conversion:Value, prop )
								ELSE
									propList:Add( conversion:Value, prop )
								ENDIF
							ENDIF
						ENDIF
					ELSEIF ( conversionKey:StartsWith("^") )
						// Special Prop!! Like Option1 => Option(1), so rule is ^Option99
						// Remove the ^
						VAR lookup := conversionKey:Substring(1)
						// Seach for 99
						VAR numberPos := lookup:IndexOf( "99" )
						IF ( numberPos > -1 )
							// So, lookup == Option
							lookup := lookup:Substring(0, numberPos)
						ENDIF
						// We may change some Keys....
						VAR modified := FALSE
						VAR newPropList := SerializableDictionary<STRING,STRING>{StringComparer.InvariantCultureIgnoreCase}
						// We will apply a "mask" to all properties matching
						FOREACH itemProp AS KeyValuePair<STRING, STRING> IN propList
							// Do we have a Property that starts with ...let's say Option
							IF ( itemProp:Key:StartsWith( lookup ) )
								VAR oneChar := ""
								// Check if we have a number after the lookup prop
								// To avoid mixing Option and Options (final s)
								oneChar := itemProp:Key:Substring( numberPos,1 )
								IF !Char.IsDigit( oneChar,0)
									// NaN... Keep the original
									newPropList:Add( itemProp:Key, itemProp:Value )
									LOOP
								ENDIF
								// Ok we have a number , how long is it ?
								VAR number := ""
								VAR nPos := numberPos
								IF ( ( nPos > -1 ) && ( nPos < itemProp:Key:Length ) )
									REPEAT
										oneChar := itemProp:Key:Substring( nPos,1 )
										IF Char.IsDigit( oneChar,0)
											number += oneChar
											nPos++
										ELSE
											EXIT
										ENDIF
									UNTIL ( nPos < itemProp:Key:Length )
								ENDIF
								// So we have the prefix and the number, let says Option1
								VAR toReplace := lookup + number
								// Get the rule
								VAR newKey := conversion:Value
								// Seach for 99, and replace it with the Number
								nPos := newKey:IndexOf( "99" )
								IF ( nPos > -1 )
									newKey := newKey:Substring(0,nPos)+number+newKey:Substring(nPos+2)
								ENDIF
								newKey += itemProp:Key:Substring( toReplace:Length )
								//Ok, now Option1 has been turned to Option(1)
								newPropList:Add( newKey, itemProp:Value )
								modified := TRUE
							ELSE
								newPropList:Add( itemProp:Key, itemProp:Value )
							ENDIF
						NEXT
						IF modified
							propList := newPropList
						ENDIF
					ENDIF
				ENDIF
			NEXT
			// After "conversion", the new Properties list is...
			SELF:PropertiesDict := propList
			RETURN

		/// <summary>
		/// Apply the Rules and Generate the code
		/// If the rules started by ::, turns the property to a method call
		/// If the rules started by :, turns the property to a method call with the ChildName if needed
		/// Else set the property (of the Child if needed) to the Value
		/// </summary>
		/// <param name="asChild">If the current Item is considered as a Child it will prefix calls by his name (we call this method from the owner/parent)</param>
		/// <param name="asLocal">The code is generated "as local", so no SELF: prefix</param>
		/// <param name="userDefMode"> 0:All Properties; 1:All but UserDef; 2:UserDef Only</param>
		/// <returns>The Generated Code</returns>
		METHOD ApplyPropertiesRules( asChild AS LOGIC, asLocal := FALSE AS LOGIC, userDefMode := 0 AS INT ) AS STRING
			VAR sb := StringBuilder{}
			// enumerate all properties
			// key : Name of the Property to generate
			// value : the value to generate (comes from FoxPro with or Without Convertion->ConvertProperties)
			FOREACH VAR prop IN SELF:PropertiesDict
				IF ( userDefMode == 1 )
					// All but no UserDef
					VAR userDef := SELF:UserDefItems:Find( { x => String.Compare( x:Name, prop:Key, TRUE ) == 0 } )
					IF userDef != NULL
						Loop
					ENDIF
				ELSEIF ( userDefMode == 2 )
					// Only UserDef
					VAR userDef := SELF:UserDefItems:Find( { x => String.Compare( x:Name, prop:Key, TRUE ) == 0 } )
					IF userDef == NULL
						Loop
					ENDIF
				ENDIF
				// Not a special internal property
				IF !prop:Key:StartsWith("_")
					IF !asLocal
						// Special Case, Method call
						sb:Append("SELF:")
					ENDIF
					// assign the Child if needed
					IF asChild
						sb:Append(SELF:Name)
						sb:Append(":")
					ENDIF
					IF prop:Value:StartsWith("::")
						// Replace the Property by a Method Call
						sb:Append(prop:Value:Substring(2))
					ELSEIF prop:Value:StartsWith(":")
						// Replace the Property by a Method Call, adding the name of the Child if needed
						sb:Append(prop:Value:Substring(1))
					ELSE
						VAR forceUsual := false
						VAR propKey := prop:Key
						// ForceUsual ??
						IF propKey:StartsWith("!")
							propKey := propKey:Substring(1)
							forceUsual := true
						ENDIF
						// Warning !! If we are processing a control placed on a container
						// the selector will be dot '.'; and the designer DONT LIKE THAT !!!!
						sb:Append(propKey:Replace( '.', ':' ))
						sb:Append(" := ")
						// HACK This is to Handle Value as USUAL and its assignment
						IF forceUsual
							sb:Append("__Usual{")
							var propValue := prop:Value
							IF propValue:StartsWith("{") .AND. propValue:EndsWith("}")
								// This is a Date !?
								sb:Append( "__Date" )
								// Add " after { & before }
								propValue := propValue:Insert(1,e"\"")
								propValue := propValue:Insert( propValue:Length-2,e"\"")
								sb:Append(propValue)
							ELSE
								sb:Append(propValue)
							ENDIF
							sb:Append("}")
						ELSE
							sb:Append(prop:Value)
						ENDIF
					ENDIF
					sb:Append(Environment.NewLine)
				ENDIF
			NEXT
			RETURN sb:ToString()

		/// <summary>
		/// Generate the EventHandlers create code
		/// </summary>
		/// <returns></returns>
        METHOD CreateEventHandlers( createChildsHandlers AS LOGIC, settings AS XPorterSettings  ) AS STRING
            //
			IF ( SELF:XPortedCode == NULL )
				RETURN ""
			ENDIF
			VAR instantiate := StringBuilder{}
			// Enumerate All CodeBlock that are attached to this Item
			FOREACH cdeBlock AS EventCode IN SELF:XPortedCode:Events
				// !!! WARNING !!! UserDef Code is also stored in XPortedCode:Events, don't Create EventHandlers for these
				IF cdeBlock:IsUserDef
					LOOP
				ENDIF
				IF !cdeBlock:IsHandled .AND. settings:GenerateOnlyHandledEvent
					XPorterLogger.Instance:Warning( "Ignored as an EventHandler : " + cdeBlock:EventName + " / " + cdeBlock:EventHandler)
					LOOP
				ENDIF
				// Looking only for Childs Item ?
				IF cdeBlock:IsChild == createChildsHandlers
					instantiate:Append("SELF")
					IF !SELF:IsTopLevel
						instantiate:Append(":")
						instantiate:Append(SELF:Name)
					ENDIF
					// If the EventName ends with "()", this is a Method Call
					IF cdeBlock:EventName:EndsWith("()")
						IF String.IsNullOrEmpty(cdeBlock:EventType)
							instantiate:Append("_")
							instantiate:Append(cdeBlock:EventName)
						ELSE
							instantiate:Append(":")
							instantiate:Append(cdeBlock:EventName:Substring(0, cdeBlock:EventName:Length -1 ))
							instantiate:Append(cdeBlock:EventType )
							instantiate:Append("{ SELF, ")
							instantiate:Append('"')
							instantiate:Append(cdeBlock:EventHandler )
							instantiate:Append('"')
							instantiate:Append(" } )")
						ENDIF
					ELSEIF cdeBlock:EventName:EndsWith("{}")
						instantiate:Append(":")
						instantiate:Append(cdeBlock:EventName:Substring(0, cdeBlock:EventName:Length -2 ))
						instantiate:Append(" := ")
						instantiate:Append(cdeBlock:EventType )
						instantiate:Append("{ SELF, ")
						instantiate:Append('"')
						instantiate:Append(cdeBlock:EventHandler )
						instantiate:Append('"')
						instantiate:Append(" }")
					ELSEIF cdeBlock:EventName:EndsWith(":=")
						instantiate:Append(":")
						instantiate:Append(cdeBlock:EventName:Substring(0, cdeBlock:EventName:Length -2 ))
						instantiate:Append(" := ")
						instantiate:Append('"')
						instantiate:Append(cdeBlock:EventHandler )
						instantiate:Append('"')
					ELSE
						instantiate:Append(":")
						//instantiate:Append(cdeBlock:EventName)
						// Warning !! If we are processing the event of a control placed on a container
						// the selector will be dot '.'; and the designer DONT LIKE THAT !!!!
						instantiate:Append(cdeBlock:EventName:Replace( '.', ':' ))
						// += System.EventHandler{ SELF, @exportButton_Click() }
						instantiate:Append(" += " )
						instantiate:Append( cdeBlock:EventType )
						instantiate:Append( "{ SELF, @")
						instantiate:Append(cdeBlock:EventHandler )
						instantiate:Append("() }")
					ENDIF
					instantiate:Append(Environment.NewLine)
				ENDIF
			NEXT
			RETURN instantiate:ToString()


		/// <summary>
		/// Convert the ClassName of the current SCXVCXItem, note that the FoxClassName is not altered here
		/// <param name="ConversionList">
		/// A dictionnary of conversion "rules" :
		///	Key : The VFP Classname
		/// Value : The new ClassName
		/// </param>
		/// </summary>
		METHOD ConvertClassName( ConversionList AS Dictionary<STRING,STRING[]> ) AS VOID
			// We know the Class ? Or the BaseClassName ?
			IF ConversionList:ContainsKey( SELF:ClassName ) // .OR. ConversionList:ContainsKey( SELF:BaseClassName )
				LOCAL data AS STRING[]
				IF ConversionList:ContainsKey( SELF:ClassName )
					// Convert a Fox ClassName to a .NET ClassName
					data := ConversionList:Item[ SELF:ClassName ]
					// Retrieve the corresponding ClassName to Use
					SELF:ClassName := data[1]
					// Is that a Container type ?
					SELF:IsContainer := ( String.Compare(data[2], "true", TRUE) == 0 )
					// Will be added to Controls ?
					SELF:AddToControls := ( String.Compare(data[3], "true", TRUE) == 0 )
				ENDIF
			ELSEIF ConversionList:ContainsKey( SELF:BaseClassName )
				LOCAL data AS STRING[]
				IF ConversionList:ContainsKey( SELF:BaseClassName )
					// Keep the Name, but look at these properties
					data := ConversionList:Item[ SELF:BaseClassName ]
					// Is that a Container type ?
					SELF:IsContainer := ( String.Compare(data[2], "true", TRUE) == 0 )
					// Will be added to Controls ?
					SELF:AddToControls := ( String.Compare(data[3], "true", TRUE) == 0 )
				ENDIF
			ENDIF

		/// <summary>
		/// Create a list of Tokens : Tokens are  enclosed by <@ and @>
		/// </summary>
		/// <param name="stringNTokens"></param>
		/// <returns></returns>
		PRIVATE METHOD BuildTokenList( stringNTokens AS STRING ) AS STRING[]
			LOCAL tempList AS List<STRING>
			tempList := List<STRING>{}
			DO WHILE stringNTokens:Contains("<@" )
				LOCAL startIndex := stringNTokens:IndexOf( "<@" ) AS INT
				LOCAL endIndex := stringNTokens:IndexOf( "@>") AS INT
				IF ( startIndex >=0 ) .AND. ( endIndex >= 0 )
					VAR token := stringNTokens:Substring( startIndex+2, endIndex - (startIndex + 2 ))
					tempList:Add( token )
					stringNTokens := stringNTokens:Substring( endIndex+2 )
				ELSE
					EXIT
				ENDIF
			ENDDO
			RETURN tempList:ToArray()

		/// <summary>
		/// Convert FoxEvent and FoxEventHandler to NetEvent and NetEventHandler.
		/// Enumerate the XPorted:Events objects as EventCode.
		/// For each whose EventCode:Name is a VFP EventName, set the EventCode:EventName to the .NET EventName.
		/// Then generate an EventCode:EventHandler with EventCode:Owner:Name + EventCode:EventName + rule PostFix (see param definition)
		/// <param name="rules">
		/// A dictionnary of conversion "rules" :
		///	Key : The VFP EventName
		/// Value : An array : The .NET EventName, and the PostFix of the EventHandler Prototype (Usually the Params et Return type)
		/// </param>
		/// </summary>
		METHOD ConvertEvents( eventRules AS Dictionary<STRING,STRING[]>, sttmnts AS List<String>, vfpElt AS Dictionary<STRING,STRING>, settings AS XPorterSettings ) AS VOID
			//
			IF ( SELF:XPortedCode == NULL )
				RETURN
			ENDIF
			//
			LOCAL converter AS CodeConverter
			converter := CodeConverter{settings:KeepOriginal, settings:ConvertHandlers, settings:ConvertThisObject, settings:ConvertStatement, settings:ConvertStatementOnlyIfLast }
			converter:Statements := sttmnts
			converter:VFPElements := vfpElt
			FOREACH cdeBlock AS EventCode IN SELF:XPortedCode:Events
				// The Event belongs to a FORM
				VAR formEvent := (String.Compare(cdeBlock:Owner:Owner:BaseClassName,"form",TRUE)==0)
				// If we are working on a control, that belongs to a container, that is placed inside a form
				// the cdeBlock:Name might be something like Command1.Click : So, the event is after the last selector (dot)
				VAR blockName := cdeBlock:Name
				VAR vfpEventName := cdeBlock:Name
				VAR selectorPos := cdeBlock:Name:LastIndexOf('.')
				IF selectorPos >= 0
					blockName := cdeBlock:Name:Substring(0, selectorPos+1)
					vfpEventName := cdeBlock:Name:Substring(selectorPos+1)
				ELSE
					blockName := String.Empty
				ENDIF
				// Search the FoxPro Event Name
				IF eventRules:ContainsKey( vfpEventName )
					cdeBlock:IsHandled := TRUE
					// For each FoxPro Event name (standard rule), we have an array :
					// 1 : The .NET EventName
					// 2 : Parameters of the .NET Event
					// 3 : The .NET EventHandler type
					// -> "Easy" Standard processing : Matching a VFP Event to a .NET Event
					// "VFP Event": [ "The .NET EventName", "Parameters of the .NET Event", "The .NET EventHandler type" ]
					// "InteractiveChange": [ "TextChanged", "(sender AS OBJECT, e AS System.EventArgs) AS VOID", "System.EventHandler" ],
					// -> VFP Event can receive various params :
					// We match to a "internal" method; this one will be called by .NET
					// and will reroute to VFP wit the right params
					// The .NET EventName is prefixed with Set_
					// "LostFocus": [ "Set_LostFocus()", "() AS USUAL CLIPPER", "VFPOverride" ],
					// ->
					// "Refresh": [ "VFPRefresh{}", "() AS USUAL CLIPPER", "VFPOverride" ],

					VAR data := eventRules:Item[ vfpEventName ]
					VAR evtType := "System.EventHandler"
					//
					IF data:Length >= 3 .AND. !String.IsNullOrEmpty( data[3] )
						evtType := data[3]
					ENDIF
					//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
					// Add .NET Informations
					cdeBlock:EventName := blockName + data[1]
					cdeBlock:EventType := evtType
					// Per default, Event in FORM are not prefixed... Not sure this is a good idea
					// Not in a FORM .OR. PrefixEvent is True
					IF !formEvent .OR. settings:PrefixEvent
						// Add the Name of the Control, or the Class Name for FORM
						cdeBlock:EventHandler := cdeBlock:Owner:Name + "_"
					ENDIF
					//
					IF cdeBlock:EventName:EndsWith("{}")
						cdeBlock:EventHandler += cdeBlock:EventName:Substring(0,cdeBlock:EventName:Length-2)
					ELSE
						LOCAL evtHandler AS STRING
						//
						IF settings:KeepFoxProEventName
							evtHandler := vfpEventName
						ELSE
							evtHandler := cdeBlock:EventName
							IF settings:RemoveSet .AND. evtHandler:StartsWith( "Set_", TRUE, System.Globalization.CultureInfo.InvariantCulture )
								evtHandler := cdeBlock:EventName:Substring(4)
							ENDIF
						ENDIF
						//
						cdeBlock:EventHandler += evtHandler
					ENDIF
					// Check that we don't have a "." (dot) in the Name
					SELF:EnforceEventHandlerName( cdeBlock )
					//
					cdeBlock:EventHandlerPrototype := cdeBlock:EventHandler
					IF data[2]:StartsWith( "(") .AND. cdeBlock:EventHandlerPrototype:EndsWith(")")
						cdeBlock:EventHandlerPrototype += data[2]:Substring(2)
					ELSE
						cdeBlock:EventHandlerPrototype += data[2]
					ENDIF
				ELSE
					// No convertion ? Try to keep the original, but prefix with the Control/Owner Name
					cdeBlock:EventName := vfpEventName
					cdeBlock:IsHandled := FALSE
					// and set with "System.EventHandler"
					cdeBlock:EventType := "System.EventHandler"
					// If we have a "(" in the Event Name, this is a METHOD call instead of an Event
					VAR pos := cdeBlock:EventName:IndexOf("(")
					// Not in a FORM .OR. PrefixEvent is True
					IF !formEvent .OR. settings:PrefixEvent
						cdeBlock:EventHandler := cdeBlock:Owner:Name + "_"
					ENDIF
					IF pos == -1
						cdeBlock:EventHandler += cdeBlock:EventName
					ELSE
						cdeBlock:EventHandler += cdeBlock:EventName:Substring(0,pos)
					ENDIF
					// Check that we don't have a "." (dot) in the Name
					SELF:EnforceEventHandlerName( cdeBlock )
					//
					cdeBlock:EventHandlerPrototype := cdeBlock:EventHandler + IIF( vfpEventName:EndsWith(")"), " AS USUAL", "() AS USUAL")
				ENDIF
				// Process Code to automatically convert some VFP Code to .NET Code : This, This.Parent, ThisForm
				converter:ProcessEvent( cdeBlock )
			NEXT
			RETURN

			// Check that we don't have a "." (dot) in the Name
		PRIVATE METHOD EnforceEventHandlerName( cdeBlock AS EventCode ) AS VOID
			IF cdeBlock:EventHandler:IndexOf('.') != -1
				cdeBlock:EventHandler := cdeBlock:EventHandler:Replace( '.', '_' )
				IF SELF:IsContainer
					cdeBlock:IsChild := TRUE
				ENDIF
			ENDIF


			// Should be an Extension Method, no ?
		/// <summary>
		/// Search and Replace a string by another string. Search is Case Insensitive.
		/// Throw an Exception if str/oldValue are NULL or Empty.
		/// Return the original if oldValue doesn't exist in the String.
		/// </summary>
		/// <param name="str">The original String.</param>
		/// <param name="oldValue">The value to replace</param>
		/// <param name="newValue">The new value to use as replacement</param>
		/// <returns>The new String after the replacement.</returns>
		PUBLIC METHOD ReplaceCaseInsensitive( str AS STRING, oldValue AS STRING, newValue AS STRING) AS STRING
			IF (str == NULL)
				THROW ArgumentNullException{nameof(str)}
			ENDIF
			IF (oldValue == NULL)
				THROW ArgumentNullException{nameof(oldValue)}
			ENDIF
			IF (oldValue.Length == 0)
				THROW ArgumentException{"String cannot be of zero length.", nameof(oldValue)}
			ENDIF
			//
			VAR position := str:IndexOf(oldValue, 0, StringComparison.OrdinalIgnoreCase)
			IF (position == -1)
				RETURN str
			ENDIF
			//
			VAR sb := StringBuilder{str.Length}
			VAR lastPosition := 0
			//
			REPEAT
				sb:Append(str, lastPosition, position - lastPosition)
				sb:Append(newValue)
				lastPosition := position + oldValue.Length
				position := str:IndexOf(oldValue, lastPosition, StringComparison.OrdinalIgnoreCase)
			UNTIL ( position == -1)
			//
			sb:Append(str, lastPosition, str.Length - lastPosition)
			RETURN sb:ToString()

	END CLASS





END NAMESPACE // FabVFPXPorter
