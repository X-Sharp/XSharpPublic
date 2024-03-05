USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml.Serialization;

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
		/// The SerializableDictionary class.
		/// XML Serializable Generic Dictionary
		/// Inspired by Paul Welter's Weblog
		/// https://weblogs.asp.net/pwelter34/444961
	/// </summary>
	
	
	
	[XmlRoot("dictionary")];
	PUBLIC CLASS SerializableDictionary<TKey, TValue> INHERIT Dictionary<TKey, TValue> IMPLEMENTS IXmlSerializable
	
		CONSTRUCTOR( )
			SUPER( )	
	
		CONSTRUCTOR( comparer AS IEqualityComparer<TKey> )
			SUPER( comparer )
	
		PUBLIC METHOD GetSchema() AS System.Xml.Schema.XmlSchema
			RETURN NULL
			
		PUBLIC METHOD ReadXml( reader AS System.Xml.XmlReader) AS VOID
			LOCAL keySerializer :=  XmlSerializer{ typeof(TKey) } AS XmlSerializer
			LOCAL valueSerializer := XmlSerializer{typeof(TValue)} AS XmlSerializer
			LOCAL wasEmpty := reader:IsEmptyElement AS LOGIC
			//
			reader:Read()
			IF (wasEmpty)
				RETURN
			ENDIF
			//
			WHILE (reader:NodeType != System.Xml.XmlNodeType.EndElement)
				reader:ReadStartElement("item")
				
				reader:ReadStartElement("key")
				LOCAL key := (TKey)keySerializer.Deserialize(reader) AS TKey
				reader:ReadEndElement()
				
				reader:ReadStartElement("value")
				LOCAL val := (TValue)valueSerializer.Deserialize(reader) AS TValue
				reader:ReadEndElement()
				
				SELF:Add(key, val)
				
				reader:ReadEndElement()
				
				reader:MoveToContent()
			ENDDO
			reader:ReadEndElement()
			
			
		PUBLIC METHOD WriteXml(writer AS System.Xml.XmlWriter ) AS VOID
			LOCAL keySerializer :=  XmlSerializer{ typeof(TKey) } AS XmlSerializer
			LOCAL valueSerializer := XmlSerializer{typeof(TValue)} AS XmlSerializer
			
			FOREACH  key AS TKey IN SELF:Keys
			
				writer:WriteStartElement("item")
				
				writer:WriteStartElement("key")
				keySerializer:Serialize(writer, key)
				writer:WriteEndElement()
				
				writer:WriteStartElement("value")
				LOCAL val := SELF[key] AS TValue
				valueSerializer:Serialize(writer, val)
				writer:WriteEndElement()
				
				writer:WriteEndElement()
			NEXT
			
	END CLASS
END NAMESPACE // FabVFPXPorter