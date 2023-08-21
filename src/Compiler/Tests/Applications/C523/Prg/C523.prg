STRUCTURE XMLData

    PRIVATE _pNode AS System.Xml.XmlNode

    INTERNAL CONSTRUCTOR (pNode AS OBJECT)

        _pNode := (System.Xml.XmlNode) pNode

    INTERNAL PROPERTY Pointer AS OBJECT GET _pNode

    PROPERTY Empty   AS LOGIC  GET _pNode == NULL

END STRUCTURE
