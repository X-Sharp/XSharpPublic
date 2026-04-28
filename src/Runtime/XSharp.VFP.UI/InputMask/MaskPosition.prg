USING System

BEGIN NAMESPACE XSharp.VFP.UI
    
    /// <summary>
    /// Represents a single position in an InputMask pattern
    /// </summary>
    PUBLIC CLASS MaskPosition
        
        /// <summary>
        /// Index position in the mask string
        /// </summary>
        PUBLIC PROPERTY Index AS INT AUTO
        
        /// <summary>
        /// Type of position: "digit", "letter", "alphanumeric", "literal", "modifier"
        /// </summary>
        PUBLIC PROPERTY Type AS STRING AUTO
        
        /// <summary>
        /// Literal character value (if Type == "literal")
        /// </summary>
        PUBLIC PROPERTY Literal AS STRING AUTO
        
        /// <summary>
        /// Whether this position is required (must be filled)
        /// </summary>
        PUBLIC PROPERTY Required AS LOGIC AUTO
        
        /// <summary>
        /// Modifier character: "!" (force uppercase) or "&" (space-fill)
        /// </summary>
        PUBLIC PROPERTY Modifier AS STRING AUTO
        
    END CLASS

END NAMESPACE
