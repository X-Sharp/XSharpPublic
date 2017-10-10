USING System
USING System.Runtime.Serialization
// the following did not compiler because
// there are the following classes in the namespace
// - internal abstract class DataContract {...}
// - PUBLIC SEALED CLASS DataContractAttribute : Attribute {...}
// - INTERNAL CLASS DataMember {...}
// - PUBLIC SEALED CLASS DataMemberAttribute : Attribute {...}

[DataContract] CLASS DataCloass
[DataMember] EXPORT cName  AS STRING
END CLASS
