#if PORTABLE

namespace Antlr4.Runtime
{
    [System.AttributeUsage(System.AttributeTargets.Class | System.AttributeTargets.Struct | System.AttributeTargets.Enum | System.AttributeTargets.Delegate, Inherited = false)]
    internal sealed class SerializableAttribute : System.Attribute
    {
    }
}

#endif
