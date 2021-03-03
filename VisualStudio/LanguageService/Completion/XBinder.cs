using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpModel;

namespace XSharp.LanguageService
{
    internal class XBinder
    {
        XBinder Next => null;
    }

    internal class XNamespaceBinder: XBinder
    {

    }

    internal class XTypeBinder : XBinder
    {
        private IXTypeSymbol _type;
        internal XTypeBinder( IXTypeSymbol type )
        {
            _type = type;
        }

        internal IXMemberSymbol SearchConstructor(Modifiers minVisibility)
        {
            if (_type != null)
            {
                //
                var xMethod = _type.Members.Where(x => x.Kind == Kind.Constructor).FirstOrDefault();
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                if (xMethod != null)
                {
                    return xMethod;
                }
            }
            return null;
        }
        internal IXMemberSymbol SearchPropertyOrField(string name, Modifiers minVisibility)
        {
            var result = SearchField (name, minVisibility);
            if (result == null)
            {
                result = SearchProperty (name, minVisibility);
            }
            if (result == null)
            {
                result = SearchEvent(name, minVisibility);
            }

            return result;
        }

        internal IXMemberSymbol SearchProperty(string name, Modifiers minVisibility)
        {
            if (_type != null)
            {

                IXMemberSymbol property = _type.GetProperties(name).FirstOrDefault();
                //
                if ((property != null) && (property.Visibility < minVisibility))
                {
                    property = null;
                }
                //
                if (property == null)
                {
                    if (!string.IsNullOrEmpty(_type.BaseType) )
                    {
                        // Parent has just a Name, so one of the System Types
                        //return SearchPropertyTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Public, out foundElement);
                    }
                }
                return property;
            }
            return null;

        }
        internal IXMemberSymbol SearchEvent(string name, Modifiers minVisibility)
        {
            if ( _type != null)
            {

                IXMemberSymbol evt = _type.GetEvents().Where(e => String.Compare(e.Name, name, true) == 0).FirstOrDefault();
                //
                if ((evt != null) && (evt.Visibility < minVisibility))
                {
                    evt = null;
                }
                //
                if (evt == null)
                {
                    if (!string.IsNullOrEmpty(_type.BaseType) )
                    {
                        // Parent has just a Name, so one of the System Types
                        //return SearchEventTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Public, out foundElement);
                    }
                }
                return evt;
            }
            return null;

        }
        internal IXMemberSymbol SearchField(string name, Modifiers minVisibility)
        {
            if (_type != null)
            {
                IXMemberSymbol field = _type.GetFields(name).FirstOrDefault();
                if ((field != null) && (field.Visibility < minVisibility))
                {
                    field = null;
                }
                if (field == null)
                {
                    if (!string.IsNullOrEmpty(_type.BaseType))
                    {
                        // Search in base type
                        //return SearchFieldTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Protected, out foundElement);
                    }
                }
                return field;
            }
            return null;
        }

        internal IXMemberSymbol SearchMethod(string name, Modifiers minVisibility)
        {
            if (_type != null)
            {
                IXMemberSymbol field = _type.GetMethods(name).FirstOrDefault();
                if ((field != null) && (field.Visibility < minVisibility))
                {
                    field = null;
                }
                if (field == null)
                {
                    if (!string.IsNullOrEmpty(_type.BaseType) )
                    {
                        // Search in base type
                        //return SearchFieldTypeIn(new CompletionType(cType.BaseType, cType.File, cType.File.Usings), currentToken, Modifiers.Protected, out foundElement);
                    }
                }
                return field;
            }
            return null;
        }

    }
    internal class XMemberBinder: XBinder
    {
        private IXMemberSymbol _member;
        internal XMemberBinder(IXMemberSymbol member)
        {
            _member = member;
        }


    }
}
