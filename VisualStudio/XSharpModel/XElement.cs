using EnvDTE;
using LanguageService.CodeAnalysis.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class XElement
    {
        private Kind _Kind;
        private Modifiers _Modifiers;
        private string _Name;
        private TextRange _range;
        private XFile _File;
        //private DTE dte;
        private Modifiers _Visibility;
        private TextInterval _interval;
        private XElement _parent;


        public XElement(string name, Kind kind, Modifiers modifiers, Modifiers visibility, TextRange range, TextInterval interval)
        {
            _Name = name;
            _Kind = kind;
            _Modifiers = modifiers;
            _Visibility = visibility;
            _range = range;
            _interval = interval;
        }


        virtual public string FullName
        {
            get
            {
                return this._Name;
            }
        }

        public Kind Kind
        {
            get
            {
                return this._Kind;
            }
        }

        public string Language
        {
            get
            {
                return "XSharp";
            }
        }

        public string Name
        {
            get
            {
                return this._Name;
            }

        }


        public XElement Parent
        {
            get
            {
                return _parent;
            }

            set
            {
                _parent = value;
            }
        }

        public String ParentName
        {
            get
            {
                if (this._parent != null)
                {
                    return this._parent.FullName;
                }
                return null;
            }
        }

        public ProjectItem ProjectItem
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public TextRange Range
        {
            get
            {
                return this._range;
            }
        }

        public TextInterval Interval
        {
            get
            {
                return this._interval;
            }
        }

        public XFile File
        {
            get
            {
                return _File;
            }

            set
            {
                _File = value;
            }
        }

        public Modifiers Modifiers
        {
            get
            {
                return _Modifiers;
            }

            set
            {
                _Modifiers = value;
            }
        }

        public Modifiers Visibility
        {
            get
            {
                return _Visibility;
            }

            set
            {
                _Visibility = value;
            }
        }

        public virtual String Prototype
        {
            get
            {
                return this.Name;
            }
        }

        public virtual String Description
        {
            get
            {
                String modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                String desc = modVis;
                //
                desc += this.Kind.ToString() + " ";
                desc += this.Prototype;
                //
                return desc;
            }
        }

        /// <summary>
        /// Glyph constant used by DropDown Types/Members Comboxes in Editor
        /// </summary>
        public int Glyph
        {
            get
            {
                ImageListKind imgK = ImageListKind.Class;
                ImageListOverlay imgOv = ImageListOverlay.Public;
                //
                switch (this.Kind)
                {
                    case Kind.Class:
                        imgK = ImageListKind.Class;
                        break;
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Method:
                    case Kind.Function:
                    case Kind.Procedure:
                        imgK = ImageListKind.Method;
                        break;
                    case Kind.Structure:
                    case Kind.VOStruct:
                    case Kind.Union:
                        imgK = ImageListKind.Structure;
                        break;
                    case Kind.Access:
                    case Kind.Assign:
                    case Kind.Property:
                        imgK = ImageListKind.Property;
                        break;
                    case Kind.Event:
                        imgK = ImageListKind.Event;
                        break;
                    case Kind.Delegate:
                        imgK = ImageListKind.Delegate;
                        break;
                    case Kind.Operator:
                        imgK = ImageListKind.Operator;
                        break;
                    case Kind.VODefine:
                        imgK = ImageListKind.Const;
                        break;
                    case Kind.Enum:
                        imgK = ImageListKind.Enum;
                        break;
                    case Kind.Interface:
                        imgK = ImageListKind.Interface;
                        break;
                    case Kind.Namespace:
                        imgK = ImageListKind.Namespace;
                        break;
                    case Kind.VOGlobal:
                    case Kind.Field:
                        imgK = ImageListKind.Field;
                        break;
                    case Kind.Parameter:
                    case Kind.Local:
                        imgK = ImageListKind.Local;
                        break;
                }
                //
                switch (this.Visibility)
                {
                    case Modifiers.Public:
                        imgOv = ImageListOverlay.Public;
                        break;
                    case Modifiers.Protected:
                        imgOv = ImageListOverlay.Protected;
                        break;
                    case Modifiers.Private:
                        imgOv = ImageListOverlay.Private;
                        break;
                    case Modifiers.Internal:
                        imgOv = ImageListOverlay.Internal;
                        break;
                    case Modifiers.ProtectedInternal:
                        imgOv = ImageListOverlay.ProtectedInternal;
                        break;

                }
                //
                return this.GetImageListIndex(imgK, imgOv);
            }
        }

        /// <summary>
        /// Glyph Group used by CompletionList in CompletionSource
        /// - See also GlyphItem
        ///  http://glyphlist.azurewebsites.net/standardglyphgroup/
        /// </summary>
        public StandardGlyphGroup GlyphGroup
        {
            get
            {
                StandardGlyphGroup imgG = StandardGlyphGroup.GlyphGroupClass;
                //
                switch (this.Kind)
                {
                    case Kind.Class:
                        imgG = StandardGlyphGroup.GlyphGroupClass;
                        break;
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Method:
                    case Kind.Function:
                    case Kind.Procedure:
                        imgG = StandardGlyphGroup.GlyphGroupMethod;
                        break;
                    case Kind.Structure:
                        imgG = StandardGlyphGroup.GlyphGroupStruct;
                        break;
                    case Kind.Access:
                    case Kind.Assign:
                    case Kind.Property:
                        imgG = StandardGlyphGroup.GlyphGroupProperty;
                        break;
                    case Kind.Parameter:
                    case Kind.Local:
                        imgG = StandardGlyphGroup.GlyphGroupVariable;
                        break;
                    case Kind.Event:
                        imgG = StandardGlyphGroup.GlyphGroupEvent;
                        break;
                    case Kind.Delegate:
                        imgG = StandardGlyphGroup.GlyphGroupDelegate;
                        break;
                    case Kind.Enum:
                        imgG = StandardGlyphGroup.GlyphGroupEnum;
                        break;
                    case Kind.Operator:
                        imgG = StandardGlyphGroup.GlyphGroupOperator;
                        break;
                    case Kind.Interface:
                        imgG = StandardGlyphGroup.GlyphGroupInterface;
                        break;
                    case Kind.Namespace:
                        imgG = StandardGlyphGroup.GlyphGroupNamespace;
                        break;
                    case Kind.Field:
                    case Kind.VOGlobal:
                        imgG = StandardGlyphGroup.GlyphGroupField;
                        break;
                    case Kind.Union:
                        imgG = StandardGlyphGroup.GlyphGroupUnion;
                        break;
                    case Kind.VODefine:
                        imgG = StandardGlyphGroup.GlyphGroupConstant;
                        break;
                    case Kind.VOStruct:
                        imgG = StandardGlyphGroup.GlyphGroupValueType;
                        break;
                }
                return imgG;
            }
        }

        /// <summary>
        /// Glyph Item used by CompletionList in CompletionSource
        /// - See also GlyphGroup
        ///  http://glyphlist.azurewebsites.net/standardglyphgroup/
        /// </summary>
        public StandardGlyphItem GlyphItem
        {
            get
            {
                StandardGlyphItem imgI = StandardGlyphItem.GlyphItemPublic;
                //
                switch (this.Visibility)
                {
                    case Modifiers.Public:
                        imgI = StandardGlyphItem.GlyphItemPublic;
                        break;
                    case Modifiers.Protected:
                        imgI = StandardGlyphItem.GlyphItemProtected;
                        break;
                    case Modifiers.Private:
                        imgI = StandardGlyphItem.GlyphItemPrivate;
                        break;
                    case Modifiers.Internal:
                        imgI = StandardGlyphItem.GlyphItemInternal;
                        break;
                    case Modifiers.ProtectedInternal:
                        imgI = StandardGlyphItem.GlyphItemProtected;
                        break;

                }
                //
                return imgI;
            }
        }

        /// <summary>
        /// Turns an image list kind / overlay into the proper index in the image list.
        /// </summary>
        private int GetImageListIndex(ImageListKind kind, ImageListOverlay overlay)
        {
            return ((int)kind) * 6 + (int)overlay;
        }
    }


    /// <summary>
    /// Values coming from XSharpParser.ClassModifiersContext
    /// The order is important !!!
    ///  -> Private < ProtectedInternal < Internal < Protected < Public
    /// </summary>
    public enum Modifiers
    {
        Abstract,
        New,
        Partial,
        Sealed,
        Static,
        Unsafe,
        Private,
        Hidden = Private,
        ProtectedInternal,
        Internal,
        Protected,
        Public,
        Export = Public,
        None
    }

    /// <summary>
    /// Values coming from EnvDTE.vsCMElement
    /// </summary>
    public enum Kind
    {
        Namespace,
        Class,
        Structure,
        Constructor,
        Destructor,
        Method,
        Access,
        Assign,
        Property,
        ClassVar,
        Function,
        Procedure,
        Field,
        Local,
        Parameter,
        Event,
        Operator,
        Interface,
        Delegate,
        Enum,
        Using,
        VOGlobal,
        VODefine,
        VODLL,
        VOStruct,
        Union,
    }

    /// <summary>
    /// An enum which is synchronized with our image list for the various
    /// kinds of images which are available.  This can be combined with the 
    /// ImageListOverlay to select an image for the appropriate member type
    /// and indicate the appropriate visibility.  These can be combined with
    /// GetImageListIndex to get the final index.
    /// 
    /// Most of these are unused as we're just using an image list shipped
    /// by the VS SDK.
    /// </summary>
    enum ImageListKind
    {
        Class,
        Const,
        Delegate,
        Enum,
        EnumValue,
        Event,
        Unknown1,
        Field,
        Interface,
        Block,
        Variant,
        VariantOption,
        Method,
        StaticMethod,
        Unknown6,
        Namespace,
        Operator,
        Property,
        Structure,
        Unknown9,
        Macro,
        Unknown11,
        Unknown12,
        Local,
        ClassMethod
    }

    /// <summary>
    /// Indicates the overlay kind which should be used for a drop down members
    /// image.  The overlay kind typically indicates visibility.
    /// 
    /// Most of these are unused as we're just using an image list shipped
    /// by the VS SDK.
    /// </summary>
    enum ImageListOverlay
    {
        Public,
        Internal,
        ProtectedInternal,
        Protected,
        Private,
        ImageListOverlayArrow,
    }

}
