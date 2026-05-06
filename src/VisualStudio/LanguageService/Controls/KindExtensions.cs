// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System.Runtime.InteropServices;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Extension methods on <see cref="Kind"/> that map an entity kind and visibility
    /// to the appropriate <see cref="ImageMoniker"/> from the VS <see cref="KnownMonikers"/> catalog.
    /// </summary>
    internal static class KindExtensions
    {
        /// <summary>
        /// Returns the <see cref="ImageMoniker"/> from <see cref="KnownMonikers"/> that best
        /// represents the given <paramref name="kind"/> and <paramref name="visibility"/>.
        /// </summary>
        internal static ImageMoniker GetImageMoniker(this Kind kind, Modifiers visibility)
        {
            switch (kind)
            {
                case Kind.Class:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.ClassPublic, KnownMonikers.ClassProtected,
                        KnownMonikers.ClassPrivate, KnownMonikers.ClassInternal);

                case Kind.Structure:
                case Kind.VOStruct:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.ValueTypePublic, KnownMonikers.ValueTypeProtected,
                        KnownMonikers.ValueTypePrivate, KnownMonikers.ValueTypeInternal);

                case Kind.Interface:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.InterfacePublic, KnownMonikers.InterfaceProtected,
                        KnownMonikers.InterfacePrivate, KnownMonikers.InterfaceInternal);

                case Kind.Delegate:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.DelegatePublic, KnownMonikers.DelegateProtected,
                        KnownMonikers.DelegatePrivate, KnownMonikers.DelegateInternal);

                case Kind.Enum:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.EnumerationPublic, KnownMonikers.EnumerationProtected,
                        KnownMonikers.EnumerationPrivate, KnownMonikers.EnumerationInternal);

                case Kind.EnumMember:
                    return KnownMonikers.EnumerationItemPublic;

                case Kind.Constructor:
                case Kind.Destructor:
                case Kind.Method:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.MethodPublic, KnownMonikers.MethodProtected,
                        KnownMonikers.MethodPrivate, KnownMonikers.MethodInternal);

                case Kind.Function:
                case Kind.Procedure:
                case Kind.LocalFunc:
                case Kind.LocalProc:
                    // Global/module-level routines have no class visibility
                    return KnownMonikers.MethodPublic;

                case Kind.Access:
                case Kind.Assign:
                case Kind.Property:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.PropertyPublic, KnownMonikers.PropertyProtected,
                        KnownMonikers.PropertyPrivate, KnownMonikers.PropertyInternal);

                case Kind.Event:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.EventPublic, KnownMonikers.EventProtected,
                        KnownMonikers.EventPrivate, KnownMonikers.EventInternal);

                case Kind.Field:
                case Kind.VOGlobal:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.FieldPublic, KnownMonikers.FieldProtected,
                        KnownMonikers.FieldPrivate, KnownMonikers.FieldInternal);

                case Kind.Operator:
                    return KnownMonikers.Operator;

                case Kind.Namespace:
                case Kind.Using:
                    return KnownMonikers.Namespace;

                case Kind.Union:
                    return KnownMonikers.Union;

                case Kind.VODefine:
                case Kind.Define:
                case Kind.Undefine:
                    return VisibilityMoniker(visibility,
                        KnownMonikers.ConstantPublic, KnownMonikers.ConstantProtected,
                        KnownMonikers.ConstantPrivate, KnownMonikers.ConstantInternal);

                case Kind.Command:
                case Kind.XCommand:
                case Kind.YCommand:
                case Kind.Translate:
                case Kind.XTranslate:
                case Kind.YTranslate:
                    return KnownMonikers.MacroPublic;

                case Kind.Include:
                    return KnownMonikers.Library;

                case Kind.Keyword:
                    return KnownMonikers.IntellisenseKeyword;

                case Kind.Attribute:
                    return KnownMonikers.Attribute;

                case Kind.TypeParameter:
                    return KnownMonikers.Type;

                case Kind.Local:
                case Kind.Parameter:
                case Kind.MemVar:
                case Kind.DbField:
                case Kind.Undeclared:
                    return KnownMonikers.LocalVariable;

                default:
                    return KnownMonikers.Item;
            }
        }

        /// <summary>
        /// Picks one of four visibility-specific monikers based on <paramref name="visibility"/>.
        /// <see cref="Modifiers.ProtectedInternal"/> maps to the <paramref name="protectedMoniker"/>
        /// because that is the closest visual equivalent available in the catalog.
        /// Any other value (including <see cref="Modifiers.None"/> and <see cref="Modifiers.Public"/>)
        /// returns <paramref name="publicMoniker"/>.
        /// </summary>
        private static ImageMoniker VisibilityMoniker(Modifiers visibility,
            ImageMoniker publicMoniker, ImageMoniker protectedMoniker,
            ImageMoniker privateMoniker, ImageMoniker internalMoniker)
        {
            switch (visibility)
            {
                case Modifiers.Protected:
                case Modifiers.ProtectedInternal:
                    return protectedMoniker;
                case Modifiers.Private:
                    return privateMoniker;
                case Modifiers.Internal:
                    return internalMoniker;
                default:
                    return publicMoniker;
            }
        }

        /// <summary>
        /// Converts an <see cref="ImageMoniker"/> to a WPF <see cref="ImageSource"/> at 16×16 logical pixels
        /// using the VS image service.  Returns <c>null</c> when the image service is unavailable.
        /// </summary>
        internal static ImageSource GetImageSource(this ImageMoniker moniker)
        {
            IVsImageService2 imageService = null;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                imageService = await VS.GetServiceAsync<SVsImageService, IVsImageService2>();
            });

            if (imageService == null)
                return null;

            var attributes = new ImageAttributes
            {
                StructSize = Marshal.SizeOf(typeof(ImageAttributes)),
                ImageType   = (uint)_UIImageType.IT_Bitmap,
                Format      = (uint)_UIDataFormat.DF_WPF,
                LogicalWidth  = 16,
                LogicalHeight = 16,
                Flags = (uint)_ImageAttributesFlags.IAF_RequiredFlags,
            };
            IVsUIObject uiObject = null;
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                uiObject = imageService.GetImage(moniker, attributes);
                if (uiObject == null)
                    return null;
                uiObject.get_Data(out object data);
                return data as BitmapSource;
            });


        }
    }
}
