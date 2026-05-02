// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using XSharpModel;
using XSharp.Settings;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class GeneratorOptionsControl : XSUserControl
    {
        public GeneratorOptionsControl()
        {
            InitializeComponent();
        }

        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            var page = (GeneratorOptionsPage)optionPage;
            switch ((PublicStyle)page.Options.PublicStyle)
            {
                case PublicStyle.Export: rbExport.IsChecked = true; break;
                case PublicStyle.None:   rbNone.IsChecked   = true; break;
                default:                 rbPublic.IsChecked = true; break;
            }
            switch ((PrivateStyle)page.Options.PrivateStyle)
            {
                case PrivateStyle.Hidden: rbHidden.IsChecked  = true; break;
                default:                  rbPrivate.IsChecked = true; break;
            }
        }

        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
            var opts = (GeneratorOptions)options;
            if      (rbExport.IsChecked == true) opts.PublicStyle  = (int)PublicStyle.Export;
            else if (rbNone.IsChecked   == true) opts.PublicStyle  = (int)PublicStyle.None;
            else                                 opts.PublicStyle  = (int)PublicStyle.Public;
            if (rbHidden.IsChecked      == true) opts.PrivateStyle = (int)PrivateStyle.Hidden;
            else                                 opts.PrivateStyle = (int)PrivateStyle.Private;
        }
    }
}
