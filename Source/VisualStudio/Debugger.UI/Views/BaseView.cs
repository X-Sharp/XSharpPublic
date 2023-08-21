//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Debugger.Support;
using Microsoft.VisualStudio.PlatformUI;
using Microsoft.VisualStudio.Shell;
using Newtonsoft.Json.Linq;
using System.Collections.Generic;
using System.Linq;

namespace XSharp.Debugger.UI
{
    internal class BaseView : ObservableObject
    {
        bool _lastLoaded = false;
        internal bool IsRTLoaded
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    var loaded = await Support.IsRTLoadedAsync();
                    if (loaded != _lastLoaded)
                    {
                        SetProperty(ref _lastLoaded, loaded);
                        NotifyPropertyChanged(nameof(IsRTLoaded));
                    }
                    return loaded;

                });
            }
        }
    }
}
