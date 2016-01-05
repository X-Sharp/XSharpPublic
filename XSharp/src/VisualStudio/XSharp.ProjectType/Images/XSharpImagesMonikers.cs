using System;
using Microsoft.VisualStudio.Imaging.Interop;

namespace XSharpLanguage
{
    public static class XSharpImagesMonikers
    {
        private static readonly Guid ManifestGuid = new Guid("6c1120ab-b5cc-4593-9413-ad506eedcec7");

        private const int ProjectIcon = 1;
        private const int ItemIcon = 0;

        public static ImageMoniker ProjectIconImageMoniker
        {
            get
            {
                return new ImageMoniker { Guid = ManifestGuid, Id = ProjectIcon };
            }
        }

        public static ImageMoniker ItemIconImageMoniker
        {
            get
            {
                return new ImageMoniker { Guid = ManifestGuid, Id = ItemIcon };
            }
        }
    }
}
