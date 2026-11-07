using XSharp.LanguageService.InlayHints;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class InlayHintColorsTests
    {
        [Fact]
        public void OpaqueStoredBackgroundIsDrawnTranslucent()
        {
            // Fonts and Colors stores colours without alpha: the background comes back opaque, like the text
            Assert.Equal(InlayHintColors.BackgroundAlpha, InlayHintColors.DrawnAlpha(0xFF));
            Assert.True(InlayHintColors.BackgroundAlpha < 0x80);
        }

        [Fact]
        public void TranslucentBackgroundIsKept()
        {
            Assert.Equal(0x10, InlayHintColors.DrawnAlpha(0x10));
            Assert.Equal(0, InlayHintColors.DrawnAlpha(0));
        }
    }
}
