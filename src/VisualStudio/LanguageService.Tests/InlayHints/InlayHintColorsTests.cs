using System.Windows.Media;
using XSharp.LanguageService.InlayHints;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class InlayHintColorsTests
    {
        private static readonly Brush CSharpText = new SolidColorBrush(Color.FromRgb(0x68, 0x68, 0x68));
        private static readonly Brush CSharpBack = new SolidColorBrush(Color.FromRgb(0xE6, 0xE6, 0xE6));
        private static readonly Brush XSharpText = new SolidColorBrush(Colors.Red);
        private static readonly Brush XSharpBack = new SolidColorBrush(Colors.Blue);

        [Fact]
        public void TheColoursOfTheCSharpHintsAreUsed()
        {
            // the C# item follows the theme and the user's choice in Fonts and Colors
            var (text, back) = InlayHintColors.Pick(CSharpText, CSharpBack, XSharpText, XSharpBack);
            Assert.Same(CSharpText, text);
            Assert.Same(CSharpBack, back);
        }

        [Fact]
        public void WithoutTheCSharpItemTheXSharpColoursAreUsed()
        {
            var (text, back) = InlayHintColors.Pick(null, null, XSharpText, XSharpBack);
            Assert.Same(XSharpText, text);
            Assert.Same(XSharpBack, back);
        }

        [Fact]
        public void AMissingBackgroundIsTransparent()
        {
            var (text, back) = InlayHintColors.Pick(CSharpText, null, XSharpText, XSharpBack);
            Assert.Same(CSharpText, text);
            Assert.Same(Brushes.Transparent, back);
        }

        [Fact]
        public void NoColoursAtAllIsGreyText()
        {
            var (text, back) = InlayHintColors.Pick(null, null, null, null);
            Assert.Same(Brushes.Gray, text);
            Assert.Same(Brushes.Transparent, back);
        }
    }
}
