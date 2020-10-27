using System.ComponentModel;

namespace XSharp.Project.Options
{
    internal class WindowEditorOptions : BaseOptionModel<WindowEditorOptions>
    {
        [Category("Grid Settings")]
        [DisplayName("Show Grid")]
        [Description("Show Grid in the Editor")]
        [DefaultValue(true)]
        public bool ShowGrid { get; set; } = true;

        [Category("Grid Settings")]
        [DisplayName("Grid distance Horizontal")]
        [Description("The Horizontal distance between the dots in the grid.")]
        [DefaultValue(8)]
        public int GridX{ get; set; } = 8;

        [Category("Grid Settings")]
        [DisplayName("Grid distance Horizontal")]
        [Description("The vertical distance between the dots in the grid.")]
        [DefaultValue(8)]
        public int GridY { get; set; } = 8;
        [Category("Paste Settings")]
        [DisplayName("Paste Offset Horizontal")]
        [Description("The Horizontal distance between the original control and the pasted control in dots.")]
        [DefaultValue(8)]
        public int PasteOffSetX { get; set; } = 8;

        [Category("Paste Settings")]
        [DisplayName("Paste OffSet Vertical")]
        [Description("The vertical distance between the original control and the pasted control in dots.")]
        [DefaultValue(8)]
        public int PasteOffSetY { get; set; } = 8;
    }


}
