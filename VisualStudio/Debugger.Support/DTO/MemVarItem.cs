using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Debugger.Support
{
    [DebuggerDisplay("{Name,nq} {Value,nq}")]
    public sealed class MemvarItem
    {
        public string Name { get; set; }
        public MemvarType Type { get; set; }
        public string Value { get; set; }
    }

    public enum MemvarType
    {
        Public = 1,
        Private = 2
    }
    public sealed class MemvarItems
    {
        private List<MemvarItem> items;
        public MemvarItems()
        {
            items = new List<MemvarItem>();
        }
        public MemvarItem[] Items
        {
            get
            {
                return items.ToArray();
            }
            set
            {
                items = new List<MemvarItem>();
                items.AddRange(value);
            }
        }
        public void Add(MemvarItem item)
        {
            items.Add(item);
        }
        public string Serialize()
        {
            var sb = new StringBuilder();
            bool first = true;
            foreach (var item in this.Items)
            {
                if (first)
                    first = false;
                else
                    sb.Append("|");
                sb.Append(item.Name);
                sb.Append(",");
                sb.Append(item.Type.ToString());
                sb.Append(",");
                sb.Append(item.Value.ToString());
            }
            return sb.ToString();
        }
        public static MemvarItems Deserialize(string str)
        {
            var result = new MemvarItems();
            var items = str.Split('|');
            foreach (var item in items)
            {
                var fields = item.Split(',');
                if (fields.Length >= 3)
                {
                    var mv = new MemvarItem();
                    mv.Name = fields[0];
                    mv.Type = fields[1] == "Public" ? MemvarType.Public : MemvarType.Private;
                    mv.Value = fields[2];
                    result.Add(mv);
                }
            }
            return result;
        }

    }

}
