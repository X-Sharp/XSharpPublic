namespace XSharp.LanguageService.OptionsPages
{
    partial class OtherOptionsControl
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.chkSingleLineDividers = new System.Windows.Forms.CheckBox();
            this.chkShowDividers = new System.Windows.Forms.CheckBox();
            this.chkHighlightWord = new System.Windows.Forms.CheckBox();
            this.chkBraceMatching = new System.Windows.Forms.CheckBox();
            this.chkKeywordMatching = new System.Windows.Forms.CheckBox();
            this.chkLightBulbs = new System.Windows.Forms.CheckBox();
            this.chkQuickInfo = new System.Windows.Forms.CheckBox();
            this.chkParameters = new System.Windows.Forms.CheckBox();
            this.chkCompletion = new System.Windows.Forms.CheckBox();
            this.chkRegions = new System.Windows.Forms.CheckBox();
            this.chkAutoPairs = new System.Windows.Forms.CheckBox();
            this.chkAutoOpen = new System.Windows.Forms.CheckBox();
            this.grpOther = new System.Windows.Forms.GroupBox();
            this.label3 = new System.Windows.Forms.Label();
            this.panel1 = new System.Windows.Forms.Panel();
            this.grpOther.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // chkSingleLineDividers
            // 
            this.chkSingleLineDividers.AutoSize = true;
            this.chkSingleLineDividers.Location = new System.Drawing.Point(20, 44);
            this.chkSingleLineDividers.Name = "chkSingleLineDividers";
            this.chkSingleLineDividers.Size = new System.Drawing.Size(147, 17);
            this.chkSingleLineDividers.TabIndex = 1;
            this.chkSingleLineDividers.Text = "&Single line Entity Dividers ";
            this.toolTip1.SetToolTip(this.chkSingleLineDividers, "This applies to GLOBALs, DEFINEs, DELEGATES, CLASS Variables, MEMBERs of structur" +
        "es etc");
            this.chkSingleLineDividers.UseVisualStyleBackColor = true;
            // 
            // chkShowDividers
            // 
            this.chkShowDividers.AutoSize = true;
            this.chkShowDividers.Location = new System.Drawing.Point(20, 20);
            this.chkShowDividers.Name = "chkShowDividers";
            this.chkShowDividers.Size = new System.Drawing.Size(123, 17);
            this.chkShowDividers.TabIndex = 0;
            this.chkShowDividers.Text = "Show Entity &Dividers";
            this.toolTip1.SetToolTip(this.chkShowDividers, "The entity dividers will have the color that is defined in the Color setting as  " +
        "\"Outlining Margin Vertical Rule");
            this.chkShowDividers.UseVisualStyleBackColor = true;
            this.chkShowDividers.CheckedChanged += new System.EventHandler(this.chkShowDividers_CheckedChanged);
            // 
            // chkHighlightWord
            // 
            this.chkHighlightWord.AutoSize = true;
            this.chkHighlightWord.Location = new System.Drawing.Point(20, 67);
            this.chkHighlightWord.Name = "chkHighlightWord";
            this.chkHighlightWord.Size = new System.Drawing.Size(114, 17);
            this.chkHighlightWord.TabIndex = 2;
            this.chkHighlightWord.Text = "&Highlight identifiers";
            this.toolTip1.SetToolTip(this.chkHighlightWord, "Highlight matchign identifiers in the editor");
            this.chkHighlightWord.UseVisualStyleBackColor = true;
            // 
            // chkBraceMatching
            // 
            this.chkBraceMatching.AutoSize = true;
            this.chkBraceMatching.Location = new System.Drawing.Point(20, 90);
            this.chkBraceMatching.Name = "chkBraceMatching";
            this.chkBraceMatching.Size = new System.Drawing.Size(101, 17);
            this.chkBraceMatching.TabIndex = 3;
            this.chkBraceMatching.Text = "&Brace Matching";
            this.toolTip1.SetToolTip(this.chkBraceMatching, "Match braces");
            this.chkBraceMatching.UseVisualStyleBackColor = true;
            // 
            // chkKeywordMatching
            // 
            this.chkKeywordMatching.AutoSize = true;
            this.chkKeywordMatching.Location = new System.Drawing.Point(213, 19);
            this.chkKeywordMatching.Name = "chkKeywordMatching";
            this.chkKeywordMatching.Size = new System.Drawing.Size(137, 17);
            this.chkKeywordMatching.TabIndex = 6;
            this.chkKeywordMatching.Text = "Highlight &Keyword Pairs";
            this.toolTip1.SetToolTip(this.chkKeywordMatching, "Highlight keyword pairs, such as FOR and NEXT, IF and ENDIF etc.");
            this.chkKeywordMatching.UseVisualStyleBackColor = true;
            // 
            // chkLightBulbs
            // 
            this.chkLightBulbs.AutoSize = true;
            this.chkLightBulbs.Location = new System.Drawing.Point(213, 42);
            this.chkLightBulbs.Name = "chkLightBulbs";
            this.chkLightBulbs.Size = new System.Drawing.Size(78, 17);
            this.chkLightBulbs.TabIndex = 7;
            this.chkLightBulbs.Text = "&Light Bulbs";
            this.toolTip1.SetToolTip(this.chkLightBulbs, "Enable Light bulbs for suggestions for code manipulation");
            this.chkLightBulbs.UseVisualStyleBackColor = true;
            // 
            // chkQuickInfo
            // 
            this.chkQuickInfo.AutoSize = true;
            this.chkQuickInfo.Location = new System.Drawing.Point(213, 65);
            this.chkQuickInfo.Name = "chkQuickInfo";
            this.chkQuickInfo.Size = new System.Drawing.Size(98, 17);
            this.chkQuickInfo.TabIndex = 8;
            this.chkQuickInfo.Text = "&Quick Info Tips";
            this.toolTip1.SetToolTip(this.chkQuickInfo, "Enable Quick Info tips with hints about identifiers");
            this.chkQuickInfo.UseVisualStyleBackColor = true;
            // 
            // chkParameters
            // 
            this.chkParameters.AutoSize = true;
            this.chkParameters.Location = new System.Drawing.Point(213, 89);
            this.chkParameters.Name = "chkParameters";
            this.chkParameters.Size = new System.Drawing.Size(97, 17);
            this.chkParameters.TabIndex = 9;
            this.chkParameters.Text = "&Parameter Tips";
            this.toolTip1.SetToolTip(this.chkParameters, "Enable Parameter Tips");
            this.chkParameters.UseVisualStyleBackColor = true;
            // 
            // chkCompletion
            // 
            this.chkCompletion.AutoSize = true;
            this.chkCompletion.Location = new System.Drawing.Point(213, 112);
            this.chkCompletion.Name = "chkCompletion";
            this.chkCompletion.Size = new System.Drawing.Size(106, 17);
            this.chkCompletion.TabIndex = 10;
            this.chkCompletion.Text = "&Code Completion";
            this.toolTip1.SetToolTip(this.chkCompletion, "Enable Code completion for \'.\' and \':\' characters");
            this.chkCompletion.UseVisualStyleBackColor = true;
            // 
            // chkRegions
            // 
            this.chkRegions.AutoSize = true;
            this.chkRegions.Location = new System.Drawing.Point(213, 135);
            this.chkRegions.Name = "chkRegions";
            this.chkRegions.Size = new System.Drawing.Size(65, 17);
            this.chkRegions.TabIndex = 11;
            this.chkRegions.Text = "&Regions";
            this.toolTip1.SetToolTip(this.chkRegions, "Enable Regions in the Editor");
            this.chkRegions.UseVisualStyleBackColor = true;
            // 
            // chkAutoPairs
            // 
            this.chkAutoPairs.AutoSize = true;
            this.chkAutoPairs.Location = new System.Drawing.Point(20, 138);
            this.chkAutoPairs.Name = "chkAutoPairs";
            this.chkAutoPairs.Size = new System.Drawing.Size(109, 17);
            this.chkAutoPairs.TabIndex = 5;
            this.chkAutoPairs.Text = "Br&ace Completion";
            this.toolTip1.SetToolTip(this.chkAutoPairs, "Type closing brace or closing quote after typing opening character \\\'(\\\', \\\'{\\\', " +
        "\\\'[\\\' and quotes\"");
            this.chkAutoPairs.UseVisualStyleBackColor = true;
            // 
            // chkAutoOpen
            // 
            this.chkAutoOpen.AutoSize = true;
            this.chkAutoOpen.Location = new System.Drawing.Point(20, 115);
            this.chkAutoOpen.Name = "chkAutoOpen";
            this.chkAutoOpen.Size = new System.Drawing.Size(187, 17);
            this.chkAutoOpen.TabIndex = 4;
            this.chkAutoOpen.Text = "&Add \'(\' to methods in completionlist";
            this.toolTip1.SetToolTip(this.chkAutoOpen, "Automatically type \'(\' when selecting a method from a completion list");
            this.chkAutoOpen.UseVisualStyleBackColor = true;
            // 
            // grpOther
            // 
            this.grpOther.Controls.Add(this.chkAutoOpen);
            this.grpOther.Controls.Add(this.chkRegions);
            this.grpOther.Controls.Add(this.chkAutoPairs);
            this.grpOther.Controls.Add(this.label3);
            this.grpOther.Controls.Add(this.chkCompletion);
            this.grpOther.Controls.Add(this.chkParameters);
            this.grpOther.Controls.Add(this.chkQuickInfo);
            this.grpOther.Controls.Add(this.chkLightBulbs);
            this.grpOther.Controls.Add(this.chkKeywordMatching);
            this.grpOther.Controls.Add(this.chkBraceMatching);
            this.grpOther.Controls.Add(this.chkHighlightWord);
            this.grpOther.Controls.Add(this.chkSingleLineDividers);
            this.grpOther.Controls.Add(this.chkShowDividers);
            this.grpOther.Location = new System.Drawing.Point(6, 3);
            this.grpOther.Name = "grpOther";
            this.grpOther.Size = new System.Drawing.Size(363, 194);
            this.grpOther.TabIndex = 0;
            this.grpOther.TabStop = false;
            this.grpOther.Text = "Source Code Editor";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(17, 166);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(309, 13);
            this.label3.TabIndex = 12;
            this.label3.Text = "Most options require closing and reopening the editor window(s).";
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.grpOther);
            this.panel1.Location = new System.Drawing.Point(3, 3);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(380, 208);
            this.panel1.TabIndex = 1;
            // 
            // OtherOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.panel1);
            this.Name = "OtherOptionsControl";
            this.Size = new System.Drawing.Size(406, 223);
            this.grpOther.ResumeLayout(false);
            this.grpOther.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.GroupBox grpOther;
        private System.Windows.Forms.CheckBox chkSingleLineDividers;
        private System.Windows.Forms.CheckBox chkShowDividers;
        private System.Windows.Forms.CheckBox chkHighlightWord;
        private System.Windows.Forms.CheckBox chkBraceMatching;
        private System.Windows.Forms.CheckBox chkKeywordMatching;
        private System.Windows.Forms.CheckBox chkLightBulbs;
        private System.Windows.Forms.CheckBox chkQuickInfo;
        private System.Windows.Forms.CheckBox chkParameters;
        private System.Windows.Forms.CheckBox chkCompletion;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.CheckBox chkRegions;
        private System.Windows.Forms.CheckBox chkAutoPairs;
        private System.Windows.Forms.CheckBox chkAutoOpen;
        private System.Windows.Forms.Panel panel1;
    }
}
