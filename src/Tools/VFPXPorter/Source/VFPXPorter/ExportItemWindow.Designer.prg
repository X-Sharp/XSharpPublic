BEGIN NAMESPACE VFPXPorter
    PUBLIC PARTIAL CLASS ExportItemWindow	
        PRIVATE components	:=	NULL AS System.ComponentModel.IContainer
        PRIVATE resultText AS System.Windows.Forms.TextBox
        PRIVATE exportButton AS System.Windows.Forms.Button
        PRIVATE label2 AS System.Windows.Forms.Label
        PRIVATE scxButton AS System.Windows.Forms.Button
        PRIVATE outputButton AS System.Windows.Forms.Button
        PRIVATE label3 AS System.Windows.Forms.Label
        PRIVATE analysisButton AS System.Windows.Forms.Button
        PUBLIC scxPathTextBox AS System.Windows.Forms.TextBox
        PUBLIC outputPathTextBox AS System.Windows.Forms.TextBox
        PRIVATE infoStrip AS System.Windows.Forms.StatusStrip
        PRIVATE infoStripLabel AS System.Windows.Forms.ToolStripStatusLabel
        PRIVATE openButton AS System.Windows.Forms.Button
        PRIVATE backgroundExport AS System.ComponentModel.BackgroundWorker
        PRIVATE infoStripBar AS System.Windows.Forms.ToolStripProgressBar
        PRIVATE cancelBtn AS System.Windows.Forms.Button
        PRIVATE infoStripError AS System.Windows.Forms.ToolStripStatusLabel
        PRIVATE label1 AS System.Windows.Forms.Label
                                                                                                                                                                                                                                                                        
        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        PROTECTED METHOD Dispose(disposing AS LOGIC) AS VOID STRICT

            IF (disposing .AND. (components != NULL))
                components:Dispose()
            ENDIF
            SUPER:Dispose(disposing)
            RETURN

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        PRIVATE METHOD InitializeComponent() AS VOID STRICT
			SELF:label1	:=	System.Windows.Forms.Label{}
			SELF:resultText	:=	System.Windows.Forms.TextBox{}
			SELF:exportButton	:=	System.Windows.Forms.Button{}
			SELF:label2	:=	System.Windows.Forms.Label{}
			SELF:scxPathTextBox	:=	System.Windows.Forms.TextBox{}
			SELF:scxButton	:=	System.Windows.Forms.Button{}
			SELF:outputButton	:=	System.Windows.Forms.Button{}
			SELF:outputPathTextBox	:=	System.Windows.Forms.TextBox{}
			SELF:label3	:=	System.Windows.Forms.Label{}
			SELF:analysisButton	:=	System.Windows.Forms.Button{}
			SELF:infoStrip	:=	System.Windows.Forms.StatusStrip{}
			SELF:infoStripBar	:=	System.Windows.Forms.ToolStripProgressBar{}
			SELF:infoStripLabel	:=	System.Windows.Forms.ToolStripStatusLabel{}
			SELF:infoStripError	:=	System.Windows.Forms.ToolStripStatusLabel{}
			SELF:openButton	:=	System.Windows.Forms.Button{}
			SELF:backgroundExport	:=	System.ComponentModel.BackgroundWorker{}
			SELF:cancelBtn	:=	System.Windows.Forms.Button{}
			SELF:infoStrip:SuspendLayout()
			SELF:SuspendLayout()
			//	
			//	label1
			//	
			SELF:label1:AutoSize	:=	true
			SELF:label1:BackColor	:=	System.Drawing.Color.Transparent
			SELF:label1:Location	:=	System.Drawing.Point{556, 12}
			SELF:label1:Name	:=	"label1"
			SELF:label1:Size	:=	System.Drawing.Size{68, 17}
			SELF:label1:TabIndex	:=	0
			SELF:label1:Text	:=	"Analysis :"
			//	
			//	resultText
			//	
			SELF:resultText:Anchor	:=	((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 	;
						| System.Windows.Forms.AnchorStyles.Left) 	;
						| System.Windows.Forms.AnchorStyles.Right)))
			SELF:resultText:Location	:=	System.Drawing.Point{559, 33}
			SELF:resultText:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:resultText:Multiline	:=	true
			SELF:resultText:Name	:=	"resultText"
			SELF:resultText:ReadOnly	:=	true
			SELF:resultText:ScrollBars	:=	System.Windows.Forms.ScrollBars.Both
			SELF:resultText:Size	:=	System.Drawing.Size{257, 118}
			SELF:resultText:TabIndex	:=	1
			SELF:resultText:WordWrap	:=	false
			//	
			//	exportButton
			//	
			SELF:exportButton:Location	:=	System.Drawing.Point{188, 126}
			SELF:exportButton:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:exportButton:Name	:=	"exportButton"
			SELF:exportButton:Size	:=	System.Drawing.Size{75, 25}
			SELF:exportButton:TabIndex	:=	4
			SELF:exportButton:Text	:=	"Export"
			SELF:exportButton:UseVisualStyleBackColor	:=	true
			SELF:exportButton:Click	+=	System.EventHandler{ SELF, @exportButton_Click() }
			//	
			//	label2
			//	
			SELF:label2:AutoSize	:=	true
			SELF:label2:Location	:=	System.Drawing.Point{15, 11}
			SELF:label2:Name	:=	"label2"
			SELF:label2:Size	:=	System.Drawing.Size{69, 17}
			SELF:label2:TabIndex	:=	3
			SELF:label2:Text	:=	"Input Item"
			//	
			//	scxPathTextBox
			//	
			SELF:scxPathTextBox:Location	:=	System.Drawing.Point{17, 32}
			SELF:scxPathTextBox:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:scxPathTextBox:Name	:=	"scxPathTextBox"
			SELF:scxPathTextBox:Size	:=	System.Drawing.Size{327, 22}
			SELF:scxPathTextBox:TabIndex	:=	0
			//	
			//	scxButton
			//	
			SELF:scxButton:Location	:=	System.Drawing.Point{349, 32}
			SELF:scxButton:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:scxButton:Name	:=	"scxButton"
			SELF:scxButton:Size	:=	System.Drawing.Size{44, 23}
			SELF:scxButton:TabIndex	:=	1
			SELF:scxButton:Text	:=	"..."
			SELF:scxButton:UseVisualStyleBackColor	:=	true
			SELF:scxButton:Click	+=	System.EventHandler{ SELF, @scxButton_Click() }
			//	
			//	outputButton
			//	
			SELF:outputButton:Location	:=	System.Drawing.Point{349, 91}
			SELF:outputButton:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:outputButton:Name	:=	"outputButton"
			SELF:outputButton:Size	:=	System.Drawing.Size{44, 23}
			SELF:outputButton:TabIndex	:=	3
			SELF:outputButton:Text	:=	"..."
			SELF:outputButton:UseVisualStyleBackColor	:=	true
			SELF:outputButton:Click	+=	System.EventHandler{ SELF, @outputButton_Click() }
			//	
			//	outputPathTextBox
			//	
			SELF:outputPathTextBox:Location	:=	System.Drawing.Point{17, 91}
			SELF:outputPathTextBox:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:outputPathTextBox:Name	:=	"outputPathTextBox"
			SELF:outputPathTextBox:Size	:=	System.Drawing.Size{327, 22}
			SELF:outputPathTextBox:TabIndex	:=	2
			//	
			//	label3
			//	
			SELF:label3:AutoSize	:=	true
			SELF:label3:Location	:=	System.Drawing.Point{15, 70}
			SELF:label3:Name	:=	"label3"
			SELF:label3:Size	:=	System.Drawing.Size{95, 17}
			SELF:label3:TabIndex	:=	6
			SELF:label3:Text	:=	"Output Folder"
			//	
			//	analysisButton
			//	
			SELF:analysisButton:Location	:=	System.Drawing.Point{479, 33}
			SELF:analysisButton:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:analysisButton:Name	:=	"analysisButton"
			SELF:analysisButton:Size	:=	System.Drawing.Size{75, 25}
			SELF:analysisButton:TabIndex	:=	5
			SELF:analysisButton:Text	:=	"Analyze"
			SELF:analysisButton:UseVisualStyleBackColor	:=	true
			SELF:analysisButton:Click	+=	System.EventHandler{ SELF, @analysisButton_Click() }
			//	
			//	infoStrip
			//	
			SELF:infoStrip:ImageScalingSize	:=	System.Drawing.Size{20, 20}
			SELF:infoStrip:Items:AddRange(<System.Windows.Forms.ToolStripItem>{ SELF:infoStripBar, SELF:infoStripLabel, SELF:infoStripError })
			SELF:infoStrip:Location	:=	System.Drawing.Point{0, 169}
			SELF:infoStrip:Name	:=	"infoStrip"
			SELF:infoStrip:Padding	:=	System.Windows.Forms.Padding{1, 0, 13, 0}
			SELF:infoStrip:Size	:=	System.Drawing.Size{831, 27}
			SELF:infoStrip:TabIndex	:=	10
			SELF:infoStrip:Text	:=	"statusStrip1"
			//	
			//	infoStripBar
			//	
			SELF:infoStripBar:Name	:=	"infoStripBar"
			SELF:infoStripBar:Size	:=	System.Drawing.Size{100, 19}
			//	
			//	infoStripLabel
			//	
			SELF:infoStripLabel:Name	:=	"infoStripLabel"
			SELF:infoStripLabel:Size	:=	System.Drawing.Size{0, 21}
			//	
			//	infoStripError
			//	
			SELF:infoStripError:Name	:=	"infoStripError"
			SELF:infoStripError:Size	:=	System.Drawing.Size{0, 21}
			//	
			//	openButton
			//	
			SELF:openButton:Location	:=	System.Drawing.Point{269, 126}
			SELF:openButton:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:openButton:Name	:=	"openButton"
			SELF:openButton:Size	:=	System.Drawing.Size{75, 25}
			SELF:openButton:TabIndex	:=	11
			SELF:openButton:Text	:=	"Open"
			SELF:openButton:UseVisualStyleBackColor	:=	true
			SELF:openButton:Click	+=	System.EventHandler{ SELF, @openButton_Click() }
			//	
			//	backgroundExport
			//	
			SELF:backgroundExport:WorkerReportsProgress	:=	true
			SELF:backgroundExport:WorkerSupportsCancellation	:=	true
			SELF:backgroundExport:DoWork	+=	System.ComponentModel.DoWorkEventHandler{ SELF, @backgroundExport_DoWork() }
			SELF:backgroundExport:ProgressChanged	+=	System.ComponentModel.ProgressChangedEventHandler{ SELF, @backgroundExport_ProgressChanged() }
			SELF:backgroundExport:RunWorkerCompleted	+=	System.ComponentModel.RunWorkerCompletedEventHandler{ SELF, @backgroundExport_RunWorkerCompleted() }
			//	
			//	cancelBtn
			//	
			SELF:cancelBtn:Location	:=	System.Drawing.Point{107, 126}
			SELF:cancelBtn:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:cancelBtn:Name	:=	"cancelBtn"
			SELF:cancelBtn:Size	:=	System.Drawing.Size{75, 25}
			SELF:cancelBtn:TabIndex	:=	12
			SELF:cancelBtn:Text	:=	"Cancel"
			SELF:cancelBtn:UseVisualStyleBackColor	:=	true
			SELF:cancelBtn:Visible	:=	false
			SELF:cancelBtn:Click	+=	System.EventHandler{ SELF, @cancelBtn_Click() }
			//	
			//	ExportItemWindow
			//	
			SELF:AutoScaleDimensions	:=	System.Drawing.SizeF{8, 16}
			SELF:AutoScaleMode	:=	System.Windows.Forms.AutoScaleMode.Font
			SELF:ClientSize	:=	System.Drawing.Size{831, 196}
			SELF:Controls:Add(SELF:cancelBtn)
			SELF:Controls:Add(SELF:openButton)
			SELF:Controls:Add(SELF:infoStrip)
			SELF:Controls:Add(SELF:analysisButton)
			SELF:Controls:Add(SELF:outputButton)
			SELF:Controls:Add(SELF:outputPathTextBox)
			SELF:Controls:Add(SELF:label3)
			SELF:Controls:Add(SELF:scxButton)
			SELF:Controls:Add(SELF:scxPathTextBox)
			SELF:Controls:Add(SELF:label2)
			SELF:Controls:Add(SELF:exportButton)
			SELF:Controls:Add(SELF:resultText)
			SELF:Controls:Add(SELF:label1)
			SELF:Margin	:=	System.Windows.Forms.Padding{3, 2, 3, 2}
			SELF:Name	:=	"ExportItemWindow"
			SELF:Text	:=	"Export Form"
			SELF:infoStrip:ResumeLayout(false)
			SELF:infoStrip:PerformLayout()
			SELF:ResumeLayout(false)
			SELF:PerformLayout()
                                                            
        #endregion
    
    END CLASS 
END NAMESPACE
