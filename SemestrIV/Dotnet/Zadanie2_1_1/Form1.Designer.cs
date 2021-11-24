
namespace Zadanie2_1_1 {
    partial class Form1 {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        ///  Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing) {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent() {
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.txtAddress = new System.Windows.Forms.TextBox();
            this.txtName = new System.Windows.Forms.TextBox();
            this.btAccept = new System.Windows.Forms.Button();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.chbCompl = new System.Windows.Forms.CheckBox();
            this.chbFull = new System.Windows.Forms.CheckBox();
            this.label3 = new System.Windows.Forms.Label();
            this.cmbCycle = new System.Windows.Forms.ComboBox();
            this.btCancel = new System.Windows.Forms.Button();
            this.fileSystemWatcher1 = new System.IO.FileSystemWatcher();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.fileSystemWatcher1)).BeginInit();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.txtAddress);
            this.groupBox1.Controls.Add(this.txtName);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(387, 112);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Nazwa:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(18, 73);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(40, 15);
            this.label2.TabIndex = 3;
            this.label2.Text = "Adres:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(18, 30);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(45, 15);
            this.label1.TabIndex = 2;
            this.label1.Text = "Nazwa:";
            // 
            // txtAddress
            // 
            this.txtAddress.Location = new System.Drawing.Point(69, 65);
            this.txtAddress.Name = "txtAddress";
            this.txtAddress.Size = new System.Drawing.Size(303, 23);
            this.txtAddress.TabIndex = 1;
            this.txtAddress.TextChanged += new System.EventHandler(this.txtAddress_TextChanged);
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(69, 27);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(303, 23);
            this.txtName.TabIndex = 0;
            this.txtName.TextChanged += new System.EventHandler(this.txtName_TextChanged);
            // 
            // btAccept
            // 
            this.btAccept.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.btAccept.Location = new System.Drawing.Point(12, 241);
            this.btAccept.Name = "btAccept";
            this.btAccept.Size = new System.Drawing.Size(75, 23);
            this.btAccept.TabIndex = 1;
            this.btAccept.Text = "Akceptuj";
            this.btAccept.UseVisualStyleBackColor = true;
            this.btAccept.Click += new System.EventHandler(this.btAccept_Click);
            // 
            // groupBox2
            // 
            this.groupBox2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox2.Controls.Add(this.chbCompl);
            this.groupBox2.Controls.Add(this.chbFull);
            this.groupBox2.Controls.Add(this.label3);
            this.groupBox2.Controls.Add(this.cmbCycle);
            this.groupBox2.Location = new System.Drawing.Point(12, 130);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(387, 82);
            this.groupBox2.TabIndex = 1;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Rodzaj studiów";
            // 
            // chbCompl
            // 
            this.chbCompl.AutoSize = true;
            this.chbCompl.Location = new System.Drawing.Point(171, 52);
            this.chbCompl.Name = "chbCompl";
            this.chbCompl.Size = new System.Drawing.Size(98, 19);
            this.chbCompl.TabIndex = 4;
            this.chbCompl.Text = "uzupełniające";
            this.chbCompl.UseVisualStyleBackColor = true;
            this.chbCompl.CheckedChanged += new System.EventHandler(this.chbCompl_CheckedChanged);
            // 
            // chbFull
            // 
            this.chbFull.AutoSize = true;
            this.chbFull.Location = new System.Drawing.Point(89, 52);
            this.chbFull.Name = "chbFull";
            this.chbFull.Size = new System.Drawing.Size(67, 19);
            this.chbFull.TabIndex = 3;
            this.chbFull.Text = "dzienne";
            this.chbFull.UseVisualStyleBackColor = true;
            this.chbFull.CheckedChanged += new System.EventHandler(this.chbFull_CheckedChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(10, 25);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(65, 15);
            this.label3.TabIndex = 2;
            this.label3.Text = "Cykl nauki:";
            // 
            // cmbCycle
            // 
            this.cmbCycle.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbCycle.FormattingEnabled = true;
            this.cmbCycle.Items.AddRange(new object[] {
            "3-letnie",
            "3.5-letnie",
            "5-letnie"});
            this.cmbCycle.Location = new System.Drawing.Point(89, 22);
            this.cmbCycle.Name = "cmbCycle";
            this.cmbCycle.Size = new System.Drawing.Size(283, 23);
            this.cmbCycle.TabIndex = 1;
            this.cmbCycle.SelectedIndexChanged += new System.EventHandler(this.cmbCycle_SelectedIndexChanged);
            // 
            // btCancel
            // 
            this.btCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btCancel.Location = new System.Drawing.Point(324, 241);
            this.btCancel.Name = "btCancel";
            this.btCancel.Size = new System.Drawing.Size(75, 23);
            this.btCancel.TabIndex = 2;
            this.btCancel.Text = "Anuluj";
            this.btCancel.UseVisualStyleBackColor = true;
            this.btCancel.Click += new System.EventHandler(this.btCancel_Click);
            // 
            // fileSystemWatcher1
            // 
            this.fileSystemWatcher1.EnableRaisingEvents = true;
            this.fileSystemWatcher1.SynchronizingObject = this;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(411, 276);
            this.Controls.Add(this.btCancel);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.btAccept);
            this.Controls.Add(this.groupBox1);
            this.Cursor = System.Windows.Forms.Cursors.Default;
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "Form1";
            this.Text = "Wybór uczelni";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.fileSystemWatcher1)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button btAccept;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Button btCancel;
        private System.Windows.Forms.TextBox txtAddress;
        private System.Windows.Forms.TextBox txtName;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.IO.FileSystemWatcher fileSystemWatcher1;
        private System.Windows.Forms.ComboBox cmbCycle;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.CheckBox chbCompl;
        private System.Windows.Forms.CheckBox chbFull;
    }
}

