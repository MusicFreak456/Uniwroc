
namespace Zadanie2_1_2 {
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
            System.Windows.Forms.TreeNode treeNode8 = new System.Windows.Forms.TreeNode("Plik1");
            System.Windows.Forms.TreeNode treeNode9 = new System.Windows.Forms.TreeNode("Plik2");
            System.Windows.Forms.TreeNode treeNode10 = new System.Windows.Forms.TreeNode("Folder1", new System.Windows.Forms.TreeNode[] {
            treeNode8,
            treeNode9});
            System.Windows.Forms.TreeNode treeNode11 = new System.Windows.Forms.TreeNode("Plik");
            System.Windows.Forms.TreeNode treeNode12 = new System.Windows.Forms.TreeNode("Podfolder", new System.Windows.Forms.TreeNode[] {
            treeNode11});
            System.Windows.Forms.TreeNode treeNode13 = new System.Windows.Forms.TreeNode("Plik");
            System.Windows.Forms.TreeNode treeNode14 = new System.Windows.Forms.TreeNode("Folder2", new System.Windows.Forms.TreeNode[] {
            treeNode12,
            treeNode13});
            System.Windows.Forms.ListViewItem listViewItem3 = new System.Windows.Forms.ListViewItem(new string[] {
            "Element 1",
            "1",
            "2021.04.25"}, -1);
            System.Windows.Forms.ListViewItem listViewItem4 = new System.Windows.Forms.ListViewItem(new string[] {
            "Element 2",
            "2",
            "2021.04.25"}, -1);
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.progressBar1 = new System.Windows.Forms.ProgressBar();
            this.listView1 = new System.Windows.Forms.ListView();
            this.ColumnName = new System.Windows.Forms.ColumnHeader();
            this.Number = new System.Windows.Forms.ColumnHeader();
            this.Edited = new System.Windows.Forms.ColumnHeader();
            this.SuspendLayout();
            // 
            // treeView1
            // 
            this.treeView1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left)));
            this.treeView1.Indent = 60;
            this.treeView1.Location = new System.Drawing.Point(12, 12);
            this.treeView1.Name = "treeView1";
            treeNode8.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            treeNode8.Name = "Node2";
            treeNode8.Text = "Plik1";
            treeNode9.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            treeNode9.Name = "Node3";
            treeNode9.Text = "Plik2";
            treeNode10.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            treeNode10.Name = "Node0";
            treeNode10.Text = "Folder1";
            treeNode11.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
            treeNode11.Name = "Node5";
            treeNode11.Text = "Plik";
            treeNode12.BackColor = System.Drawing.Color.Silver;
            treeNode12.Name = "Node4";
            treeNode12.Text = "Podfolder";
            treeNode13.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            treeNode13.Name = "Node6";
            treeNode13.Text = "Plik";
            treeNode14.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            treeNode14.Name = "Node1";
            treeNode14.Text = "Folder2";
            this.treeView1.Nodes.AddRange(new System.Windows.Forms.TreeNode[] {
            treeNode10,
            treeNode14});
            this.treeView1.Size = new System.Drawing.Size(248, 212);
            this.treeView1.TabIndex = 0;
            // 
            // progressBar1
            // 
            this.progressBar1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.progressBar1.Location = new System.Drawing.Point(266, 196);
            this.progressBar1.Name = "progressBar1";
            this.progressBar1.Size = new System.Drawing.Size(412, 28);
            this.progressBar1.TabIndex = 1;
            this.progressBar1.Value = 20;
            // 
            // listView1
            // 
            this.listView1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.listView1.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ColumnName,
            this.Number,
            this.Edited});
            this.listView1.HideSelection = false;
            this.listView1.Items.AddRange(new System.Windows.Forms.ListViewItem[] {
            listViewItem3,
            listViewItem4});
            this.listView1.Location = new System.Drawing.Point(266, 12);
            this.listView1.Name = "listView1";
            this.listView1.Size = new System.Drawing.Size(412, 178);
            this.listView1.TabIndex = 2;
            this.listView1.UseCompatibleStateImageBehavior = false;
            this.listView1.View = System.Windows.Forms.View.Details;
            // 
            // ColumnName
            // 
            this.ColumnName.Text = "Nazwa";
            this.ColumnName.Width = 200;
            // 
            // Number
            // 
            this.Number.Text = "Numer";
            // 
            // Edited
            // 
            this.Edited.Text = "Edytowano";
            this.Edited.Width = 170;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(690, 236);
            this.Controls.Add(this.listView1);
            this.Controls.Add(this.progressBar1);
            this.Controls.Add(this.treeView1);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "Form1";
            this.Text = "Prezentacja";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TreeView treeView1;
        private System.Windows.Forms.ProgressBar progressBar1;
        private System.Windows.Forms.ListView listView1;
        private System.Windows.Forms.ColumnHeader ColumnName;
        private System.Windows.Forms.ColumnHeader Number;
        private System.Windows.Forms.ColumnHeader Edited;
    }
}

