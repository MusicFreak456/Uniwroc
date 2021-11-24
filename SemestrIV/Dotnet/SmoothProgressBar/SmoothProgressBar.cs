using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace SmoothProgressBar {
    public class SmoothProgressBar : Control{
        private int value;
        private int min;
        private int max;

        [Category("Data")]
        public int Max {
            get {
                return max;
            }
            set {
                if (value != this.max) {
                    this.max = value;
                    this.Invalidate();
                }
            }
        }
        [Category("Data")]
        public int Min {
            get {
                return min;
            }
            set {
                if (value != this.min) {
                    this.min = value;
                    this.Invalidate();
                }
            }
        }
        [Category("Data")]
        public int Value {
            get {
                return value;
            }
            set {
                if (value != this.value) {
                    this.value = value;
                    this.Invalidate();
                }
            }
        }

        public SmoothProgressBar() : base() {
            this.Max = 100;
            this.Min = 0;
            this.Value = 0;
        }

        protected override void OnPaint(PaintEventArgs pevent) {
            int controlWidth = this.Width;
            int controlHeight = this.Height;

            int range = this.Max - this.Min;
            int relativeValue = this.Value - this.Min;
            float fraction = (float)relativeValue / (float)range;
            float newWidth = fraction * controlWidth;

            using (Pen p = new Pen(Color.Black)) {
                pevent.Graphics.DrawRectangle(p, 0, 0, controlWidth - 1, controlHeight - 1);
            }

            if (newWidth > controlWidth) newWidth = controlWidth;
            if (controlWidth - 1 <= 0 ||
                controlHeight - 1 <= 0 ||
                newWidth <= 0) return;

            Rectangle bar = new Rectangle(0, 0, (int)newWidth, controlHeight - 1);
            using (LinearGradientBrush b = new LinearGradientBrush(bar, Color.LightBlue, Color.DarkBlue, 0f)) {
                pevent.Graphics.FillRectangle(b, bar);
            }


            pevent.Graphics.DrawRectangle(Pens.Black, bar);
            base.OnPaint(pevent);
        }

        protected override void OnResize(EventArgs e) {
            this.Invalidate();
            base.OnResize(e);
        }
    }
}
