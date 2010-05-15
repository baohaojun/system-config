//
// CairoFu.cs
//
// Copyright (C) 2007 Novell, Inc.
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using System.Reflection;
using System.Runtime.InteropServices;

using Gdk;
using Cairo;

namespace Beagle.Search {
    
	public static class CairoFu {        

		public static Cairo.Color GdkColorToCairoColor (Gdk.Color color)
		{
			return GdkColorToCairoColor (color, 1.0);
		}
        
		public static Cairo.Color GdkColorToCairoColor (Gdk.Color color, double alpha)
		{
			return new Cairo.Color ((double)(color.Red >> 8) / 255.0,
						(double)(color.Green >> 8) / 255.0,
						(double)(color.Blue >> 8) / 255.0,
						alpha);
		}
        
		public static void HsbFromColor (Cairo.Color color, out double hue, out double saturation, out double brightness)
		{
			double min, max, delta;
			double red = color.R, green = color.G, blue = color.B;
			
			hue = 0;
			saturation = 0;
			brightness = 0;
			
			if (red > green) {
				max = Math.Max (red, blue);
				min = Math.Min (green, blue);
			} else {
				max = Math.Max (green, blue);
				min = Math.Min (red, blue);
			}
			
			brightness = (max + min) / 2;
			
			if (Math.Abs (max - min) < 0.0001) {
				hue = 0;
				saturation = 0;
			} else {
				saturation = brightness <= 0.5 ? (max - min) / (max + min) : (max - min) / (2 - max - min);
				delta = max - min;
				
				if (red == max) {
					hue = (green - blue) / delta;
				} else if (green == max) {
					hue = 2 + (blue - red) / delta;
				} else if (blue == max) {
					hue = 4 + (red - green) / delta;
				}
				
				hue *= 60;

				if(hue < 0)
					hue += 360;
			}
		}
		
		private static double Modula (double number, double divisor)
		{
			return ((int)number % divisor) + (number - (int)number);
		}
		
		public static Cairo.Color ColorFromHsb (double hue, double saturation, double brightness)
		{
			double [] hue_shift = { 0, 0, 0 };
			double [] color_shift = { 0, 0, 0 };
			double m1, m2, m3;
			int i;
			
			m2 = brightness <= 0.5 ? brightness * (1 + saturation) : brightness + saturation - brightness * saturation;
			m1 = 2 * brightness - m2;
			
			hue_shift[0] = hue + 120;
			hue_shift[1] = hue;
			hue_shift[2] = hue - 120;
			
			color_shift[0] = color_shift[1] = color_shift[2] = brightness;
			
			i = saturation == 0 ? 3 : 0;
			
			while (i < 3) {
				m3 = hue_shift[i];
				
				if(m3 > 360) {
					m3 = Modula (m3, 360);
				} else if (m3 < 0) {
					m3 = 360 - Modula (Math.Abs (m3), 360);
				}
				
				if (m3 < 60) {
					color_shift[i] = m1 + (m2 - m1) * m3 / 60;
				} else if (m3 < 180) {
					color_shift[i] = m2;
				} else if (m3 < 240) {
					color_shift[i] = m1 + (m2 - m1) * (240 - m3) / 60;
				} else {
					color_shift[i] = m1;
				}

				i++;
			}
			
			return new Cairo.Color (color_shift[0], color_shift[1], color_shift[2]);
		}
		
		public static Cairo.Color ColorShade (Cairo.Color @base, double ratio)
		{
			double h, s, b;
			
			HsbFromColor (@base, out h, out s, out b);
			
			b = Math.Max (Math.Min (b * ratio, 1), 0);
			s = Math.Max (Math.Min (s * ratio, 1), 0);
			
			return ColorFromHsb (h, s, b);
		}
		
		public static Cairo.Color ColorAdjustBrightness (Cairo.Color @base, double brightness)
		{
			double h, s, b;

			HsbFromColor (@base, out h, out s, out b);
			b = Math.Max (Math.Min (brightness, 1), 0);

			return ColorFromHsb (h, s, b);
		}
		
		public static string ColorGetHex (Cairo.Color color, bool has_alpha)
		{
			if (has_alpha) {
				return String.Format ("#{0:x2}{1:x2}{2:x2}{3:x2}", (byte)(color.R * 255), (byte)(color.G * 255), 
						      (byte)(color.B * 255), (byte)(color.A * 255));
			}

			return String.Format ("#{0:x2}{1:x2}{2:x2}", (byte)(color.R * 255), (byte)(color.G * 255), 
					      (byte)(color.B * 255));
		}
		
		public static void RoundedRectangle (Cairo.Context cr, double x, double y, double w, double h, double r)
		{
			RoundedRectangle (cr, x, y, w, h, r, CairoCorners.All);
		}
		
		public static void RoundedRectangle (Cairo.Context cr, double x, double y, double w, double h, 
						     double r, CairoCorners corners)
		{
			if (r < 0.0001 || corners == CairoCorners.None) {
				cr.Rectangle (x, y, w, h);
				return;
			}
			
			if ((corners & CairoCorners.TopLeft) != 0) {
				cr.MoveTo (x + r, y);
			} else {
				cr.MoveTo (x, y);
			}
			
			if ((corners & CairoCorners.TopRight) != 0) {
				cr.Arc (x + w - r, y + r, r, Math.PI * 1.5, Math.PI * 2);
			} else {
				cr.LineTo (x + w, y);
			}
			
			if ((corners & CairoCorners.BottomRight) != 0) {
				cr.Arc (x + w - r, y + h - r, r, 0, Math.PI * 0.5);
			} else {
				cr.LineTo (x + w, y + h);
			}
			
			if ((corners & CairoCorners.BottomLeft) != 0) {
				cr.Arc (x + r, y + h - r, r, Math.PI * 0.5, Math.PI);
			} else {
				cr.LineTo (x, y + h);
			}
			
			if ((corners & CairoCorners.TopLeft) != 0) {
				cr.Arc (x + r, y + r, r, Math.PI, Math.PI * 1.5);
			} else {
				cr.LineTo (x, y);
			}
		}

		public static void RoundedSelection (Cairo.Context cr, Gtk.Widget widget, double x, double y, double w, double h, double a)
		{
			Cairo.Color selection_color = CairoFu.GdkColorToCairoColor (widget.Style.Backgrounds [(int)Gtk.StateType.Selected]);

			Cairo.Color selection_stroke = CairoFu.ColorShade (selection_color, 0.85);
			Cairo.Color selection_fill_light = CairoFu.ColorShade (selection_color, 1.1);
			Cairo.Color selection_fill_dark = CairoFu.ColorShade (selection_color, 0.90);
			selection_stroke.A = selection_fill_light.A = selection_fill_dark.A = a;
			
			Cairo.LinearGradient pattern = new Cairo.LinearGradient (x, y, x, y + h);
			pattern.AddColorStop (0, selection_fill_light);
			pattern.AddColorStop (1, selection_fill_dark);
			
			RoundedRectangle (cr, x, y, w, h, 3);
			cr.Pattern = pattern;
			cr.Fill ();
			
			RoundedRectangle (cr, x + 0.5, y + 0.5, w - 0.5, h - 0.5, 3);
			cr.Color = selection_stroke;
			cr.LineWidth = 1.0;
			cr.Stroke ();
		}

		public static void RoundedSelection (Cairo.Context cr, Gtk.Widget widget, double x, double y, double w, double h)
		{
			RoundedSelection (cr, widget, x, y, w, h, 1.0);
		}

		public static void DisposeContext (Cairo.Context cr)
		{
			((IDisposable)cr.Target).Dispose ();
			((IDisposable)cr).Dispose ();
		}
		
		private struct CairoInteropCall
		{
			public string Name;
			public MethodInfo ManagedMethod;
			public bool CallNative;
			
			public CairoInteropCall (string name)
			{
				Name = name;
				ManagedMethod = null;
				CallNative = false;
			}
		}
		
		private static bool CallCairoMethod (Cairo.Context cr, ref CairoInteropCall call)
		{
			if (call.ManagedMethod == null && !call.CallNative) {
				MemberInfo [] members = typeof (Cairo.Context).GetMember (call.Name, MemberTypes.Method, 
											  BindingFlags.InvokeMethod | BindingFlags.Instance | BindingFlags.Public);
				
				if (members != null && members.Length > 0 && members[0] is MethodInfo) {
					call.ManagedMethod = (MethodInfo)members[0];
				} else {
					call.CallNative = true;
				}
			}
			
			if (call.ManagedMethod != null) {
				call.ManagedMethod.Invoke (cr, null);
				return true;
			}
			
			return false;
		}
		
		[DllImport ("libcairo.so.2")]
		private static extern void cairo_push_group (IntPtr ptr);
		private static CairoInteropCall cairo_push_group_call = new CairoInteropCall ("PushGroup");
        
		public static void PushGroup (Cairo.Context cr)
		{
			if (!CallCairoMethod (cr, ref cairo_push_group_call)) {
				cairo_push_group (cr.Handle);
			}
		}
        
		[DllImport ("libcairo.so.2")]
		private static extern void cairo_pop_group_to_source (IntPtr ptr);
		private static CairoInteropCall cairo_pop_group_to_source_call = new CairoInteropCall ("PopGroupToSource");
		
		public static void PopGroupToSource (Cairo.Context cr)
		{
			if (!CallCairoMethod (cr, ref cairo_pop_group_to_source_call)) {
				cairo_pop_group_to_source (cr.Handle);
			}
		}
	}

	[Flags]
	public enum CairoCorners
	{
		None = 0,
		TopLeft = 1,
		TopRight = 2,
		BottomLeft = 4,
		BottomRight = 8,
		All = 15
	}
}
