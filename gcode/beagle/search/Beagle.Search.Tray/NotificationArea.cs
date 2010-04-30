//
//  NotificationAreaIcon.cs
// 
//  Copyright (C) 2005 Todd Berman <tberman@off.net>
//  Copyright (C) 2005 Ed Catmur <ed@catmur.co.uk>
//  Copyright (C) 2005 Novell, Inc. (Miguel de Icaza, Aaron Bockover)
//

//  NOTE: throughout IntPtr is used for the Xlib long type, as this tends to 
//  have the correct width and does not require any configure checks.

using System;
using System.Runtime.InteropServices;

using Gtk;
using Gdk;

namespace Beagle.Search.Tray {

	public class NotificationArea : Plug
	{
		private uint stamp;
		private Orientation orientation;
		
		private IntPtr selection_atom;
		private IntPtr manager_atom;
		private IntPtr system_tray_opcode_atom;
		private IntPtr orientation_atom;
		private IntPtr message_data_atom;
		private IntPtr manager_window;
		private FilterFunc filter;
		
		public NotificationArea (string name)
		{
			Title = name;
			Init ();
		}
	
		public NotificationArea (string name, Gdk.Screen screen)
		{
			Title = name;
			Screen = screen;
			Init ();
		}
		
		[DllImport ("libc.so.6")]
		private static extern IntPtr memcpy (ref XClientMessageEvent.DataUnion dest, IntPtr src, IntPtr len);

		public uint SendMessage (uint timeout, string message)
		{
			if (manager_window == IntPtr.Zero) {
				return 0;
			}
			
			byte[] arr = System.Text.Encoding.UTF8.GetBytes (message);
			IntPtr unmanaged_arr = Marshal.AllocHGlobal (arr.Length);
			Marshal.Copy (arr, 0, unmanaged_arr, arr.Length);

			SendManagerMessage (SystemTrayMessage.BeginMessage, (IntPtr) Id, timeout, (uint) arr.Length, ++stamp);

			gdk_error_trap_push ();
		
			for (int index = 0; index < message.Length; index += 20) {
				XClientMessageEvent ev = new XClientMessageEvent ();
			
				IntPtr display = gdk_x11_display_get_xdisplay (Display.Handle);
			
				ev.type = XEventName.ClientMessage;
				ev.window = (IntPtr) Id;
				ev.format = 8;
				ev.message_type = message_data_atom;
            
				int len = Math.Min (arr.Length - index, 20);
				memcpy (ref ev.data, (IntPtr)((int)unmanaged_arr + index), (IntPtr)len);
			
				XSendEvent (display, manager_window, false, (IntPtr) EventMask.StructureNotifyMask, ref ev);
				XSync (display, false);
			}
		
			gdk_error_trap_pop ();
		
			return stamp;
		}
	
		public void CancelMessage (uint id)
		{
			if (id == 0) {
				return;
			}
		
			SendManagerMessage (SystemTrayMessage.CancelMessage, (IntPtr) Id, id, 0, 0);
		}
	
		private void Init ()
		{
			stamp = 1;
			orientation = Orientation.Horizontal;
			AddEvents ((int)EventMask.PropertyChangeMask);
			filter = new FilterFunc (ManagerFilter);
		}

		[GLib.ConnectBefore]
		private void TransparentExposeEvent (object obj, Gtk.ExposeEventArgs args)
		{
			Gtk.Widget widget = (Gtk.Widget)obj;
			Gdk.Rectangle area = args.Event.Area;

			widget.GdkWindow.ClearArea (area.X, area.Y, area.Width, area.Height);
		}

		private void MakeTransparentAgain (object obj, Gtk.StyleSetArgs args)
		{
			Gtk.Widget widget = (Gtk.Widget)obj;

			widget.GdkWindow.SetBackPixmap (null, true);
		}
	
		private void MakeTransparent (object obj, EventArgs args)
		{
			Gtk.Widget widget = (Gtk.Widget)obj;
			if (widget.IsNoWindow || widget.IsAppPaintable)
				return;

			widget.AppPaintable = true;
			widget.DoubleBuffered = false;
			widget.GdkWindow.SetBackPixmap (null, true);
			widget.ExposeEvent += TransparentExposeEvent;
			widget.StyleSet += MakeTransparentAgain;
		}

		protected override void OnRealized ()
		{
			base.OnRealized ();
			MakeTransparent (this, EventArgs.Empty);
			Display display = Screen.Display;
			IntPtr xdisplay = gdk_x11_display_get_xdisplay (display.Handle);
			selection_atom = XInternAtom (xdisplay, "_NET_SYSTEM_TRAY_S" + Screen.Number.ToString (), false);
			manager_atom = XInternAtom (xdisplay, "MANAGER", false);
			system_tray_opcode_atom = XInternAtom (xdisplay, "_NET_SYSTEM_TRAY_OPCODE", false);
			orientation_atom = XInternAtom (xdisplay, "_NET_SYSTEM_TRAY_ORIENTATION", false);
			message_data_atom = XInternAtom (xdisplay, "_NET_SYSTEM_TRAY_MESSAGE_DATA", false);
			UpdateManagerWindow (false);
			SendDockRequest ();
			Screen.RootWindow.AddFilter (filter);
		}

		protected override void OnAdded (Gtk.Widget child)
		{
			child.Realized += MakeTransparent;
			base.OnAdded (child);
		}
	
		protected override void OnUnrealized ()
		{
			if (manager_window != IntPtr.Zero) {
				Gdk.Window gdkwin = Gdk.Window.ForeignNewForDisplay (Display, (uint)manager_window);
				if (gdkwin != null) {
					gdkwin.RemoveFilter (filter);
				}
			}
		
			Screen.RootWindow.RemoveFilter (filter);
			base.OnUnrealized ();
		}
	
		private void UpdateManagerWindow (bool dock_if_realized)
		{
			IntPtr xdisplay = gdk_x11_display_get_xdisplay (Display.Handle);
		
			if (manager_window != IntPtr.Zero) {
				return;
			}
		
			XGrabServer (xdisplay);
		
			manager_window = XGetSelectionOwner (xdisplay, selection_atom);
			if (manager_window != IntPtr.Zero) {
				XSelectInput (xdisplay, manager_window, (IntPtr) (EventMask.StructureNotifyMask | EventMask.PropertyChangeMask));
			}
		
			XUngrabServer (xdisplay);
			XFlush (xdisplay);

			if (manager_window != IntPtr.Zero) {
				Gdk.Window gdkwin = Gdk.Window.ForeignNewForDisplay (Display, (uint)manager_window);
				if (gdkwin != null) {
					gdkwin.AddFilter (filter);
				}
			
				if (dock_if_realized && IsRealized) {
					SendDockRequest ();
				}
			
				GetOrientationProperty ();
			}
		}
	
		private void SendDockRequest ()
		{
			SendManagerMessage (SystemTrayMessage.RequestDock, manager_window, Id, 0, 0);
		}
	
		private void SendManagerMessage (SystemTrayMessage message, IntPtr window, uint data1, uint data2, uint data3)
		{
			XClientMessageEvent ev = new XClientMessageEvent ();
			IntPtr display;
		
			ev.type = XEventName.ClientMessage;
			ev.window = window;
			ev.message_type = system_tray_opcode_atom;
			ev.format = 32;
			ev.data.ptr1 = (IntPtr)gdk_x11_get_server_time (GdkWindow.Handle);
			ev.data.ptr2 = (IntPtr)message;
			ev.data.ptr3 = (IntPtr)data1;
			ev.data.ptr4 = (IntPtr)data2;
			ev.data.ptr5 = (IntPtr)data3;

			display = gdk_x11_display_get_xdisplay (Display.Handle);
			gdk_error_trap_push ();
			XSendEvent (display, manager_window, false, (IntPtr) EventMask.NoEventMask, ref ev);
			XSync (display, false);
			gdk_error_trap_pop ();
		}

		private FilterReturn ManagerFilter (IntPtr xevent, Event evnt)
		{
			XAnyEvent xev = (XAnyEvent) Marshal.PtrToStructure (xevent, typeof(XAnyEvent));
        
			if (xev.type == XEventName.ClientMessage){
				XClientMessageEvent xclient = (XClientMessageEvent) Marshal.PtrToStructure (xevent, typeof(XClientMessageEvent));

				if (xclient.message_type == manager_atom && xclient.data.ptr2 == selection_atom) {
					UpdateManagerWindow (true);
					return FilterReturn.Continue;
				}
			}

			if (xev.window == manager_window) {
				if (xev.type == XEventName.PropertyNotify){
					XPropertyEvent xproperty = (XPropertyEvent) Marshal.PtrToStructure (xevent, typeof(XPropertyEvent));
					if (xproperty.atom == orientation_atom) {
						GetOrientationProperty();
						return FilterReturn.Continue;
					}
				}

				if (xev.type == XEventName.DestroyNotify) {
					ManagerWindowDestroyed();
				}
			}
        
			return FilterReturn.Continue;
		}

		private void ManagerWindowDestroyed ()
		{
			if (manager_window != IntPtr.Zero) {
				Gdk.Window gdkwin = Gdk.Window.ForeignNewForDisplay (Display, (uint) manager_window);
            
				if (gdkwin != null) {
					gdkwin.RemoveFilter (filter);
				}
            
				manager_window = IntPtr.Zero;
				UpdateManagerWindow (true);
			}
		}

		private void GetOrientationProperty ()
		{
			IntPtr display;
			IntPtr type;
			int format;
			IntPtr prop_return;
			IntPtr nitems, bytes_after;
			int error, result;

			if (manager_window == IntPtr.Zero) {
				return;
			}

			display = gdk_x11_display_get_xdisplay (Display.Handle);
        
			gdk_error_trap_push ();
			type = IntPtr.Zero;
        
			result = XGetWindowProperty (display, manager_window, orientation_atom, (IntPtr) 0, 
						     (IntPtr) System.Int32.MaxValue, false, (IntPtr) XAtom.Cardinal, out type, out format, 
						     out nitems, out bytes_after, out prop_return);
        
			error = gdk_error_trap_pop ();

			if (error != 0 || result != 0) {
				return;
			}

			if (type == (IntPtr) XAtom.Cardinal) {
				orientation = ((SystemTrayOrientation) Marshal.ReadInt32 (prop_return) == SystemTrayOrientation.Horz) 
					? Orientation.Horizontal 
					: Orientation.Vertical;
				if (OrientationChanged != null)
					OrientationChanged (this, orientation);
			}

			if (prop_return != IntPtr.Zero) {
				XFree (prop_return);
			}
		}

		public Orientation Orientation {
			get {
				return orientation;
			}
		}

		public delegate void OrientationChangedHandler (NotificationArea area, Orientation orientation);
		public event OrientationChangedHandler OrientationChanged;

		[DllImport ("libgdk-x11-2.0.so.0")]
		private static extern IntPtr gdk_x11_display_get_xdisplay (IntPtr display);
    
		[DllImport ("libgdk-x11-2.0.so.0")]
		private static extern int gdk_x11_get_server_time (IntPtr window);
    
		[DllImport ("libgdk-x11-2.0.so.0")]
		private static extern void gdk_error_trap_push ();
    
		[DllImport ("libgdk-x11-2.0.so.0")]
		private static extern int gdk_error_trap_pop ();
    
		[DllImport ("libX11.so.6")]
		private extern static IntPtr XInternAtom(IntPtr display, string atom_name, bool only_if_exists);
    
		[DllImport ("libX11.so.6")]
		private extern static void XGrabServer (IntPtr display);
    
		[DllImport ("libX11.so.6")]
		private extern static void XUngrabServer (IntPtr display);
    
		[DllImport ("libX11.so.6")]
		private extern static int XFlush (IntPtr display);
   
		[DllImport ("libX11.so.6")]
		private extern static int XSync (IntPtr display, bool discard);
    
		[DllImport ("libX11.so.6")]
		private extern static int XFree (IntPtr display);
    
		[DllImport ("libX11.so.6")]
		private extern static IntPtr XGetSelectionOwner (IntPtr display, IntPtr atom);
   
		[DllImport ("libX11.so.6")]
		private extern static IntPtr XSelectInput (IntPtr display, IntPtr window, IntPtr mask);
    
		[DllImport ("libX11.so.6")]
		private extern static int XSendEvent(IntPtr display, IntPtr window, bool propagate, IntPtr event_mask, 
						     ref XClientMessageEvent send_event);

		[DllImport("libX11.so.6")]
		private extern static int XGetWindowProperty(IntPtr display, IntPtr w, IntPtr property, IntPtr long_offset, 
							     IntPtr long_length, bool deleteProp, IntPtr req_type,
							     out IntPtr actual_type_return, out int actual_format_return, 
							     out IntPtr nitems_return, out IntPtr bytes_after_return, 
							     out IntPtr prop_return);

		[Flags]
		private enum EventMask {
			NoEventMask              = 0,
			KeyPressMask             = 1 << 0,
			KeyReleaseMask           = 1 << 1,
			ButtonPressMask          = 1 << 2,
			ButtonReleaseMask        = 1 << 3,
			EnterWindowMask          = 1 << 4,
			LeaveWindowMask          = 1 << 5,
			PointerMotionMask        = 1 << 6,
			PointerMotionHintMask    = 1 << 7,
			Button1MotionMask        = 1 << 8,
			Button2MotionMask        = 1 << 9,
			Button3MotionMask        = 1 << 10,
			Button4MotionMask        = 1 << 11,
			Button5MotionMask        = 1 << 12,
			ButtonMotionMask         = 1 << 13,
			KeymapStateMask          = 1 << 14,
			ExposureMask             = 1 << 15,
			VisibilityChangeMask     = 1 << 16,
			StructureNotifyMask      = 1 << 17,
			ResizeRedirectMask       = 1 << 18,
			SubstructureNotifyMask   = 1 << 19,
			SubstructureRedirectMask = 1 << 20,
			FocusChangeMask          = 1 << 21,
			PropertyChangeMask       = 1 << 22,
			ColormapChangeMask       = 1 << 23,
			OwnerGrabButtonMask      = 1 << 24
		}

		private enum SystemTrayMessage {
			RequestDock = 0,
			BeginMessage = 1,
			CancelMessage = 2
		}

		private enum SystemTrayOrientation {
			Horz = 0,
			Vert = 1
		}

		private enum XEventName {
			KeyPress                = 2,
			KeyRelease              = 3,
			ButtonPress             = 4,
			ButtonRelease           = 5,
			MotionNotify            = 6,
			EnterNotify             = 7,
			LeaveNotify             = 8,
			FocusIn                 = 9,
			FocusOut                = 10,
			KeymapNotify            = 11,
			Expose                  = 12,
			GraphicsExpose          = 13,
			NoExpose                = 14,
			VisibilityNotify        = 15,
			CreateNotify            = 16,
			DestroyNotify           = 17,
			UnmapNotify             = 18,
			MapNotify               = 19,
			MapRequest              = 20,
			ReparentNotify          = 21,
			ConfigureNotify         = 22,
			ConfigureRequest        = 23,
			GravityNotify           = 24,
			ResizeRequest           = 25,
			CirculateNotify         = 26,
			CirculateRequest        = 27,
			PropertyNotify          = 28,
			SelectionClear          = 29,
			SelectionRequest        = 30,
			SelectionNotify         = 31,
			ColormapNotify          = 32,
			ClientMessage           = 33,
			MappingNotify           = 34,
			TimerNotify             = 100,
			LASTEvent
		}

		private enum XAtom {
			Cardinal                = 6,
			LASTAtom
		}
	
		[StructLayout(LayoutKind.Sequential)]
		private struct XAnyEvent 
		{
			internal XEventName    type;
			internal IntPtr        serial;
			internal bool          send_event;
			internal IntPtr        display;
			internal IntPtr        window;
		}

		[StructLayout(LayoutKind.Sequential)]
		private struct XPropertyEvent 
		{
			internal XEventName    type;
			internal IntPtr        serial;
			internal bool          send_event;
			internal IntPtr        display;
			internal IntPtr        window;
			internal IntPtr        atom;
			internal IntPtr        time;
			internal int           state;
		}

		[StructLayout(LayoutKind.Sequential)]
		private struct XClientMessageEvent 
		{
			internal XEventName     type;
			internal IntPtr         serial;
			internal bool           send_event;
			internal IntPtr         display;
			internal IntPtr         window;
			internal IntPtr         message_type;
			internal int            format;
	    
			[StructLayout(LayoutKind.Sequential)]
			internal struct DataUnion 
			{
				internal IntPtr ptr1;
				internal IntPtr ptr2;
				internal IntPtr ptr3;
				internal IntPtr ptr4;
				internal IntPtr ptr5;
			}
	    
			internal DataUnion      data;
		}
	}
}

	
