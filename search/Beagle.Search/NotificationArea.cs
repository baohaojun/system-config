//
// NotificationArea.cs
//
// Copyright (c) 2006 Novell, Inc.
//

using System;
using System.Collections;

using Gtk;
using Mono.Unix;

namespace Beagle.Search {

	public class NotificationMessage : HBox {

		private static Gtk.Style style;

		private Gtk.Image icon;
		private Gtk.Label title;
		private Gtk.Label message;
		private Gtk.Box action_box;

		private NotificationArea area;

		static NotificationMessage ()
		{
			Gtk.Window temp_win = new Gtk.Window (WindowType.Popup);
			temp_win.Name = "gtk-tooltips";
			temp_win.EnsureStyle ();

			style = temp_win.Style.Copy ();
		}

		public NotificationMessage () : this (null, null) { }

		public NotificationMessage (string t, string m) : base (false, 5)
		{
			this.Style = style;

			BorderWidth = 5;

			icon = new Image (Stock.DialogInfo, IconSize.Dialog);
			this.PackStart (icon, false, true, 5);

			VBox vbox = new VBox (false, 5);
			this.PackStart (vbox, true, true, 0);

			title = new Label ();
			title.SetAlignment (0.0f, 0.5f);
			this.Title = t;
			vbox.PackStart (title, false, true, 0);

			message = new Label ();
			message.LineWrap = true;
			message.SetSizeRequest (500, -1); // ugh, no way to sanely reflow a gtk label
			message.SetAlignment (0.0f, 0.0f);
			this.Message = m;			
			vbox.PackStart (message, true, true, 0);

			action_box = new HBox (false, 3);

			Button hide_button = new Button (Catalog.GetString ("Hide"));
			hide_button.Clicked += OnHideClicked;
			action_box.PackEnd (hide_button, false, true, 0);

			Alignment action_align = new Alignment (1.0f, 0.5f, 0.0f, 0.0f);
			action_align.Add (action_box);
			vbox.PackStart (action_align, false, true, 0);
		}

		protected override bool OnExposeEvent (Gdk.EventExpose e)
		{
			Style.PaintBox (Style, GdkWindow, StateType.Normal,
					ShadowType.Out, e.Area, this, "notification area",
					Allocation.X, Allocation.Y,
					Allocation.Width, Allocation.Height);

			return base.OnExposeEvent (e);
		}

		public void AddAction (string name, EventHandler e)
		{
			Button action = new Button (name);
			
			if (e != null)
				action.Clicked += e;

			action_box.PackStart (action, false, true, 0);
		}

		private void OnHideClicked (object o, EventArgs args)
		{
			area.Hide ();
		}

		public string Title {
			get { return title.Text; }
			set { title.Markup = "<big><b>" + value + "</b></big>"; }
		}

		public string Message {
			get { return message.Text; }
			set { message.Markup = value; }
		}

		public string Icon {
			set { icon.SetFromStock (value, Gtk.IconSize.Dialog); }
		}

		public Gdk.Pixbuf Pixbuf {
			set { icon.Pixbuf = value; }
		}

		public NotificationArea Area {
			set { area = value; }
		}
	}

	public class NotificationArea : Frame {

		private NotificationMessage message;
		
		public NotificationArea ()
		{
			NoShowAll = true;
			ShadowType = ShadowType.Out;
		}

		public new void Display (NotificationMessage m)
		{
			if (message != m) {
				if (message != null)
					Remove (message);

				Add (m);

				message = m;
				m.Area = this;
				m.ShowAll ();
			}

			this.Show ();
		}
	}
}
