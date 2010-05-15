using System;

using Beagle.Util;

namespace Beagle.Search {

	public class DetailsPane : Gtk.Table {

		private Gtk.Image icon;
		private Gtk.Label snippet;
		private Gtk.Tooltips snippet_tip;

		private bool maximized = false;

		public DetailsPane () : base (1, 2, false)
		{
			RowSpacing = ColumnSpacing = 6;
			BorderWidth = 6;

			icon = new Gtk.Image ();
			icon.SetAlignment (0.5f, 0.5f);
			icon.Show ();
			Attach (icon, 0, 1, 0, 1, fill, fill, 6, 0);

			base.SizeRequested += DetailsSizeRequested;
		}

		// FIXME: overriding OnSizeRequested directly results in a 0x0 req
		[GLib.ConnectBefore]
		private void DetailsSizeRequested (object obj, Gtk.SizeRequestedArgs args)
		{
			if (maximized)
				return;

			// Add a placeholder widget
			Gtk.Label label = WidgetFu.NewLabel ("");
			Attach (label, 0, 2, current_row, ++current_row, fill, expand, 0, 0);

			Gtk.Table.TableChild[,] children = new Gtk.Table.TableChild[NColumns, NRows];

			foreach (Gtk.Widget child in Children) {
				Gtk.Table.TableChild tc = this[child] as Gtk.Table.TableChild;
				children[tc.LeftAttach, tc.TopAttach] = tc;
			}

			// Expand the icon down to the bottom or the first label
			if (children[0, 0] != null && children[0, 0].Child == icon) {
				uint max_icon_row;
				for (max_icon_row = 1; max_icon_row < NRows; max_icon_row++) {
					if (children[0, max_icon_row] != null)
						break;
				}

				children[0, 0].BottomAttach = max_icon_row;
			}

			// Expand all labels (except in column 0) rightward
			for (uint row = 0; row < NRows; row++) {
				for (uint col = 1; col < NColumns; col++) {
					if (children[col, row] == null ||
					    !(children[col, row].Child is Gtk.Label))
						continue;
					uint end = col + 1;
					while (end < NColumns &&
					       children[end, row] == null)
						end++;
					if (end > col + 1)
						children[col, row].RightAttach = end;
				}
			}

			// Vertically expand only the placeholder row
			for (uint row = 0; row < NRows; row++) {
				for (uint col = 1; col < NColumns; col++) {
					if (children[col, row] == null)
						continue;
					children[col, row].YOptions = (row == NRows - 1) ? expand : fill;
				}
			}

			maximized = true;
		}

		private const Gtk.AttachOptions expand = Gtk.AttachOptions.Expand | Gtk.AttachOptions.Fill;
		private const Gtk.AttachOptions fill = Gtk.AttachOptions.Fill;
		private uint current_row = 0;

		private Gtk.Label AddGrayLabel (string text, uint row, uint column)
		{
			Gtk.Label label = WidgetFu.NewGrayLabel (text);
			label.SetAlignment (1.0f, 0.0f);
			label.Show ();
			Attach (label, column, column + 1, row, row + 1, fill, fill, 0, 0);
			maximized = false;
			return label;
		}

		private Gtk.Label AddLabel (string text, uint row, uint column)
		{
			Gtk.Label label = WidgetFu.NewLabel (text);
			label.Selectable = true;
			label.SetAlignment (0.0f, 0.0f);
			WidgetFu.EllipsizeLabel (label);
			label.Show ();
			Attach (label, column, column + 1, row, row + 1, expand, fill, 0, 0);
			maximized = false;
			return label;
		}

		private Gtk.Label AddBoldLabel (string text, uint row, uint column)
		{
			Gtk.Label label = WidgetFu.NewBoldLabel (text);
			label.SetAlignment (0.0f, 0.0f);
			WidgetFu.EllipsizeLabel (label);
			label.Show ();
			Attach (label, column, column + 1, row, row + 1, expand, fill, 0, 0);
			maximized = false;
			return label;
		}

		public Gtk.Label AddTitleLabel (string text)
		{
			Gtk.Label label = AddBoldLabel (text, current_row++, 1);
			label.SetAlignment (0.0f, 0.0f);
			label.Selectable = true;
			return label;
		}

		public Gtk.Label AddTextLabel (string text)
		{
			Gtk.Label label = AddLabel (text, current_row++, 1);
			return label;
		}

		public void AddLabelPair (string label, string text)
		{
			AddGrayLabel (label, current_row, 1);
			AddLabel (text, current_row++, 2);
		}

		public Gtk.Label AddSnippet ()
		{
			AddNewLine ();

			snippet = WidgetFu.NewLabel ();
			snippet.SetAlignment (0.0f, 0.0f);
			snippet.Selectable = true;
			WidgetFu.EllipsizeLabel (snippet);
			snippet.Show ();
			Attach (snippet, 1, 2, current_row, ++current_row, expand, fill, 0, 0);
			maximized = false;

			snippet_tip = new Gtk.Tooltips ();

			return snippet;
		}

		public Gtk.Label AddNewLine ()
		{
			Gtk.Label label = WidgetFu.NewLabel ("");
			label.Show ();
			Attach (label, 1, 2, current_row, ++current_row, fill, fill, 0, 0);
			return label;
		}

		public void GotSnippet (string text)
		{
			snippet.Markup = text;
			
			string tip = text.Replace ("<b>", String.Empty).Replace ("</b>", String.Empty);
			snippet_tip.SetTip (snippet, StringFu.ConvertSpecialEntities (tip), null);
		}

		public Gtk.Image Icon {
			get { return icon; }
		}

		public Gtk.Label Snippet {
			get { return snippet; }
		}
	}
}
