/*
 * search-entry.c
 *
 * Copyright (C) 2006 Novell, Inc.
 *
 */

/*
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include "search-entry.h"
#include "search-entry-watermark.h"

#include <librsvg/rsvg.h>
#include <string.h>

typedef struct {
	GdkPixbuf *watermark;
	int width, height;
} SearchEntryPrivate;
#define SEARCH_ENTRY_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SEARCH_TYPE_ENTRY, SearchEntryPrivate))

static void search_entry_class_init (SearchEntryClass *);
static void search_entry_init       (SearchEntry *);
static void search_entry_finalize   (GObject *);

static void search_entry_realize           (GtkWidget      *widget);
static gboolean search_entry_expose_event  (GtkWidget      *widget,
					    GdkEventExpose *event);

G_DEFINE_TYPE (SearchEntry, search_entry, GTK_TYPE_ENTRY)

static void
search_entry_class_init (SearchEntryClass *search_entry_class)
{
	GObjectClass *g_obj_class = G_OBJECT_CLASS (search_entry_class);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (search_entry_class);

	g_type_class_add_private (search_entry_class, sizeof (SearchEntryPrivate));

	widget_class->realize = search_entry_realize;
	widget_class->expose_event = search_entry_expose_event;

	g_obj_class->finalize = search_entry_finalize;
}

static void
search_entry_init (SearchEntry *entry)
{
}

static void
search_entry_finalize (GObject *object)
{
	SearchEntryPrivate *priv = SEARCH_ENTRY_GET_PRIVATE (object);

	if (priv->watermark)
		g_object_unref (priv->watermark);

	G_OBJECT_CLASS (search_entry_parent_class)->finalize (object);
}


static void
rsvg_size_callback (int *width, int *height, gpointer user_data)
{
	SearchEntryPrivate *priv = user_data;

	*width = priv->width = priv->height * (double)*width / (double)*height;
	*height = priv->height;
}

static void
search_entry_realize (GtkWidget *widget)
{
	SearchEntryPrivate *priv = SEARCH_ENTRY_GET_PRIVATE (widget);
	int height;
	GdkColor *gdkcolor;
	char *svg, color[7];
	RsvgHandle *rsvg;

	GTK_WIDGET_CLASS (search_entry_parent_class)->realize (widget);

	gdk_window_get_geometry (GTK_ENTRY (widget)->text_area,
				 NULL, NULL, NULL, &height, NULL);

	if (height - 2 == priv->height)
		return;
	priv->height = height - 2;

	gdkcolor = &widget->style->fg[GTK_WIDGET_STATE (widget)];
	snprintf (color, 6, "%02x%02x%02x",
		  gdkcolor->red >> 8,
		  gdkcolor->green >> 8,
		  gdkcolor->blue >> 8);
	svg = g_strdup_printf (SEARCH_ENTRY_WATERMARK_SVG, color, color);

	rsvg = rsvg_handle_new ();
	rsvg_handle_set_size_callback (rsvg, rsvg_size_callback, priv, NULL);
	rsvg_handle_write (rsvg, (const guchar *)svg, strlen (svg), NULL);
	rsvg_handle_close (rsvg, NULL);
	g_free (svg);

	if (priv->watermark)
		g_object_unref (priv->watermark);
	priv->watermark = rsvg_handle_get_pixbuf (rsvg);
	rsvg_handle_free (rsvg);
}

static gboolean
search_entry_expose_event (GtkWidget *widget, GdkEventExpose *event)
{
	SearchEntryPrivate *priv = SEARCH_ENTRY_GET_PRIVATE (widget);
	GTK_WIDGET_CLASS (search_entry_parent_class)->expose_event (widget, event);

	if (event->window == GTK_ENTRY (widget)->text_area) {
		int width, height, x;

		if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) {
			gdk_drawable_get_size (event->window, &width, &height);
			x = width - priv->width - 1;
		} else
			x = 1;
		gdk_draw_pixbuf (event->window,
				 widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
				 priv->watermark, 0, 0, x, 1,
				 priv->width, priv->height,
				 GDK_RGB_DITHER_NORMAL, 0, 0);
	}

	return FALSE;
}


GtkWidget *
search_entry_new (void)
{
	return g_object_new (SEARCH_TYPE_ENTRY, NULL);
}
