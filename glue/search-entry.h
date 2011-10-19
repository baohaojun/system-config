#ifndef __SEARCH_ENTRY_H__
#define __SEARCH_ENTRY_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define SEARCH_TYPE_ENTRY            (search_entry_get_type ())
#define SEARCH_ENTRY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), SEARCH_TYPE_ENTRY, SearchEntry))
#define SEARCH_ENTRY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), SEARCH_TYPE_ENTRY, SearchEntryClass))
#define SEARCH_IS_ENTRY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SEARCH_TYPE_ENTRY))
#define SEARCH_IS_ENTRY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), SEARCH_TYPE_ENTRY))
#define SEARCH_ENTRY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), SEARCH_TYPE_ENTRY, SearchEntryClass))

typedef struct {
	GtkEntry parent;

} SearchEntry;

typedef struct {
	GtkEntryClass parent_class;

} SearchEntryClass;

GType      search_entry_get_type (void);

GtkWidget *search_entry_new      (void);

G_END_DECLS

#endif /* __SEARCH_ENTRY_H__ */
