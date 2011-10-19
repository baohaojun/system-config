/*
 * mono-glue.c
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

#include <glib.h>
#include <mono/jit/jit.h>
#include <mono/metadata/environment.h>
#include <mono/metadata/profiler.h>
#include <mono/metadata/tokentype.h>
#include <mono/metadata/debug-helpers.h>
#include <mono/metadata/assembly.h>
#include <string.h>

#define FIELD_ATTRIBUTE_STATIC 0x10
#define FIELD_ATTRIBUTE_HAS_FIELD_RVA 0x100

static int memory_usage (MonoObject *obj, GHashTable *visited);

static int
memory_usage_array (MonoArray *array, GHashTable *visited)
{
        int total = 0;
        MonoClass *array_class = mono_object_get_class ((MonoObject *) array);
        MonoClass *element_class = mono_class_get_element_class (array_class);
        MonoType *element_type = mono_class_get_type (element_class);

        if (MONO_TYPE_IS_REFERENCE (element_type)) {
                int i;

                for (i = 0; i < mono_array_length (array); i++) {
                        MonoObject *element = mono_array_get (array, gpointer, i);

                        if (element != NULL)
                                total += memory_usage (element, visited);
                }
        }

        return total;
}

static int
memory_usage (MonoObject *obj, GHashTable *visited)
{
        int total = 0;
        MonoClass *klass;
        MonoType *type;
        gpointer iter = NULL;
        MonoClassField *field;

        if (g_hash_table_lookup (visited, obj))
                return 0;

        g_hash_table_insert (visited, obj, obj);

        klass = mono_object_get_class (obj);
        type = mono_class_get_type (klass);

        /* This is an array, so drill down into it */
        if (type->type == MONO_TYPE_SZARRAY)
                total += memory_usage_array ((MonoArray *) obj, visited);

        while ((field = mono_class_get_fields (klass, &iter)) != NULL) {
                MonoType *ftype = mono_field_get_type (field);
                gpointer value;

                if ((ftype->attrs & (FIELD_ATTRIBUTE_STATIC | FIELD_ATTRIBUTE_HAS_FIELD_RVA)) != 0)
                        continue;

                /* FIXME: There are probably other types we need to drill down into */
                switch (ftype->type) {

                case MONO_TYPE_CLASS:
                case MONO_TYPE_OBJECT:
                        mono_field_get_value (obj, field, &value);

                        if (value != NULL)
                                total += memory_usage ((MonoObject *) value, visited);

                        break;

                case MONO_TYPE_SZARRAY:
                        mono_field_get_value (obj, field, &value);

                        if (value != NULL) {
                                total += memory_usage_array ((MonoArray *) value, visited);
                                total += mono_object_get_size ((MonoObject *) value);
                        }

                        break;

                default:
                        /* printf ("Got type 0x%x\n", ftype->type); */
                        /* ignore, this will be included in mono_object_get_size () */
                        break;
                }
        }

        total += mono_object_get_size (obj);

        return total;
}

/*
 * Only returns data for instances, not for static fields, those might
 * be larger, or hold larger structures
 */
static int
GetObjectSize (MonoObject *this)
{
	GHashTable *visited = g_hash_table_new (NULL, NULL);
	int n;
	
	n = memory_usage (this, visited);

	g_hash_table_destroy (visited);
	
	return n;
}

static long
GetObjectPointer (MonoObject *this)
{
	return (long) this;
}

void
mono_glue_install_icall (void)
{
	mono_add_internal_call ("Beagle.Util.SystemInformation::GetObjectSizeIcall", GetObjectSize);
	mono_add_internal_call ("Beagle.Util.SystemInformation::GetObjectPointerIcall", GetObjectPointer);
}
