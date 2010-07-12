/*
 * Copyright (c) 2004, 2010 Tama Communications Corporation
 *
 * This file is part of GNU GLOBAL.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "checkalloc.h"
#include "die.h"
#include "htags.h"
#include "assoc.h"

/*
 * assoc_open: open associate array.
 *
 *	r)		descriptor
 */
ASSOC *
assoc_open()
{
	ASSOC *assoc = (ASSOC *)check_malloc(sizeof(ASSOC));

	/*
	 * Use invisible temporary file.
	 */
	assoc->dbop = dbop_open(NULL, 1, 0600, 0);
	if (assoc->dbop == NULL)
		abort();
	assoc->dbop->put_errmsg = "cannot write to temporary file.\nYou can specify the directory for the temporary file using environment variable 'TMPDIR'.";
	return assoc;
}
/*
 * assoc_close: close associate array.
 *
 *	i)	assoc	descriptor
 */
void
assoc_close(ASSOC *assoc)
{
	if (assoc == NULL)
		return;
	if (assoc->dbop == NULL)
		abort();
	dbop_close(assoc->dbop);
	free(assoc);
}
/*
 * assoc_put: put data into associate array.
 *
 *	i)	assoc	descriptor
 *	i)	name	name
 *	i)	value	value
 */
void
assoc_put(ASSOC *assoc, const char *name, const char *value)
{
	if (assoc->dbop == NULL)
		abort();
	dbop_put(assoc->dbop, name, value);
}
/*
 * assoc_put_withlen: put data into associate array.
 *
 *	i)	assoc	descriptor
 *	i)	name	name
 *	i)	value	value
 *	i)	len	length
 */
void
assoc_put_withlen(ASSOC *assoc, const char *name, const char *value, int len)
{
	if (assoc->dbop == NULL)
		abort();
	dbop_put_withlen(assoc->dbop, name, value, len);
}
/*
 * assoc_get: get data from associate array.
 *
 *	i)	assoc	descriptor
 *	i)	name	name
 *	r)		value
 */
const char *
assoc_get(ASSOC *assoc, const char *name)
{
	if (assoc->dbop == NULL)
		abort();
	return dbop_get(assoc->dbop, name);
}
