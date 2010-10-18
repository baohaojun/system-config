# Convert unsorted gzipped dump-entries list (ie. first-pass index) to sorted and
# seekable gzipped entries list. Use the third-party 'zran' program almost unmodified

import os
from os.path import join as J
import re, pickle
import bisect, os

global zranbin

def assert_zran_runtime():
	global zranbin
	zranbin = J(os.path.dirname(__file__), './zran_wdr')
	assert os.path.exists(zranbin), "can't find 'zran_wdr' binary at '%s'" % zranbin
	assert 'usage' in os.popen(zranbin + ' 2>&1').read(), "unexpected error calling 'zran_wdr'"

def build_sorted_entrylist(zindexfilename):
	# assert everything is ok before starting
	assert_zran_runtime()
	assert zindexfilename.endswith('.idx.gz'), "wrongly named .idx.gz filename"
	zindexfilename_s = zindexfilename[:-3] + '_s.gz'
	assert not os.path.exists(zindexfilename_s), "a file named %s already exists" % zindexfilename_s
	assert 'sorted' in os.popen("LANG=C sort --help").read(), "unexpected error calling 'sort'"
	assert 'counts' in os.popen("LANG=C wc --help").read(), "unexpected error calling 'wc'"
	filesize = int(os.stat(zindexfilename)[6]) // 1024
	tmp_freespace = int(os.popen('/bin/df -P /tmp').readlines()[1].split()[3])
	assert filesize < tmp_freespace, "not enough space left on /tmp (report %dK, need %sK)" % (tmp_freespace, filesize)

	# Do actual sorting - blocking + slow + i don't think i can monitor progress

	tmpname = os.tmpnam()
	# it looks that utf8-encoded strings won't work on shell commands after an ">", thus tmpname
	print "zcat input | LANG=C sort | gzip -c > %s" % tmpname # this print was crashing the app with utf8 args when run from the gnome-panel (?!)
	os.popen(("zcat %s | LANG=C sort | gzip -c > %s" % (zindexfilename.encode('utf-8'), tmpname)))
	print "checking"
	nblines_old = int(os.popen(("zcat %s | wc -l" % zindexfilename).encode('utf-8')).read().strip())
	nblines_new = int(os.popen("zcat %s | wc -l" % tmpname).read().strip())
	assert nblines_new == nblines_old, "number of entries don't match"
	os.popen("/bin/mv -f %s %s" % (tmpname, zindexfilename.encode('utf-8')))
	print "indexing entrylist"
	#filesize = int(os.stat(zindexfilename)[6]) / 100
	bufsize = "409600"
	cmd = os.popen( zranbin + " %s -i %s -S %s -c 2>&1 | grep zran_index_save_point" % (zindexfilename.encode('utf-8'), zindexfilename_s.encode('utf-8'), bufsize))
	L = [('', '0', '0')]
	for l in cmd:
		r = re.findall('(.*)zran_index_save_point out=(\d+), in=(\d+)_(.*)', l)[0]
		L.append((r[0]+r[3], r[1], r[2]))
		#print int(r[2]) // filesize # progress bar

	Ltxt = pickle.dumps(L, protocol=2) # almost __repr__

	# Cat the entrylist tab and its file-offset at the end of the _s file.
	f = open(zindexfilename_s, 'a')
	f.seek(0, 2)
	l=f.tell()
	length = '0x%08X' % l
	f.write(Ltxt)
	f.write(length)
	f.close()
	print "Finished"

def load_entrylist_table(zindexfilename_s):
	try:
		assert_zran_runtime()
	except AssertionError:
		return None
	f = open(zindexfilename_s)
	f.seek(-10, 2)
	f.seek(eval(f.read(10)))
	idx_s = pickle.loads(f.read()[:-10])
	return idx_s

def load_entry_addr(entry, idx_s, zindexfilename): # fixme entry & filename must be already utf8-decoded
	global zranbin
	zindexfilename_s = zindexfilename[:-3] + '_s.gz'
	i=bisect.bisect(idx_s, (entry,))
	return i != len(idx_s) and os.popen(zranbin + ' %s -i %s %s -s %d | grep "^%s\t"' % (zindexfilename, zindexfilename_s, idx_s[i-1][1], int(idx_s[i][1]) - int(idx_s[i-1][1]) + 255, entry) ).read()

