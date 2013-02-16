# This is the sql for a script to find text in page bodies and replace the text with other text.
# Pages are updated like normal page updates - a new page version is created.
# Use the sql to limit the pages to be searched to fit your specific needs.
# Limit the number of pages selected to only those with the specific find text since you don't want other pages updated.
# Test and verify this on a test instance first to avoid making errors in a production environment.
#
# Run using something like
# confluence --action runFromSql --file src/main/resources/examples/replace.sql --database confluence-3.5.0 --host imac.local --driver postgresql
#
# where replace.txt looks something like:
# --action getPageSource --space @space@ --title "@title@" --file out.txt
# --action storePage --space @space@ --title "@title@" --file out.txt --findReplace "Help:Get Support"
#

select
'run' as action,
'src/main/resources/examples/replace.txt' as file,
'@space@:' || s.spacekey || ',@title@:' || c.title as "findReplace"
from content as c, bodycontent as b, spaces as s
where
c.contentid = b.contentid
and s.spaceid = c.spaceid
and c.content_status = 'current'
and c.contentid = b.contentid
and b.body like '%Help%'
and c.contentid is not null
and c.spaceid is not null
