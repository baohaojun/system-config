#!/bin/sh

format_one()
{
	cmd=`echo $1 | sed -e 's/\.txt//'`
	desc=`cat "$1" | awk '
BEGIN{state=0}
/^NAME$/{state=1; next}
/^----$/ && (state==1){state=2; next}
(state!=2){next}
{print $0; exit}
'`

	len=`expr length "${cmd}"`

	if [ -z "$desc" ]; then
		echo "No description found in $cmd.txt" >&2
		exit 1
	elif [ "`expr substr "$desc" 1 $len`" != "$cmd" ]; then
		echo "Description does not match $cmd: $desc" >&2
		exit 1
	fi

	echo "linkguilt:$cmd[1]::"
	echo "	$desc."
	echo ""
}

for m in guilt-*.txt ; do
	format_one "$m"
done > cmds.txt+

mv cmds.txt+ cmds.txt
