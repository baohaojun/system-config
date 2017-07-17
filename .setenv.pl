chomp(my $bin_dir = qx(which beagrep-static-query|tr -d '\n'));
(my $lib_dir = $bin_dir) =~ s,/bin/beagrep-static-query,/lib,;

$ENV{LD_LIBRARY_PATH} = "$lib_dir:$ENV{LD_LIBRARY_PATH}";
$ENV{PATH} = "$ENV{HOME}/external/bin/Linux/ext/debug/:$bin_dir:$ENV{PATH}";
