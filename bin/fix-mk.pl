#!/usr/bin/env perl
use strict;
my @lines;
my $line;
my $cross_compile_export = <<'EOF';
ifneq ($(CCACHE_COMMAND),)
export CROSS_COMPILE:=$(CCACHE_COMMAND) $(KERNEL_TOOLCHAIN_PREFIX)
else
export CROSS_COMPILE:=$(KERNEL_TOOLCHAIN_PREFIX)
endif
EOF

    0;
my $cross_compile_export_uboot = <<'EOF';
ifneq ($(CCACHE_COMMAND),)
export CROSS_COMPILE:=$(CCACHE_COMMAND) arm-eabi-
else
export CROSS_COMPILE:=arm-eabi-
endif
EOF


    0; # fix the indentation

while (<>) {
    s/^\s*\Qexport CROSS_COMPILE:=arm-eabi-\E/$cross_compile_export_uboot/;
    s/^\s*\Qexport CROSS_COMPILE:=$(KERNEL_TOOLCHAIN_PREFIX)\E/$cross_compile_export/;
    if (m/make.*cross_compile.*=/i) {
	s/ CROSS_COMPILE\s*:?=\s*\$(\(|\{)(CROSS_COMPILE|KERNEL_TOOLCHAIN_PREFIX)[^({})]*(\)|\})//;
	s/ CROSS_COMPILE\s*:?=\s*\$\$CROSS_COMPILE//;
    }
    s/ -j\$\(MAKE_JOBS\)//;
    if ($line) {
	$line = $line . $_;
    } else {
	$line = $_;
    }
    if (m/\\$/) {
	1;
    } else {
	push @lines, $line;
	$line = "";
    }
}

for (@lines) {
    s/^(\t(\s*)make\b)/\t\2\$(MAKE)/mg;
    if (m/\$[({]MAKE[)}]/ and m/^\t/) {
	s/^\t\+*/\t+/;
    }
    print $_;
}
