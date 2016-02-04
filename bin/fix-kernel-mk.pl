#!/usr/bin/perl
use strict;
my @lines;
my $line;
my $cross_compile_export_kernel = <<'EOF';

ifneq ($(USE_CCACHE),)

export CCACHE_COMMAND := $(PWD)/prebuilts/misc/$(CCACHE_HOST_TAG)/ccache/ccache
ifeq ($(wildcard $(CCACHE_COMMAND)),)
export CCACHE_COMMAND := $(PWD)/prebuilt/$(CCACHE_HOST_TAG)/ccache/ccache
endif
$(warning ccache is $(CCACHE_COMMAND))

else

export CCACHE_COMMAND :=

endif

$(KERNEL_CONFIG) : export CROSS_COMPILE := $(CCACHE_COMMAND) $(KERNEL_CROSS_COMPILE)
$(TARGET_PREBUILT_INT_KERNEL): export CROSS_COMPILE := $(CCACHE_COMMAND) $(KERNEL_CROSS_COMPILE)
$(KERNEL_HEADERS_INSTALL): export CROSS_COMPILE := $(CCACHE_COMMAND) $(KERNEL_CROSS_COMPILE)

EOF

0;

my @input_lines = <>;


unless (join('', @input_lines) =~ m/KERNEL_CROSS_COMPILE/) {
    $cross_compile_export_kernel =~ s/\Q$(KERNEL_CROSS_COMPILE)\E/arm-eabi-/g;
}


for (@input_lines) {
    s/^\s*\Q$(KERNEL_OUT):\E.*/$cross_compile_export_kernel\n$&/;
    if (m/make.*cross_compile.*=/i) {
        s/ CROSS_COMPILE\s*:?=\s*\$(\(|\{)(CROSS_COMPILE|KERNEL_CROSS_COMPILE|KERNEL_TOOLCHAIN_PREFIX)[^({})]*(\)|\})//;
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
