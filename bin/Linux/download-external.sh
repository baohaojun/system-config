#!/bin/bash

mkdir -p ~/external/bin/Linux/ext/`uname -m` \
    ~/external/bin/Linux/ext/media-wiki-templates/ \
    ~/external/bin/Linux/ext/media-wiki-images \
    ~/external/bin/Linux/ext/media-wiki-thumb \
    ~/external/bin/Linux/ext/media-wiki-math

function download_lisp() {
    mkdir -p ~/external/.emacs_d/lisp/ext/
    builtin cd ~/system-config/.emacs_d/lisp/ext/
}

function download_books() {
    mkdir -p ~/Downloads/intel
    builtin cd ~/Downloads/intel

#only download these things for myself, or else it will take too much time
    cat <<EOF|lftp || true
get -c http://download.intel.com/design/PentiumII/manuals/24319002.pdf -o "Intel Architecture Software Developer's Manual, Volume 1: Basic Architecture.pdf"
get -c http://download.intel.com/design/PentiumII/manuals/24319102.pdf -o "Intel Architecture Software Developer's Manual, Volume 2: Instruction Set Reference Manual.pdf"
get -c http://download.intel.com/design/PentiumII/manuals/24319202.pdf -o "Intel Architecture Software Developer's Manual, Volume 3: System Programming.pdf"
get -c http://www.intel.com/Assets/PDF/manual/318148.pdf -o "Intel® 64 Architecture x2APIC Specification.pdf"
get -c http://www.intel.com/Assets/PDF/manual/252046.pdf -o "Intel® 64 and IA-32 Architectures Software Developer's Manual, Documentation Changes.pdf"
get -c http://www.intel.com/Assets/PDF/manual/253665.pdf -o "Intel® 64 and IA-32 Architectures Software Developer's Manual, Volume 1: Basic Architecture.pdf"
get -c http://www.intel.com/Assets/PDF/manual/253666.pdf -o "Intel® 64 and IA-32 Architectures Software Developer's Manual, Volume 2A: Instruction Set Reference, A-M.pdf"
get -c http://www.intel.com/Assets/PDF/manual/253667.pdf -o "Intel® 64 and IA-32 Architectures Software Developer's Manual, Volume 2B: Instruction Set Reference, N-Z.pdf"
get -c http://www.intel.com/Assets/PDF/manual/253668.pdf -o "Intel® 64 and IA-32 Architectures Software Developer's Manual, Volume 3A: System Programming Guide.pdf"
get -c http://www.intel.com/Assets/PDF/manual/253669.pdf -o "Intel® 64 and IA-32 Architectures Software Developer's Manual, Volume 3B: System Programming Guide.pdf"
get -c http://www.intel.com/Assets/PDF/manual/248966.pdf -o "Intel® 64 and IA-32 Architectures Optimization Reference Manual.pdf"
get -c http://www.acpi.info/DOWNLOADS/ACPIspec40.pdf
EOF
}

function download_external() {
    download_lisp
    download_books    
}

