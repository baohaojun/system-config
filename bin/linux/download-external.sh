#!/bin/bash

mkdir -p ~/external/bin/linux/ext/`uname -m` \
    ~/external/bin/linux/ext/media-wiki-templates/ \
    ~/external/bin/linux/ext/media-wiki-images \
    ~/external/bin/linux/ext/media-wiki-thumb
function download_antlr() {
    mkdir -p ~/external/bin/linux/ext
    ln -sf  ~/external/bin/linux/ext ~/bin/linux/
    builtin cd ~/bin/linux/ext/
    wget http://android.git.kernel.org/repo
    chmod +x repo

    ext_download=(
        http://www.antlr.org/download/antlr-3.2.tar.gz
        http://www.antlr.org/download/antlr-3.2.jar
        http://www.antlr.org/download/antlrworks-1.3.1.jar
        http://www.stringtemplate.org/download/stringtemplate-3.2.1.tar.gz
    )
    for x in "${ext_download[@]}"; do 
        while ! wget -c --timeout 30 --connect-timeout 30 --tries 1 $x; do echo `basename $x` 'download failed, retry!'; done
    done
    ln -sf antlr-3.2.jar antlr3.jar
    tar zxfv stringtemplate*tar.gz
    ln -sf stringte*/lib/string*.jar stringtemplate.jar
    ln -sf antlrworks-1.3.1.jar antlrworks.jar

}

function download_lisp() {
    mkdir -p ~/external/.emacs_d/lisp/ext/
    ln -sf ~/external/.emacs_d/lisp/ext ~/.emacs_d/lisp/
    builtin cd ~/.emacs_d/lisp/ext/
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
    download_antlr
    download_lisp
    download_books    
}

