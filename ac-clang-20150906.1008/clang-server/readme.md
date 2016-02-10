<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. About this document</a></li>
<li><a href="#sec-2">2. Step of self-build</a>
<ul>
<li><a href="#sec-2-1">2.1. LLVM self-build</a></li>
<li><a href="#sec-2-2">2.2. clang-server self-build</a></li>
</ul>
</li>
<li><a href="#sec-3">3. Software required for self-build</a>
<ul>
<li><a href="#sec-3-1">3.1. Windows</a>
<ul>
<li><a href="#sec-3-1-1">3.1.1. LLVM</a></li>
<li><a href="#sec-3-1-2">3.1.2. Visual Studio 2015/2013/2012/2010</a></li>
<li><a href="#sec-3-1-3">3.1.3. CMake</a></li>
</ul>
</li>
<li><a href="#sec-3-2">3.2. Linux</a>
<ul>
<li><a href="#sec-3-2-1">3.2.1. LLVM</a></li>
<li><a href="#sec-3-2-2">3.2.2. CMake</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-4">4. Self-Build</a>
<ul>
<li><a href="#sec-4-1">4.1. Windows</a>
<ul>
<li><a href="#sec-4-1-1">4.1.1. LLVM</a></li>
<li><a href="#sec-4-1-2">4.1.2. clang-server</a></li>
</ul>
</li>
<li><a href="#sec-4-2">4.2. Linux</a>
<ul>
<li><a href="#sec-4-2-1">4.2.1. LLVM</a></li>
<li><a href="#sec-4-2-2">4.2.2. clang-server</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-5">5. Patch was applied binary(Windows Only)</a></li>
<li><a href="#sec-6">6. Restrictions when you use LLVM official libclang without applying a patch</a>
<ul>
<li><a href="#sec-6-1">6.1. A specific file is locked and cannot save it</a>
<ul>
<li><a href="#sec-6-1-1">6.1.1. Solution in Emacs side</a></li>
<li><a href="#sec-6-1-2">6.1.2. Issue(Implementation issues explanation, it wanted suggested solutions)</a></li>
</ul>
</li>
<li><a href="#sec-6-2">6.2. Miscellaneous</a></li>
</ul>
</li>
<li><a href="#sec-7">7. Patch commentary</a>
<ul>
<li><a href="#sec-7-1">7.1. Patch</a></li>
<li><a href="#sec-7-2">7.2. The contents of the LLVM patch(invalidate-mmap.patch)</a></li>
<li><a href="#sec-7-3">7.3. <span class="todo TODO">TODO</span> Additional Specification of LLVM3.5</a></li>
</ul>
</li>
</ul>
</div>
</div>


[Japanese Manual](./readme.ja.md)  

# About this document<a id="sec-1" name="sec-1"></a>

This is explained about self-build of clang-server.  
When you use clang-server binary for distribution on Windows environment, it is not necessary to read this document.  

# Step of self-build<a id="sec-2" name="sec-2"></a>

A build of clang-server requires libclang of LLVM.  
Therefore, first self-build is LLVM libclang, next self-build is clang-server.  

## LLVM self-build<a id="sec-2-1" name="sec-2-1"></a>

You have to do four step of following.  
There is also a script which simplify this work.  
-   Checkout LLVM
-   Apply patch to LLVM for libclang
-   Project file generation by CMAKE or configure
-   Build

## clang-server self-build<a id="sec-2-2" name="sec-2-2"></a>

Patch applied libclang library use by LLVM self-build  
Use the patch applied libclang library  
-   Project file generation by CMAKE
-   Build
-   Installation

# Software required for self-build<a id="sec-3" name="sec-3"></a>

## Windows<a id="sec-3-1" name="sec-3-1"></a>

The following is required.  

### LLVM<a id="sec-3-1-1" name="sec-3-1-1"></a>

The following built library is required.  
libclang.lib or libclang.imp  
libclang.dll  

### Visual Studio 2015/2013/2012/2010<a id="sec-3-1-2" name="sec-3-1-2"></a>

any OK  

### CMake<a id="sec-3-1-3" name="sec-3-1-3"></a>

<http://www.cmake.org/>  

Download cmake archive, and decompress to any location.  
CMake is used for Visual Studio Solution File generation and build and installation.  

## Linux<a id="sec-3-2" name="sec-3-2"></a>

The following is required.  

### LLVM<a id="sec-3-2-1" name="sec-3-2-1"></a>

The following built library is required.  
libclang.so  

### CMake<a id="sec-3-2-2" name="sec-3-2-2"></a>

    $ sudo apt-get install cmake

If you want to use latest version, download from following  

<http://www.cmake.org/>  

e.g.  
Download cmake-3.1.3.tar.gz, and decompress to work directory.  
You perform a build and installation.  

    $ tar -xf cmake-3.1.3.tar.gz .
    $ cd cmake-3.1.3
    $ ./configure && make
    $ make install

# Self-Build<a id="sec-4" name="sec-4"></a>

## Windows<a id="sec-4-1" name="sec-4-1"></a>

### LLVM<a id="sec-4-1-1" name="sec-4-1-1"></a>

Required process for LLVM self-build.  
And you must apply patch to LLVM before build.  
It isn't necessary install a package after self-build.  
You must designate generated binary PATH after LLVM self-build at CMake project generation argument.  
Therefore clang-server is able to build.  
When LLVM is already installed, you must designate installed directory of LLVM.  

When you want to self-build,  
Checkout from SVN by yourself, and perform the LLVM Solution File generation and build by cmake.  
Or, use following script.  
<https://github.com/yaruopooner/llvm-build-shells>  

1.  How to designate the LLVM patch in the script

    You will designate -tasks to the argument of builderShell,  
    and designate PATCH to the parameter of -tasks,  
    It will gives a table that describes the path to apply the patch and patch file to parameter of -patchInfos.  
    Please refer to the sample.ps1 of llvm-build-shells for details.  

2.  The contents of the LLVM patch

    Use of mmap always invalidation.  

### clang-server<a id="sec-4-1-2" name="sec-4-1-2"></a>

Use the ac-clang/build/builder\_sample.bat  
Please edit the builder\_sample.bat as necessary.  
It's necessary to execute in the command line or Windows Explorer.  

-   example  
    
        cmake -G "Visual Studio 12 2013 Win64" ../clang-server -DLIBRARY_PATHS="c:/cygwin-x86_64/tmp/llvm-build-shells/ps1/clang-360/build/msvc-64/" -DCMAKE_INSTALL_PREFIX="c:/cygwin-x86_64/usr/local/bin/"

-   Option commentary  
    -   `-DLIBRARY_PATHS`  
        You have to designate location of  LLVM self-build completed directory.  
        It is necessary to designate the directory that a binary was generated.(e.g. {LLVM output path}/Release/)  
        If you omit this option, value will be use `ac-clang/clang-server` .
    -   `-DCMAKE_INSTALL_PREFIX`  
        You have to designate installation location of clang-server.  
        If you omit this option, value will be use `C:/Program Files/clang-server` .

## Linux<a id="sec-4-2" name="sec-4-2"></a>

### LLVM<a id="sec-4-2-1" name="sec-4-2-1"></a>

Required process for LLVM self-build.  
And you must apply patch to LLVM before build.  
It isn't necessary install a package after self-build.  
You must designate generated binary PATH after LLVM self-build at CMake project generation argument.  
Therefore clang-server is able to build.  
When LLVM is already installed, you must designate installed directory of LLVM.  

When you want to self-build,  
Checkout from SVN by yourself, and perform the LLVM Solution File generation and build by cmake.  
Or, use following script.  
<https://github.com/yaruopooner/llvm-build-shells>  

1.  How to designate the LLVM patch in the script

    You will designate -patch to the argument of executeBuilder.  
    Add to -patchApplyLocation the path where you want to apply the patch.  
    You write the patch file to -patchPath gives as an parameter.  
    -patchApplyLocation,-patchPath becomes the pair, it is possible to multiple times designate.  
    Please refer to the sample.sh of llvm-build-shells for details.  

2.  The contents of the LLVM patch

    Use of mmap always invalidation.  

### clang-server<a id="sec-4-2-2" name="sec-4-2-2"></a>

Use the ac-clang/build/builder\_sample.sh  
Please edit the builder\_sample.sh as necessary.  
Execute the builder\_sample.sh  

-   example  
    
        cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="/home/yaruopooner/work/llvm-build-shells/sh/clang-350/build" -DCMAKE_INSTALL_PREFIX="~/work/clang-server"

-   Option commentary  
    -   `-DLIBRARY_PATHS`  
        You have to designate location of  LLVM self-build completed directory.  
        It is necessary to designate the directory that a binary was generated.(e.g. {LLVM output path}/Release/)  
        If you omit this option, value will be use `ac-clang/clang-server` .
    -   `-DCMAKE_INSTALL_PREFIX`  
        You have to designate installation location of clang-server.  
        If you omit this option, value will be use `/usr/local/bin` .

# Patch was applied binary(Windows Only)<a id="sec-5" name="sec-5"></a>

<https://github.com/yaruopooner/ac-clang/releases>  

clang-server-X.X.X.zip is you can download from the above  
The archive is 3 files contain, these file applied patch.  
-   clang-server.exe
-   libclang.dll
-   libclang.lib or libclang.imp

When you want to self-build only clang-server without LLVM,  
clang-server-X.X.X.zip decompress to ac-clang directory.  
Then, it will be placed in the following.  
ac-clang/clang-server/binary/clang-server.exe  
ac-clang/clang-server/library/x86\_64/release/libclang.dll  
ac-clang/clang-server/library/x86\_64/release/libclang.lib  

# Restrictions when you use LLVM official libclang without applying a patch<a id="sec-6" name="sec-6"></a>

## A specific file is locked and cannot save it<a id="sec-6-1" name="sec-6-1"></a>

When you try to save the edited header file,  
it will be "basic-save-buffer-2: Opening output file: invalid argument \`HEADER-FILE-NAME\`",  
and you can't save.  
This occur if it meets certain conditions.  
This condition is met when the header file size is larger than 16kB.  
It is not at all occur when header file size is smaller than 16kB.  
This issue belong to TranslationUnit(TU) of libclang.  
The inclusion target file is locked by TU of libclang.  
By performing a provisional transaction in ac-clang side, the more or less is erased, but it can't be avoided perfectly.  
When this issue is occurring, only manual handle can be avoided.  

### Solution in Emacs side<a id="sec-6-1-1" name="sec-6-1-1"></a>

I suppose that combination of source file is foo.cpp/foo.hpp.  
When foo.hpp(modified) can't save, foo.cpp is (modified) often, so foo.cpp have to saved.  
Therefore, foo.hpp should be possible to save.  
When this can't save,  
foo.hpp is included by source files besides foo.cpp, and it has (modified) status.  
You have to save those.  
And, when corresponding source is activated by definition jump feature, even if buffer don't modified that buffer is activated.  
You try remove corresponding buffer, or (ac-clang-deactivate) must be execute in buffer.  
In other cases, when you try save header file that file size larger than 16kB  
When you save a header file of larger than 16kB, if it fails.  
And that header file does not opened.  
In this case, header file is included by a far module from current source file.  
When you having developed a library module framework, it may be easy to occur.  
because library and framework is included from application side.  

### Issue(Implementation issues explanation, it wanted suggested solutions)<a id="sec-6-1-2" name="sec-6-1-2"></a>

When session of "foo.cpp" is edited in the buffer,  
TU continue locking to included header file after parsed "foo.cpp".  

When you edit and save to "foo.hpp" in this state, it occur error, because file is locked by mmap.  

Therefore I modified a server as follows.  
So while maintaining the session when "foo.cpp" saving,  
TU is generated when "foo.cpp" is edited after TU released.  

Therefore "foo.hpp" is possible to save that the included header file is unlocked after "foo.cpp" saved.  

When a "foo.hpp" is included buffer where exist in buffer editing group without buffer of "foo.cpp",  
the lock is not released when you does not save all them.  

In the Windows environment,  
This lock is not open function of I/O, is a lock by CreateFileMapping of WindowsAPI.  
libclang FileManager does allocation to memory mapped file for the files larger than 16kB.  
When TU is released, memory mapped file is released by UnmapViewOfFile, these becomes writable to file.  

In the Linux environment,  
problems with mmap/munmap bug differ slightly from the Windows environment, but also occurred in Linux environment.  
The method add to "class Foo" in "foo.hpp" in the state that holds TU of "foo.cpp", and save to file.  
After "foo.hpp" update, when you try complete method of "class Foo" in the "foo.cpp", TU will crash.  
in this case, libclang output to STDOUT that "libclang: crash detected in code completion"  
libclang output "libclang: crash detected in code completion" to STDOUT.  
The process of clang-server is living in this situation.  
Completion is possible after deletion of session and creation of session.  

## Miscellaneous<a id="sec-6-2" name="sec-6-2"></a>

The above problems are solved by patching for libclang.  

When you use the patch applied release binary(libclang.dll or so) it is not occur.  
When you use the patch does not applied to LLVM self-build and LLVM official binary, this problem is occur.  
I think specification bug of clang side. This problem has been reported to LLVM bugzilla. in the corresponding waitting.  
<http://llvm.org/bugs/show_bug.cgi?id=20880>  

# Patch commentary<a id="sec-7" name="sec-7"></a>

## Patch<a id="sec-7-1" name="sec-7-1"></a>

Use the ac-clang/clang-server/patch/invalidate-mmap.patch  

    cd llvm/
    svn patch ac-clang/clang-server/patch/invalidate-mmap.patch

## The contents of the LLVM patch(invalidate-mmap.patch)<a id="sec-7-2" name="sec-7-2"></a>

Patch is applied so as not to use mmap.  
Apply to the following source code to  
`clang-trunk/llvm/lib/Support/MemoryBuffer.cpp`  

    static error_code getOpenFileImpl(int FD, const char *Filename,
                                   OwningPtr<MemoryBuffer> &result,
                                   uint64_t FileSize, uint64_t MapSize,
                                   int64_t Offset, bool RequiresNullTerminator) {

It is determined availability of mmap for file by shouldUseMmap call from the above function.  

    static bool shouldUseMmap(int FD,
                           size_t FileSize,
                           size_t MapSize,
                           off_t Offset,
                           bool RequiresNullTerminator,
                           int PageSize) {

When the result of function is always false, mmap is not never used.  
Therefore, the following modify has been applied to the top of this function.  

    #if 1
    return false;
    #else
    /* original codes */
    #endif

## TODO Additional Specification of LLVM3.5<a id="sec-7-3" name="sec-7-3"></a>

IsVolatileSize has been added to arguments of shouldUseMmap and getOpenFileImpl.  
This will be passed unchanged to shouldUseMmap.  

It is executed as follows in the shouldUseMmap top.  

    if (IsVolatileSize)
       return false;

Following comments had been attached  

    // mmap may leave the buffer without null terminator if the file size changed
    // by the time the last page is mapped in, so avoid it if the file size is
    // likely to change.

Although that said, there is a situation which isn't assumed Variously, I'm supposing that mistake of specification.  
Moreover, upstream function of buffer association function  
I was found some place where is not designated value of IsVolatileSize and is used default value of constructor argument.  
I tried modified it.  
Result was become more better than conventional.  
But it seem to have a problem, because I was not able to control mmap like a assumption.  
I'm not enough understand the specification of around the file system and memory of LLVM.  
For that reason that the rightly correction is difficult.  
Therefore, the above patching becomes the most safe way at present.
