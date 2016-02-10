<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. このドキュメントについて</a></li>
<li><a href="#sec-2">2. セルフビルド手順</a>
<ul>
<li><a href="#sec-2-1">2.1. LLVMセルフビルド</a></li>
<li><a href="#sec-2-2">2.2. clang-serverセルフビルド</a></li>
</ul>
</li>
<li><a href="#sec-3">3. セルフビルドに必要なソフトウェア</a>
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
<li><a href="#sec-4">4. セルフビルド</a>
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
<li><a href="#sec-5">5. パッチ適用済みバイナリ(Windows Only)</a></li>
<li><a href="#sec-6">6. パッチを適用せずLLVMオフィシャルのlibclangを使用する場合の制限事項</a>
<ul>
<li><a href="#sec-6-1">6.1. 特定ファイルがロックされセーブできなくなる</a>
<ul>
<li><a href="#sec-6-1-1">6.1.1. emacs側での対処方法</a></li>
<li><a href="#sec-6-1-2">6.1.2. 原因（実装上の問題説明、解決案求む）</a></li>
</ul>
</li>
<li><a href="#sec-6-2">6.2. その他</a></li>
</ul>
</li>
<li><a href="#sec-7">7. パッチ解説</a>
<ul>
<li><a href="#sec-7-1">7.1. パッチ</a></li>
<li><a href="#sec-7-2">7.2. パッチ(invalidate-mmap.patch)で行っている事</a></li>
<li><a href="#sec-7-3">7.3. LLVM3.5の追加仕様</a></li>
</ul>
</li>
</ul>
</div>
</div>


[English Manual](./readme.md)  

# このドキュメントについて<a id="sec-1" name="sec-1"></a>

clang-serverのセルフビルドについて説明します。  
※Windows環境で付属の実行ファイルを利用する場合は読まなくても問題ありません。  

# セルフビルド手順<a id="sec-2" name="sec-2"></a>

clang-serverのビルドにはLLVMのlibclangが必要になります。  
ですのでLLVM libclangをセルフビルドしてからclang-serverをセルフビルドします。  

## LLVMセルフビルド<a id="sec-2-1" name="sec-2-1"></a>

以下の４つを行います。  
この作業を簡略化するスクリプトもあります。  
-   LLVMのチェックアウト
-   パッチの適用
-   CMake or configureによるプロジェクトファイル生成
-   ビルド

## clang-serverセルフビルド<a id="sec-2-2" name="sec-2-2"></a>

LLVMセルフビルドで生成したパッチ適用済みのライブラリ libclang を使用します。  
-   CMakeによるプロジェクトファイル生成
-   ビルド
-   インストール

# セルフビルドに必要なソフトウェア<a id="sec-3" name="sec-3"></a>

## Windows<a id="sec-3-1" name="sec-3-1"></a>

以下が必要になります。  

### LLVM<a id="sec-3-1-1" name="sec-3-1-1"></a>

ビルド済みライブラリ  
libclang.lib or libclang.imp  
libclang.dll  
が必要です。  

### Visual Studio 2015/2013/2012/2010<a id="sec-3-1-2" name="sec-3-1-2"></a>

どれでもOK  

### CMake<a id="sec-3-1-3" name="sec-3-1-3"></a>

<http://www.cmake.org/>  

Windows ZIPをダウンロードして何処かへ展開。  
Visual Studio ソリューション＆プロジェクトファイル生成と、ビルド＆インストールのmsbuild呼び出しで使用されます。  

## Linux<a id="sec-3-2" name="sec-3-2"></a>

以下が必要になります。  

### LLVM<a id="sec-3-2-1" name="sec-3-2-1"></a>

ビルド済みライブラリ  
libclang.so  
が必要です。  

### CMake<a id="sec-3-2-2" name="sec-3-2-2"></a>

    $ sudo apt-get install cmake

最新版の場合は↓からダウンロード  

<http://www.cmake.org/>  

cmake-3.1.3.tar.gzをダウンロードし解凍、ビルド、インストールを行う。  

    $ tar -xf cmake-3.1.3.tar.gz .
    $ cd cmake-3.1.3
    $ ./configure && make
    $ make install

# セルフビルド<a id="sec-4" name="sec-4"></a>

## Windows<a id="sec-4-1" name="sec-4-1"></a>

### LLVM<a id="sec-4-1-1" name="sec-4-1-1"></a>

LLVMのセルフビルドが必要になります。  
またセルフビルド時にパッチを適用する必要があります。  
セルフビルド後のパッケージはインストールする必要はありません。  
ビルド後に生成されたバイナリを指すパスを  
CMakeによるプロジェクト生成時に設定すればビルド可能です。  
LLVMがインストール済みであればインストールされているディレクトリを指定します。  

LLVMセルフビルドを行う場合は  
自前でチェックアウトし、CMakeでLLVMソリューションファイルを生成するか、以下のshell scriptを使用してください。  
<https://github.com/yaruopooner/llvm-build-shells>  

1.  スクリプトでLLVMパッチを適用する方法

    builderShell の引数に -tasks を指定し、-tasks パラメーターに PATCH を追加、  
    パッチを適用するパスとパッチファイルを記述したテーブルを -patchInfos パラメーターとして与えます。  
    詳しくはllvm-build-shellsのsample.ps1を参考にしてください。  

2.  LLVMパッチの内容

    mmapの使用が常時無効化されます。  

### clang-server<a id="sec-4-1-2" name="sec-4-1-2"></a>

ac-clang/build/builder\_sample.bat  
を使用します。  
必要に応じてbuilder\_sample.batを編集してください。  
コマンドラインかエクスプローラーから実行します。  

-   example  
    
        cmake -G "Visual Studio 12 2013 Win64" ../clang-server -DLIBRARY_PATHS="c:/cygwin-x86_64/tmp/llvm-build-shells/ps1/clang-360/build/msvc-64/" -DCMAKE_INSTALL_PREFIX="c:/cygwin-x86_64/usr/local/bin/"

-   オプション解説  
    -   `-DLIBRARY_PATHS`  
        セルフビルドしたLLVMが配置されているディレクトリを指定します。  
        LLVMのトップディレクトリである必要があります。  
        省略した場合は ac-clang/clang-server が使われます。
    -   `-DCMAKE_INSTALL_PREFIX`  
        clang-serverのインストールパスを指定します。  
        省略した場合は  
        `C:/Program Files/clang-server`  
        になります。

## Linux<a id="sec-4-2" name="sec-4-2"></a>

### LLVM<a id="sec-4-2-1" name="sec-4-2-1"></a>

LLVMのセルフビルドが必要になります。  
またセルフビルド時にパッチを適用する必要があります。  
セルフビルド後のパッケージはインストールする必要はありません。  
ビルド後に生成されたバイナリを指すパスを  
CMakeによるプロジェクト生成時に設定すればビルド可能です。  
LLVMがインストール済みであればインストールされているディレクトリを指定します。  

LLVMセルフビルドを行う場合は  
自前でチェックアウトし、CMakeでLLVMソリューションファイルを生成するか、以下のshell scriptを使用してください。  
<https://github.com/yaruopooner/llvm-build-shells>  

1.  スクリプトでLLVMパッチを適用する方法

    executeBuilder の引数に -patch を追加し、  
    パッチを適用するパスを-patchApplyLocation、  
    パッチファイルを-patchPathに記述して引数として与えます。  
    -patchApplyLocation,-patchPathはペアになっており、複数回指定可能です。  
    詳しくはllvm-build-shellsのsample.shを参考にしてください。  

2.  LLVMパッチの内容

    mmapの使用が常時無効化されます。  

### clang-server<a id="sec-4-2-2" name="sec-4-2-2"></a>

ac-clang/build/builder\_sample.sh  
を使用します。  
必要に応じてbuilder\_sample.shを編集してください。  
builder\_sample.shを実行します。  

-   example  
    
        cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="/home/yaruopooner/work/llvm-build-shells/sh/clang-350/build" -DCMAKE_INSTALL_PREFIX="~/work/clang-server"

-   オプション解説  
    -   `-DLIBRARY_PATHS`  
        セルフビルドしたLLVMが配置されているディレクトリを指定します。  
        LLVMのトップディレクトリである必要があります。  
        省略した場合は ac-clang/clang-server が使われます。
    -   `-DCMAKE_INSTALL_PREFIX`  
        clang-serverのインストールパスを指定します。  
        省略した場合は  
        `/usr/local/bin`  
        になります。

# パッチ適用済みバイナリ(Windows Only)<a id="sec-5" name="sec-5"></a>

<https://github.com/yaruopooner/ac-clang/releases>  

上記に置いてあるclang-server-X.X.X.zipは  
パッチ適用済みのバイナリとライブラリファイル  
-   clang-server.exe
-   libclang.dll
-   libclang.lib or libclang.imp

の３ファイルが格納されています。  

LLVMはセルフビルドせずにclang-serverのみをセルフビルドする場合は  
clang-server-X.X.X.zipをac-clangに解凍します。  
すると以下のように配置されます。  
ac-clang/clang-server/binary/clang-server.exe  
ac-clang/clang-server/library/x86\_64/release/libclang.dll  
ac-clang/clang-server/library/x86\_64/release/libclang.lib  

# パッチを適用せずLLVMオフィシャルのlibclangを使用する場合の制限事項<a id="sec-6" name="sec-6"></a>

## 特定ファイルがロックされセーブできなくなる<a id="sec-6-1" name="sec-6-1"></a>

編集したヘッダファイルをセーブしようとすると "basic-save-buffer-2: Opening output file: invalid argument \`HEADER-FILE-NAME\`" となりセーブできない。  
必ず発生するわけではなく特定の条件を満たしたファイルサイズが16kBを越えるヘッダファイルで発生する。  
16kB以下のヘッダファイルではまったく発生しない。  
libclang の TranslationUnit(以下TU) の問題。  
libclang の TU がinclude対象のファイルをロックしている。  
ac-clang側で暫定対処パッチを施してあるので多少は緩和されているが完全に回避はできない。  
発生した場合はマニュアル対処する以外ない。  

### emacs側での対処方法<a id="sec-6-1-1" name="sec-6-1-1"></a>

include対象なので大抵は foo.cpp/foo.hpp という構成だとおもわれます。  
foo.hpp(modified)がセーブできない場合、大抵foo.cppが(modified)になっているのでfoo.cppをセーブしましょう。  
これによりfoo.hppはセーブ可能になるはずです。  
これでもセーブできない場合は、foo.cpp以外のソースでfoo.hppをインクルードしており(modified)になっているバッファがあるはずなので  
それもセーブしましょう。  
また、定義へのジャンプ機能で該当ソースがアクティブ化されている場合は、未編集バッファであってもアクティブ化されています。  
該当バッファを削除してみるか、そのバッファへスイッチして (ac-clang-deactivate) を実行してください。  
これ以外でも16kBを越えるヘッダを編集しようとした際に、そのファイルのcppはオープンしてもいないのにセーブできない場合、  
該当ヘッダファイルを何処か遠いモジュールでインクルードしている場合なども同様の症状になります。  
ライブラリモジュールやフレームワークなどを開発している場合は発生しやすいかもしれません。  
※ライブラリ・フレームワークはアプリ側からよくincludeされるため。  

### 原因（実装上の問題説明、解決案求む）<a id="sec-6-1-2" name="sec-6-1-2"></a>

foo.cpp(modified)のとき foo.cppのセッションで  
TUが foo.cpp パース後もincludeされているファイルのロックを保持しつづけている。  
この状態で foo.hpp を編集してセーブしようとするとロックでエラーになる。  
ロックを解除するには、 foo.cpp のTUをリリースする。  
なので foo.cpp セーブ時にセッションは保持した状態で TU だけをリリースして、  
foo.cpp が再び modified になったときに TU を生成するように修正。  
これにより foo.cpp セーブ後であればincludeロックでが全解除されるので foo.hpp がセーブ可能になる。  
当然 foo.cpp 以外に foo.hpp をinclude しているソースでかつ、編集中のバッファがある場合は、  
それら全てを保存しないとロックでは解除されない。  

Windows環境において、  
このロックはI/Oのopen関数によるロックはではなくWindowsAPIのCreateFileMappingによるロックである。  
libclang FileManagerは16kB以上のファイルをメモリマップドファイルとしてアロケーションする。  
TUがリリースされるとUnmapViewOfFileによりメモリマップドファイルがリリースされるようになりファイルに対して書き込み可能になる。  

Linux環境においても発現する不具合はWindows環境と若干異なるものの mmap/munmapによる問題は発生する。  
foo.cppのTUを保持している状態でfoo.hppにおいてclass fooのメソッドを追加・削除し保存する。  
foo.hpp更新後にfoo.cppにおいてclass fooのメソッドを補間しようとするとTUがクラッシュする。  
libclangがSTDOUTに "libclang: crash detected in code completion" を出力する。  
clang-serverのプロセスは生きており、セッションを破棄して再生成すれば補間続行は可能。  

## その他<a id="sec-6-2" name="sec-6-2"></a>

上記の問題はlibclangにパッチを適用して改善している。  

パッチを適用したリリースバイナリのlibclang-x86\_XX.(dll or so)を使用している場合は発生しない。  
パッチを適用していないLLVMセルフビルドおよび、LLVMオフィシャルバイナリを使用する場合にのみ問題が発生します。  
clang側の仕様バグなので現在LLVM bugzilla に報告済み。対応待ち中。  
<http://llvm.org/bugs/show_bug.cgi?id=20880>  

# パッチ解説<a id="sec-7" name="sec-7"></a>

## パッチ<a id="sec-7-1" name="sec-7-1"></a>

ac-clang/clang-server/patch/invalidate-mmap.patch  
を使用。  

    cd llvm/
    svn patch ac-clang/clang-server/patch/invalidate-mmap.patch

## パッチ(invalidate-mmap.patch)で行っている事<a id="sec-7-2" name="sec-7-2"></a>

mmapを使わないようにパッチを適用している  
適用するのは以下のソース  
clang-trunk/llvm/lib/Support/MemoryBuffer.cpp  

    static error_code getOpenFileImpl(int FD, const char *Filename,
                                   OwningPtr<MemoryBuffer> &result,
                                   uint64_t FileSize, uint64_t MapSize,
                                   int64_t Offset, bool RequiresNullTerminator) {

↑の関数内で呼ばれる shouldUseMmap によりファイルに対するmmapの使用可否が判断される  

    static bool shouldUseMmap(int FD,
                           size_t FileSize,
                           size_t MapSize,
                           off_t Offset,
                           bool RequiresNullTerminator,
                           int PageSize) {

この関数のresultが常時falseであればmmapは恒久的に使用されない。  
よってこの関数の先頭で  

    return false;

とすればよい。  
以降のコードは#if 0 end するなりすればよい。  

## LLVM3.5の追加仕様<a id="sec-7-3" name="sec-7-3"></a>

shouldUseMmap,getOpenFileImplに引数IsVolatileSizeが追加された。  
これはshouldUseMmapまで加工なしでパスされ、  
shouldUseMmap先頭において、  

    if (IsVolatileSize)
       return false;

される。  
コメントがついていた  

    // mmap may leave the buffer without null terminator if the file size changed
    // by the time the last page is mapped in, so avoid it if the file size is
    // likely to change.

mmapはファイルサイズが最後のページがマップされたされた時点で変更された場合はnull終端せずにバッファを残すので、ファイルサイズが変更される可能性がある場合は、それを回避することができる。  

とは言っているものの、想定されていない事態がいろいろあるようで仕様抜けの模様。  
またバッファ確保系関数の上流で IsVolatileSize が指定されていなかったりコンストラクタのデフォルト値のまま運用されている箇所が何箇所か見受けられた。  
そういった箇所を自前で修正してみたところ従来よりマシになったものの、他にも問題があるようで想定通りにmmapを制御は出来なかった。  
LLVMのファイルシステム・メモリ周りの仕様を完全に把握していないと、ここら辺の修正は厳しいのかもしれない。  
よって現時点においては上記パッチ適用が一番無難なやり方となる。
