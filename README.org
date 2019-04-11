System-config is my customizations of Posix systems (Linux/Cygwin/Mac
OS X, but mainly Debian Linux or Ubuntu). It aims to reproduce my
complete working environment with a single command. I made an
introductory video about it on youtube (It's in Chinese, with English
subtitles).

#+BEGIN_HTML
<div class="figure">
<p><a href="https://www.youtube.com/watch?v=qp2b3-Guej0"><img src="http://baohaojun.github.io/images/system-config-youtube.png" alt="system-config-youtube.png" /></a>
</p>
</div>

#+END_HTML

During set-up, system-config will download, install, compile many open
source projects that I use frequently.

One of these projects is [[https://github.com/baohaojun/beagrep][beagrep]]. It combines a search engine with
grep. It is very useful for reading source code. In fact I use it to
read system-config's own source code (code reading is like solving
puzzles, solving puzzles is about looking for answers, and what's best
for looking for answers? Search Engines. Google. Beagrep is a local
search engine best tuned for software projects).

* How to install

Under Debian Linux or Ubuntu, it's simple to set up system-config:

#+BEGIN_SRC sh
  git clone https://github.com/baohaojun/system-config/ ~/system-config &&
      ~/system-config/bin/Linux/after-check-out.sh
#+END_SRC

You probably need to fix some bugs when running =after-check-out.sh=. And then, make system-config your own by forking it and customizing it.

On non debian systems, or if you don't have sudo permission, you can try to use [[http://linuxbrew.sh/][Linuxbrew]]:

#+BEGIN_SRC sh
  git clone https://github.com/baohaojun/system-config/ ~/system-config &&
      ~/system-config/bin/after-co-ln-s.sh && (
          . ~/.bashrc
          install-brew.sh
      )
#+END_SRC

** Install Under Mac/Windows

First, please note that installation under Mac/Windows is not as complete as under Linux.

To install under Windows, you need install Cygwin first, then run this command, and fix the errors that might happen (in most cases, they can be fixed by installing additional cygwin packages).
#+BEGIN_SRC sh
~/system-config/bin/windows/after-check-out.sh
#+END_SRC

To install under Mac, you need install HomeBrew first, then run this command, and fix the errors that might happen (in most cases, they can be fixed by: 1. installing additional HomeBrew packages and 2. Changing the PATH environment variable, so that the default command can be forced to use HomeBrew's version).

For your reference, I have used the following setup in my Mac (you may or may not copy&paste it, since the version numbers might change):

#+BEGIN_SRC sh
if test "$(uname)" = Darwin; then
    extra_path=(
        /usr/local/Cellar/coreutils/8.30/libexec/gnubin
        /usr/local/Cellar/gnu-getopt/1.1.6/bin
        /usr/local/Cellar/bash/4.4.23/bin
        /usr/local/opt/grep/libexec/gnubin
        /usr/local/opt/findutils/libexec/gnubin
        /usr/local/bin
    )
    export SC_PATH_PREFIX=$(
        for x in "${extra_path[@]}"; do
            echo -n $x:
        done
           )
    if test "$(which grep)" != /usr/local/opt/grep/libexec/gnubin/grep; then
        PATH=${SC_PATH_PREFIX}${PATH}
    fi
fi

#+END_SRC

#+BEGIN_SRC sh
PATH-TO-HOMEBREWS-VERSION-OF-BASH ~/system-config/bin/Darwin/after-check-out.sh # The Mac's /bin/bash's version is too low.
#+END_SRC

There's some difference between Mac and Windows/Linux, because Mac is a kind of Unix by itself, and it comes with traditional versions of Unix tools, for e.g., Mac's native =readlink(1)= command does not support the =-f= flag, and you must switch to the HomeBrew version. To make it worse, HomeBrew won't by default replace some important system tools, it will only remind you to make that decision yourself (and it will tell you how-to ONLY ONCE if you really want to replace, so watch the HomeBrew installation output carefully).

Currently, the Mac version of after-check-out.sh will only do a minimun setup (just like after-co-ln-s.sh), if you need to build other software, such as beagrep, you need figure it out by yourself. Mainly it's only a problem of installing dependent packages from HomeBrew.

* How to update

System-config come's with a script named =system-config-update=, you can run it to update to the latest version.

* How to remove

If you tried system-config and don't like it, you can run the following command to stop it:

=sc stop=

Next time, if you want to try again, you can simply run:

=sc start=

** How to completely remove system-config

Unfortunately, there is no clean way to remove system-config and change everything back to the way it was before you installed system-config.

You may run: =rm -rf ~/system-config ~/src/github=, and reboot to see if everything works the old way.

If not, you may need to re-build your HOME directory or even re-install your Liunx.

See [[#disclaimer][disclaimer]].

* How to use

You should be able to use system-config just like using any Linux distribution.

But if you want more power, you can watch the youtube video and try to copy whatever that you like.

(If you are in China, note that the video is also uploaded to [[http://www.bilibili.com/video/av3376647/][bilibili]].)

* How does it work?

Please refer to my [[http://baohaojun.github.io/][github blog]], there are quite some articles about system-config. Most are in Chinese though, sorry😅.

Especially this article: [[http://baohaojun.github.io/blog/2016/04/13/0-system-config-how-does-it-work-and-how-to-use-it.html][How does it work and how to use it]].

* Disclaimer
  :PROPERTIES:
  :CUSTOM_ID: disclaimer
  :END:

System-config started out as a very personal project. If you decided to use it, please do so at your own risk, there is absolutely no warranty of any kind😅.

If you have your own customization, it would be a good idea that you back them up before you try system-config. It's a good idea that you too put your own customization into a git repo, if you have not done so already.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

* Contribute

Bug reports, patches, ideas are welcome.

Or, if you like it, you may also donate some money.

** With a paypal account:

#+BEGIN_HTML
<a href='https://pledgie.com/campaigns/33066'><img alt='Click here to lend your support to: Well done, Mr. Bao Haojun. and
make a donation at pledgie.com !' src='https://pledgie.com/campaigns/33066.png?skin_name=chrome' border='0' ></a>
#+END_HTML

** 支付宝（AliPay）

[[http://baohaojun.github.io/images/bhj-alipay.png]]

** 微信支付（WeChat Pay）

[[http://baohaojun.github.io/images/bhj-wechat-pay.png]]
