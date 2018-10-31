% Created 2018-10-31 水 10:32
% Intended LaTeX compiler: pdflatex
\documentclass[11pt,dvipdfmx,CJKbookmarks]{article}
\usepackage{CJKutf8}
\usepackage{atbegshi}
\AtBeginShipoutFirst{\special{pdf:tounicode UTF8-UTF16}} % for UTF-8
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{graphicx}
\usepackage[export]{adjustbox}
\usepackage{lmodern}
\usepackage{grffile}
\usepackage{longtable}
\usepackage{wrapfig}
\usepackage{rotating}
\usepackage[normalem]{ulem}
\usepackage{amsmath}
\usepackage{textcomp}
\usepackage{amssymb}
\usepackage{capt-of}
\usepackage{hyperref}
 \usepackage{minted}
 \usepackage{mycv}
\author{Bao Haojun}
\date{\today}
\title{包昊军的个人简历}
\hypersetup{
 pdfauthor={Bao Haojun},
 pdftitle={包昊军的个人简历},
 pdfkeywords={},
 pdfsubject={},
 pdfcreator={Emacs 26.1 (Org mode 9.1.9)},
 pdflang={English}}
\begin{document}

\maketitle

\section{工作经历}
\label{sec:orge8c17e5}
\subsubsection{2014 年 3 月 - 现在}
\label{sec:org10e52f9}
\begin{itemize}
\item \textbf{CM 架构师，总监}，锤子科技，北京
\begin{itemize}
\item 设计、实现配置管理系统，并在运维过程中不断改进

\begin{itemize}
\item 此系统支持了锤子科技所有手机产品的日常版本编译和正式版本发布
\end{itemize}

\item 设计并实现安卓手机编译的持续集成

\item 设计并实现 SmartBuilder 系统，方便项目经理和工程师自助编译版本

\item 编写开发流程文档，建立软件开发流程并不断改进

\item 结合个人的 \href{https://github.com/baohaojun/system-config}{system-config} 开源项目，统一配置工程师的开发环境

\begin{itemize}
\item 其中包含我自己开发的各种工具，比如用于快速全局搜索海量代码的 \href{https://github.com/baohaojun/beagrep}{beagrep}，我在服务器上创建了索引并开放给工程师，让所有人搜索代码的速度都得到数量级的提升
\end{itemize}

\item 对工程师工作中碰到的各种疑难杂症提供支持

\item 对工程师提供开发工具、编程语言等培训

\begin{itemize}
\item 拍摄了两小时长的 Linux 系统工具使用技巧视频，在 \href{https://www.youtube.com/watch?v\%3Dqp2b3-Guej0}{YouTube} 和 \href{https://www.bilibili.com/video/av3376647/}{bilibili} 上
\end{itemize}
\end{itemize}

\item \textbf{工具架构师}，锤子科技，北京

\begin{itemize}
\item 负责部分工厂生产测试工具、工程师开发工具的设计、实现等
\item 设计、实现 PC 端操作手机的开源软件 \href{https://github.com/SmartisanTech/Wrench}{小扳手}
\item 调试并解决手机过热问题
\end{itemize}
\end{itemize}

\subsubsection{2013 年 9 月 - 2014 年 3 月}
\label{sec:org7f22c87}
\begin{itemize}
\item \textbf{技术专家}，技术质量部测试工具组，Alibaba，北京
\begin{itemize}
\item 负责 SMART(软件测量分析报告工具）安卓客户端开发
\end{itemize}
\end{itemize}

\subsubsection{2011 年 11 月 - 2013 年 9 月}
\label{sec:org9415787}
\begin{itemize}
\item \textbf{Staff Engineer}，\href{http://marvell.com}{Marvell}，北京

\begin{itemize}
\item 在 Tools 团队担任架构师的职务，负责 Marvell 手机整体解决方案中工厂生产工具的设计与实现
\item 在 BSP 团队负责与工具相关的功能模块的设计与实现，包括 Uboot，Kernel 接口，工厂分区等
\end{itemize}
\end{itemize}
\subsubsection{2010 年 3 月 - 2011 年 10 月}
\label{sec:org6ff852e}
\begin{itemize}
\item \textbf{Staff Engineer}，RayzerLink/Letou
\begin{itemize}
\item 负责基于 Nvidia Tegra2 芯片的平板电脑底层软件开发，包括 Linux Kernel bring-up，驱动（Touch、LCD、Sensors），Hal（硬件抽象层）的开发等工作
\end{itemize}
\end{itemize}
\subsubsection{2008 年 11 月 - 2010 年 3 月}
\label{sec:orgc4d7621}
\begin{itemize}
\item \textbf{Senior Engineer}，\href{http://www.borqs.com}{播思通讯}

\begin{itemize}
\item 在 Tools 组工作， 设计并实现手机开发、测试、生产以及售后等各个环节需要用到的一系列工具
\end{itemize}
\end{itemize}
\subsubsection{2005 年 9 月 - 2008 年 9 月}
\label{sec:org89ca168}
\begin{itemize}
\item \textbf{Software Engineer}，\href{http://motorola.com}{摩托罗拉}， MD/GSG

\begin{itemize}
\item 手机多媒体软件开发，自动调试工具开发
\end{itemize}
\end{itemize}

\subsubsection{2004 年 10 月 - 2005 年 9 月}
\label{sec:org45f78b6}
\begin{itemize}
\item \textbf{Software Engineer}，麒麟软件
\begin{itemize}
\item 企业集成应用软件测试
\end{itemize}
\end{itemize}

\section{自由软件项目}
\label{sec:orgdd88403}

\subsubsection{Emacs}
\label{sec:org95e859b}
\begin{description}
\item[{\href{http://github.com/baohaojun/bbyac}{bbyac.el}}] Emacs 下的补齐工具
\item[{\href{https://github.com/baohaojun/org-jira}{org-jira.el}}] Emacs 下用 org-mode 来进行 Jira 开发流程管理的工具
\item[{\href{https://github.com/baohaojun/ajoke}{Ajoke.el}}] 一个 Emacs 下的 Java/Android 集成开发环境
\end{description}

\subsubsection{Android}
\label{sec:org2074d2c}
\begin{description}
\item[{\href{https://github.com/baohaojun/BTAndroidWebViewSelection}{CrossDict}}] Android 下的英文字典软件
\item[{\href{https://github.com/SmartisanTech/Wrench}{Wrench}}] 用 PC 连接、控制手机的工具软件
\begin{itemize}
\item 允许流畅同步显示手机屏幕
\item 用 Lua 编程语言录制屏幕操作脚本
\item 在 PC 端显示、处理手机端通知消息
\item 用 Qt + Lua 开发，支持所有主流操作系统
\end{itemize}
\end{description}

\subsubsection{Input Method}
\label{sec:org8b07b7e}
\begin{description}
\item[{\href{https://github.com/baohaojun/system-config/tree/master/gcode/scim-cs/ime-py}{sdim}}] 跨平台（Win32/Linux/Mac OS/Emacs）的输入法（研发中使用的编程语言包括 Python，C++，ObjC，Emacs-lisp）
\item[{\href{https://github.com/scim-im/scim-fcitx}{scim-fcitx}}] GNU/Linux 下的输入法，基于 scim 和 fcitx 移植
\end{description}

\subsubsection{System Software}
\label{sec:org6dd0b91}
\begin{description}
\item[{\href{https://github.com/baohaojun/beagrep}{beagrep}}] 结合搜索引擎的源代码 grep 工具，0.23 秒 grep 两 G 代码
\item[{\href{https://github.com/baohaojun/system-config}{system-config}}] 其他一些较小的脚本/程序，均放在 \href{https://github.com/baohaojun}{github} 上用 git 管理
\end{description}

\section{技术技能}
\label{sec:org700cd5e}

\subsubsection{编程语言 \& 库}
\label{sec:orged949c3}
\begin{description}
\item[{熟练}] Perl，Python，Bash，Emacs Lisp，C，C++，Java，Lua，Qt
\item[{用过}] ObjC，C\#，PHP，Ruby
\end{description}
\subsubsection{写作}
\label{sec:orga25cc0c}
\begin{description}
\item[{文本}] Org-mode，Emacs
\end{description}
\subsubsection{版本管理}
\label{sec:orgcded668}
Git \& Gerrit
\subsubsection{系统管理}
\label{sec:orgbdef7b8}
基于 Debian 的 Linux 发行版系统管理、Bash 脚本编程

\section{教育}
\label{sec:orgc86d002}

\subsubsection{1997 - 2001}
\label{sec:org59613e3}
本科，竺可桢学院、控制科学与工程学院，浙江大学
\subsubsection{2001 - 2004}
\label{sec:org33efaa3}
硕士，中科院自动化所

\section{个人信息}
\label{sec:org9b378f6}
\subsubsection{出生日期}
\label{sec:org4a94583}
1980 年 3 月 10 日
\subsubsection{手机}
\label{sec:org7133008}
18610314439
\subsubsection{E-mail}
\label{sec:org4a06874}
\href{mailto:baohaojun@gmail.com}{baohaojun@gmail.com}
\subsubsection{网址}
\label{sec:orgce54321}
\begin{description}
\item[{博客}] \url{http://baohaojun.github.io}
\item[{代码}] \url{https://github.com/baohaojun}
\item[{System-config}] \url{https://github.com/baohaojun/system-config}
\item[{System-config 使用视频}] \url{https://www.youtube.com/watch?v=qp2b3-Guej0}
\item[{Wrench}] \url{https://github.com/SmartisanTech/Wrench}
\item[{Wrench 视频}] \url{https://v.qq.com/x/page/h0519beib91.html}
\item[{Bbyac}] \url{http://github.com/baohaojun/bbyac}
\item[{Org-jira}] \url{https://github.com/baohaojun/org-jira}
\item[{Ajoke}] \url{https://github.com/baohaojun/ajoke}
\item[{Beagrep}] \url{https://github.com/baohaojun/beagrep}
\end{description}
\end{document}
