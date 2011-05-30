;;-*-coding: emacs-mule;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'apache-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'asm-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'browse-kill-ring-edit-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("bse" "bhj_sock_error(\"\");" nil 4)
    ("c_" "c_str()" nil 0)
    ("cc" "const char*" nil 7)
    ("ccr" "const CRect&" nil 2)
    ("ccs" "const cstring&" nil 8)
    ("cms" "const CString&" nil 11)
    ("cr" "CRect" nil 0)
    ("cs" "cstring" nil 35)
    ("dcs" "c_str()" nil 10)
    ("ld" "LOGD(\"%s: %d\", __FUNCTION__, __LINE__);" nil 0)
    ("le" "LOGE(\"Error: %s %d\\n\", __FUNCTION__, __LINE__);" nil 1)
    ("lk" "LOGW(\"hello world %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 1)
    ("lw" "LOGW(\"hello world %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 0)
    ("mcs" "const CString&" nil 16)
    ("ms" "CString" nil 7)
    ("ndb" "NvOsDebugPrintf(\"hello world %s %d\\r\\n\", __FUNCTION__, __LINE__);" nil 1)
    ("se" "SOCKET_ERROR" nil 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("bpk" "printf(\"hello world %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 0)
    ("cpk" "printk(KERN_WARNING \"for continue %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 25)
    ("ipk" "#include <linux/types.h>
#include <linux/module.h>
#include <linux/proc_fs.h>
#include <linux/kernel.h>
" nil 1)
    ("ld" "LOGD(\"%s: %d\\n\", __FUNCTION__, __LINE__);" nil 6)
    ("ndb" "NvOsDebugPrintf(\"hello world %s %d\\r\\n\", __FUNCTION__, __LINE__);" nil 44)
    ("ne" "NvOsDebugPrintf(\"Error: %s %s %d\\r\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 1)
    ("nk" "NvOsDebugPrintf(\"hello world %s %s %d\\r\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 7)
    ("pe" "printk(KERN_WARNING \"hello error: %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 0)
    ("pf" "printf(\"hello world %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 1)
    ("pk" "printk(KERN_WARNING \"hello world %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 202)
    ("pkh" "#include <linux/types.h>
#include <linux/module.h>
#include <linux/proc_fs.h>
#include <linux/kernel.h>" nil 2)
    ("psk" "printk(KERN_WARNING \"hello world %s %d\\n\", __FUNCTION__, __LINE__);" nil 2)
    ("pw" "printk(KERN_WARNING \"hello warning %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 1)
    ("sdk" "printk(KERN_WARNING \"hello world %s %s %d %s\\n\", __FILE__, __FUNCTION__, __LINE__, dev_name(host->parent));" nil 9)
    ("tab8" "/* Local Variables: */
/* tab-width: 8 */
/* indent-tabs-mode: t */
/* c-basic-offset: 8 */
/* End: */" nil 1)
    ("wpk" "wpa_printf(MSG_ERROR,  \"hello world %s %s %d\\n\", __FILE__, __FUNCTION__, __LINE__);" nil 12)
   ))

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'conf-colon-mode-abbrev-table '())

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '())

(define-abbrev-table 'conf-ppd-mode-abbrev-table '())

(define-abbrev-table 'conf-space-mode-abbrev-table '())

(define-abbrev-table 'conf-unix-mode-abbrev-table '())

(define-abbrev-table 'conf-windows-mode-abbrev-table '())

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '())

(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("=head1" "=head1" cperl-electric-pod 0)
    ("=head2" "=head2" cperl-electric-pod 0)
    ("=over" "=over" cperl-electric-pod 0)
    ("=pod" "=pod" cperl-electric-pod 0)
    ("continue" "continue" cperl-electric-else 0)
    ("do" "do" cperl-electric-keyword 0)
    ("else" "else" cperl-electric-else 0)
    ("elsif" "elsif" cperl-electric-keyword 0)
    ("for" "for" cperl-electric-keyword 0)
    ("foreach" "foreach" cperl-electric-keyword 0)
    ("foreachmy" "foreachmy" cperl-electric-keyword 0)
    ("formy" "formy" cperl-electric-keyword 0)
    ("head1" "head1" cperl-electric-pod 0)
    ("head2" "head2" cperl-electric-pod 0)
    ("if" "if" cperl-electric-keyword 1)
    ("over" "over" cperl-electric-pod 0)
    ("pod" "pod" cperl-electric-pod 0)
    ("unless" "unless" cperl-electric-keyword 0)
    ("until" "until" cperl-electric-keyword 0)
    ("while" "while" cperl-electric-keyword 0)
   ))

(define-abbrev-table 'csharp-mode-abbrev-table
  '(
    ("lv" "Console.WriteLine(\"" nil 1)
   ))

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'dig-mode-abbrev-table '())

(define-abbrev-table 'docTeX-mode-abbrev-table '())

(define-abbrev-table 'edebug-eval-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'emacs-wiki-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table
  '(
    ("ch" "{toc:type=flat|separator=pipe|maxLevel=3}

{numberedheadings}
{numberedheadings}" nil 0)
    ("xdef" "^#def.*" nil 3)
    ("xfun" "^[a-zA-Z_].*" nil 0)
   ))

(define-abbrev-table 'gdb-script-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("bhjd" "BHJDEBUG(\"\");" bhj-bhjd 103)
    ("cltenv" "DISPLAY=10.194.68.84:0 PATH=/usr/atria/bin/:\"$PATH\" " nil 11)
    ("eclt" "DISPLAY=10.194.68.84:0 PATH=/usr/atria/bin/:\"$PATH\" /usr/atria/bin/cleartool" nil 11)
    ("eld" "EnterLeaveDebug();" nil 32)
    ("incbhj" "#define ENABLE_BHJDEBUG
#include \"bhjdebug.h\"" nil 31)
   ))

(define-abbrev-table 'gnus-article-edit-mode-abbrev-table '())

(define-abbrev-table 'gnus-sticky-article-mode-abbrev-table '())

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'gud-mode-abbrev-table '())

(define-abbrev-table 'html-mode-abbrev-table '())

(define-abbrev-table 'ibuffer-occur-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("le" "Log.e(\"bhj\", \"" nil 4)
    ("lv" "Slog.v(TAG, String.format(\"" nil 1)
    ("pk" "Log.i(\"Bhj\", \"hello world\");" nil 1)
    ("sle" "Slog.e(\"bhj\", \" hello world\", new Exception());" nil 1)
    ("string" "String" nil 19)
    ("sv" "System.out.print(String.format(\"" nil 0)
   ))

(define-abbrev-table 'js-mode-abbrev-table '())

(define-abbrev-table 'jython-mode-abbrev-table '())

(define-abbrev-table 'ld-script-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'log-view-mode-abbrev-table '())

(define-abbrev-table 'makefile-automake-mode-abbrev-table '())

(define-abbrev-table 'makefile-bsdmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-gmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-imake-mode-abbrev-table '())

(define-abbrev-table 'makefile-makepp-mode-abbrev-table '())

(define-abbrev-table 'makefile-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table '())

(define-abbrev-table 'moinmoin-mode-abbrev-table '())

(define-abbrev-table 'muse-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'perl-mode-abbrev-table '())

(define-abbrev-table 'php-mode-abbrev-table
  '(
    ("wpk" "wfDebugLog('bhj', __FUNCTION__ . \" hello bhj\\n\");" nil 0)
   ))

(define-abbrev-table 'pike-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'ps-mode-abbrev-table '())

(define-abbrev-table 'ps-run-mode-abbrev-table '())

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("false" "False" nil 2)
    ("ifmain" "if __name__ == '__main__':
   " nil 1)
    ("pqimport" "from PyQt4.QtCore import *
from PyQt4.QtGui import * 
import sys
import os, traceback, thread
import pickle
" nil 4)
    ("pytab" "# Local Variables: #
# tab-width: 4 #
# python-indent: 4 #
# End: #
" nil 0)
    ("signal" "SIGNAL" nil 3)
    ("slef" "self" nil 4)
    ("true" "True" nil 2)
   ))

(define-abbrev-table 'sawfish-console-mode-abbrev-table '())

(define-abbrev-table 'sawfish-mode-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sgml-mode-abbrev-table
  '(
    ("adnroid" "android" nil 1)
    ("andriod" "android" nil 0)
   ))

(define-abbrev-table 'sh-mode-abbrev-table
  '(
    ("cdscript" "if [[ ${0:0:1} == / ]]; then
    cd `dirname $0`;
else 
    cd `pwd`/`dirname $0`;
fi
" nil 0)
    ("shmode" "# Local variables:
# mode: shell-script
# sh-basic-offset: 4
# sh-indent-comment: t
# indent-tabs-mode: nil
# End:
# ex: ts=4 sw=4 et filetype=sh
" nil 0)
   ))

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'sql-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table
  '(
    ("everyborqs" "Cc: \"Gary Mou\" <gary.mou@borqs.com>, \"Sunny Sun\" <sunny.sun@borqs.com>, \"Terry Liang\" <terry.liang@borqs.com>, \"Daphne Mao\" <daphne.mao@borqs.com>, \"Wickey Huang\" <wei.huang@borqs.com>, \"Irene Gao\" <irene.gao@borqs.com>, \"Zhang Hongkuan\" <hongkuan.zhang@borqs.com>, \"Gary Li\" <gary.li@borqs.com>, \"Mike Liu\" <mike.liu@borqs.com>, \"Bi Song\" <song.bi@borqs.com>, \"Zhou Jianmin\" <jianmin.zhou@borqs.com>, \"Liao Xuwang\" <xuwang.liao@borqs.com>, \"Yang Ailin\" <ailin.yang@borqs.com>, \"Mason Xie\" <mason.xie@borqs.com>, \"Young Qu\" <yongzhi.qu@borqs.com>, \"Zhang Ming\" <ming.zhang@borqs.com>, \"Shawn Bow\" <shawn.bow@borqs.com>, \"Derek Hong\" <mingsong.hong@borqs.com>, \"Leo Yao\" <shihong.yao@borqs.com>, \"Zhao Liang\" <liang.zhao@borqs.com>, \"Vincent Ying\" <vincent.ying@borqs.com>, \"Andy Luo\" <andy.luo@borqs.com>, \"Roy Ru\" <yi.ru@borqs.com>, \"Zhou Qi\" <qi.zhou@borqs.com>, \"Tide Yin\" <tide.yin@borqs.com>, \"He Xiaocong\" <xiaocong.he@borqs.com>, \"Zhang Shaofang\" <shaofang.zhang@borqs.com>, \"Xue Fei\" <fei.xue@borqs.com>, \"Charlie Wood\" <chenliang.mu@borqs.com>, \"Deng Yanqun\" <yanqun.deng@borqs.com>, \"David Ding\" <david.ding@borqs.com>, \"Song Zhiqiang\" <zhiqiang.song@borqs.com>, \"Doris Liang\" <dannan.liang@borqs.com>, \"Dumas Liao\" <cheng.liao@borqs.com>, \"Lu Wei\" <wei.lu@borqs.com>, \"Li Jianbing\" <jianbing.li@borqs.com>, \"Wayne Wang\" <wei.wang@borqs.com>, \"Ji Haijun\" <haijun.ji@borqs.com>, \"Zhang Yueting\" <yueting.zhang@borqs.com>, \"Han Bingtian\" <bingtian.han@borqs.com>, \"Eric Lu\" <eric.lu@borqs.com>, \"Zhang Jinyun\" <jinyun.zhang@borqs.com>, \"Liu Huadong\" <huadong.liu@borqs.com>, \"Zhou Dengxiang\" <dengxiang.zhou@borqs.com>, \"Li Rui\" <rui.li@borqs.com>, \"Zhou Anle\" <anle.zhou@borqs.com>, \"Liang Yumin\" <yumin.liang@borqs.com>, \"Jing Kewei\" <kewei.jing@borqs.com>, \"Liu Qiang\" <qiang.liu@borqs.com>, \"Sandy Yang\" <sandy.yang@borqs.com>, \"Yang Xiao\" <xiao.yang@borqs.com>, \"Gong Zheng\" <zheng.gong@borqs.com>, \"Zou Wei\" <wei.zou@borqs.com>, \"Zhou Xin\" <xin.zhou@borqs.com>, \"Bao Hongbin\" <hongbin.bao@borqs.com>, \"Tian Yang\" <yang.tian@borqs.com>, \"Ma chaoying\" <chaoying.ma@borqs.com>, \"Calin Jiang\" <calin.jiang@borqs.com>, \"Kevin Yan\" <kevin.yan@borqs.com>, \"Amanda Li\" <amanda.li@borqs.com>, \"Dong Guiwei\" <guiwei.dong@borqs.com>, \"Harold Shi\" <harold.shi@borqs.com>, \"Leo He\" <leo.he@borqs.com>, \"Chen Jiliang\" <jiliang.chen@borqs.com>, \"Emichael Li\" <emichael.li@borqs.com>, \"Vince Kim\" < vince.kim@borqs.com>, \"River Wang\" <river.wang@borqs.com>, \"Tian Yu\" <yu.tian@borqs.com>, \"Kuang Zheng\" <zheng.kuang@borqs.com>, \"Ray Fan\" <ray.fan@borqs.com>, \"Ma Yilei\" <yilei.ma@borqs.com>, \"Zhang Zhaodong\" <zhaodong.zhang@borqs.com>, \"Zhu Zhongjie\" <zhongjie.zhu@borqs.com>, \"Zheng Yan\" <yan.zheng@borqs.com>, \"Zhang Jianli\" <jianli.zhang@borqs.com>, \"Zhang Min\" <min.zhang@borqs.com>, \"Ren Hailin\" <hailin.ren@borqs.com>, \"Stone Zhang\" <stone.zhang@borqs.com>, \"Hamody Wang\" <hamody.wang@borqs.com>, \"Wen Canfeng\" <canfeng.wen@borqs.com>, \"Xu Jingjing\" <jing.xu@borqs.com>, \"Gao Yang\" <yang.gao@borqs.com>, \"Jessie Zhang\" <xuan.zhang@borqs.com>, \"Zhou Jiabo\" <jiabo.zhou@borqs.com>, \"Wang Yu\" <yu.wang@borqs.com>, \"Li Hui\" <hui.li@borqs.com>, \"Gu Quanyou\" <quanyou.gu@borqs.com>, \"Liang Xunren\" <xunren.liang@borqs.com>, \"Tong Yonghui\" <yonghui.tong@borqs.com>, \"Ren Mingqi\" <mingqi.ren@borqs.com>, \"Yang Guang\" <guang.yang@borqs.com>, \"Zhou Jingze\" <jingze.zhou@borqs.com>, \"Ihero Gu\" <ihero.gu@borqs.com>, \"Weber Wang\" <weber.wang@borqs.com>, \"Bai Yu\" <yu.bai@borqs.com>, \"Qi Zhiheng\" <zhiheng.qi@borqs.com>, \"Nkight Sun\" <nkight.sun@borqs.com>, \"Peng Songbai\" <songbai.peng@borqs.com>, \"Zhang He\" <he.zhang@borqs.com>, \"Yu Shufeng\" <shufeng.yu@borqs.com>, \"Gao Shujun\" <shujun.gao@borqs.com>, \"Zhang Shaowei\" <shaowei.zhang@borqs.com>, \"Nick Zhang\" <nick.zhang@borqs.com>, \"Yang Yinan\" <yinan.yang@borqs.com>, \"Liu Quanhui\" <quanhui.liu@borqs.com>, \"Zhao Fei\" <fei.zhao@borqs.com>, \"Zhang Hui\" <hui.zhang@borqs.com>, \"Wei Jun\" <jun.wei@borqs.com>, \"Wang Xianfeng\" <xianfeng.wang@borqs.com>, \"Betty He\" <betty.he@borqs.com>, \"Kong Xiuli\" <xiuli.kong@borqs.com>, \"Eva Zhang\" <cong.zhang@borqs.com>, \"Anael An\" <anael.an@borqs.com>, \"Wang Yongjie\" <yongjie.wang@borqs.com>, \"Maurice Liu\" <maurice.liu@borqs.com>, \"Zheng Ziyou\" <ziyou.zheng@borqs.com>, \"Zhong Ming\" <ming.zhong@borqs.com>, \"Zhu Jianhua\" <jianhua.zhu@borqs.com>, \"Bao Haojun\" <haojun.bao@borqs.com>, \"Gao Wei\" <wei.gao@borqs.com>, \"Guo Zhen\" <zhen.guo@borqs.com>, \"Jack Bai\" <jack.bai@borqs.com>, \"Ke Tian\" <tian.ke@borqs.com>, \"Yu Xueting\" <xueting.yu@borqs.com>, \"Ding Zhaonan\" <zhaonan.ding@borqs.com>, \"Liu Changhui\" <changhui.liu@borqs.com>, \"He Xiaojuan\" <xiaojuan.he@borqs.com>, \"Liu Yaguang\" <yaguang.liu@borqs.com>, \"Li Peng\" <peng.li@borqs.com>, \"Xu Haibo\" <haibo.xu@borqs.com>, \"Li Junxue\" <junxue.li@borqs.com>, \"Ma Hongbo\" <hongbo.ma@borqs.com>, \"Fei Jianjiang\" <jianjiang.fei@borqs.com>, \"Tony Dong\" <tony.dong@borqs.com>, \"Daniel Jing\" <daniel.jing@borqs.com>, \"Mike Pei\" <mike.pei@borqs.com>, \"David Lin\" <weiqiang.lin@borqs.com>, \"Gao Xun\" <xun.gao@borqs.com>, \"Michael Wang\" <huimin.wang@borqs.com>, \"Jiang Lili\" <lili.jiang@borqs.com>, \"Lin Qian\" <qian.lin@borqs.com>, \"Xue Bin\" <bin.xue@borqs.com>, \"Chen Xiaoyi\" <xiaoyi.chen@borqs.com>, \"Jiang Yu\" <yu.jiang@borqs.com>, \"Cai Jinyan\" <jinyan.cai@borqs.com>, \"Xiao Wei\" <wei.xiao@borqs.com>, \"Liang Hong\" <hong.liang@borqs.com>, \"Li Ruofei\" <ruofei.li@borqs.com>, \"Katie Li\" <katie.li@borqs.com>, \"Nie Lin\" <lin.nie@borqs.com>, \"Chen Kaibing\" <kaibing.chen@borqs.com>, \"Zhao Hui\" <hui.zhao@borqs.com>, \"Chen Xuetong\" <xuetong.chen@borqs.com>, \"Zhang Liming\" <liming.zhang@borqs.com>, \"Don Chen\" <don.chen@borqs.com>, \"Sun Lulu\" <lulu.sun@borqs.com>, \"Zeng Luojun\" <luojun.zeng@borqs.com>, \"Steven Zhang\" <steven.zhang@borqs.com>, \"Guo Zhiqiang\" <zhiqiang.guo@borqs.com>, \"Zou Yuanyuan\" <yuanyuan.zou@borqs.com>, \"Li Jia\" <jia.li@borqs.com>, \"Xia Shuang\" <shuang.xia@borqs.com>, \"Liu Nianrong\" <nianrong.liu@borqs.com>, \"Zhao Ying\" <ying.zhao@borqs.com>, \"David Zhang\" <jun.zhang@borqs.com>, \"Xiao Ping\" <ping.xiao@borqs.com>, \"Sonic Wang\" <zhimeng.wang@borqs.com>, \"Yu Lin\" <lin.yu@borqs.com>, \"Chen Liang\" <liang.chen@borqs.com>, \"Wang Hong\" <hong.wang@borqs.com>, \"Michelle Wang\" <michelle.wang@borqs.com>, \"Luke Zhang\" <luke.zhang@borqs.com>, \"Adam Zou\" <adam.zou@borqs.com>, \"Amanda Xu\" <amanda.xu@borqs.com>, \"Hou Wei\" <wei.hou@borqs.com>, \"Du Minjie\" <minjie.du@borqs.com>, \"Ni Zhenzhou\" <zhenzhou.ni@borqs.com>, \"David Mu\" <yanfeng.mu@borqs.com>, \"Hu Zhili\" <zhili.hu@borqs.com>, \"Liang Wei\" <wei.liang@borqs.com>, \"Liang Xuebin\" <xuebin.liang@borqs.com>, \"Wang Jun\" <jun.wang@borqs.com>, \"LI Yue\" <yue.li@borqs.com>, \"Zheng Qiang\" <qiang.zheng@borqs.com>, \"Chen Liangsi\" <liangsi.chen@borqs.com>, \"Tang Jiyu\" <jiyu.tang@borqs.com>, \"Zhou Can\" <can.zhou@borqs.com>, \"Jia Shikun\" <shikun.jia@borqs.com>, \"Cai Luya\" <luya.cai@borqs.com>, \"Zhang Yuelin\" <yuelin.zhang@borqs.com>, \"Javen Ni\" <javen.ni@borqs.com>, \"Sheng Gao\" <gao.sheng@borqs.com>, \"Sun Yuechen\" <yuechen.sun@borqs.com>, \"Li Kezhen\" <kezhen.li@borqs.com>, \"Ryan Zhao\" <ren.zhao@borqs.com>, \"Zhou Minghao\" <minghao.zhou@borqs.com>, \"Rebecca Sun\" <rebecca.sun@borqs.com>, \"xx\" <yuan.liu@borqs.com>, \"Che Chunli\" <chunli.che@borqs.com>, \"Zhao Qingzhi\" <qingzhi.zhao@borqs.com>, \"Zhang Chun\" <chun.zhang@borqs.com>, \"Xue Yuan\" <yuan.xue@borqs.com>, \"Chen Peng\" <peng.chen@borqs.com>, \"Liu Jie\" <jie.liu@borqs.com>, \"Liu Jiazi\" <jiazi.liu@borqs.com>, \"Zheng Wenkun\" <wenkun.zheng@borqs.com>, \"Zhou Hu\" <hu.zhou@borqs.com>, \"Du Shunpeng\" <shunpeng.du@borqs.com>, \"Wu Hao\" <hao.wu@borqs.com>, \"Lang Yan\" <yan.lang@borqs.com>, \"Agatha Tang\" <agatha.tang@borqs.com>, \"Wang Wei\" <wei.wang1@borqs.com>, \"Li Peng\" <peng.li1@borqs.com>, \"Yang Chunhao\" <chunhao.yang@borqs.com>, \"Li Tao\" <tao.li@borqs.com>, \"Zhang Yu\" <yu.zhang@borqs.com>, \"Li Zhongru\" <zhongru.li@borqs.com>, \"Zhang Xiaohan\" <xiaohan.zhang@borqs.com>, \"Ji Pengcheng\" <pengcheng.ji@borqs.com>, \"Yuan Junfeng\" <junfeng.yuan@borqs.com>, \"Hu Yue\" <yue.hu@borqs.com>, \"Tina Wu\" <jiajia.wu@borqs.com>, \"Shao Yue\" <yue.shao@borqs.com>, \"Wang Bingsen\" <cn001@borqs.com>, \"Shi Donghai\" <cn003@borqs.com>, \"Chen Xixiong\" < ticatica@163.com>, \"Wang Kun\" <kun.wang@borqs.com>, \"Lang Ying\" <ying.lang@borqs.com>, \"Wang Ying\" <ying.wang@borqs.com>, \"Mi Xiaoya\" <mixiaoya2008@yahoo.cn>, \"Liu Xuhua\" <lady19850616@yahoo.com.cn>, \"Cao Lina\" <lina6lina@163.com>, \"Liu Song\" <song.liu@borqs.com>, \"Dong Yan\" <yan.dong@borqs.com>, \"Meng Yang\" <yang.meng@borqs.com>, \"Ling Dongyi\" <lingdongyi@126.com>, \"Zhu Wanjing\" <wanjing.zhu@borqs.com>, \"Zhang Rong\" <zr86aileen@163.com>, \"Gu Jia\" <gj811151878@126.com>, \"Guan Mingjie\" <mingjie.guan@borqs.com>, \"Li Fang\" <fang.li@borqs.com>, \"Zhao Qiuhuan\" <zhaoqiuhuan616@163.com>, \"Guan Bopeng\" <bopeng.guan@borqs.com>, \"Shinro Luo\" <shinro.luo@borqs.com>, \"Qi Yanhong\" <yanhong.qi@borqs.com>, \"Wang Sujuan\" <sujuan.wang@borqs.com>, \"Chen Xu\" <chenxu19880704@126.com>, \"Xiao Yi\" <yi.xiao@borqs.com>, \"Li Meng\" <meng.li@borqs.com>, \"Ji Ying\" <jybnu2006@yahoo.com.cn>, \"Lliu Zhilian\" <zhilian.liu@borqs.com>, \"Zheng Ping\" <pzheng_nego@126.com>, \"Wang Qing\" <dragon624@163.com>, \"Cheng Hui\" <tntch@tom.com>, \"Wang Man\" <wangmannice@126.com>, \"Zhang Mingming\" <mingming.zhang@borqs.com>, \"Fang Di\" <di.fang@borqs.com>, \"Li Zhenhua\" <janeva.lee@gmail.com>, \"Lv Chengyuan\" <lcy644@pku.edu.cn>, \"Yan Cuixia\" <cuixia.yan@borqs.com>, \"Liu Yuming\" <yuming.liu@borqs.com>, \"Wu Qinghua\" <qinghua.wu@borqs.com>, \"Wu Xiaoju\" <xiaoju.wu@borqs.com>, \"Gao Yan\" <yan.gao@borqs.com>, \"Lu Zhenping\" <zhenping.lu@borqs.com>, \"Danny Zhang\" <danny.zhang@borqs.com>, \"Hu Qian\" <huqian23456@163.com>, \"Zheng Zhen\" <zhen.zheng@borqs.com>, \"Lu Xiaowen\" <lesleyluxiaowen@163.com>" nil 0)
    ("fanfan" "Dong Guiwei ‘¶­‘¹ó‘Íþ <guiwei.dong@borqs.com>
Cc: rui li <rui.li@borqs.com>, \"Gao Shujun ‘¸ß‘Êç‘¾ü\" <shujun.gao@borqs.com>, \"'jianli.zhang'\" <jianli.zhang@borqs.com>" nil 1)
    ("lall" "Hong Liang <hliang@adsnexus.com>
Cc: Haojun Bao <hjbao@adsnexus.com>,  Ningbo Zhou <nbzhou@adsnexus.com>,  Lu Yu <lyu@adsnexus.com>,  Jinsong Li <jsli@adsnexus.com>,  Hui Li <hli@adsnexus.com>,  Yi Ru <yru@adsnexus.com>,  Xinjie Guo <xjguo@adsnexus.com>,  Jinglin Shu <jlshu@adsnexus.com>,  Kaihao Chen <khchen@adsnexus.com>,  Feng Yan <fyan@adsnexus.com>,  Nan Geng <ngeng@adsnexus.com>,  Lei Liao <ll@adsnexus.com>,  Ying Wang <ywang@adsnexus.com>,  Jianhua Xing <jhxing@adsnexus.com>,  Jinfeng Lv <jflv@adsnexus.com>,  Jialiang Geng <jlgeng@adsnexus.com>,  Jiyu Zhou <jyzhou@adsnexus.com>,  Ni Li <nli@adsnexus.com>,  Ping Yin <ping@adsnexus.com>,  Shishi Liu <ssliu@adsnexus.com>,  Wentuo Jiang <wtjiang@adsnexus.com>,  Jie Zhang <jzhang@adsnexus.com>,  Leo Yao <lyao@adsnexus.com>,  Yang Gao <ygao@adsnexus.com>,  Charlie Mu <cmu@adsnexus.com>,  Long Liu <lliu@adsnexus.com>,  Zheng Hu <zhu@adsnexus.com>,  Liang Chen <lchen@adsnexus.com>,  Yan Zhao <yzhao@adsnexus.com>,  Wei Liu <wliu@adsnexus.com>,  Xianglin Deng <xldeng@adsnexus.com>,  Xin Liu <xliu@adsnexus.com>,  Xiaoyi Chen <xchen@adsnexus.com>,  Chaoying Ma <cyma@adsnexus.com>,  Na Li <nali@adsnexus.com>,  Bin Li <libin@adsnexus.com>,  Xiao Wang <xwang@adsnexus.com>,  Yunchao Chen <ycchen@adsnexus.com>,  Cunmin Duan <cmduan@adsnexus.com>,  Jian Sun <jsun@adsnexus.com>,  Yu Bai <ybai@adsnexus.com>,  Ke Wang <kwang@adsnexus.com>,  Shawn Bow <shbow@adsnexus.com>,  Li Chen <lichen@adsnexus.com>,  Zhongjie Zhu <zjzhu@adsnexus.com>,  Weifeng Zhou <wfzhou@adsnexus.com>,  Xianfeng Wang <xfwang@adsnexus.com>,  Vincent Ying <vying@adsnexus.com>,  Yunpeng Gao <ypgao@adsnexus.com>,  Xing Chang <xchang@adsnexus.com>,  Sufen Niu <sfniu@adsnexus.com>,  An Wang <awang@adsnexus.com>,  Yi He <yhe@adsnexus.com>,  Jim Zhou <zzhou@adsnexus.com>,  Kuan Wu <kwu@adsnexus.com>,  Yongning Zhao <ynzhao@adsnexus.com>,  Sunny Zhang <sunnyzhang@adsnexus.com>,  Dong Liu <dliu@adsnexus.com>,  Silu Zhang <slzhang@adsnexus.com>,  John Mu <mzy@adsnexus.com>,  Yang Liu <yliu@adsnexus.com>,  Weixing Liu <wxliu@adsnexus.com>,  Jianhua Zhang <jhzhang@adsnexus.com>,  Xiaoyong Wu <bonderwu@adsnexus.com>,  Anle Zhou <azhou@adsnexus.com>,  Ning Liu <nliu@adsnexus.com>,  Ming Shi <mshi@adsnexus.com>,  Jin Wu <jwu@adsnexus.com>,  \"tyin@hzwowpad.com\" <tyin@hzwowpad.com>,  Tongzhu Liu <tzliu@adsnexus.com>,  Li Yang <lyang@adsnexus.com>,  Lu Jiang <jl@adsnexus.com>,  Hewei Wang <hwwang@adsnexus.com>,  Qin Zhang <qzhang@adsnexus.com>,  Fei Liu <fliu@adsnexus.com>,  Fengwei Sun <fwsun@adsnexus.com>,  Bob Di <bdi@adsnexus.com>,  Pan Wang <pwang@adsnexus.com>,  Bo Tong <btong@adsnexus.com>,  Hailin Ren <hlren@adsnexus.com>,  Hongwei Mi <hwmi@adsnexus.com>,  Yan Guan <yguan@adsnexus.com>" nil 0)
    ("nvteam" "Roy <yru@eee168.com>
Cc: Coco Wang <kwang@eee168.com>,  Hong Liang <hliang@eee168.com>,  Hui Li <hli@eee168.com>,  hjbao@eee168.com,  Fei Liu <fliu@adsnexus.com>,  Zhongjie Zhu <zjzhu@adsnexus.com>,  Tide Yin <tyin@hzwowpad.com>,  Vincent Ying <vying@eee168.com>,  Leo Yao <lyao@eee168.com>,  ybai <ybai@eee168.com>, Ming Shi <mshi@adsnexus.com>,  hlren <hlren@eee168.com>, cyma <cyma@eee168.com>" nil 0)
    ("telteam" "Anle Zhou <anle.zhou@gmail.com>
Cc: Kimber <haijun.ji@borqs.com>,  Tide Yin Gmail <tideyin@gmail.com>,  Vincent Ying <vincent.ying@borqs.com>,  \"kevin.yan\" <kevin.yan@borqs.com>,  Anle Zhou <anle.zhou@borqs.com>,  Cheng Liao <cheng.liao@borqs.com>,  Damon Zheng <qiang.zheng@borqs.com>,  Derek Hong <mingsong.hong@borqs.com>,  Eric Lu <eric.lu@borqs.com>,  Harold Shi <harold.shi@borqs.com>,  HL Ren <hailin.ren@borqs.com>,  Hong Liang <hong.liang@borqs.com>,  Hui Li <hui.li@borqs.com>,  JG Ma <junguo.ma@borqs.com>,  Jing Xu <jing.xu@borqs.com>,  JM Zhou <jianmin.zhou@borqs.com>,  Jun Wei <jun.wei@borqs.com>,  KB Chen <kaibing.chen@borqs.com>,  Leo Yao <shihong.yao@borqs.com>,  MQ Ren <mingqi.ren@borqs.com>,  Ping Xiao <ping.xiao@borqs.com>,  Ray Fan <ray.fan@borqs.com>,  Shawn Bow <shawn.bow@borqs.com>,  Stone Zhang <stone.zhang@borqs.com>,  Tide Yin <tide.yin@borqs.com>,  Wei Liang <wei.liang@borqs.com>,  XF Wang <xianfeng.wang@borqs.com>,  XY Chen <xiaoyi.chen@borqs.com>,  YM Liang <yumin.liang@borqs.com>,  Yu Bai <yu.bai@borqs.com>,  Yu Wang <yu.wang@borqs.com>,  ZD Zhang <zhaodong.zhang@borqs.com>,  ZH Qi <zhiheng.qi@borqs.com>" nil 2)
    ("toolsg" "Tide Yin ‘£¨‘Òü‘ÌÎ‘£© <tide.yin@borqs.com>
Cc: \"'Bai yu'\" <yu.bai@borqs.com>, \"'Harold'\" <harold.shi@borqs.com>, \"'yumin'\" <yumin.liang@borqs.com>, 'Ren Mingqi ‘ÈÎ‘Ã÷‘ç÷' <mingqi.ren@borqs.com>, \"'yu.wang'\" <yu.wang@borqs.com>, \"'‘Ð¤‘Æ¼'\" <ping.xiao@borqs.com>" nil 3)
    ("toolteam" "Tide Yin ‘£¨‘Òü‘ÌÎ‘£© <tide.yin@borqs.com>
Cc: \"'Bai yu'\" <yu.bai@borqs.com>, \"'Harold'\" <harold.shi@borqs.com>, \"'yumin'\" <yumin.liang@borqs.com>, 'Ren Mingqi ‘ÈÎ‘Ã÷‘ç÷' <mingqi.ren@borqs.com>, \"'yu.wang'\" <yu.wang@borqs.com>, \"'‘Ð¤‘Æ¼'\" <ping.xiao@borqs.com> " nil 0)
   ))

(define-abbrev-table 'thumbs-mode-abbrev-table '())

(define-abbrev-table 'thumbs-view-image-mode-abbrev-table '())

(define-abbrev-table 'vc-bzr-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-dir-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

(define-abbrev-table 'weblogger-entry-mode-abbrev-table '())

