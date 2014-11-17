;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((c-mode
  (flycheck-mode . nil)
  (ac-clang-flags
   "-D__KERNEL__"
   "-include"
   "/home/bhj/src/linux/include/generated/autoconf.h"
   "-I/home/bhj/src/linux/arch/arm/mach-msm/include/"
   "-I/home/bhj/src/linux/include/"
   "-I/home/bhj/src/linux/arch/arm/include/"
   "-I/home/bhj/src/linux/arch/arm/include/generated/"
   "-I.")))
