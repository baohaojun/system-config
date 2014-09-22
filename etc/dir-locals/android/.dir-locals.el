;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((prog-mode
  (flycheck-mode . nil)
  (ac-clang-flags "-I/usr/include/c++/4.9"
                  "-I/usr/include/x86_64-linux-gnu/c++/4.9"
                  "-I/usr/include/c++/4.9/backward"
                  "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include"
                  "-I/usr/local/include"
                  "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed"
                  "-I/usr/include/x86_64-linux-gnu"
                  "-I/usr/include"
                  "-I/home/bhj/src/android/bionic/libc/include"
                  "-I.")
  (flycheck-clang-include-path "/usr/include/c++/4.9"
                               "/usr/include/x86_64-linux-gnu/c++/4.9"
                               "/usr/include/c++/4.9/backward"
                               "/usr/lib/gcc/x86_64-linux-gnu/4.9/include"
                               "/usr/local/include"
                               "/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed"
                               "/usr/include/x86_64-linux-gnu"
                               "/usr/include"
                               ".")))
