(eval-after-load 'bbyac
  '(progn
     (define-key bbyac-mode-map (kbd "<f1> <return>") 'bbyac-expand-symbols)
     (define-key bbyac-mode-map (kbd "<f1> .") 'bbyac-expand-symbols-by-major-mode)
     (define-key bbyac-mode-map (kbd "<f1> <RET>") 'bbyac-expand-symbols)
     (define-key bbyac-mode-map (kbd "<f2> <return>") 'bbyac-expand-substring)
     (define-key bbyac-mode-map (kbd "<f2> <RET>") 'bbyac-expand-substring)
     (define-key bbyac-mode-map (kbd "<f2> l") 'bbyac-expand-lines)
     ))
