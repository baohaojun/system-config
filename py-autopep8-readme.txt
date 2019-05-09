Provides the `py-autopep8' command, which uses the external "autopep8"
tool to tidy up the current buffer according to Python's PEP8.

To automatically apply when saving a python file, use the
following code:

  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

To customize the behaviour of "autopep8" you can set the
py-autopep8-options e.g.

  (setq py-autopep8-options '("--max-line-length=100"))
