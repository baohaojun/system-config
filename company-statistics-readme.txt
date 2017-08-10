Package installed from elpa.gnu.org:

  (add-hook 'after-init-hook #'company-statistics-mode)

Manually installed: make sure that this file is in load-path, and

  (require 'company-statistics)
  (company-statistics-mode)

Every time a candidate is chosen using company-mode, we keep track of this
(for a limited amount of recent choices).  When presenting completion
candidates next time, they are sorted according to the score thus acquired.

The same candidate might occur in different modes, projects, files etc., and
possibly has a different meaning each time.  Therefore along with the
completion, we store some context information.  In the default (heavy)
configuration, we track the overall frequency, the major-mode of the buffer,
the last preceding keyword, the parent symbol, and the filename (if it
applies), and the same criteria are used to score all possible candidates.
