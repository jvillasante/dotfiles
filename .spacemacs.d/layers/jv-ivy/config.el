(setq ivy-display-style 'fancy
  ivy-count-format "(%d/%d) "
  ivy-use-virtual-buffers t      ; to make ivy-views appear on the buffers list
  ivy-virtual-abbreviate 'full   ; default is name
  ivy-initial-inputs-alist nil   ; remove initial ^ input.
  ivy-extra-directories nil      ; remove . and .. directory. (default value: ("../" "./"))
  ivy-height 10)
