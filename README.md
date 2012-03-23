# origize.el

Origize centers your content at visual "origo". It inserts spaces and
blank lines to center your content horizontally and vertically, making
it a sweet fit to present pretty ASCII art slides that quickly respond
to zoom and so on.

Origize goes well with a key-binding of your choice:

    (require 'origize)
    (global-set-key (kbd "C-S-l") 'origize/recenter-buffer)

Origize also provides a handy "zoom in/out and recenter" command. In the
future, it might even be improved to recenter all open buffers, perhaps
limited to those in a origize-minor-mode or something like that.

    (global-set-key (kbd "C-M-+") 'origize/zoom-frm-in)
    (global-set-key (kbd "C-M--") 'origize/zoom-frm-out)
