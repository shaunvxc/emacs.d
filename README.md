# .emacs.d
My emacs configuration- now structured to work out of the box.

## Install
To setup this emacs configuation as your own, simply run:

```bash
 $ git clone http://github.com/shaunvxc/emacs.d
 $ ln -s emacs.d ~/.emacs.d
```
## Packages
This configuration includes the following packages out of the box (at some point I will add docs for the keybindings, etc):

- [Helm] (https://github.com/emacs-helm/helm) - file/project navigation
- [Magit] (https://github.com/magit/magit) - an amazing git interface
- [Company] (https://github.com/company-mode/company-mode) - autocompletion package
- [Powerline] (https://github.com/milkypostman/powerline) - a souped-up modeline
- [dumb-jump] (https://github.com/jacktasia/dumb-jump) - quickly jump to function definitions
- [expand-region] (https://github.com/magnars/expand-region.el) - a great way to work with blocks of text
- [multiple-cursors] (https://github.com/magnars/multiple-cursors.el) - when you want to do something 1 time for 10 things
- [nyan-cat] (https://github.com/TeMPOraL/nyan-mode) - a fun indicator of your position in the buffer 
- [dark-mint-theme] (https://github.com/shaunvxc/dark-mint-theme) - my choice theme
- And many other custom functions I've written & found on the online overtime (for these, see `elisp/init-fnkeys-vig.el`).
