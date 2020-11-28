


## GNU Emacs / N Λ N O 

**GNU Emacs / N Λ N O** is a set of configuration files for GNU Emacs
such as to provide a nice and consistent look and feel as shown below.
It is based on design principles I described in the article "[On the
design of text Editors](https://arxiv.org/abs/2008.06030)" that is
available on arXiv. The light theme is based on [Material
colors](https://material.io/) and the dark theme is based on [Nord
colors](https://www.nordtheme.com/).

<div>
<img src="./images/nano-emacs-light.png" width=47.5%>
<img src="./images/nano-emacs-dark.png"  width=47.5%>
</div>

### Requirements

You need a recent version of
[GNU Emacs](https://www.gnu.org/software/emacs/) and to have the
[Roboto Mono](https://fonts.google.com/specimen/Roboto+Mono) and
[Fira Code](https://fonts.google.com/specimen/Fira+Code) fonts
installed on your system. There are no other dependencies.

### Quick test

The easiest way to test nano emacs is to clone the directory on your
desktop and to type (from inside the cloned repository):

```
$ emacs -q -l nano.el
```

### Installation

If you like the result, you can merge the content of
[nano.el](nano.el) into your emacs configuration file. To do so,
you'll need to modify the `load-path` to include the nano emacs
repository and then call for the different modules.











