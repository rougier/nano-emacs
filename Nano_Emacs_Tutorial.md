# Nano Theme Tutorial
### Clean And Minimalistic Emacs Theme
[Original GitHub Repo Author: Rougier](https://github.com/rougier/nano-emacs)

## Installation

1. First let’s create a directory where we will install our files. With the “File Explorer” go to…

```
C:\Program Files\Users\"User"
```
Here we want to delete the folder ".emacs.d".

2. In this directory create two more folders. Like so. 

```
C:\Program Files\Users\"User"\.config\emacs
```

Create a folder and name it ”.config”.  Open the ”.config” folder. Create a new folder then rename it “emacs“.

In the "emacs" folder you want to create three files. An “early-init.el”, “init.el”, and a “config.org” file. You can do this by creating a normal “.txt” and renaming the “.txt” to el or org. But make sure you actually changed the extension. Otherwise, If you feel comfortable using GNU Emacs go ahead and create those files there. 

3. From here, simply copy and paste the code for each file as shown.


### early-init.el

```emacs-lisp
(setq package-enable-at-startup nil)
```





### init.el

```emacs-lisp
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
```




### config.org

```emacs-lisp
#+TITLE: Enter any title for the document
#+AUTHOR: Your name here
#+DESCRIPTION: pretty self-explanatory.
#+STARTUP: showeverything ;; makes the document open all nested items.
#+OPTIONS: toc:2 ;; shows only the H1 and H2 headers in the table of contents.

* IMPORTANT PROGRAMS TO LOAD FIRST
** Straight Package Manager

#+begin_src emacs-lisp
    (defvar bootstrap-version)
    (let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
	(with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

  ;; Integration with use-package
  (straight-use-package 'use-package)
#+end_src

** Theme NANO
#+begin_src emacs-lisp
  (straight-use-package
    '(nano :type git :host github :repo "rougier/nano-emacs"))

  (require 'nano)
#+end_src

```

4. Make sure to save each file and run emacs. If all is well you should see emacs doing the installation. After you close and reopen emacs you should see the nano theme.






## Emacs Meta/Modifier Keys

The M in M-x refers to the “Meta” key or modifier key. 

A **Meta/modifier key** — is a key on a computer keyboard that modifies the action of another key when the two are pressed together.

The Meta/modifier key, in this case, is `Alt` and `⌥ Option`.

So, M-x becomes `Alt` + `x` for Windows or `⌥ Ooption` + `x` for Mac.






## Toggle Theme

To toggle between themes you have to type `M-x: nano-theme-toggle` or `M-x: nano-t` to switch back and forth from “light” and “dark.”


For **Windows** press `Alt` + `x` and type `nano-theme-toggle` or just `nano-t`.

For **Mac OS** press `⌥ Ooption` + `x` and type `nano-theme-toggle` or just `nano-t`.






## Start Theme in Dark Mode


### Shortcut

If you’re launching emacs from a shortcut, Right-click the shortcut to get to “Properties” and add `-dark` (including the hyphen) after the entire path where it says “Target” For example:

```
“Target: C:\Program Files\Emacs\.config\emacs\emacs-29.1\bin\runemacs.exe” -dark
```

Click “Apply,“ then click “OK.”






### Command Prompt

For **Windows** press `⊞ Win` + `r` then type “cmd” and press `Enter`.

For **Mac OS** press `⌘ Command` + `␣ Spacebar` then type “Terminal” and press `Enter`.

At the Terminal type “emacs -dark” and press `enter`.






### A better way?

I haven’t found a more sophisticated way of doing it yet, so this will have to do for now. Take care and have a great day. God bless you.

