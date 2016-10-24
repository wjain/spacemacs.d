;;; packages.el --- myorg Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq myorg-packages
      '(
        ;; package names go here
        org
        org-pomodoro
        latex-preview-pane
        org-page
        ))

;; List of packages to exclude.
(setq myorg-excluded-packages '())

;; For each package, define a function myorg/init-<package-name>
;;
;; (defun myorg/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun myorg/post-init-org ()
  (myorg/init-myorg-capture)
  (myorg/init-myorg-publish)
  (myorg/init-myorg-export-pdf)
  (myorg/init-myorg-export-odt)
  (myorg/init-myorg-clock)
  (myorg/init-myorg-babel)
  (myorg/init-myorg-other)
  (myorg/init-myorg-notify)
  )

(defun myorg/post-init-org-pomodoro ()
  (progn
    (setq alert-default-style 'growl)
    (when (spacemacs/system-is-mswindows)
        (setq alert-growl-command "~/.spacemacs.d/plugins/growlforwindows/growlnotify.exe")
        )))

(defun myorg/init-latex-preview-pane ()
  (use-package latex-preview-pane
    :defer t
    :init
    (progn
      ;; (latex-preview-pane-enable)
      ( if (spacemacs/system-is-mswindows)
          ;; write to xelatex.bat on windows
          ;; xelatex -interaction nonstopmode %1 %2 %3 %4 %5
          ;; exit 0
          (setq pdf-latex-command "~/.spacemacs.d/plugins/xelatex/xelatex.bat")
        ;; #!/bin/bash
        ;; xelatex -interaction nonstopmode $*
        ;; exit 0
        (setq pdf-latex-command "~/.spacemacs.d/plugins/xelatex/xelatex.sh")
        )
      )
    )
  )


(defun myorg/init-org-page ()
  (use-package org-page
    :defer t
    :init
    (progn
      (require 'org-page)
      (setq op/repository-directory "~/github/wjain.github.com/")
      (setq op/site-domain "http://wjain.github.io/")
      (setq op/site-main-title "Jain's Page")
      (setq op/site-sub-title "Jain's 闲言碎语。")
      (setq op/personal-github-link "https://github.com/wjain")
      (setq op/personal-disqus-shortname "Jain")
      (setq op/personal-google-analytics-id "UA-69292321-1")
      (setq op/category-config-alist
            '(("blog" ;; this is the default configuration
               :show-meta t
               :show-comment t
               :uri-generator op/generate-uri
               :uri-template "/blog/%y/%m/%d/%t/"
               :sort-by :date     ;; how to sort the posts
               :category-index t) ;; generate category index or not
              ("wiki"
               :show-meta t
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/wiki/%t/"
               :sort-by :mod-date
               :category-index t)
              ("index"
               :show-meta nil
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/"
               :sort-by :date
               :category-index nil)
              ("about"
               :show-meta nil
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/about/"
               :sort-by :date
               :category-index nil)))
      (setq op/theme 'jain)
      )
    )
  )

(defun myorg/init-myorg-export-pdf ()
  "Org export to pdf"
  (progn
    (require 'ox-latex)
    (setq org-latex-pdf-process
          '(
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "rm -fr %b.out %b.log %b.tex auto"
            ))

    (add-to-list 'org-latex-classes  '("ctexart" "\\documentclass[11pt,a4paper]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage{graphicx}
                                        \\usepackage{xeCJK}
                                        \\usepackage{lmodern}
                                        \\usepackage{verbatim}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{tikz}
                                        \\usepackage{wrapfig}
                                        \\usepackage{soul}
                                        \\usepackage{textcomp}
                                        \\usepackage{geometry}
                                        \\usepackage{algorithm}
                                        \\usepackage{algorithmic}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{latexsym}
                                        \\usepackage{natbib}
                                        \\usepackage{fancyhdr}
                                        \\usepackage[xetex,colorlinks=true,CJKbookmarks=true,linkcolor=blue,urlcolor=blue,menucolor=blue]{hyperref}
                                        \\definecolor{foreground}{RGB}{220,220,204}      % 浅灰
                                        \\definecolor{background}{RGB}{100,100,100}      % 浅黑
                                        \\definecolor{preprocess}{RGB}{250,187,249}      % 浅紫
                                        \\definecolor{var}{RGB}{239,224,174}             % 浅肉色
                                        \\definecolor{string}{RGB}{154,150,230}          % 浅紫色
                                        \\definecolor{type}{RGB}{225,225,116}            % 浅黄
                                        \\definecolor{function}{RGB}{140,206,211}        % 浅天蓝
                                        \\definecolor{keyword}{RGB}{239,224,174}         % 浅肉色
                                        \\definecolor{comment}{RGB}{180,98,4}            % 深褐色
                                        \\definecolor{doc}{RGB}{175,215,175}             % 浅铅绿
                                        \\definecolor{comdil}{RGB}{111,128,111}          % 深灰
                                        \\definecolor{constant}{RGB}{220,162,170}        % 粉红
                                        \\definecolor{buildin}{RGB}{127,159,127}         % 深铅绿
                                        \\punctstyle{kaiming}
                                        \\title{}
                                        \\fancyfoot[C]{\\bfseries\\thepage}
                                        \\chead{\\MakeUppercase\\sectionmark}
                                        \\pagestyle{fancy}
                                        \\tolerance=1000
                                        \\usepackage{xcolor}
                                        \\usepackage{listings}
                                        \\lstset{
                                        basicstyle=\\color{foreground}\\small,           % 源代码字体样式
                                        keywordstyle=\\color{function}\\bfseries\\small, % 关键词字体样式
                                        identifierstyle=\\color{doc}\\small,
                                        commentstyle=\\color{comment}\\small,            % 批注样式
                                        stringstyle=\\color{string}\\small,              % 字符串样式
                                        showstringspaces=false,                          % 字符串空格显示
                                        numbers=left,                                    % 行号显示
                                        numberstyle=\\color{preprocess},                 % 行号样式
                                        stepnumber=1,                                    % 行号递增
                                        backgroundcolor=\\color{background},             % 代码框背景色
                                        tabsize=4,                                       % TAB等效空格数
                                        captionpos=t,                                    % 标题位置 top or buttom(t|b)
                                        breaklines=true,                                 % 自动断行
                                        breakatwhitespace=true,                          % 只在空格分行
                                        showspaces=false,                                % 显示空格
                                        columns=flexible,                                % 列样式
                                        frame=none,                                      % 代码框：阴影盒
                                        frameround=tttt,                                 % 代码框： 圆角
                                        framesep=0pt,
                                        framerule=8pt,
                                        rulecolor=\\color{background},
                                        fillcolor=\\color{white},
                                        rulesepcolor=\\color{comdil},
                                        framexleftmargin=10mm
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes '("macart" "\\documentclass[11pt]{article}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 ;; beamer class, for presentations
                 '("beamer"
                   "\\documentclass[11pt,professionalfonts]{beamer}
                    \\mode
                    \\usetheme{{{{Warsaw}}}}
                    %\\usecolortheme{{{{beamercolortheme}}}}
                    \\beamertemplateballitem
                    \\setbeameroption{show notes}
                    \\usepackage{graphicx}
                    \\usepackage{tikz}
                    \\usepackage{xcolor}
                    \\usepackage{xeCJK}
                    \\usepackage{amsmath}
                    \\usepackage{lmodern}
                    \\usepackage{fontspec,xunicode,xltxtra}
                    \\usepackage{polyglossia}
                    \\usepackage{listings}
                    \\institute{{{{beamerinstitute}}}}
                    \\subject{{{{beamersubject}}}}
                    \\definecolor{foreground}{RGB}{220,220,204}      % 浅灰
                    \\definecolor{background}{RGB}{100,100,100}      % 浅黑
                    \\definecolor{preprocess}{RGB}{250,187,249}      % 浅紫
                    \\definecolor{var}{RGB}{239,224,174}             % 浅肉色
                    \\definecolor{string}{RGB}{154,150,230}          % 浅紫色
                    \\definecolor{type}{RGB}{225,225,116}            % 浅黄
                    \\definecolor{function}{RGB}{140,206,211}        % 浅天蓝
                    \\definecolor{keyword}{RGB}{239,224,174}         % 浅肉色
                    \\definecolor{comment}{RGB}{180,98,4}            % 深褐色
                    \\definecolor{doc}{RGB}{175,215,175}             % 浅铅绿
                    \\definecolor{comdil}{RGB}{111,128,111}          % 深灰
                    \\definecolor{constant}{RGB}{220,162,170}        % 粉红
                    \\definecolor{buildin}{RGB}{127,159,127}         % 深绿铅
                    \\lstset{
                    basicstyle=\\color{foreground}\\small,           % 源代码字体样式
                    keywordstyle=\\color{function}\\bfseries\\small, % 关键词字体样式
                    identifierstyle=\\color{doc}\\small,
                    commentstyle=\\color{comment}\\small,            % 批注样式
                    stringstyle=\\color{string}\\small,              % 字符串样式
                    showstringspaces=false,                          % 字符串空格显示
                    %numbers=left,                                    % 行号显示
                    %numberstyle=\\color{preprocess},                 % 行号样式
                    %stepnumber=1,                                    % 行号递增
                    backgroundcolor=\\color{background},             % 代码框背景色
                    tabsize=4,                                       % TAB等效空格数
                    captionpos=t,                                    % 标题位置 top or buttom(t|b)
                    breaklines=true,                                 % 自动断行
                    breakatwhitespace=true,                          % 只在空格分行
                    showspaces=false,                                % 显示空格
                    columns=flexible,                                % 列样式
                    frame=none,                                      % 代码框：阴影盒
                    frameround=tttt,                                 % 代码框： 圆角
                    framesep=0pt,
                    framerule=8pt,
                    rulecolor=\\color{background},
                    fillcolor=\\color{white},
                    rulesepcolor=\\color{comdil},
                    framexleftmargin=10mm
                    }
                    "
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\begin{frame}[fragile]\\frametitle{%s}"
                    "\\end{frame}"
                    "\\begin{frame}[fragile]\\frametitle{%s}"
                    "\\end{frame}")))

    (setq org-latex-default-class "ctexart")

    (setq org-latex-listings t)

    ;; MobileOrg
    (setq org-mobile-directory "~/Dropbox/Dropbox")
    (setq org-mobile-files org-agenda-files)
    (setq org-directory "~/Documents/TODO/")
    (setq org-mobile-inbox-for-pull "~/Documents/TODO/inbox.org")
    )
  )

(defun myorg/init-myorg-publish ()
  "Org publish project"
  (progn
    (setq org-publish-project-alist
          '(("note-org"
             :base-directory "~/Documents/notes/src"
             :publishing-directory "~/Documents/notes/publish"
             :base-extension "org"
             :recursive t
             :publishing-function org-publish-org-to-html
             :auto-index t
             :index-filename "index.org"
             :index-title "index"
             :link-home "index.html"
             :section-numbers t
             :style "<link rel=\"stylesheet\" href=\"./wheer.css\" type=\"text/css\"/>")
            ("note-static"
             :base-directory "~/Documents/notes/org/src"
             :publishing-directory "~/Documents/notes/publish"
             :recursive t
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
             :publishing-function org-publish-attachment)
            ("note"
             :components ("note-org" "note-static")
             :author "jain_y@126.com"
             )))
    (setq org-export-htmlize-output-type 'inline-css)

    (defadvice org-publish (around org-publish-advice activate)
      "Stop running major-mode hook when org-publish"
      (let ((old load-user-customized-major-mode-hook))
        (setq load-user-customized-major-mode-hook nil)
        ad-do-it
        (setq load-user-customized-major-mode-hook old)))
    )
  )

(defun myorg/init-myorg-capture ()
  "Org capture templates"
  (progn
    ;; org-capture-templates
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/Documents/TODO/gtd.org" "Tasks")
             "* TODO %?\n  %i" :prepend t)

            ("j" "Journal" entry (file+datetree "~/Documents/notes/src/notes/journal.org" "Journal")
             "* %?\nEntered on %U\n %i\n %a" :prepend t :empty-lines 1)

            ("w" "WorkNote" entry (file+headline "~/Documents/notes/src/notes/worknotes.org" "WorkNotes")
             "* %U %?\n\n  %i" :prepend t :empty-lines 1)

            ("l" "LifeNote" entry (file+headline "~/Documents/notes/src/notes/liftnotes.org" "LiftNotes")
             "* %U %?\n\n  %i" :prepend t :empty-lines 1)

            ("s" "StudyNote" entry (file+headline "~/Documents/notes/src/notes/studynotes.org" "StudyNotes")
             "* %U %?\n\n  %i" :prepend t :empty-lines 1)
            ))

    (setq org-agenda-files (list  "~/Documents/TODO/gtd.org"
                                  "~/Documents/notes/src/notes/liftnotes.org"
                                  "~/Documents/notes/src/notes/journal.org"
                                  "~/Documents/notes/src/notes/worknotes.org"
                                  "~/Documents/notes/src/notes/studynotes.org")))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

  )

(defun myorg/init-myorg-export-odt ()
  "Org export odt"
  (progn

    (if (spacemacs/system-is-mac)
        (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
      (setq org-odt-convert-processes '(("LibreOffice" "soffice --headless --convert-to %f%x --outdir %d %i"))))

    ;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
    (defun narrow-or-widen-dwim ()
      "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
      (interactive)
      (cond ((buffer-narrowed-p) (widen))
            ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
            ((equal major-mode 'org-mode) (org-narrow-to-subtree))
            (t (error "Please select a region to narrow to"))))

    ;; Various preferences
    (setq org-log-done t
          org-completion-use-ido t
          org-edit-src-content-indentation 0
          org-edit-timestamp-down-means-later t
          org-agenda-start-on-weekday nil
          org-agenda-span 14
          org-agenda-include-diary t
          org-agenda-window-setup 'current-window
          org-fast-tag-selection-single-key 'expert
          org-export-kill-product-buffer-when-displayed t
          ;; org v7
          org-export-odt-preferred-output-format "docx"
          ;; org v8
          org-odt-preferred-output-format "docx"
          org-tags-column 80
          ;; org-startup-indented t
          ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
          ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
          org-agenda-inhibit-startup t ;; ~50x speedup
          org-agenda-use-tag-inheritance nil ;; 3-4x speedup
          ;; }}
          )
    )
  )

(defun myorg/init-myorg-clock ()
  "Org clock"
  (progn

    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Show the clocked-in task - if any - in the header line
    (defun sanityinc/show-org-clock-in-header-line ()
      (setq-default header-line-format '((" " org-mode-line-string " "))))

    (defun sanityinc/hide-org-clock-from-header-line ()
      (setq-default header-line-format nil))

    (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
    (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
    (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

    (eval-after-load 'org-clock
      '(progn
         (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
         (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

    (eval-after-load 'org
      '(progn
         (require 'org-clock)
                                        ; @see http://irreal.org/blog/?p=671
         (setq org-src-fontify-natively t)
         ;; (require 'org-fstree)
         (defun soft-wrap-lines ()
           "Make lines wrap at window edge and on word boundary,
        in current buffer."
           (interactive)
           ;; (setq truncate-lines nil)
           (setq word-wrap t)
           )
         (add-hook 'org-mode-hook '(lambda ()
                                     (setq evil-auto-indent nil)
                                     (soft-wrap-lines)
                                     ))))
    )
  )

(defun myorg/init-myorg-babel ()
  "active Babel languages"
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (R . t)
       (emacs-lisp . t)
       (dot . t)
       (ditaa . t)
       (python . t)
       (C . t)
       (perl . t)
       (ruby . t)
       (gnuplot . t)
       (clojure . t)
       (sh . t)
       (ledger . t)
       (org . t)
       (plantuml . t)
       (latex . t)
       (sql . t)
       (java . t)
       ))

    (setq org-plantuml-jar-path "~/.spacemacs.d/plugins/plantuml/plantuml.jar")
    (setq org-ditaa-jar-path "~/.spacemacs.d/plugins/ditaa/ditaa0_9.jar")

    (setf org-latex-default-packages-alist
          (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

    (setq org-babel-clojure-backend 'cider)

    )
  )

(defun myorg/init-myorg-other ()
  "Org misc"
  (progn
    (defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
      (let ((browse-url-browser-function
             (cond ((equal (ad-get-arg 0) '(4))
                    'browse-url-generic)
                   ((equal (ad-get-arg 0) '(16))
                    'choose-browser)
                   (t
                    (lambda (url &optional new)
                      (w3m-browse-url url t))))))
        ad-do-it))

    (require 'org-protocol)

    ;; {{ org2nikola set up
    (setq org2nikola-output-root-directory "~/.config/nikola")
    (setq org2nikola-use-google-code-prettify t)
    (setq org2nikola-prettify-unsupported-language
          '(elisp "lisp"
                  emacs-lisp "lisp"))
    ;; }}
    )
  )

(defun myorg/init-myorg-notify ()
  "growl and appt"

  (setq appt-growl-command (executable-find "growlnotify"))
  (when (spacemacs/system-is-mswindows)
      (setq appt-growl-command "~/.spacemacs.d/plugins/growlforwindows/growlnotify.exe")
    )

  (setq appt-time-msg-list nil)    ;; clear existing appt list
  (setq appt-display-interval '10) ;; warn every 10 minutes from t - appt-message-warning-time
  (setq
   appt-message-warning-time '10  ;; send first warning 10 minutes before appointment
   appt-display-mode-line nil     ;; don't show in the modeline
   appt-display-format 'window    ;; pass warnings to the designated window function
   )
  (appt-activate 1)                ;; activate appointment notification
  (display-time)                   ;; activate time display
  (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

  ;; 保存時にorg-agenda-to-apptを実行
  (add-hook 'org-mode-hook
            (lambda() (add-hook 'before-save-hook
                                'org-agenda-to-appt)))

  (setq appt-coding-system 'utf-8)
  (if (spacemacs/system-is-mswindows)
      (progn
        (setq appt-coding-system 'gbk)
        )
      )

  (defun appt-growl-notify (title msg)
    (shell-command (concat appt-growl-command
                           " --message "    (encode-coding-string msg appt-coding-system)
                           " --title "      (encode-coding-string title appt-coding-system)
                           " --appIcon Emacs")))

  ;; designate the window function for my-appt-send-notification
  (defun my-appt-display (min-to-app new-time msg)
    (appt-growl-notify
     (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title
     (format "'%s'" msg)))                                ;; passed to -message

  (setq appt-disp-window-function (function my-appt-display))
  (setq appt-delete-window-function 'ignore)
  )

(defun jain/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun jain/org-archive-cancelled-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))
