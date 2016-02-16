;; backspace
(keyboard-translate ?\C-h ?\C-?)

;; undo
(global-set-key (kbd "C-z") 'undo)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; close bracket automatically
(electric-pair-mode 1)

;; copy current line
(global-set-key "\C-x\C-y" "\C-a\C-k\C-k\C-y")

;; duplicate current line
(global-set-key (kbd "C-s-d") "\C-a\C-k\C-k\C-y\C-y\C-p")

;; swap lines seamlessly
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
      (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-s-n") 'move-line-down)
(global-set-key (kbd "C-s-p") 'move-line-up)

;; 正規表現検索のデフォルト化
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-s") 'vr/query-replace)

(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

;; 起動時に画面を2分割
(setq w (selected-window))
(setq w2 (split-window w nil t))

;; C-c c で compile コマンドを呼び出す
(define-key mode-specific-map "c" 'compile)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)


;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
 ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; 日本語の設定（UTF-8）
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)


;; Auto Complete
;;
;; auto-complete-config の設定ファイルを読み込む。
(require 'auto-complete-config)

;; よくわからない
(ac-config-default)

;; TABキーで自動補完を有効にする
(ac-set-trigger-key "TAB")

;; auto-complete-mode を起動時に有効にする
(global-auto-complete-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (spolsky)))
 '(custom-safe-themes
   (quote
    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" default)))
 '(inhibit-startup-screen t)
 '(slime-backend "~/.emacs.d/slime/swank-loader.lisp"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; undo-tree
;;
;; undo-tree を読み込む
(require 'undo-tree)

;; undo-tree を起動時に有効にする
(global-undo-tree-mode t)

;; M-/ をredo に設定する。
(global-set-key (kbd "M-/") 'undo-tree-redo)


;; rotate
(require 'rotate)
(global-set-key (kbd "C-x w") 'rotate-window)
(global-set-key (kbd "C-x C-;") 'rotate-layout)


;; helm
(require 'helm-config)
(helm-mode 1)


;; slime
;;
;; SBCLをデフォルトのCommon Lisp処理系に設定
(setq inferior-lisp-program "sbcl")
;; ~/.emacs.d/slimeをload-pathに追加
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
;; SLIMEのロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner)) 


;; twittering-mode
;;
(require 'twittering-mode)
(setq twittering-use-master-password t)
; パスワード暗号ファイル保存先変更 (デフォはホームディレクトリ)
(setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")
;; アイコンを表示する
(setq twittering-icon-mode t)
;; 300秒ごとに更新
(setq twittering-timer-interval 300)


;; ;;popwin
;; ;;
;; (require 'popwin)
;; (setq display-buffer-function 'popwin:display-buffer)
;; ;; Apropos
;; (push '("*slime-apropos*") popwin:special-display-config)
;; ;; Macroexpand
;; (push '("*slime-macroexpansion*") popwin:special-display-config)
;; ;; Help
;; (push '("*slime-description*") popwin:special-display-config)
;; ;; Compilation
;; (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
;; ;; Cross-reference
;; (push '("*slime-xref*") popwin:special-display-config)
;; ;; Debugger
;; (push '(sldb-mode :stick t) popwin:special-display-config)
;; ;; REPL
;; (push '(slime-repl-mode) popwin:special-display-config)
;; ;; Connections
;; (push '(slime-connection-list-mode) popwin:special-display-config)
