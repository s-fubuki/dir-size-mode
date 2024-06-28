;;; dir-size-mode.el --- Directory size mode
;; Copyright (C) 2023-2024  Shiina fubuki

;; Author: Shiina fubuki <fubukiATfrill.org>
;; Keywords: files
;; Version: $Revision: 1.15 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; dired のマーク操作と連動した directory のサイズ表示.
;; ディレクトリをマークするとそのディレクトリのサイズがサイズ表示位置に表示され
;; マークしたディレクトリサイズの総計がモードラインに表示されます.
;;
;; ディレクトリサイズの出ない `Windows' の Emacs 向けです.
;; Windows エクスプローラのプロパティで見れるやつと*だいたい*同じで
;; 目標は同じにするですが、細かく解析した訳でもないので
;; 誤差がある場合があるかもしれません.

;; 注意1
;; 音楽ファイルディレクトリのサイズ検出用に作ったため、
;; ディレクトリの深さやファイルの数等でのセーフティ処理は行なっていません.
;; 巨大な子孫持ちディレクトリ等だと長らく帰ってこれなくなるかもしれません.

;; 注意2
;; dired の表示設定によっては表示がズレる等してうまく動かないかもしれません.
;; 一応 `ls-lisp-verbosity' の設定は影響しないように気をつけました.

;;; Installation:

;; ;; for init.el 忘れたときのメニュー設定
;; ;; dired operate menu の最下部にこのコマンドが設定されます.
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (define-key-after
;;               (lookup-key dired-mode-map [menu-bar operate])
;;               [dir-size-mode]
;;               (list 'menu-item "Dir Size Mode" #'dir-size-mode
;;                     :button '(:toggle . dir-size-mode)))))
;; 
;; (autoload 'dir-size-mode "dir-size-mode" nil t)

;;; Code:
(require 'dired)

(defgroup mds nil
  "Directory Size group."
  :group 'dired
  :version "30.0.50"
  :prefix "dir-size-")

(defcustom dir-size-mode nil
  "Directory size minor mode start value."
  :type 'boolean
  :group 'mds)

(defcustom dir-size-suffix "." ;; "\\.mp3\\'"
  "Target suffix regexp."
  :type 'regexp
  :group 'mds)

(defcustom dir-size-lighter nil
  "mode line に表示される `dir-size-mode' lighter.
設定するときは先頭にスペースを入れること.
末尾に区切記号を入れるとサイズ表示が見易くなる.
Example: \" Mds:\""
  :type '(choice (const nil) string)
  :group 'mds)

(defcustom dir-dead-line 700
  "Face change dead line MB."
  :type 'integer
  :group 'mds)

(defgroup mds-faces nil
  "Directory Size face group."
  :group 'mds
  :group 'faces
  :version "30.0.50"
  :prefix "dir-size-")

(defface dir-size-mark
  '((t :inherit bold :foreground "Yellow"))
  "Mdir size face."
  :group 'mds-faces)

(defface dir-size-safe nil
  "Mdir size dead face."
  :group 'mds-faces)

(defface dir-size-dead
  '((t :inherit bold :foreground "Firebrick"))
  "Mdir size dead face."
  :group 'mds-faces)

(defvar dir-buffer-list nil)

(defvar-local dir-size-alist nil)
(defvar-local dir-size-cache nil)
(defvar-local dir-size-zero  0) ;; 0 or 0.0
(defvar-local dir-size-sum   0) 
(defvar-local dir-size-over  nil) 

(defun dir-size-directory (dir)
  "DIR を再帰的に降り `dir-size-suffix' ファイルのサイズの合計を MB で戻す.
一応ファイルにも対応しているが上位関数で弾いているので意味はない."
  (let ((sum dir-size-zero)
        (files (if (not (file-accessible-directory-p dir))
                   (list dir)
                 (directory-files-recursively dir dir-size-suffix))))
    (dolist (f files (/ sum (* 1024 1024))) ; MB
      (and (file-attributes f)
           (setq sum (+ (file-attribute-size (file-attributes f)) sum))))))

(defun dir-size-total ()
  (interactive)
  (let (over)
    (setq dir-size-sum (dir-size-sum)
          dir-size-over (and dir-dead-line (< dir-dead-line dir-size-sum)))
    (message "%s MB" (propertize (number-to-string dir-size-sum)
                                 'face (if dir-size-over 'dir-size-dead)))))

(defun dir-size-string (val width face)
  "VAL を WIDTH 幅に文字列化し FACE でプロパティライズ.
WIDTH に足りない分はアタマに SPACE をパディング."
  (let ((str (number-to-string (round val))))
    (propertize
     (concat (make-string (- width (length str)) 32) str)
     'face face)))

(defun dir-size-pos ()
  "Dired 上(ls -l)の file size 表示位置を \(BEG END) のリストで戻す."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (re-search-forward dired-re-perms nil t 1) ; ファイル表示行 パーミッションの終りへ
      (re-search-forward
       "\\(?1: +\\)\\(?2:[0-9.GMK]+\\) [JFMAJSOND][a-z][a-z] [ 123][0-9] \
\\([012][0-9]:[0-9][0-9]\\| [0-9]\\{4\\}\\) "  nil t 1)
      (list (1+ (match-beginning 1)) (match-end 2)))))

(defun dir-size-sum ()
  "`dir-size-alist' の cdr の総計を返す."
  (let ((result 0))
    (dolist (a dir-size-alist result)
      (setq result (+ (cdr a) result)))))

(defun dir-size-unmark-all (&optional round)
  (interactive)
  (dolist (buff (or (and round dir-buffer-list) (list (current-buffer))))
    (with-current-buffer buff
      (remove-overlays nil nil 'category 'dir-size)
      (setq dir-size-alist nil
            dir-size-sum   dir-size-zero
            dir-size-over  nil)
      (setq dir-buffer-list (remove buff dir-buffer-list))
      (and (local-variable-p 'minor-mode-alist)
           (kill-local-variable 'minor-mode-alist)))))

(defun dir-size-mark (arg &optional interactive)
  "Dired 上でポイントされた Directory を再帰下降して総サイズを通知.
結果は `dir-size-alist' に積算されていくが、
同じ位置で再実行するとその分は減算される.
`dired-mark' 用 around アドバイス関数."
  (interactive (list current-prefix-arg t))
  (let ((marks (dired-remember-marks (point-min) (point-max))))
    (remove-overlays nil nil 'category 'dir-size)
    (setq dir-size-alist nil
          dir-size-sum   dir-size-zero
          dir-size-over  nil)
    (dolist (f marks) (dir-size-mark-body (car f)))))

(defun dir-size-mark-body (file)
  (save-excursion
    (dired-goto-file file)
    (let* ((range (dir-size-pos))
           (width (- (apply #'- range)))
           (ov    (apply #'make-overlay range))
           (size  (or (cdr (assoc file dir-size-cache))
                      (dir-size-directory file))))
      (add-to-list 'dir-size-alist (cons file size))
      (add-to-list 'dir-size-cache (cons file size))
      (add-to-list 'dir-buffer-list (current-buffer))
      (overlay-put ov 'category 'dir-size)
      (overlay-put ov 'display
                   (dir-size-string size width 'dir-size-mark))
      (dir-size-total))))

(defvar dir-size-menu-map
  (let ((map (make-sparse-keymap "Mdir Size")))
    (define-key map [dir-size-mode]   
                '(menu-item "mdir size mode" dir-size-mode
                            :button (:toggle . dir-size-mode)))
    map))

(fset 'dir-size-menu-map dir-size-menu-map)

(defvar dir--size-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'dir-size-menu-map)
    map))

(defvar-local dir-size-lighter*
    `(dir-size-over
      (:propertize
       (:eval (format "%s%d" (or dir-size-lighter " ") dir-size-sum))
       face dir-size-dead
       mouse-face mode-line-highlight
       keymap ,dir--size-menu-map)
      (:propertize
       (:eval (format "%s%d" (or dir-size-lighter " ") dir-size-sum))
       face dir-size-safe
       mouse-face mode-line-highlight
       keymap ,dir--size-menu-map)))

(put 'dir-size-lighter* 'risky-local-variable t)

(define-minor-mode dir-size-mode "Directory size mode."
  :global  t
  :lighter dir-size-lighter*
  (cond
   (dir-size-mode
    (advice-add 'dired-mark :after #'dir-size-mark)
    (advice-add 'dired-unmark-all-marks :after #'dir-size-unmark-all)
    (dir-size-mark nil))
   (t
    (dir-size-unmark-all 'round)
    (advice-remove 'dired-mark #'dir-size-mark)
    (advice-remove 'dired-unmark-all-marks #'dir-size-unmark-all))))

(provide 'dir-size-mode)
;;; dir-size-mode.el ends here
