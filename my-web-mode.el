;;; my-web-mode.el --- custmized my/web-mode

(use-package web-mode
  :mode (("\\.s?html?\\'" . web-mode))
  :hook ((web-mode .  my/set-block-php-code-properties)
		 (web-mode . (lambda ()
					   (setq-local corfu-auto nil)
					   (setq-local completion-at-point-functions
								   (list (cape-capf-super
										  #'my/web-completion-at-point
										  #'cape-keyword)))
					   (setq-local completion-styles '(basic orderless partial-completion))
					   ))
		 (after-change-functions . my-set-string-php-code-properties))
  :bind (:map web-mode-map
			  ("C-M-f" . my/web-mode-element-next)
			  ("C-M-b" . my/web-mode-element-previous)
			  ("C-M-a" . my/web-mode-element-beginning)
			  ("C-M-e" . my/web-mode-element-end)
			  ("C-M-p" . my/web-mode-element-parent)
			  ("C-M-d" . my/web-mode-element-down)
			  ("C-c C-n" . my/web-mode-nextSibling)
			  ("C-c C-f" . my/php-search-documentation)
			  ("C-c /" . web-mode-element-close)
			  ("C-c TAB" . completion-at-point)
			  ("C-c C-w" . nil)
			  )
  :custom-face
  (web-mode-html-tag-face
   ((t (:foreground "aquamarine1"))))
  (web-mode-html-attr-name-face
   ((t (:foreground "LightPink1"))))
  (web-mode-script-face
   ((t (:background "gray15"))))
  (web-mode-style-face
   ((t (:background "gray20"))))
  (web-mode-css-selector-tag-face
   ((t (:foreground "SkyBlue1"))))
  (web-mode-css-property-name-face
   ((t (:foreground "gold1"))))
  (web-mode-function-call-face
   ((t (:foreground "DeepSkyBlue1"))))
  (web-mode-string-face
   ((t (:foreground "NavajoWhite1"))))
  (web-mode-comment-face
   ((t (:foreground "RosyBrown"))))
  :init
  (require 'css-mode)
  (setq web-mode-markup-indent-offset 0)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  ;; (setq web-mode-ac-sources-alist
  ;; 	  '(("css" . (ac-source-css-property))
  ;; 		("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  ;; begin codes
  ;; ------------------------------------------------
  ;; completions
  (defun my/web-completion-at-point ()
	"Provide completion at point based on the current context."
	(let ((context (my/web-detect-context))
          completions)
      (setq completions
			(cond
			 ((eq context 'html-attribute-value) (my/web-html-attribute-value-completions))
			 ((eq context 'html-attribute) web-mode-attribute-list)
			 ((eq context 'html-tag) web-mode-tag-list)
			 ((eq context 'css-value) (my/web-css-value-completions))
			 ((eq context 'css-property) my/web-css-property-completions)
			 (t nil)))
      (when completions
		(let ((bounds (or (bounds-of-thing-at-point 'symbol) (cons (point) (point)))))
          (list (car bounds) (cdr bounds) completions)))))

  (defun my/web-detect-context ()
	"Detect the current context for completion."
	(save-excursion
      (cond
       ;; HTMLタグの判定
       ((or (my/web-mode-element-p) (my/web-partial-html-tag-p))
		(cond
		 ;; 属性値の判定
		 ((re-search-backward "<[^ ]+ [a-zA-Z-]*=\"[^\"]*" (line-beginning-position) t)
          'html-attribute-value)
		 ;; 属性の判定
		 ((re-search-backward "<[^ ]+ [a-zA-Z-]*=*" (line-beginning-position) t)
          'html-attribute)
		 (t
          'html-tag)))
       ;; CSS値の判定
       ((and (my/web-inside-style-block-p)
			 (re-search-backward ":[^;}]*" (line-beginning-position) t))
		'css-value)
       ;; CSSプロパティの判定
       ((my/web-inside-style-block-p)
		'css-property)
       (t nil))))

  (defun my/web-partial-html-tag-p ()
	"Check if the context is a partial HTML tag."
	(save-excursion
      (re-search-backward "<[a-zA-Z]*" (line-beginning-position) t)))

  (defun my/web-inside-style-block-p ()
	"Check if the point is inside a <style> block or style attribute."
	(save-excursion
      (or (and (eq (get-text-property (point) 'face) 'web-mode-style-face) (my/web-inside-curly-braces-p))
          (and (eq (get-text-property (point) 'part-side) 'css) (my/web-inside-curly-braces-p))
          (re-search-backward "style=\"[^\"]*" (line-beginning-position) t))))

  (defun my/web-inside-curly-braces-p ()
	"Check if the point is inside curly braces."
	(save-excursion
      (let ((pos (point)))
		(and (re-search-backward "{" nil t)
			 (re-search-forward "}" nil t)
			 (> (point) pos)))))

  (defun my/web-html-attribute-value-completions ()
	"Return a list of completions for specific HTML attribute values."
	(let ((attr (save-excursion
                  (re-search-backward " \\([a-zA-Z-]+\\)=" (line-beginning-position) t)
                  (match-string 1))))
      (cond
       ((string= attr "method") '("post" "get" "dialog"))
       ((string= attr "type") '("text" "password" "submit" "button" "reset" "checkbox" "radio" "hidden" "color" "number" "date" "datetime-local" "time" "email" "file" "range"))
       (t nil))))

  (defvar my/web-css-property-completions nil
	"List of CSS property completions.")
  (setq my/web-css-property-completions
		(mapcar 'car css-property-alist))

  (defvar my/web-css-value-completions-alist nil
	"Alist of CSS property values for completions.")
  (dolist (prop my/web-css-property-completions)
	(let ((values (cdr (assoc prop css-property-alist))))
      (dolist (value values)
		(when (symbolp value)
          (setq values (append values (cdr (assoc value css-value-class-alist))))))
      ;; valuesをフィルタリングして文字列のみを残す
      (setq values (delq nil (mapcar (lambda (x) (and (stringp x) x)) values)))
      (push (cons prop values) my/web-css-value-completions-alist)))

  (defun my/web-css-value-completions ()
	"Return a list of completions for specific CSS property values."
	(let ((prop (save-excursion
                  (goto-char (line-beginning-position))
                  (re-search-forward "\\([a-zA-Z-]+\\)\\s-*:" (line-end-position) t)
                  (match-string 1))))
      (cdr (assoc prop my/web-css-value-completions-alist))))

  ;; -----------------------------------------------
  ;; set property php code block
  (defun my/set-block-php-code-properties ()
	"Set custom properties for code blocks."
	(when (eq major-mode 'web-mode)
      (save-excursion
		(goto-char (point-min))
		(while (re-search-forward "<\\?php\\|<\\?=" nil t)
          (let ((start (match-beginning 0))
				(end (progn (re-search-forward "\\?>") (point))))
			(with-silent-modifications (put-text-property start end 'php-code t)))))))

  (defun my-set-string-php-code-properties ()
	"Set text properties for the changed region if it's inside a PHP block."
	(let ((pos (point)))
      (save-excursion
		(let ((before (buffer-substring-no-properties (max (point-min) (- pos 2)) pos))
              (after (buffer-substring-no-properties pos (min (point-max) (+ pos 3)))))
          (when (and (not (string= before "?>"))
					 (not (string= after "<?p"))
					 (or (get-text-property (1- pos) 'php-code)
						 (get-text-property (1+ pos) 'php-code)))
			(with-silent-modifications (put-text-property pos (1+ pos) 'php-code t))
			;; (put-text-property pos (1+ pos) 'face 'font-lock-keyword-face)
			)))))

  ;; エレメントかテキストノードかの内部判定用
  (defun my/web-mode-element-p(&optional pos)
	""
	(unless pos (setq pos (point)))
	(let* ((props (text-properties-at pos)))
	  (when (and (null (plist-get props 'php-code)) (plist-get props 'tag-name) (plist-get props 'tag-type))
		(plist-get props 'tag-type)
		)))

  (defun my/web-mode-textnode-p(&optional pos)
	""
	(unless pos (setq pos (point)))
	(when (null (my/web-mode-element-p pos))
	  (my/web-mode-text-node-boundaries))
	)

  ;; カーソル上のテキストノードの文字位置
  (defun my/web-mode-text-node-boundaries ()
	"Find the start and end positions of the text node at point in web-mode.
Return nil if the point is at an element."
	(interactive)
	(unless (my/web-mode-element-p)
      (save-excursion
		(let ((start (point))
              (end (point)))
          ;; テキストノードの先頭を見つける
          (save-excursion
			(while (and (not (bobp))
						(null (my/web-mode-element-p)))
              (setq start (point))
              (backward-char)))
          ;; テキストノードの後尾を見つける
          (save-excursion
			(while (and (not (eobp))
						(null (my/web-mode-element-p)))
              (setq end (point))
              (forward-char)))
          (cons start end)))))

  ;; テキストノードがchildrenのpointリストに入るように改変
  (defun my/web-mode-element-children(&optional pos)
	(unless pos (setq pos (point)))
	(let* ((list nil)
		   (tag-type (my/web-mode-element-p)))
	  ;; テキストノード・空要素エレメントは子がないのでnil
	  (when (not (or (null tag-type) (eq tag-type 'void)))
		(let* ((element_bd (web-mode-element-boundaries))
			   (begin (+ (cdar element_bd) 1)) ;; 子の開始位置
			   (end (cadr element_bd)) ;; 終了位置（閉じタグ位置）
			   )

		  (save-excursion
			(dolist (p (web-mode-element-children pos))
			  (goto-char p)
			  (when (my/web-mode-element-p)
				(add-to-list 'list p))))

		  (when (/= begin end)
			(save-excursion
			  (goto-char begin)
			  (while (< (point) end)
				(if (my/web-mode-textnode-p)
					(let* ((bd (my/web-mode-text-node-boundaries)))
					  (add-to-list 'list (car bd))
					  (goto-char (cdr bd)))
				  (when (member (point) list)
					(goto-char (cddr (web-mode-element-boundaries)))
					))
				(forward-char 1)))
			(setq list (sort list '<)))))
	  list))

  (defun my/web-mode-node-move(&optional arg)
	"Moves the cursor from the current position to the beginning or end of node."
	(unless (and arg (member arg '(beginning end innerbegin innerend))) (setq arg 'beginning))
	(let* ((pos (point)) (elem (my/web-mode-element-p)))
	  (setq pos
			(cond
			 (elem
			  (cond
			   ((or
				 (and (eq arg 'innerbegin) (not (eq elem 'void)))
				 (and (eq arg 'innerend) (eq elem 'void)))  ;; void(empty) element end brackets
				(+ (cdar (web-mode-element-boundaries)) (if (not (eq elem 'void)) 1 0)))
			   ((and (eq arg 'innerend) (not (eq elem 'void)))
				(cadr (web-mode-element-boundaries)))
			   ((eq arg 'end)
				(+ (cddr (web-mode-element-boundaries)) 1))
			   (t ;; (eq arg 'beginning) , and void element inner-begin
				(caar (web-mode-element-boundaries)))))
			 (t ;; text-node
			  (cond
			   ((eq arg 'innerend)
				(cdr (my/web-mode-text-node-boundaries)))
			   ((eq arg 'end)
				(+ (cdr (my/web-mode-text-node-boundaries)) 1))
			   (t ;; (eq arg 'beginning)
				(car (my/web-mode-text-node-boundaries))
				)))))
      ;; Error handling: Check if pos is valid
      (if (and pos (>= pos (point-min)) (<= pos (point-max)))
          (goto-char pos)
		(message "No valid node found at the current position"))))

  (defun my/web-mode-element-parent()
	"Move to the parentElement of the current element."
	(interactive)
	(my/web-mode-node-move 'beginning)
	(let* ((pos (point)) (p (point)))
	  (save-excursion
		(catch 'done
		  (while (re-search-backward ">" nil t)
			(my/web-mode-node-move 'beginning)
			(when (eq (get-text-property (point) 'tag-type) 'doctype)
			  (setq pos nil)
			  (throw 'done nil))
			(when (member pos (my/web-mode-element-children))
			  (setq pos (point))
			  (throw 'done nil)
			  )
			))
		)
	  (when (and (not (null pos)) (/= p pos)) (goto-char pos))
	  ))

  ;; go to element (beginning,end,previous,next,down)
  (defun my/web-mode-element-move-to(&optional arg)
	"Move to the beginning or end of the current element based on POSITION.
POSITION is (beginning,end,previous,next,down)."

	(unless (and arg (member arg '(beginning end previous next down))) (setq arg 'next))
	(catch 'exit
	  (let* ((pos (point)) (elem (my/web-mode-element-p)))

		;; エレメント・（カーソル）改行・子エレメント（開始タグ）のdownは子エレメント内に降りて終了
		(when (eq arg 'down)
		  (let ((char (char-after)))
			(when (and (or (eq char ?\n) (eq char ?\r)) (null elem))
			  (save-excursion
				(forward-char)
				(when (eq (my/web-mode-element-p) 'start)
				  (setq pos (point))
				  (setq elem (my/web-mode-element-p))))
			  (goto-char pos)
			  (my/web-mode-node-move 'innerbegin)
			  (throw 'exit nil))))

		;; テキストノード・空要素エレメントは子がないので down中断
		(when (and (eq arg 'down) (or (null elem) (eq elem 'void) ))
		  (message "no children at textNode or voidElement.")
		  (throw 'exit nil))

		;; 閉じタグ位置で nextは終端なので中断
		;; 親の次ノードに移動するなら、ここはコメントアウト
		(when (and (eq arg 'next) (eq elem 'end))
		  (message "end of Element.")
		  (throw 'exit nil))

		;; 事前の位置統一（基本はノード先頭に）
		(cond
		 ((eq elem 'end) ;; 閉じタグ位置
		  (cond
		   ((eq arg 'beginning)
			(my/web-mode-node-move 'innerbegin)
			(throw 'exit nil)) ;; 先頭はinnerの先頭で終了
		   ((eq arg 'end)
			(my/web-mode-node-move 'innerend)
			(throw 'exit nil)) ;; 後尾はinnerの後尾（閉じタグブラケット位置）で終了
		   ((eq arg 'previous) 
			(my/web-mode-node-move 'innerend)
			(backward-char))
		   (t  (my/web-mode-node-move 'beginning))))
		 (t (my/web-mode-node-move 'beginning)))

		(cond
		 ((eq arg 'down) ;; 子要素先頭へ
		  (my/web-mode-node-move 'innerbegin)
		  (throw 'exit nil))
		 ((member arg '(beginning end))
		  (my/web-mode-element-parent) ;; いったん親に移動し
		  (cond
		   ((eq arg 'beginning) ;; 同ノード列先頭へ
			(my/web-mode-node-move 'innerbegin)
			(throw 'exit nil))
		   ((eq arg 'end)  ;; 同ノード列後尾へ
			(my/web-mode-node-move 'innerend)
			(throw 'exit nil))
		   (t)))
		 ((eq arg 'next)
		  (my/web-mode-node-move 'end)
		  (throw 'exit nil))
		 ((eq arg 'previous)
		  (setq pos (point))
		  (let* ((children nil))
			(save-excursion
			  (my/web-mode-element-parent) ;; いったん親に移動して、子位置リスト取得
			  (setq children (my/web-mode-element-children)))
			;; 配列インデックスを走査して、１つ前の位置を取得・移動
			(let* ((array (vconcat children))
				   (len (length array))
				   (index 0)
				   (res nil))
			  (catch 'done
				(while (< index len)
				  (if (eq (aref array index) pos)
			  		  (throw 'done index)
					(setq index (1+ index)))))
			  (when (> index 0)
				(setq res (aref array (- index 1)))
				(when res (goto-char res))))))
		 (t)))))

  (defun my/web-mode-element-beginning()
	"Move to the beginning of the current element."
	(interactive)
	(my/web-mode-element-move-to 'beginning))

  (defun my/web-mode-element-end()
	"Move to the end of the current element."
	(interactive)
	(my/web-mode-element-move-to 'end))

  (defun my/web-mode-element-previous()
	"Move to the previous of the current element."
	(interactive)
	(my/web-mode-element-move-to 'previous))

  (defun my/web-mode-element-next()
	"Move to the next of the current element."
	(interactive)
	(my/web-mode-element-move-to 'next))

  (defun my/web-mode-element-down()
	"Move to the firstChild of the current element."
	(interactive)
	(my/web-mode-element-move-to 'down))

  (defun my/web-mode-nextSibling()
	"Move to the next Sibling of the current parent element."
	(interactive)
	(let* ((elem (my/web-mode-element-p)))
	  (when (eq elem 'end)
		(my/web-mode-node-move 'innerend)
		(backward-char)
		)
	  (my/web-mode-element-parent)
	  (my/web-mode-node-move 'end)
	  ))

  ;; end codes
  )

;;; my-web-mode.el ends here
