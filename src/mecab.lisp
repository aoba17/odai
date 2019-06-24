(ql:quickload :cl-ppcre)
(ql:quickload :drakma)
(ql:quickload :plump)
(ql:quickload :clss)

(defparameter *mecab-path* "/usr/local/bin/mecab")

(defun run-mecab (string &key args)
  "Mecabを呼び出し形態素解析を行う"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (sb-ext:run-program *mecab-path* args
                          :search t
                          :input in
                          :output out))))

(defun unknown-word-p (one-line)
  "辞書に登録されている単語かどうか判別"
  (if (= 7 (length (cl-ppcre:split "," one-line)))
      t))

(defun collect-mecab-output (one-line)
  "Mecabの出力結果を解析しリスト化する"
  (if (unknown-word-p one-line)
      (cl-ppcre:register-groups-bind (morpheme pos pos-subclass-1
                                      pos-subclass-2 pos-subclass-3
                                      inflectional-form conjugation prototype)
          ("(.*)\\t(.*),(.*),(.*),(.*),(.*),(.*),(.*)" one-line)
        (list 'morpheme morpheme                   ;; 表層形
              'pos pos                             ;; 品詞
              'pos-subclass-1 pos-subclass-1       ;; 品詞細分類1
              'pos-subclass-2 pos-subclass-2       ;; 品詞細分類2
              'pos-subclass-3 pos-subclass-3       ;; 品詞細分類3
              'inflectional-form inflectional-form ;; 活用形
              'conjugation conjugation             ;; 活用型
              'prototype prototype                 ;; 原形
              'reading nil                         ;; 読み
              'phonation nil))                     ;; 発音
      (cl-ppcre:register-groups-bind (morpheme pos pos-subclass-1
                                      pos-subclass-2 pos-subclass-3
                                      inflectional-form conjugation
                                      prototype reading phonation)
          ("(.*)\\t(.*),(.*),(.*),(.*),(.*),(.*),(.*),(.*),(.*)" one-line)
        (list 'morpheme morpheme                   ;; 表層形
              'pos pos                             ;; 品詞
              'pos-subclass-1 pos-subclass-1       ;; 品詞細分類1
              'pos-subclass-2 pos-subclass-2       ;; 品詞細分類2
              'pos-subclass-3 pos-subclass-3       ;; 品詞細分類3
              'inflectional-form inflectional-form ;; 活用形
              'conjugation conjugation             ;; 活用型
              'prototype prototype                 ;; 原形
              'reading reading                     ;; 読み
              'phonation phonation))))             ;; 発音

(defparameter *args-neologd* (list "-d" "/usr/local/lib/mecab/dic/mecab-ipadic-neologd"))

(defun mecab (string)
  "引数の文字列を形態素解析"
  (with-input-from-string
      (in (run-mecab
           (cl-ppcre:regex-replace-all "\\s|\\n" string "")
           :args *args-neologd*))
    (loop for i = (read-line in)
          until (string= "EOS" i)
          collect (collect-mecab-output i))))

(defun pickup (word mecab-result)
  (loop for i in mecab-result
        when (string= word (get-part 'morpheme i))
          return i))

(defun get-part (part morpheme-info)
  "引数のplistから指定した情報を取得"
  (getf morpheme-info part)) 

(defun defcondition (subclass value)
  #'(lambda (word-info)
      (string= (get-part subclass word-info) value)))

(defun deffilter (sub-1 sub-2 sub-3 &optional &key regex)
  (remove-if #'null (list (when sub-1 (defcondition 'pos-subclass-1 sub-1))
                          (when sub-2 (defcondition 'pos-subclass-2 sub-2))
                          (when sub-3 (defcondition 'pos-subclass-3 sub-3))
                          (when regex (lambda (word-info)
                                        (cl-ppcre:scan regex (get-part 'morpheme word-info)))))))

(defparameter *word-filters* (list
                              (deffilter "固有名詞" "人名" "一般")
                              (deffilter "固有名詞" "一般" nil
                                :regex "[\\p{Hiragana}]{4,}|[\\p{Han}]{2,}|[\\p{Katakana}ー]{2,}")
                              ))

(defun validate-word (word-info filter)
  (loop for condition in filter
        unless (funcall condition word-info)
          return nil
        finally (return t)))
  
(defun filter (word-info filters)
  (loop for f in filters
        when (validate-word word-info f)
          return t
        finally (return nil)))

(defun get-noun (mecab-out)
  "mecabの出力結果リストから名詞だけを抽出"
  (remove-duplicates
   (loop for x in mecab-out
         when (and (string= (get-part 'pos x) "名詞")
                   (filter x *word-filters*))
           collect (get-part 'morpheme x))
   :test #'string=))

(defun request-html (uri)
  (plump:parse (drakma:http-request uri)))



