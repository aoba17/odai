(ql:quickload :hunchentoot)
(ql:quickload :djula)

(defvar *document-root* (pathname "/Users/Takafumi/dev/project/odai/"))

(djula:add-template-directory (merge-pathnames "templates/" *document-root*))

(defvar *acceptor*)

(defun start ()
  "ODAI 起動"
  (setf *acceptor* 
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port 4242
                        :document-root *document-root*))))

(defun stop ()
  "ODAI 停止"
  (hunchentoot:stop *acceptor*))


;; 起動
(start)

;; テストハンドラ定義
(defparameter +hello.html+ (djula:compile-template* "hello.html"))
(defparameter +top.html+ (djula:compile-template* "top.html"))

(hunchentoot:define-easy-handler (test-handler :uri "/")
    ()
  (djula:render-template* +top.html+
                          nil
                          :odai (get-odai)))

(hunchentoot:define-easy-handler (test-handler :uri "/hello")
    ()
  (djula:render-template* +hello.html+))

(defvar *odai-db* (list "わさビーフ" "ポテトチップス" "堅あげポテト"))

(defun get-odai ()
  (nth (random 3) *odai-db*))
