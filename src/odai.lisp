(let ((quicklisp-init (pathname "/Users/Takafumi/quicklisp/quicklisp/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :hunchentoot)
(ql:quickload :djula)

(defvar *document-root* "/Users/Takafumi/dev/project/odai/")

(djula:add-template-directory
 (pathname *document-root*))

(defvar *acceptor*)

(defun start ()
  "ODAI 起動"
  (setf *acceptor* 
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port 4242))))

(defun stop ()
  "ODAI 停止"
  (hunchentoot:stop *acceptor*))


;; 起動
(start)

;; テストハンドラ定義
(defparameter +hello.html+ (djula:compile-template* "hello.html"))

(hunchentoot:define-easy-handler (test-handler :uri "/hello")
    ()
  (djula:render-template* +hello.html+))
    
(hunchentoot:define-easy-handler (test-handler :uri "/test")
    ((name :init-form "Pumpkin"))
  (format nil "<!doctype html>
<title>Common Lisp Recipes</title>
<link rel=\"stylesheet\" href=\"odai/static/odai.css\" type=\"text/css\">
<body>
  <h1>こんにちは, ~A! The Lisp time is ~A.</h1>
  <div class=\"main-image\">
    <img src=\"/image.jpg\">
  </div>
</body>"
          name (get-universal-time)))

  
