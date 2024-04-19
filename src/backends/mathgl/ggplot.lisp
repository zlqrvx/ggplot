(in-package :ggplot.mathgl)


(defmethod mgl-backend-convert-data ((x simple-array))
  (let* ((xc (mgl-create-data))
         (free-xc (lambda () (mgl-delete-data xc))))
    (trivial-garbage:finalize xc free-xc)
    (cffi:with-pointer-to-vector-data (x-ptr x)
      (mgl-data-set-double xc x-ptr (length x) 1 1))
    xc))

(defun mgl-backend-color-codes (color)
  (declare (string color))
  (alexandria:switch (color :test #'equal)
    ("black" "k")
    ("red" "r")
    ("dark red" "R")
    ("green" "g")
    ("dark green" "G")
    ("blue" "b")
    ("dark blue" "B")
    ("cyan" "c")
    ("dark cyan" "C")
    ("magenta" "m")
    ("dark magenta" "M")
    ("yellow" "y")
    ("dark yellow" "Y")
    ("gold" "Y")
    ("gray" "h")
    ("dark gray" "H")
    ("white" "w")
    ("bright gray" "W")
    ("green-blue" "l")
    ("dark green-blue" "L")
    ("green-yellow" "e")
    ("dark green-yellow" "E")
    ("sky-blue" "n")
    ("dark sky-blue" "N")
    ("blue-violet" "u")
    ("dark blue-violet" "U")
    ("purple" "p")
    ("dark purple" "P")
    ("orange" "q")
    ("dark orange" "Q")
    ("brown" "Q")
    (otherwise "")))

(defun ggplot-backend-mgl (ggp)
  (let* ((aes (ggplot::get-aes ggp))
         (geom (ggplot::get-geom ggp))
         (labs (ggplot::get-labs ggp))
         (plot-commands
           `(let* ((gr (mgl-create-graph 600 400))
                   (free-gr (lambda () (mgl-delete-graph gr))))
              (trivial-garbage:finalize gr free-gr)
              (mgl-set-ranges gr
                              ,(loop :for i
                                       :across (ggplot::get-x aes)
                                     :minimize i)
                              ,(loop :for i
                                       :across (ggplot::get-x aes)
                                     :maximize i)
                              ,(loop :for i
                                       :across (ggplot::get-y aes)
                                     :minimize i)
                              ,(loop :for i
                                       :across (ggplot::get-y aes)
                                     :maximize i)
                              0d0 0d0)
              ,(if labs `(mgl-title gr ,(ggplot::get-title labs) "" -1d0))
              (mgl-axis gr "" "" "")
              ,@(loop :for g :in geom
                      :collect
                      `(mgl-plot-xy gr
                                    ,(mgl-backend-convert-data
                                      (ggplot::get-x aes))
                                    ,(mgl-backend-convert-data
                                      (ggplot::get-y aes))
                                    ,(concatenate 'string
                                                  (mgl-backend-color-codes
                                                   (ggplot::get-color g))
                                                  (case (ggplot::get-type g)
                                                    (ggplot::line "r")
                                                    (ggplot::point " o")))
                                    ""))
              ;; (mgl-write-frame gr "test.png" "")
              gr)))
    (setq ggplot::*ggplot-last-plot*
          (eval plot-commands)))
  t)


(defun ggplot (&rest rest)
  (cffi:use-foreign-library libmgl)
  (ggplot-backend-mgl (apply #'ggplot::make-ggplot rest)))


(defun ggsave (fname &key (plot ggplot::*ggplot-last-plot*))
  (declare (string fname))
  (mgl-write-frame plot fname ""))

