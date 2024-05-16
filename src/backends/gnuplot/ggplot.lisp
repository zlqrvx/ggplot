(in-package :ggplot.gnuplot)

(defstruct ggplot-gnuplot
  script-file
  data-file)

(defmethod ggplot::deallocate-plot ((plot ggplot-gnuplot))
  (uiop:delete-file-if-exists (ggplot-gnuplot-script-file plot))
  (uiop:delete-file-if-exists (ggplot-gnuplot-data-file plot)))


(defun write-data-to-stream (stream data)
  (loop :for i :below (length data)
        :do (format stream "~A " (tagged-data-tag (nth i data)))
        :finally (format stream "~%"))

  (loop :for i :below (length (tagged-data-data (car data)))
        :do
           (loop :for d :below (length data)
                 :do (format stream "~A "
                             (aref (tagged-data-data (nth d data)) i))
                 :finally (format stream "~%"))))

(defmethod data-to-array ((data simple-array))
  (let ((x (make-array (length data)
                       :element-type 'string)))
    (loop :for i :below (length x)
          :do
             (setf (aref x i)
                   (format nil "~A" (aref data i))))
    x))

(defmethod data-to-array ((data list))
  (if (not data) (return-from data-to-array nil))
  (let ((x (make-array (length data)
                       :element-type 'string)))
    (loop :for i :below (length x)
          :do
             (setf (aref x i)
                   (format nil "~A" (nth i data))))
    x))

(defstruct tagged-data
  tag
  data)

(defun tag+process-data (data)
  (let ((new-data (data-to-array data)))
    (if new-data
        (make-tagged-data :tag (gensym) :data new-data)   
        nil)))

(defun tag-data (ggp)
  (let ((aes (ggplot::get-aes ggp)))
    (and aes
         (progn
           (setf (ggplot::get-x aes)
                 (tag+process-data (ggplot::get-x aes)))
           (setf (ggplot::get-y aes)
                 (tag+process-data (ggplot::get-y aes))))))
  (let ((geoms (ggplot::get-geom ggp)))
    (loop :for g :in geoms
          :if (ggplot::get-aes g)
            :do
               (progn
                 (setf (ggplot::get-x (ggplot::get-aes g))
                       (tag+process-data (ggplot::get-x (ggplot::get-aes g))))
                 (setf (ggplot::get-y (ggplot::get-aes g))
                       (tag+process-data (ggplot::get-y (ggplot::get-aes g))))))))

(defun get-all-data (aes geom-lst)
  (append
   (loop :for l :in 
                (list
                 (ggplot::get-x aes)
                 (ggplot::get-y aes))
         :if l
           :collect l)
   (loop :for g :in geom-lst
         :if (and (ggplot::get-aes g) (ggplot::get-x (ggplot::get-aes g)))
           :collecting (ggplot::get-x (ggplot::get-aes g))
         :if (and (ggplot::get-aes g) (ggplot::get-y (ggplot::get-aes g)))
           :collecting (ggplot::get-y (ggplot::get-aes g)))))


(defun ggplot-backend-gnuplot (ggp)
  (let* ((aes (ggplot::get-aes ggp))
         (geom (ggplot::get-geom ggp))
         (labs (ggplot::get-labs ggp))
         (script-file (uiop:tmpize-pathname "/tmp/cl-ggplot"))
         (data-file (uiop:tmpize-pathname "/tmp/cl-ggplot")))
    (with-open-file (gnuplot-script script-file
                                    :direction :io
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
      (with-open-file (gnuplot-data data-file
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
        (write-data-to-stream gnuplot-data (get-all-data aes geom))
        (write-script gnuplot-script ggp data-file)
        (uiop:launch-program
         `("/usr/bin/gnuplot" "--persist" ,(format nil "~A" script-file)))))
    (setq ggplot::*ggplot-last-plot*
          (make-ggplot-gnuplot
           :script-file script-file
           :data-file data-file)))
  t)

(defun write-if-exists (fmt val)
  (and val
       (format nil fmt val)))

(defun collect-labels (labs)
  (concatenate
   'string
   (write-if-exists "set title '~A'~%" (ggplot::get-title labs))
   (write-if-exists "set xlabel '~A'~%" (ggplot::get-x labs))
   (write-if-exists "set ylabel '~A'~%" (ggplot::get-y labs))))

(defun select-columns (g aes &key (type nil) (plot-num 0))
  (let ((x
          (if (and (ggplot::get-aes g) (ggplot::get-x (ggplot::get-aes g)))
              (tagged-data-tag (ggplot::get-x (ggplot::get-aes g)))
              (if (ggplot::get-x aes) (tagged-data-tag (ggplot::get-x aes)))))
        (y
          (if (and (ggplot::get-aes g) (ggplot::get-y (ggplot::get-aes g)))
              (tagged-data-tag (ggplot::get-y (ggplot::get-aes g)))
              (if (ggplot::get-y aes) (tagged-data-tag (ggplot::get-y aes))))))
    (cond
      ((and x y)
       (format nil "\"~A\":\"~A\"" x y))
      ((and x (not y))
       (format nil "\"~A\"" x))
      ((and y (not x))
       (if (string= type "boxplot")
           (format nil "(~A):\"~A\"" plot-num y)
           (format nil "\"~A\"" y))))))

(defun select-type (g)
  (cond
    ((equal (ggplot::get-type g) 'ggplot::line)
     "lines")
    ((equal (ggplot::get-type g) 'ggplot::point)
     "points")
    ((equal (ggplot::get-type g) 'ggplot::col)
     "boxes")
    ((equal (ggplot::get-type g) 'ggplot::boxplot)
     "boxplot")))

(defun write-script (stream ggp data-file)
  (let* ((aes (ggplot::get-aes ggp))
         (geom (ggplot::get-geom ggp))
         (labs (ggplot::get-labs ggp)))
    (and labs
         (format stream (collect-labels labs)))
    (format stream "set border 2~%")
    (format stream "plot ")
    (unless geom
      (error "No geom provided, nothing to plot."))
    (loop :for g :in geom
          :and i :from 0
          :collect
          (format
           stream
           (concatenate
            'string
            (format nil " '~A' using ~A with ~A "
                    data-file
                    (select-columns g aes :type (select-type g) :plot-num i)
                    (select-type g))
            (write-if-exists "linecolor rgb '~A'" (ggplot::get-color g))
            ",")))))



(defun ggplot (&rest rest)
  (ggplot::clear 'ggplot::*ggplot-last-plot*)
  (let ((ggp (apply #'ggplot::make-ggplot rest)))
    (tag-data ggp)
    (ggplot-backend-gnuplot ggp)))


;; (defun ggsave (fname &key (plot ggplot::*ggplot-last-plot*))
;;   (declare (string fname))
;;   (float-features:with-float-traps-masked t
;;     (mgl-write-frame plot fname "")))

