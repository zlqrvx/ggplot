(in-package :ggplot)

(defvar *ggplot-last-plot* nil
  "Global variable to hold the result of the last plot.")

(defclass aesthetic ()
  ((x :initarg :x :initform nil :reader get-x)
   (y :initarg :y :initform nil :reader get-y)))

(defclass geometry ()
  ((type :initarg :type :type 'symbol :reader get-type)
   (color :initarg :color :type 'string :reader get-color)))

(defclass labs ()
  ((title :initarg :title :type 'string :initform "" :reader get-title)
   (x :initarg :x :type 'string :initform "" :reader get-x)
   (y :initarg :y :type 'string :initform "" :reader get-y)
   (caption :initarg :caption :type 'string :initform "" :reader get-caption)))

(defclass ggplot-class ()
  ((aes :type 'aesthetic :initform nil :reader get-aes)
   (geom :type 'list :initform '() :reader get-geom)
   (labs :type 'labs :initform nil :reader get-labs)))

(defun geom-line (&key color)
  (make-instance 'geometry
                 :type 'line
                 :color color
                 ))

(defun geom-point (&key color)
  (make-instance 'geometry
                 :type 'point
                 :color color
                 ))

(defun aes (&key (x nil) (y nil))
  (make-instance 'aesthetic :x x :y y))

(defun labs (&key (title "") (x "") (y "") (caption ""))
  (make-instance 'labs
                 :title title
                 :x x
                 :y y
                 :caption caption))


(defun make-ggplot (&rest rest)
  (let ((ggp (make-instance 'ggplot-class)))
    (dolist (gr-el rest)
      (typecase gr-el
        (aesthetic
         (setf (slot-value ggp 'aes) gr-el))
        (geometry
         (setf (slot-value ggp 'geom)
               (append (slot-value ggp 'geom) (list gr-el))))
        (labs
         (setf (slot-value ggp 'labs) gr-el))))
    ggp))


;; (defun ggplot (&rest rest)
;;   (let ((ggp (make-instance 'ggplot)))
;;     (dolist (gr-el rest)
;;       (typecase gr-el
;;         (aesthetic
;;          (setf (slot-value ggp 'aes) gr-el))
;;         (geometry
;;          (setf (slot-value ggp 'geom)
;;                (append (slot-value ggp 'geom) (list gr-el))))
;;         (labs
;;          (setf (slot-value ggp 'labs) gr-el))))
;;     (ggplot-backend-mathgl::ggplot-backend-mgl ggp)))


