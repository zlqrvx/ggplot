(defpackage ggplot
  (:use :cl)
  (:export
   #:aes
   #:labs
   #:geom-point
   #:geom-line
   ))


(defpackage ggplot.mathgl
  (:use :cl)
  (:export
   #:ggplot
   #:ggsave))
