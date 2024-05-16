(defpackage ggplot
  (:use :cl)
  (:export
   #:aes
   #:labs
   #:geom-point
   #:geom-line
   #:geom-col
   #:geom-bar
   #:geom-boxplot
   #:clear
   ))

(defpackage ggplot.gnuplot
  (:use :cl)
  (:export
   #:ggplot))

(defpackage ggplot.mathgl
  (:use :cl)
  (:export
   #:ggplot
   #:ggsave))
