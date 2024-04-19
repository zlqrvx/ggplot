(in-package :ggplot.mathgl)


(cffi:define-foreign-library libmgl
  (:unix (:or "libmgl.so.1" "libmgl.so" "libmgl.so.1.0.0"))
  (t (:default "libmgl")))


;; (cffi:use-foreign-library libmgl)


(cffi:defcfun "mgl_create_graph"
    :pointer (width :int) (height :int))

(cffi:defcfun "mgl_delete_graph"
    :pointer (gr :pointer))

(cffi:defcfun "mgl_create_data"
    :pointer)

(cffi:defcfun "mgl_delete_data"
    :void (dat :pointer))

(cffi:defcfun "mgl_data_set_double"
    :void (dat :pointer) (A :pointer) (NX :int) (NY :int) (NZ :int))

(cffi:defcfun "mgl_fplot"
    :void (gr :pointer) (eqY :string) (pen :string) (opt :string))

(cffi:defcfun "mgl_plot_xy"
    :void (gr :pointer) (x :pointer) (y :pointer) (pen :string) (opt :string))

(cffi:defcfun "mgl_axis"
    :void (gr :pointer) (dir :string) (stl :string) (opt :string))

(cffi:defcfun "mgl_set_ranges"
    :void (gr :pointer)
  (x1 :double) (x2 :double)
  (y1 :double) (y2 :double)
  (z1 :double) (z2 :double))

(cffi:defcfun "mgl_title"
    :void (gr :pointer) (text :string) (font :string) (size :double))

(cffi:defcfun "mgl_write_frame"
    :void (gr :pointer) (fname :string) (descr :string))

