(in-package :om)

(defvar *melodizer-sources-dir* nil)
(setf *melodizer-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))


(mapc 'compile&load (list
                     (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-utils" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-csp" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-csts" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "block" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "rock-utils" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "rock" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "rock-AB" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "rock-srdc" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "rock-accompaniment" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "rock-csp" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "rock-csts" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "dummy-problem" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "golomb-ruler" :type "lisp")
                     ))


;; remplir à la fin
(fill-library '(("ALL" nil (mldz::melodizer mldz::block mldz::search mldz::rock) nil)
                ("UTILS" Nil Nil (mldz::get-voice mldz::to-midicent) nil)
))

(print "Melodizer Loaded")
