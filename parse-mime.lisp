;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parse-mime.lisp: Tools for parsing a mime string/stream 
;;;; Copyright (C) 2004 Robert Marlow <bobstopper@bobturf.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package :mime)

(defgeneric parse-mime (mime &optional headers)
  (:documentation
   "Parse a string or stream containing a MIME message and return a mine
object representing it or nil if the message is not MIME compatible"))


(defmethod parse-mime ((mime string) &optional headers)
  (declare (ignore headers))
  (parse-mime (make-string-input-stream mime)))


(defmethod parse-mime ((mime stream) &optional headers)
  (catch 'bad-mime
    (let* ((headers (or headers (parse-headers mime)))
	   (content-type-header (assoc :content-type headers))
	   (content-disposition-header (assoc :content-disposition headers))
	   (content-type-subtype (split "/" (header-value
					     content-type-header)))
	   (content-type (first content-type-subtype))
	   (content-subtype (second content-type-subtype))
	   (content-parm (header-parms content-type-header))
	   (content-disposition (header-value content-disposition-header))
	   (content-disposition-parm (when content-disposition-header
				       (header-parms
					content-disposition-header)))
	   (boundary nil)
	   (mime-version (or (header-value (assoc :mime-version headers)) "1.0"))
	   (mime-type (cond
		       ((equal content-type "text") 'text-mime)
		       ((equal content-type "multipart") 'multipart-mime)
		       (t 'mime))))

      (if (equal mime-version "1.0")
	
	  (let ((mime-obj-gen
		 (list
		  mime-type
		  :type content-type
		  :subtype content-subtype
					;		:parameters content-parm
		  :encoding (cdr (assoc :content-transfer-encoding
					headers))
		  :description (cdr (assoc :content-description
					   headers))
		  :id (remove #\< (remove #\> (cdr (assoc :content-id headers))))
		  :disposition content-disposition
		  :disposition-parameters content-disposition-parm)))
	      
	    (case mime-type
	      ((text-mime)
	       (setq mime-obj-gen
		     (append mime-obj-gen
			     (list
			      :charset (cdr (assoc :charset content-parm))
			      :parameters (delete (assoc :charset content-parm)
						  content-parm)))))
	      ((multipart-mime)
	       (setq boundary (second (assoc :boundary content-parm)))
	       (setq mime-obj-gen 
		     (append mime-obj-gen
			     (list
			      :boundary boundary
			      :parameters (delete (assoc :boundary content-parm)
						  content-parm)
			      :prologue (get-prologue mime boundary)))))
						    
	      (t (setq mime-obj-gen
		       (append mime-obj-gen (list :parameters content-parm)))))

	    (setq mime-obj-gen
		  (append mime-obj-gen
			  (list :content (parse-body mime
						     (ensure-keyword mime-type)
						     boundary))))

	    (case mime-type
	      ((multipart-mime)
	       (setq mime-obj-gen
		     (append mime-obj-gen
			     (list :epilogue (get-epilogue mime))))))

	    (apply #'make-instance mime-obj-gen))

	;; If we decide this isn't MIME 1.0 compatible, we just return nil.
	nil))))

(defun parse-headers (stream)
  "Parses headers from a stream and converts them into keyword/value pairs"
  (let ((headers nil)
	(previous-line nil))
	
    (read-lines (line stream)
		((equal line "")
		 (if previous-line
		     (push (create-header previous-line) headers))
		 headers)
				 
		;; Headers beginning with whitespace are continuations
		;; from the header on the previous line. Headers not
		;; beginning with complete lines are starts of new headers
		(aif ;(match "^[[:blank:]]+(.*)" line)
		 (match "^\\s+(.*)" line)
		     (setq previous-line
			   (concatenate 'string previous-line " "
					(svref it 0)))
		     (progn
		       (if previous-line
			   (push (create-header previous-line) headers))
		       (setq previous-line line))))))


(defun header-value (header)
  "Takes a header string and returns the value component"
  (aif (match "^([^;\\s]*)" (cdr header))
       (svref it 0)
       nil))


(defun header-parms (header)
  "Takes a header string and returns all parameters contained within"
  (extract-parms
   (regex-replace-all "\\(.*?\\)"
		      (aif (match "^[^;\\s]*(;.*)$" (cdr header))
			   (svref it 0)
			   (return-from header-parms nil))
		      "") nil))


(defun header-comments (header)
  "Returns all comments from the keyword/value header pair in HEADER"
  (extract-header-comments (cdr header)))


(defun extract-header-comments (header-value-string &optional comment-list)
  "Takes a header string and optional list of already extracted comments and
returns all comments contained within that string"
  (aif (match "\\((.*?)\\)(.*)" header-value-string)
       (extract-header-comments (svref it 1)
				(cons (svref it 0) comment-list))
       comment-list))
			       
(defun create-header (header-string)
  "Takes a header string and returns a keyword/value header pair"
  (let* ((match (match "^([^:\\s]+):\\s*(.*)$" line))
	 (header-name (svref match 0))
	 (header-value (svref match 1)))
    (if match
	(cons (ensure-keyword header-name)
	      header-value)
      nil)))


(defun extract-parms (parm-string &optional parms)
  "Takes a string of parameters and returns a list of keyword/value
parameter pairs"
  (aif (match ";\\s*(.*?)=\"?([^;\"\\s]*)\"?[\\s]*(;?.*)" parm-string)
       (extract-parms (svref it 2) (cons (list (ensure-keyword (svref it 0))
					      (svref it 1))
					parms))
       parms))


(defgeneric parse-body (body mime-type &optional boundary)
  (:documentation
   "Parses a mime body within the context of the mime type expected.
Assumes the stream's position is already at the body. If it's not,
you should call parse headers first or read through to the first null
line."))


(defmethod parse-body ((body string) (mime-type (eql :mime))
		       &optional boundary)
  (declare (ignore boundary))
  body)


(defmethod parse-body ((body stream) (mime-type (eql :mime))
		       &optional boundary)
  (declare (ignore boundary))
  (read-stream-to-string body line))


(defmethod parse-body ((body string) (mime-type (eql :text-mime))
		       &optional boundary)
  (declare (ignore boundary))
  body)


(defmethod parse-body ((body stream) (mime-type (eql :text-mime))
		       &optional boundary)
  (declare (ignore boundary))
  (read-stream-to-string body line))


(defmethod parse-body ((body string) (mime-type (eql :multipart-mime))
		       &optional boundary)
  (parse-body (make-string-input-stream body) mime-type boundary))


(defmethod parse-body ((body stream) (mime-type (eql :multipart-mime))
		       &optional boundary)
  (multiple-value-bind (part end-type) (read-until-boundary body boundary)
    (cons (parse-mime part)
	  (case end-type
	    ((end-part) (parse-body body mime-type boundary))
	    ((end-mime) nil)
	    ((eof) (throw 'bad-mime nil))
	    (t (throw 'bad-mime "Unexpected Parse Return Value"))))))
    

(defgeneric get-prologue (body boundary)
  (:documentation "Grab the prologue from a Multipart MIME message"))


(defmethod get-prologue ((body string) boundary)
  (get-prologue (make-string-input-stream body) boundary))


(defmethod get-prologue ((body stream) boundary)
  (multiple-value-bind (text end-type) (read-until-boundary body boundary)
    (case end-type
      ((end-part) text)
      ((end-mime eof) (throw 'bad-mime nil))
      (t (throw 'bad-mime "Unexpected Parse Return Value")))))


(defgeneric get-epilogue (body)
  (:documentation "Grab the prologue from a Multipart MIME message"))


(defmethod get-epilogue ((body string))
  (read-stream-to-string (make-string-input-stream body) line))


(defmethod get-epilogue ((body stream))
  (read-stream-to-string body line))
      

(defun read-until-boundary (stream boundary)
  "Reads a MIME body from STREAM until it reaches a boundary defined by
BOUNDARY"
  (let ((end-type 'eof)
	(actual-boundary (concatenate 'string "--" boundary)))
    (values
     (read-stream-to-string
      stream line
      
      (or (and (equal (delete #\return line) actual-boundary)
	       (setq end-type 'end-part))
	  (and (equal (delete #\return line) (concatenate 'string
							  actual-boundary
							  "--"))
	       (setq end-type 'end-mime))))
     end-type)))
 
