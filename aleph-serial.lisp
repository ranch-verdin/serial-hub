(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel)
  (use-package '(:optima :cffi :iterate)))
(defparameter *start-flag* #x12)
(defparameter *end-flag* #x13)
(defparameter *dle* #x7D)

(cffi:defcenum serial-msg-types
  :eSerialMsg_debug
  :eSerialMsg_dumpIns
  :eSerialMsg_insDump
  :eSerialMsg_dumpParams
  :eSerialMsg_paramsDump
  :eSerialMsg_triggerParam
  :eSerialMsg_triggerIn
  :eSerialMsg_queryIn
  :eSerialMsg_inVal
  :eSerialMsg_queryParam
  :eSerialMsg_paramVal
  :eSerialMsg_outVal

  :eSerialMsg_dumpOutputs
  :eSerialMsg_outputsDump
  :eSerialMsg_dumpConnections
  :eSerialMsg_connectionsDump
  :eSerialMsg_connect
  :eSerialMsg_disconnect
  :eSerialMsg_dumpOps
  :eSerialMsg_opsDump
  :eSerialMsg_dumpOpDescriptions
  :eSerialMsg_opDescriptionsDump
  :eSerialMsg_newOp
  :eSerialMsg_deleteOp
  :eSerialMsg_storePreset
  :eSerialMsg_recallPreset
  :eSerialMsg_bfinProgStart
  :eSerialMsg_bfinHexChunk
  :eSerialMsg_bfinDscChunk
  :eSerialMsg_bfinProgEnd
  :eSerialMsg_bfinProgEcho;;DEBUG - can we round-trip the hex?

  :eSerialMsg_numParams
  )


(defun send-framed-message (bytes stream)
  (write-byte *start-flag* stream)
  (loop for byte in bytes
     do (if (or (= byte *start-flag*)
		(= byte *end-flag*)
		(= byte *dle*))
	    (progn (write-byte *dle* stream)
		   (write-byte byte stream))
	    (write-byte byte stream)))
  (write-byte *end-flag* stream))

(defun send-serial-command (msg-type params)
  (prog1 (send-framed-message (cons (foreign-enum-value
				     'serial-msg-types msg-type)
				    params)
			      *aleph-output-stream*)
    (force-output *aleph-output-stream*)))

(defparameter *s16-max* #x7FFF)
(defparameter *s16-min* (- #x8000))
(defvar *char-max* #xFF)

(defun u16-s16 (unsigned)
  (assert (<= unsigned  #xFFFF))
  (assert (>= unsigned 0))
  (if (> unsigned #x7FFF)
      (+ (- #x10000)
	 unsigned)
      unsigned))

(defun s16-u16 (signed)
  (assert (<= signed  #x7FFF))
  (assert (>= signed (- #x8000)))
  (if (< signed 0)
      (+ #x10000
	 signed)
      signed))

(defun s16-chars (s16)
  (assert (<= s16 *s16-max*))
  (assert (>= s16 *s16-min*))
  (let ((u16 (s16-u16 s16)))
    (list (ash u16 -8)
	  (logand u16
		  #x00FF))))

(defun chars-s16 (hi lo)
  (assert (<=  hi *char-max*))
  (assert (<=  lo *char-max*))
  (u16-s16 (+ lo (ash hi 8))))

(defun serial-debug (control-string &rest format-arguments)
  (send-serial-command :eSerialMsg_debug
		       (coerce (string-to-octets
				(apply #'format nil control-string
				       format-arguments)
				:external-format :ascii)
			       'list)))

(defun serial-dumpIns ()
  (send-serial-command :eSerialMsg_dumpIns
		       '()))

(defun serial-dumpParams ()
  (send-serial-command :eSerialMsg_dumpParams
		       '()))
(defun serial-dumpOutputs ()
  (send-serial-command :eSerialMsg_dumpOutputs
		       '()))
(defun serial-dumpConnections ()
  (send-serial-command :eSerialMsg_dumpConnections
		       '()))
(defun serial-dumpOps ()
  (send-serial-command :eSerialMsg_dumpOps
		       '()))
(defun serial-dumpOpDescriptions ()
  (send-serial-command :eSerialMsg_dumpOpDescriptions
		       '()))


(defun serial-trigger-param (addr val)
  (send-serial-command :eSerialMsg_triggerParam
		       (append (s16-chars addr)
			       (s16-chars val))))

(defun serial-trigger-in (addr val)
  (send-serial-command :eSerialMsg_triggerIn
		       (append (s16-chars addr)
			       (s16-chars val))))

(defun serial-query-in (addr)
  (send-serial-command :eSerialMsg_queryIn
		       (s16-chars addr)))

(defun serial-query-param (addr)
  (send-serial-command :eSerialMsg_queryParam
		       (s16-chars addr)))
(defun serial-connect (out in)
  (send-serial-command :eSerialMsg_connect
		       (append (s16-chars out)
			       (s16-chars in))))
(defun serial-disconnect (out)
  (send-serial-command :eSerialMsg_disconnect
		       (s16-chars out)))
(defun serial-newOp (type)
  (send-serial-command :eSerialMsg_newOp
		       (s16-chars type)))
(defun serial-deleteOp (op)
  (send-serial-command :eSerialMsg_deleteOp
		       (s16-chars op)))
(defun serial-storePreset (preset)
  (send-serial-command :eSerialMsg_storePreset
		       (s16-chars preset)))
(defun serial-recallPreset (preset)
  (send-serial-command :eSerialMsg_recallPreset
		       (s16-chars preset)))

(defun serial-bfinProgStart ()
  (send-serial-command :eSerialMsg_bfinProgStart
		       '()))
(defun serial-bfinHexChunk (bytes)
  (send-serial-command :eSerialMsg_bfinHexChunk
		       bytes))
(defun serial-bfinDscChunk (bytes)
  (send-serial-command :eSerialMsg_bfinDscChunk
		       bytes))
(defun serial-bfinProgEnd ()
  (send-serial-command :eSerialMsg_bfinProgEnd
		       '()))

(defun serial-send-aleph-module (module-path dsc-path
				 &optional (chunk-pause 0.01))
  (serial-bfinProgStart)
  (let ((buf (make-array 64
			 :element-type '(unsigned-byte 8))))
    (with-open-file (module-stream module-path
				   :direction :input
				   :element-type '(unsigned-byte 8))
      (loop for read = (read-sequence buf module-stream)
	 while (plusp read)
	 do (sleep chunk-pause)
	   (serial-bfinHexChunk (subseq (coerce buf 'list)
					       0 read))))
    (with-open-file (dsc-stream dsc-path
				:direction :input
				:element-type '(unsigned-byte 8))
      (loop for read = (read-sequence buf dsc-stream)
	 while (plusp read)
	 do (sleep chunk-pause)
	   (serial-bfinDscChunk (subseq (coerce buf 'list)
					       0 read)))))
  (sleep chunk-pause)
  (serial-bfinProgEnd))

(defun eat-leading-string (chars)
  (iterate (for remaining on chars)
	   (until (= 0 (car remaining)))
	   (collect (car remaining) into octets)
	   (finally (return (cons (octets-to-string (coerce octets
							    '(vector (unsigned-byte 8))))
				  (cdr remaining))))))

(defun unpack-string-s16-s16-xN (connections-dump)
  (destructuring-bind (string . rest) (eat-leading-string connections-dump)
    (when (and string
	       (car rest)
	       (cadr rest)
	       (caddr rest)
	       (cadddr rest))
      (cons (list string
		  (chars-s16 (car rest)
			     (cadr rest))
		  (chars-s16 (caddr rest)
			     (cadddr rest)))
	    (unpack-string-s16-s16-xN (cddddr rest))))))
#+nil
(unpack-string-s16-s16-xN '(44 45 46 0 5 6 7 8 44 45 46 0 5 6 7 8))

(defun unpack-string-s16-xN (connections-dump)
  (destructuring-bind (string . rest) (eat-leading-string connections-dump)
    (when (and string
	       (car rest)
	       (cadr rest))
      (cons (list string
		  (chars-s16 (car rest)
			     (cadr rest)))
	    (unpack-string-s16-xN (cddr rest))))))
#+nil
(unpack-string-s16-xN '(44 45 46 0 5 6 44 45 46 0 5 6))

(defun unpack-s16-s16-xN (dump)
  (when (and (car dump)
	     (cadr dump)
	     (caddr dump)
	     (cadddr dump))
  (cons (list (chars-s16 (car dump)
			 (cadr dump))
	      (chars-s16 (caddr dump)
			 (cadddr dump)))
	(unpack-s16-s16-xN (cddddr dump)))))

(defun unpack-string-xN (octets)
  (when octets
    (destructuring-bind (str . rest) (eat-leading-string octets)
      (cons str (unpack-string-xN rest)))))
#+nil
(unpack-s16-s16-xN '(5 6 7 8 5 6 7 8))

(defun serial-recv-msg ()
  (let ((state :waiting)
	(bytes nil))
    (iterate (until (eq state :done))
	     ;; (if (> (length bytes)
	     ;; 	    4096)
	     ;; 	 (break "why the long message?"))
	     (let ((new-byte (read-byte *aleph-input-stream*)))
	       (match state
		 (:waiting (if (= new-byte *start-flag*)
			       (setf state :receiving)))
		 (:receiving (cond
			       ((= new-byte *end-flag*) (setf state :done))
			       ((= new-byte *dle*) (setf state :escaping))
			       (t (push new-byte bytes))))
		 (:escaping (push new-byte bytes)
			    (setf state :receiving))
		 (otherwise (error "indeterminate state")))))
    (reverse bytes)))

(defvar *echo-bfin-prog* nil)

(defun serial-unpack-message (bytes)
  (match bytes
    ((cons #.(foreign-enum-value 'serial-msg-types :eSerialMsg_debug)
	   octets)
     (list :debug
	   (octets-to-string (coerce octets
				     '(vector (unsigned-byte 8))))))
    ((cons #.(foreign-enum-value 'serial-msg-types :eSerialMsg_insDump)
	   octets)
     (cons :ins-dump
	   (unpack-string-xN octets)))
    ((cons #.(foreign-enum-value 'serial-msg-types :eSerialMsg_paramsDump)
	   octets)
     (cons :paramsDump
	   (unpack-string-xN octets)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_inVal)
	    addr-hi addr-lo val-hi val-lo _)
     (list :in-val (chars-s16 addr-hi addr-lo) (chars-s16 val-hi val-lo)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_paramVal)
	    addr-hi addr-lo val-hi val-lo _)
     (list :param-val (chars-s16 addr-hi addr-lo) (chars-s16 val-hi val-lo)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_outVal)
	    addr-hi addr-lo val-hi val-lo _)
     (list :out-val (chars-s16 addr-hi addr-lo) (chars-s16 val-hi val-lo)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_opDescriptionsDump)
	    op-descriptions)
     (cons :op-descriptions (unpack-string-xN op-descriptions)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_opsDump)
	    ops-dump)
     (cons :ops-dump (unpack-string-s16-s16-xN ops-dump)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_connectionsDump)
	    connections-dump)
     (cons :connections-dump (unpack-s16-s16-xN connections-dump)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_outputsDump)
	    outputs-dump)
     (cons :outputs-dump (unpack-string-xN outputs-dump)))
    ((list* #.(foreign-enum-value 'serial-msg-types :eSerialMsg_bfinProgEcho)
	    outputs-dump)
     (setf *echo-bfin-prog* outputs-dump)
     (list :bfin-echo-received))
    (otherwise (break "unknown message: ~A" bytes))))

(defparameter *aleph-dev* "/dev/ttyACM0")

(defvar *aleph-input-stream* nil)

(defmacro with-aleph-input-stream (&body body)
  `(with-open-file (*aleph-input-stream* *aleph-dev*
					 :direction :io
					 :if-exists :overwrite
					 :element-type '(unsigned-byte 8))
     ,@body))

(defun start-debug-listener ()
  (list (multiple-value-list
	 (external-program:run "stty" '("-F" "/dev/ttyACM0" "115200" "raw")))
	(bt:make-thread
	 (lambda ()
	   (with-aleph-input-stream
	     (loop (print (serial-unpack-message
			   (serial-recv-msg)))))))))

(defvar *aleph-output-stream* nil)

(defmacro with-aleph-output-stream (&body body)
  `(with-open-file (*aleph-output-stream* *aleph-dev*
					  :direction :output
					  :if-exists :overwrite
					  :element-type '(unsigned-byte 8))
     ,@body))

(defun test-all-commands ()
  (with-aleph-output-stream
    (serial-dumpIns)
    (serial-dumpParams)
    (serial-trigger-param 3 3)
    (serial-trigger-in 4 4)
    (serial-query-in 4)
    (serial-query-param 5)
    (serial-dumpoutputs)
    (serial-dumpconnections)
    (serial-dumpops)(sleep 1)
    (serial-dumpopdescriptions)
    (serial-connect 1 1)
    (serial-disconnect 1)
    (serial-newOp 1)
    (serial-deleteop 1)
    (serial-recallPreset 1)
    (serial-storePreset 1)
    ))

(defun test-bfin-module-load ()
  (with-aleph-output-stream
    (serial-send-aleph-module "/home/rick/git_checkouts/aleph/modules/grains/grains.ldr"
			      "/home/rick/git_checkouts/aleph/modules/grains/grains.dsc" 0.001)))

(defvar *local-hex-buf* nil)

(defun hex-loopback-test ()
  (let ((buf (make-array (* 1024 256) :initial-element nil
			 :fill-pointer (* 1024 256))))
    (with-open-file (module-stream "/home/rick/git_checkouts/aleph/modules/analyser/analyser.ldr"
				   :direction :input
				   :element-type '(unsigned-byte 8))
      (setf (fill-pointer buf)
	    (read-sequence buf module-stream))
      (setf *local-hex-buf* (coerce buf 'list))
      (let ((i 0))
	(loop for c1 in *local-hex-buf*
	   for c2 in *echo-bfin-prog*
	   do (unless (= c1 c2)
		(break "~Ath char doesn't match" i))
	     (incf i))))))

(defun hexdump (li &optional)
  (loop for el in li
     collect (format nil "~X" el)))
