;;;; creds.lisp

(in-package #:creds)

;;; "creds" goes here. Hacks and glory await!

(defparameter *creds* (make-hash-table :test #'equal))
(defparameter *creds-file* "~/creds.yaml")

(defun load-creds (&optional creds-file)
  "Load creds from the creds file. Defaults to ~/creds.yaml. Returns
nil if the creds file does not exist."
  (if creds-file (setf *creds-file* creds-file))
  (if (probe-file *creds-file*)
      (setf *creds* (cl-yy::yaml-load-file "~/creds.yaml" :on-size-exceed :warn))
      nil))

(defun get-cipher (key)
  (ironclad:make-cipher :blowfish
    :mode :ecb
    :key (ironclad:ascii-string-to-byte-array key)))

(defun encrypt (plaintext key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:ascii-string-to-byte-array plaintext)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))

(defun decrypt (ciphertext-int key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:integer-to-octets ciphertext-int)))
    (ironclad:decrypt-in-place cipher msg)
    (coerce (mapcar #'code-char (coerce msg 'list)) 'string)))

(defun get-cred (name &optional key)
  "Return the specified credential value, decrypting it if a key is
provided."
  (let ((res (gethash name *creds*)))
    (if res
	(if key
	    (decrypt (gethash name *creds*) key)
	    res)
	nil)))

(defun set-cred (name value &optional key)
  "Store a new credential using an optional encryption key."
  (if key
      (setf (gethash name *creds*) (encrypt value key))
      (setf (gethash name *creds*) value))
  (write-yaml))

(defun delete-cred (name)
  "Delete a stored cred."
  (remhash name *creds*)
  (write-yaml))

(defun write-yaml (&optional (file-name *creds-file*))
  "Write creds to a file."
  (with-open-file (yaml file-name :direction :output :if-exists :supersede)
    (format yaml "---~%")
    (mapcar (lambda (n) (format yaml "~A: ~A~%" n (gethash n *creds*))) (alexandria:hash-table-keys *creds*)))
  t)
