# creds-lisp
A Common Lisp client for securely storing credentials.

This library is designed to store all of your credentials for various services in a standardized (and optionally encrypted) manner.  For unencrypted fields, the credentials can be shared with non-Lisp (ie, Ruby, Python, etc) code.

The credentials file is stored by default as ~/creds.yaml. This file name can be overridden by the user. This is a standard YAML file, and contains one keyword and value per line (in the standard YAML format).

Start by loading the creds file. If a filename is not specified, it will default to ~/creds.yaml. If that file does not exist, the file name will be saved in case you wish to add values to (ie, create from scratch) a new creds file. Loading the creds file is mandatory, even if it does not exist, as that initializes all of the internal data structures:

```
(ql:quickload :creds)
(creds:load-creds)
```

Assuming a given credential exists in the file, it can now be used in your code. For example:

```
CL-USER> (format t "The secret word is ~A.~%" (creds:get-cred "secretword"))
The secret word is banana.
NIL
CL-USER>
```

If there is no match to the specified credential name, nil is returned. If the credential value was encrypted, you must provide the encryption key specific to that entry. Example, assuming the encryption key is "secret squirrel" (note that without the key, you get the encrypted data):

```
CL-USER> (creds:get-cred "secretphrase")
4428965676694255868841270619508011529429363
CL-USER> (creds:get-cred "secretphrase" "secret squirrel")
"big yellow bananas"
CL-USER>
```

Setting a creds value works very much like reading one. You specify the name, the value, and an optional key. Each added or changed value results in writing the new creds file to disk. Note that their seems to be a bug (or a misunderstanding on my part) in the ironclad encryption ilbrary where any value stored with a length of less than eight characters can be decrypted with any password. Consequently, if you try to store an encrypted value with too short of a length with this package, it fails and warns you (rather than storing your data insecurely):

```
CL-USER> (creds:set-cred "unencryptedthing" "this is a test")
NIL
CL-USER> (creds:set-cred "encryptedthing" "this is a test" "secret passphrase")
NIL
CL-USER> (creds:set-cred "foo" "this is a test" "secret passphrase")
NIL
CL-USER> (creds:set-cred "encryptedthing" "foo" "secret passphrase")
Unable to encrypt keys with length < 8 due to a bug in the ironclad encryption library.
NIL
CL-USER>
```

You can also delete a credential (wihch also results in a write to disk):

```
CL-USER> (creds:set-cred "testvalue" "Mein Hut, der hat Drei Ecken")
NIL
CL-USER> (creds:get-cred "testvalue")
"Mein Hut, der hat Drei Ecken"
CL-USER> (creds:delete-cred "testvalue")
NIL
CL-USER> (creds:get-cred "testvalue")
NIL
CL-USER>
```
