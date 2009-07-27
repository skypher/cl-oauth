
(in-package :oauth-test)

(def-suite signature :in oauth)

(in-suite signature)

(defvar *sample-signature-base-string* 
  (format nil "GET&http%3A%2F%2Fphotos.example.net%2Fphotos~
               &file%3Dvacation.jpg%26oauth_consumer_key~
               %3Ddpf43f3p2l4k3l03%26oauth_nonce%3Dkllo9940pd9333jh~
               %26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp~
               %3D1191242096%26oauth_token%3Dnnch734d00sl2jdk~
               %26oauth_version%3D1.0%26size%3Doriginal"))

;; A.5.1
(test signature-base-string/spec
  (let* ((*request-method* :get)
         (uri "http://photos.example.net/photos")
         (parameters (format nil "file=vacation.jpg&oauth_consumer_key=dpf43f3p2l4k3l03~
                                  &oauth_nonce=kllo9940pd9333jh&oauth_signature_method=HMAC-SHA1~
                                  &oauth_timestamp=1191242096&oauth_token=nnch734d00sl2jdk~
                                  &oauth_version=1.0&size=original"))
         (parameters-alist (oauth::query-string->alist parameters))
         (*get-parameters* parameters-alist)
         (signature-base-string (signature-base-string :uri uri)))
    (is (equal signature-base-string
               *sample-signature-base-string*))))

;; A.5.2
(test hmac-sha1-digest/spec
  (let* ((key "kd94hf93k423kf44&pfkkdhi9sl3r4s00")
         (text *sample-signature-base-string*)
         (digest (hmac-sha1 text key))
         (digest/base64 (cl-base64:usb8-array-to-base64-string digest)))
    (is (equal digest/base64 "tR3+Ty81lMeYAr/Fid0kMTYa/WM="))))

