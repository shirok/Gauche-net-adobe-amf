;;;
;;; Test net.adobe.amf
;;;

(use gauche.test)

(test-start "net.adobe.amf")

(use net.adobe.amf.0)
(test-module 'net.adobe.amf.0)

(use net.adobe.amf.3)
(test-module 'net.adobe.amf.3)

(use net.adobe.amf)
(test-module 'net.adobe.amf)

(test-end)





