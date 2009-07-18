
(in-package :oauth-test)

(def-suite parameters :in oauth)

(in-suite parameters)

(test splice-alist/nil
  (is (null (oauth::splice-alist nil))))

(test splice-alist/simple
  (is (equal (oauth::splice-alist '((a . 1)(b . 2)))
             '(a 1 b 2))))

(test alist->query-string/nil
  (is (equal (oauth::alist->query-string nil) "")))

(test alist->query-string/simple
  (is (equal (oauth::alist->query-string '(("foo" . 1) ("bar" . 2)))
             "&foo=1&bar=2")))

(test alist->query-string/no-ampersand
  (is (equal (oauth::alist->query-string '(("foo" . 1) ("bar" . 2))
                                  :include-leading-ampersand nil)
             "foo=1&bar=2")))

(test normalized-parameters/spec-example
  (let ((*post-parameters* '(("a" . "1")
                             ("c" . "hi%20there")
                             ("f" . "25")
                             ("f" . "50")
                             ("f" . "a")
                             ("z" . "p")
                             ("z" . "t"))))
    (is (equal
          (oauth::alist->query-string
            (oauth::normalized-parameters)
            :include-leading-ampersand nil)
          "a=1&c=hi%20there&f=25&f=50&f=a&z=p&z=t"))))

