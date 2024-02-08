;;; verb-oauth1.el --- OAuth 1 support for Verb -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Tom Small

;; Author: Tom Small <tsmall3@gmail.com>
;; Version: 1.0
;; Created: 8 Feb 2024
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") dash sha1 url)

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a Verb Request Mapping Function that adds an OAuth 1.0
;; Authorization header to requests. To use it, you need to first tell Verb to
;; use the mapping function with this property:
;;
;;   :Verb-Map-Request: verb-oauth1-sign-request
;;
;; Then you need to pass your OAuth credentials via other properties. These
;; properties are only metadata; they won't be sent in the request. The mapping
;; function looks for these properties:
;;
;; - Verb-OAuth-Consumer-Key
;; - Verb-OAuth-Consumer-Secret
;; - Verb-OAuth-Token
;; - Verb-OAuth-Token-Secret
;;
;; You will need the consumer key and secret in all requests. The token and
;; token secret should not be in the initial request token request (since you
;; don't have a token yet at that point). It's then used for both the request
;; token and the access token.
;;
;; This library only supports HMAC-SHA1 signing, because that is the only one
;; I've needed.

;;; Helpful Resources

;; The following two websites are really helpful for understanding the OAuth
;; 1.0 authorization workflow and, especially, the signing algorithm.
;;
;; http://lti.tools/oauth/
;; https://oauth1.wp-api.org/docs/basics/Signing.html

;;; Todo

;; - Create helper for getting metadata

;;; Code:

(require 'dash)
(require 'sha1)
(require 'url)

(defun verb-oauth1--normalize-params (params)
  "Normalize the OAuth PARAMS as required by the signing algorithm.
Returns the normalized string. PARAMS must be an alist, in which
each element is a proper list."
  (let* ((sorted (-sort (lambda (a b) (string< (car a) (car b))) params))
         (joined (mapcar (lambda (l) (let ((k (car l))
                                           (v (cadr l)))
                                       (format "%s=%s"
                                               (url-hexify-string k)
                                               (url-hexify-string v))))
                         sorted)))
    (string-join joined "&")))

(defun verb-oauth1--base-string (verb-request-spec oauth-params)
  "Create the base string needed to sign the OAuth request.
Uses request info from VERB-REQUEST-SPEC and the OAuth-specific
values in OAUTH-PARAMS. The OAUTH-PARAMS must be an alist, in
which each element is a proper list."
  (let* ((url (slot-value verb-request-spec :url))
         (scheme (url-type url))
         (host (url-host url))
         (path-and-query (url-path-and-query url))
         (path (car path-and-query))
         (query (if (cdr path-and-query)
                    (url-parse-query-string (cdr path-and-query))
                  nil))
         (method (slot-value verb-request-spec :method))
         (params (append query oauth-params)))
    (string-join
     (list method
           (url-hexify-string (concat scheme "://" host path))
           (url-hexify-string (verb-oauth1--normalize-params params)))
     "&")))

;; Courtesy of https://www.emacswiki.org/emacs/HmacShaOne
(defun verb-oauth1--hmac-sha1 (key message)
  "Create an HMAC-SHA1 digest of MESSAGE signed with the given KEY.
Both MESSAGE and KEY must be strings. Returns a binary value."
  (let ((block-size 64))
    (when (< block-size (length key))
      (setq key (sha1 key nil nil t)))
    (let ((key-block (make-vector block-size 0)))
      (dotimes (i (length key))
        (aset key-block i (aref key i)))
      (let ((opad (make-vector block-size #x5c))
            (ipad (make-vector block-size #x36)))
        (dotimes (i block-size)
          (aset ipad i (logxor (aref ipad i) (aref key-block i)))
          (aset opad i (logxor (aref opad i) (aref key-block i))))
        (when (fboundp 'unibyte-string)
          (setq opad (apply 'unibyte-string (mapcar 'identity opad)))
          (setq ipad (apply 'unibyte-string (mapcar 'identity ipad))))
        (sha1 (concat opad
                      (sha1 (concat ipad message)
                            nil nil t))
              nil nil t)))))

(defun verb-oauth1--signature (verb-request-spec oauth-params)
  "Create an OAuth signature string.
Gets the HTTP request information from VERB-REQUEST-SPEC and the
OAuth-specific values in OAUTH-PARAMS. The OAUTH-PARAMS must be
an alist, in which each element is a proper list."
  (let* ((message (verb-oauth1--base-string verb-request-spec oauth-params))
         (metadata (oref verb-request-spec metadata))
         (sha-key (concat (alist-get 'VERB-OAUTH-CONSUMER-SECRET metadata nil nil #'string=)
                          "&" (alist-get 'VERB-OAUTH-TOKEN-SECRET metadata "" nil #'string=))))
    (thread-last (verb-oauth1--hmac-sha1 sha-key message)
                 (base64-encode-string)
                 (url-hexify-string))))

(defun verb-oauth1--nonce (&optional length)
  "Create a nonce to include in the OAuth signature.
Returns the nonce as a string. If the optional LENGTH is
provided, the nonce will be that many characters long (defaults
to 40)."
  (let* ((length (or length 40))
         (valid-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (max-index (length valid-chars)))
    (with-temp-buffer
      (dotimes (_ length)
        (let ((index (random max-index)))
          (insert (aref valid-chars index))))
      (buffer-string))))

(defun verb-oauth1--auth-header-value (verb-request-spec)
  "Create the OAuth value for the `Authorization' header.
Gets the HTTP request info from the provided VERB-REQUEST-SPEC."
  (let* ((metadata (oref verb-request-spec metadata))
         (oauth-params `(("oauth_version" "1.0")
                         ("oauth_consumer_key" ,(alist-get 'VERB-OAUTH-CONSUMER-KEY metadata nil nil #'string=))
                         ("oauth_nonce" ,(verb-oauth1--nonce 16))
                         ("oauth_timestamp" ,(number-to-string (round (float-time))))
                         ("oauth_signature_method" "HMAC-SHA1")))
         (token (alist-get 'VERB-OAUTH-TOKEN metadata nil nil #'string=))
         (token-param (if token `(("oauth_token" ,token))))
         (oauth-params (append oauth-params token-param))
         (signature (verb-oauth1--signature verb-request-spec oauth-params))
         (final-oauth-params (append `(("oauth_signature" ,signature)) oauth-params))
         (parts (mapcar (lambda (each) (format "%s=\"%s\"" (car each) (cadr each))) final-oauth-params)))
    (concat "OAuth realm=\"\", " (string-join parts ", "))))

(defun verb-oauth1-sign-request (verb-request-spec)
  "Sign the provided VERB-REQUEST-SPEC.
Adds the OAuth 1.0 `Authorization' header. Modifies the
VERB-REQUEST-SPEC and returns it. Intended for use as a Verb
Request Mapping Function."
  (let* ((auth-value (verb-oauth1--auth-header-value verb-request-spec))
         (auth-header `(("Authorization" . ,auth-value)))
         (new-headers (append auth-header (oref verb-request-spec headers))))
    (oset verb-request-spec headers new-headers)
    verb-request-spec))

(provide 'verb-oauth1)
;;; verb-oauth1.el ends here
