;; https://rosettacode.org/wiki/LZW_compression

(to (show-lzw string)
  (for each ((code (encode string)))
    (if (<= code 255)
        (string<- (char<- code))
        code)))

(let default-codebook (each string<- ((char<- 0) .span 256)))

(to (encode string @(optional codebook))
  (let codes ((or codebook default-codebook) .inverse))
  (let result (flexarray<-))
  (let chunk
    (for foldl ((chunk "") (c string.values))
      (let s1 (string<- c))
      (let chunk-1 (chain chunk s1))
      (if (codes .maps? chunk-1)
          chunk-1
          (do (result .push! (codes chunk))
              (codes .set! chunk-1 codes.count)
              s1))))
  (unless chunk.empty?
    (result .push! (codes chunk)))
  result.values)

(to (decode codes @(optional codebook))
  ("" .join (chunked-decode codes (or codebook default-codebook))))

;; TODO ugly!
(to (chunked-decode codes codebook)
  (if codes.empty?
      '()
      (do
        (let chunks (flexarray<-list codebook))
        (let chunk-1 (chunks codes.first))
        (let output (flexarray<-))
        (begin decoding ((chunk chunk-1)
                         (codes codes.rest)) ;TODO not a great idea, reusing the name
          (chunks .push! chunk)
          (output .push! chunk)
          (if codes.empty?
              output.values
              (do (let code codes.first)
                  (let new-chunk (chunks code))
                  (chunks .set! (- chunks.count 1) (chain chunk (string<- new-chunk.first)))
                  (decoding (chunks code) codes.rest)))))))

(print (show-lzw "TOBEORNOTTOBEORTOBEORNOT"))

(let m "XXXXXXXXXXXX")
(print (show-lzw m))
;; ['X', 256, 257, 258, 256]
(let x (encode m))
(print (chunked-decode x default-codebook))
;; ['X', 'XX', 'XXX', 'XXXX', 'XX']

(surely (= m (decode x)))

(let p2 "TOBEORNOTTOBEORTOBEORNOT")
(let m2 (encode p2))
(let x2 (decode m2))
(surely (= x2 p2))
