;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 6(Final)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "./file_operations.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: Recursion over the File System Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sum : (listof number) -> number

(define (sum lst)
  (foldr + 0 lst))

(check-expect (sum (list 0 1 2 3 4))
              10)

(check-expect (sum (list 0 2 4))
              6)

;; count-files : path -> number
(define (count-files dir-path)
  (length (append (directory-files dir-path)
               (map (lambda (subdir)
                      (count-files subdir))
                    (directory-subdirectories dir-path)))))

(check-expect (count-files (build-path "test"))
              6)

(check-expect (count-files (build-path "test" "test_2"))
              2)

(check-expect (count-files (build-path "test" "test_3"))
              2)
              

;; directory-size : path -> number
(define (directory-size dir-path)
  (sum (append (map file-size (directory-files dir-path))
               (map (lambda (subdirectory1)
                      (directory-size subdirectory1))
                    (directory-subdirectories dir-path)))))

(check-expect (directory-size "test")
              22401)

(check-expect (directory-size (build-path "test" "test_2"))
              520)

(check-expect (directory-size (build-path "test" "test_3"))
              6148)

;; concat : (listof (listof path)) -> (listof path)
(define (concat lst-of-lists)
  (cond [(empty? lst-of-lists) empty]
        [else
         (append (first lst-of-lists)
                 (concat (rest lst-of-lists)))]))
          

(check-expect (concat
               (list (list (build-path "test"))
                     (list (build-path "test" "test_2")
                           (build-path "test" "test_3"))))
              (list (build-path "test")
                    (build-path "test" "test_2")
                    (build-path "test" "test_3")))

(check-expect (concat (list (list (build-path "test" "test.txt"))
                            (list (build-path "test" "test_2" "foo.bmp")
                                  (build-path "test" "test_3"))))
                      (list (build-path "test" "test.txt")
                            (build-path "test" "test_2" "foo.bmp")
                            (build-path "test" "test_3")))

;; all-directories : path -> (listof path)
(define (all-directories dir-path)
  (cons dir-path
        (concat (map (lambda (subdirectory1)
                       (all-directories subdirectory1))
                     (directory-subdirectories dir-path)))))
  

(check-expect (all-directories (build-path "test"))
              (list (build-path "test")
                    (build-path "test" "test_2")
                    (build-path "test" "test_3")
                    (build-path "test" "test_3" "test3_subfolder")))

(check-expect (all-directories (build-path "test" "test_3"))
              (list (build-path "test" "test_3")
                    (build-path "test" "test_3" "test3_subfolder")))

;; search-file-name : string path -> (listof path)


(define (search-file-name name dir-path)
  (concat (cons (filter (lambda (file3)
                          (string-contains? name (path->string (path-filename file3))))
                        (directory-files dir-path))
                  (map (lambda (subdirectory5)
                         (search-file-name name subdirectory5))
                       (directory-subdirectories dir-path)))))
          
  

(check-expect (search-file-name "foo" (string->path "test"))
              (list (build-path "test" "foo.bmp")
                    (build-path "test" "test_2" "foo.bmp")))
(check-expect
 (search-file-name "test" (string->path "test"))
 (list (build-path "test" "test.txt")))


(check-expect
 (search-file-name "bar" (string->path "test"))
 (list (build-path "test" "test_2" "bar.txt")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Imperative Programming and File Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; backup! : Path Path -> Void
;
; Recursively copies all the files and subdirectories in the `from`
; directory to the `to` directory. This is a modified version of the
; `copy-tree` procedure we discussed in class.
;
; EFFECT: `to` and all its contents now exist
; EFFECT: may overwrite existing files at `to`
(define (backup! from to) 
  (begin
    ; create the destination directory if it doesn't already exist
    (unless (directory-exists? to)
      (make-directory! to))

    ; for each file (leaf node) in the origin directory,
    ; copy it over to the destination directory
    (for-each (λ (file)
                ; print the name of the file being copied into the REPL
                ; for more on how `printf` works, see Appendix 1 in the pdf
                (unless (and (file-exists? (build-path to (path-filename file)))
                             (= (file-or-directory-modify-seconds (build-path to (path-filename file)))
                                (file-or-directory-modify-seconds (build-path from (path-filename file)))))
                (begin
                  (printf "Copying file ~A to ~A~n"
                          file
                          (build-path to (path-filename file)))
                  (copy-file! file
                              (build-path to (path-filename file))
                              #true))))
              (directory-files from))

    ; for each folder (recursive child node) in the origin directory,
    ; recursively `backup!` its contents
    (for-each (λ (subdir)
                (backup! subdir
                         ; add the subdirectory's name to the
                         ; end of the original destination path
                         (build-path to (path-filename subdir))))
              (directory-subdirectories from))))
