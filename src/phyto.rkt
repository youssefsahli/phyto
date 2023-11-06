#lang racket/base

(require file/unzip
         file/zip
         mischief/for
         racket/fasl
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/match
         racket/serialize
         racket/struct)

(provide (all-defined-out))

(serializable-struct db<> (data freed-ids) #:mutable #:transparent)
(serializable-struct familia<> (id latin) #:mutable #:transparent)
(serializable-struct genus<> (id latin familia-id) #:mutable #:transparent)
(serializable-struct species<> (id latin genus-id) #:mutable #:transparent)
(serializable-struct plant<>
                     (id french species-id smell toxic? image-path)
                     #:mutable
                     #:transparent)

(define (make-new-db)
  (db<> (make-hash) (make-hash)))

(define db-collection
  (for/hash! {(db-key (in-list '(families genera species plants)))}
             (values db-key (make-new-db))))

;; Shortcuts for db-collection keys
(match-define {hash-table
               ('families families)
               ('genera genera)
               ('species species)
               ('plants plants)}
  db-collection)

(define (get-struct-db sym)
  (match sym
    ['familia<> families]
    ['genus<> genera]
    ['species<> species]
    ['plant<> plants]))

(define (make-blank-instance sym)
  (match sym
    ['familia (familia<> -1 "")]
    ['genus (genus<> -1 "" -1)]
    ['species (species<> -1 "" -1)]
    ['plant (plant<> -1 "" -1 "" #f "")]))

(define (last-id db)
  (apply max 0 (hash-keys (db<>-data db))))

(define (generate-id db)
  (if (not (hash-empty? (db<>-freed-ids db)))
      ;; If there are freed IDs, reuse one.
      (let ([reuse-id (first (hash-keys (db<>-freed-ids db)))])
        (hash-remove! (db<>-freed-ids db) reuse-id)
        reuse-id)
      ;; Otherwise, generate a new ID.
      (add1 (last-id db))))

(define (db-exists? db)
  (db<>? db))

(define (db-has? db id)
  (let ([dat (db<>-data db)]) (hash-has-key? dat id)))

(define (db-id-exists? db id)
  (and (db-exists? db) (db-has? db id)))

(define (db-get db id)
  (if (db-id-exists? db id) (hash-ref (db<>-data db) id) #f))

(define (db-add db make-record-fn . args)
  (define id (generate-id db))
  ;; Create the record with the correct ID.
  (define record (apply make-record-fn id args))
  (hash-set! (db<>-data db) id record)
  id) ; Return the ID of the newly added item.

(define (db-remove db id)
  (hash-remove! (db<>-data db) id)
  (hash-set! (db<>-freed-ids db) id #t)) ; Adding ID to freed-ids.

(define (db-list db)
  ;; returns the list of structs instances, and the method to extract the names
  ;; Each structure has it's name as it's second field
  (define (extract-name choice)
    (define obj (cdr choice))
    (second (struct->list obj)))
  (values (hash->list (db<>-data db)) extract-name))

;; Persistence

(define @CURRENT-DB (@ "data.db"))

(define (load [path #f])
  (when path
    (:= @CURRENT-DB path))
  (call-with-unzip (obs-peek @CURRENT-DB)
                   (λ (temp)
                     (let ([lop (directory-list temp)])
                       (for ([filepath (in-list lop)])
                         (let ([filename (file-name-from-path filepath)])
                           (hash-set! db-collection
                                      (string->symbol (path->string filename))
                                      (load-database-from-file filepath))))))))

(define (init)
  (load-database-from-file (obs-peek @CURRENT-DB)))

(define (save [path #f])
  (when path
    (:= @CURRENT-DB path))
  (define temp (make-temporary-directory))
  (define exported-paths
    (for/list ([key (in-list (hash-keys db-collection))])
      (let ([path (build-path (symbol->string key))])
        (save-database-to-file (hash-ref db-collection key) path)
        path)))
  (call-with-output-file (obs-peek @CURRENT-DB)
                         (λ (out) (zip->output exported-paths out))
                         #:exists 'replace))

(define (save-and-exit)
  (save)
  (exit))

(define (save-database-to-file db filename)
  (call-with-output-file filename
                         (λ (out) (s-exp->fasl (serialize db) out))
                         #:exists 'replace))

(define (load-database-from-file filename)
  (when (file-exists? filename)
    (call-with-input-file filename (λ (in) (deserialize (fasl->s-exp in))))))
