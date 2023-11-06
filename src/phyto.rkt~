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
    ['familia<> (familia<> -1 "")]
    ['genus<> (genus<> -1 "" -1)]
    ['species<> (species<> -1 "" -1)]
    ['plant<> (plant<> -1 "" -1 "" #f "")]))

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

;; ------ [Form construction tools]------

(define (form-add-struct-dialog db obs)
  (define-values (los extract-name) (db-list db))
  (define (get-id obj)
    (first (struct->list obj)))
  (define (choice=? obj1 obj2)
    (= (get-id obj1) (get-id obj2)))
  (dialog #:stretch '(#t #t)
          #:min-size '(320 160)
          (vpanel (choice los
                          (λ (choice)
                            (define obj (cdr choice))
                            (let ([id (first (struct->list obj))]) (:= obs id)))
                          #:choice->label extract-name
                          #:choice=? choice=?))))

(define (form-show-obs label obs)
  (text (~a label (obs-peek obs))))

(define (form-input-obs obs)
  (input obs (λ (ev text) (:= obs text))))

(define (form-cb-obs label obs)
  (checkbox (λ (bval) (:= obs bval)) #:label label #:checked? (obs-peek obs)))

(define (form-image-path obs)
  (define (is-img? path)
    (bytes=? (path-get-extension path) #".png"))
  (let ([img-path (obs-peek obs)] [@default-img-path (@ "plant-icon.png")])
    (vpanel (image (if (non-empty-string? img-path) obs @default-img-path)
                   #:size '(50 50)
                   #:mode 'fit)
            (button "Image Path"
                    (λ ()
                      (let* ([path (get-file)])
                        (when (not (is-img? path))
                          (render (dialog (text "Not an image !"))))
                        (when (and path is-img?)
                          (:= obs path))))))))

(define (form-show-id-link obs struct-type-sym)
  (let ([id (obs-peek obs)] [db (get-struct-db struct-type-sym)])
    (case id
      [(-1)
       (hpanel (text "Not set")
               (button "Add" (λ () (render (form-add-struct-dialog db obs)))))]
      [else
       (vpanel (text (~a "ID: " id)) (button "Remove" (λ () (:= obs -1))))])))

(define (make-form record)
  (match record
    [(familia<> id latin)
     (let ([@id (@ id)] [@latin (@ latin)])
       (group "Famille" (form-show-obs "ID: " @id) (form-input-obs @latin)))]

    [(genus<> id latin familia-id)
     (let ([@id (@ id)] [@latin (@ latin)] [@familia-id (@ familia-id)])
       (group "Genre" (form-show-obs "ID: " @id) (form-input-obs @latin)))]

    [(species<> id latin genus-id)
     (let ([@id (@ id)] [@latin (@ latin)] [@genus-id (@ genus-id)])
       (group "Espèce" (form-show-obs "ID: " @id) (form-input-obs @latin)))]

    [(plant<> id french species-id smell toxic? image-path)
     (let ([@id (@ id)]
           [@french (@ french)]
           [@species-id (@ species-id)]
           [@smell (@ smell)]
           [@toxic? (@ toxic?)]
           [@image-path (@ image-path)])
       (group "Plante"
              (form-show-obs "ID: " @id)
              (form-input-obs @french)
              (form-input-obs @smell)
              (form-image-path @image-path)
              (form-show-id-link @species-id 'species<>)
              (form-cb-obs "Toxique" @toxic?)))]
    [sym (make-form (make-blank-instance sym))]))

(define main-menu
  (menu-bar (menu "File"
                  (menu-item "Open ..."
                             (λ ()
                               (let ([path (get-file)])
                                 (when path
                                   (load-database-from-file path)))))
                  (menu-item "Save")
                  (menu-item "Save As ...")
                  (menu-item-separator)
                  (menu-item "Quit" save-and-exit))
            (menu "Help" (menu-item "About"))))

(define main-window (vpanel main-menu))

(define main-frame (window #:size '(800 640) main-window))

;; Local Variables:
;; apheleia-formatters: ((raco-fmt-col "raco" "fmt" "--width" "80"))
;; apheleia-formatter: raco-fmt-col
;; eval: (racket-xp-mode)
;; End:
