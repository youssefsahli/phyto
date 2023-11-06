#lang racket/base

(require racket/format
         racket/gui/base
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         racket/path
         racket/string
         racket/struct
         "phyto.rkt")

(provide (all-defined-out))
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
