#lang racket/gui
(require drracket/tool)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (void))
    (define (phase2) (void))
    
    (define-local-member-name update-counts get-counts-txt)
    
    (define (tab-mixin super%)
      (class super%
        (define txt (new text%))
        (define/public (get-counts-txt) txt)
        (define/public (update-counts lst)
          (send txt begin-edit-sequence)
          (send txt erase)
          (for ([lst (in-list lst)])
            (let ([c (list-ref lst 0)]
                  [v (list-ref lst 1)])
              (send txt insert (format "~s ~s\n" c v))))
          (send txt end-edit-sequence))
        (super-new)))
    
    (define (defs-mixin super%)
      (class super%
        (inherit get-tab last-position get-character)
        (define/augment (after-insert start len)
          (inner (void) after-insert start len)
          (update-counts))
        (define/augment (after-delete start len)
          (inner (void) after-delete start len)
          (update-counts))
        (define/augment (after-load-file success?)
          (inner (void) after-load-file success?)
          (update-counts))
        (define/private (update-counts)
          (let ([ht (make-hash)])
            (for ([i (in-range 0 (last-position))])
              (let ([c (get-character i)])
                (hash-set! ht c (+ 1 (hash-ref ht c 0)))))
            (send (get-tab) update-counts
                  (sort (hash-map ht list) cmp))))
        (super-new)))
    
    (define (cmp lst1 lst2)
      (cond
        [(= (list-ref lst1 1) (list-ref lst2 1))
         (char<=? (list-ref lst1 0) (list-ref lst2 0))]
        [else
         (> (list-ref lst1 1) (list-ref lst2 1))]))
    
    (define (frame-mixin super%)
      (class super%
        (inherit get-current-tab get-menu-bar get-button-panel
                 register-toolbar-button)
        
        (define freq-canvas #f)
        (define freq-panel-parent #f)
        (define/override (make-root-area-container cls parent)
          (set! freq-panel-parent (super make-root-area-container
                                         horizontal-panel% parent))
          (let ([root (make-object cls freq-panel-parent)])
            (set! freq-canvas (new editor-canvas%
                                   [parent freq-panel-parent]
                                   [stretchable-width #f]
                                   [min-width 200]))
            root))
        
        (define/augment (on-tab-change from-tab to-tab)
          (inner (void) on-tab-change from-tab to-tab)
          (send freq-canvas set-editor (send to-tab get-counts-txt)))
        
        (super-new)
        (send freq-canvas set-editor (send (get-current-tab) get-counts-txt))))
    
    (drracket:get/extend:extend-unit-frame frame-mixin)
    (drracket:get/extend:extend-tab tab-mixin)
    (drracket:get/extend:extend-definitions-text defs-mixin)))
