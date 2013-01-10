;; Traditional MacOS theme
;; 7/12/00 Jason F. McBrayer jmcbray@carcosa.net
;; Some code borrowd from microGUI and Chromium.

(require 'make-theme)

(let*
    (
     
     (title-width
      (lambda (w)
        (let
	    ((w-width (car (window-dimensions w))))
	  (max 0 (+ 20 (min (- w-width 74) (text-width (window-name w))))))))
        
     (half-title-width
      (lambda (w)
	(/ (title-width w) 2)))
         
     (title-position
      (lambda (w)
	(- (/ (car (window-dimensions w)) 2) (half-title-width w)) ))
          
     (update-title-width
      (lambda (w)
        (if (eq (window-get w 'current-frame-style) 'MacOS) 
	    (rebuild-frame w))))

     (patterns-alist
      '(("title-bar"
         (inactive
          "title-inactive.png")
         (focused
          "title.png"))
        ("title"
         (inactive
          "title-inactive.png")
         (focused
          "title-text.png"))
        ("text-colors"
         (inactive . "#a8b9a8b9a8b9")
         (focused . "#01f001f001f0"))
        ("title-left"
         (inactive
          "title-inactive-left.png")
         (focused
          "title-left.png"))
        ("title-right"
         (inactive
          "title-inactive-right.png")
         (focused
          "title-inactive-right.png"))
        ("border-pixel"
         (inactive
          "border-pixel.png")
         (focused
          "border-pixel.png"))
        ("resize-button"
         (inactive
          "resize-inactive.png")
         (focused
          "resize.png"))
        ("close-button"
         (inactive
          "close-inactive.png")
         (focused
          "close.png")
         (clicked
          "close-click.png"))
        ("maximize-button"
         (inactive
          "maximize-inactive.png")
         (focused
          "maximize.png")
	 (clicked
	  "maximize-click.png"))
	("horizontal"
         (inactive
          "frameless-h.png")
         (focused
          "frameless-h.png"))
        ("vertical"
         (inactive
          "frameless-v.png")
         (focused
          "frameless-v.png"))
        ("top-left"
         (inactive
          "frameless-tl.png")
         (focused
          "frameless-tl.png"))
        ("top-right"
         (inactive
          "frameless-tr.png")
         (focused
          "frameless-tr.png"))
        ("bottom-left"
         (inactive
          "frameless-ll.png")
         (focused
          "frameless-ll.png"))
        ("bottom-right"
         (inactive
          "frameless-lr.png")
         (focused
          "frameless-lr.png")
	 )))

     (frames-alist
      `(
	("default"
	 ((left-edge . ,title-position)
	  (right-edge . ,title-position)
	  (top-edge . -19)
	  (foreground . "text-colors")
	  (y-justify . center)
	  (x-justify . center)
	  (text . window-name)
	  (background . "title")
	  (class . title))
	 ((background . "title-bar")
	  (top-edge . -19)
	  (width . ,(lambda (w) (+ (title-position w) 2)))
	  (left-edge . -1)
          (class . title))
         ((background . "title-bar")
          (top-edge . -19)
          (width . ,(lambda (w) (+ (title-position w) 2)))
          (right-edge . -2)
          (class . title))
         ((background . "title-left")
          (width . 3)
          (top-edge . -19)
          (left-edge . -1)
          (class . title))
         ((background . "title-right")
          (width . 3)
          (top-edge . -19)
          (right-edge . -2)
          (class . title))
         ((background . "border-pixel")
          (width . 1)
          (bottom-edge . 0)
          (top-edge . 0)
          (left-edge . -1)
          (class . ignore-border))
         ((bottom-edge . -2)
          (background . "border-pixel")
          (height . 2)
          (right-edge . 14)
          (left-edge . -1)
          (class . ignore-border))
         ((background . "border-pixel")
          (width . 2)
          (bottom-edge . 14)
          (top-edge . 0)
          (right-edge . -2)
          (class . ignore-border))
         ((background . "resize-button")
          (bottom-edge . -2)
          (right-edge . -2)
          (class . bottom-right-corner))
         ((background . "close-button")
          (top-edge . -16)
          (left-edge . 4)
          (class . close-button))
         ((background . "maximize-button")
          (top-edge . -16)
          (right-edge . 6)
          (class . maximize-button)))
	("transient"
	 ((left-edge . ,title-position)
	  (right-edge . ,title-position)
          (top-edge . -19)
          (foreground . "text-colors")
          (y-justify . center)
          (x-justify . center)
          (text . window-name)
          (background . "title")
          (class . title))
         ((background . "title-bar")
          (top-edge . -19)
          (width . ,(lambda (w) (+ (title-position w) 2)))
          (left-edge . -1)
          (class . title))
         ((background . "title-bar")
          (top-edge . -19)
          (width . ,(lambda (w) (+ (title-position w) 2)))
          (right-edge . -2)
          (class . title))
         ((background . "title-left")
          (width . 3)
          (top-edge . -19)
          (left-edge . -1)
          (class . title))
         ((background . "title-right")
          (width . 3)
          (top-edge . -19)
          (right-edge . -2)
          (class . title))
         ((background . "border-pixel")
          (width . 1)
          (bottom-edge . 0)
          (top-edge . 0)
          (left-edge . -1)
          (class . ignore-border))
         ((bottom-edge . -2)
          (background . "border-pixel")
          (height . 2)
          (right-edge . -1)
          (left-edge . -1)
          (class . ignore-border))
         ((background . "border-pixel")
          (width . 2)
          (bottom-edge . -1)
          (top-edge . 0)
          (right-edge . -2)
          (class . ignore-border))
	 )
	("shaded-transient"
	 ((left-edge . ,title-position)
	  (right-edge . ,title-position)
          (top-edge . -19)
          (foreground . "text-colors")
          (y-justify . center)
          (x-justify . center)
          (text . window-name)
          (background . "title")
          (class . title))
         ((background . "title-bar")
          (top-edge . -19)
          (width . ,(lambda (w) (+ (title-position w) 2)))
          (left-edge . -1)
          (class . title))
         ((background . "title-bar")
          (top-edge . -19)
          (width . ,(lambda (w) (+ (title-position w) 2)))
          (right-edge . -2)
          (class . title))
         ((background . "title-left")
          (width . 3)
          (top-edge . -19)
          (left-edge . -1)
          (class . title))
	 ((background . "title-right")
	  (width . 3)
	  (top-edge . -19)
	  (right-edge . -2)
	  (class . title))
	 )
	("frameless"
         ((background . "vertical")
          (left-edge . -4)
          (bottom-edge . 0)
          (top-edge . 0)
          (class . ignore-border))
         ((background . "vertical")
          (bottom-edge . 0)
          (top-edge . 0)
          (right-edge . -4)
          (class . ignore-border))
         ((background . "horizontal")
          (top-edge . -4)
          (right-edge . 0)
          (left-edge . 0)
          (class . ignore-border))
         ((background . "horizontal")
          (right-edge . 0)
          (left-edge . 0)
          (bottom-edge . -4)
          (class . ignore-border))
         ((background . "top-left")
          (top-edge . -4)
          (left-edge . -4)
          (class . ignore-border))
         ((left-edge . -4)
          (background . "bottom-left")
          (bottom-edge . -4)
          (class . ignore-border))
         ((background . "top-right")
          (top-edge . -4)
          (right-edge . -4)
          (class . ignore-border))
         ((background . "bottom-right")
          (bottom-edge . -4)
          (right-edge . -4)
          (class . ignore-border)))
        ("shaded"
	 ((left-edge . ,title-position)
	  (right-edge . ,title-position)
          (top-edge . -19)
          (foreground . "text-colors")
          (y-justify . center)
          (x-justify . center)
          (text . window-name)
          (background . "title")
          (class . title))
         ((background . "title-bar")
          (top-edge . -19)
          (width . ,(lambda (w) (+ (title-position w) 2)))
          (left-edge . -1)
          (class . title))
         ((background . "title-bar")
          (top-edge . -19)
          (width . ,(lambda (w) (+ (title-position w) 2)))
          (right-edge . -2)
          (class . title))
         ((background . "title-left")
          (width . 3)
          (top-edge . -19)
          (left-edge . -1)
          (class . title))
         ((background . "title-right")
          (width . 3)
          (top-edge . -19)
          (right-edge . -2)
          (class . title))
         ((left-edge . 4)
          (top-edge . -16)
          (background . "close-button")
          (class . close-button))
         ((background . "maximize-button")
          (top-edge . -16)
          (right-edge . 6)
          (class . maximize-button)))))

     (mapping-alist
      '((default . "default")
        (shaded . "shaded")
	(transient . "transient")
	(unframed . "frameless") ; This doesn't actually work :(
        (shaded-transient . "shaded-transient")
	(shaped . "shaded")
	(shaped-transient . "frameless")
	))

     (theme-name 'MacOS))

  (add-frame-style
   theme-name (make-theme patterns-alist frames-alist mapping-alist))
  (when (boundp 'mark-frame-style-editable)
    (mark-frame-style-editable theme-name))

  (call-after-property-changed 'WM_NAME update-title-width)
  )
