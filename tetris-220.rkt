;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris-220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101
; Assignment 5
; tetris-220.rkt
; Solution
; Marc Smith

(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define WIDTH 10)     ; # of blocks, horizontally 
(define HEIGHT WIDTH) ; the maximal number of blocks vertically
;where did we define the height

; graphical constants 
(define SIZE 40)      ; blocks are square 
(define BLOCK         ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black"))) 

(define SCENE-SIZE (* WIDTH SIZE))
(define MT-SCENE
  (overlay (rectangle (- SCENE-SIZE 1) (- SCENE-SIZE 1) "solid" "white")
           (rectangle SCENE-SIZE SCENE-SIZE "solid" "black")))

; data and structure definitions:

; A Tetris is a structure:
;   (make-tetris Block Landscape)
(define-struct tetris [block landscape])

; A Landscape is one of: a list of blocks that have landed
; – '() 
; – (cons Block Landscape)

; A Block is a structure:
;   (make-block N N)
(define-struct block [x y])
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

; Data examples
(define landscape0 '())
(define block-0-0 (make-block 0 0))
(define block-0-1 (make-block 0 1))
(define block-dropping block-0-0)
(define tetris0 (make-tetris block-dropping landscape0))
(define tetris0-drop (make-tetris block-0-1 landscape0))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape1 (list block-landed))
(define landscape2 (list block-on-block block-landed))
(define tetris1 (make-tetris block-dropping landscape2))

; Functions

; Block -> number
; returns physical x-coordinate of given block
(define (physical-x b)
  (+ (/ SIZE 2) (* SIZE (block-x b))))

(check-expect (physical-x block-0-0) (+ (/ SIZE 2) 0)) 
(check-expect (physical-x (make-block 9 0)) (+ (/ SIZE 2) (* SIZE 9)))

; Block -> number
; returns physical y-coordinate of given block
(define (physical-y b)
  (+ (/ SIZE 2) (* SIZE (block-y b))))

(check-expect (physical-y block-0-0) (+ (/ SIZE 2) 0))
(check-expect (physical-y (make-block 0 9)) (+ (/ SIZE 2) (* SIZE 9)))

; Landscape -> image
; renders image of given Landscape
(define (blocks-render blocks) 
  (cond
    [(empty? blocks) MT-SCENE]
    [(cons? blocks) (place-image BLOCK
                                 (physical-x (first blocks)) 
                                 (physical-y (first blocks)) 
                                 (blocks-render (rest blocks)))]));calling blocks-render recursively on the rest of the list

(check-expect (blocks-render '()) MT-SCENE)
(check-expect (blocks-render landscape1)
              (place-image BLOCK
                           (physical-x block-landed)
                           (physical-y block-landed)
                           MT-SCENE))

; Tetris;state of the world -> image
; renders image of given Tetris game
(define (tetris-render t)
  (blocks-render (cons (tetris-block t) (tetris-landscape t))))

; Examples
(tetris-render tetris0)
(tetris-render tetris0-drop)
(tetris-render tetris1)

(check-expect (tetris-render tetris0)
              (place-image BLOCK
                           (physical-x block-dropping)
                           (physical-y block-dropping)
                           MT-SCENE))
(check-expect (tetris-render tetris0-drop)
              (place-image BLOCK
                           (physical-x block-0-1)
                           (physical-y block-0-1)
                           MT-SCENE))
(check-expect (tetris-render tetris1)
              (place-image BLOCK
                           (physical-x block-dropping)
                           (physical-y block-dropping)
                           (blocks-render (tetris-landscape tetris1))))
