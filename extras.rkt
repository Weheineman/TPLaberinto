#lang racket/base

(provide interseca?)

(require (except-in 2htdp/image image?))
(require lang/htdp-beginner)

;; image-area : Image -> Number
;; Calcula el area de una imagen.
(check-expect (image-area (rectangle 20 40 "solid" "red")) 800)
(check-expect (image-area (circle 15 "solid" "blue")) 900)
(define (image-area img) (* (image-width img) (image-height img)))

;; poner-imagen : Image Posn Image -> Image
;; Ubica la primera imagen en la posición dada sobre la segunda imagen.
(define (poner-imagen img p scene) (place-image img (posn-x p) (posn-y p) scene))

;; posn-sum : Posn Posn -> Posn
;; Suma dos posiciones.
(check-expect (posn-sum (make-posn 1 2) (make-posn 3 4)) (make-posn 4 6))
(define (posn-sum p1 p2) (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

;; posn-neg : Posn -> Posn
;; Obtiene la posición opuesta a la dada.
(check-expect (posn-neg (make-posn 1 2)) (make-posn -1 -2))
(define (posn-neg p) (make-posn (- (posn-x p)) (- (posn-y p))))

;; empty-img : Image -> Image
;; Obtiene una imagen de iguales dimensiones a la dada, pero de fondo transparente.
(define (empty-img img) (rectangle (image-width img) (image-height img) "solid" "trasparent"))

;; center : Image -> Posn
;; Obtiene el centro de una imagen.
(check-expect (center (circle 15 "solid" "red")) (make-posn 15 15))
(define (center img) (make-posn (/ (image-width img) 2) (/ (image-height img) 2)))

;; rel-pos : Posn Posn Image -> Posn
;; Dada la posición p2, y la posición p1 que representa la ubicación del centro de la imagen img1,
;; calcula la posición relativa de p2 respecto al extremo top left de la imagen img1.
(check-expect (rel-pos (make-posn 40 20) (make-posn 10 30) (rectangle 20 20 "solid" "red")) (make-posn 40 0))
(define (rel-pos p2 p1 img1) (posn-sum (posn-neg p1) (posn-sum (center img1) p2)))

;; interseca-aux? : Posn Image Image -> Boolean
;; Determina si cambia el resultado al dibujar primero la imagen base y luego la imagen img sobre la posición p, y viceversa.
(check-expect (interseca-aux? (make-posn 10 10) (circle 10 "solid" "blue") (rectangle 20 20 "solid" "red")) #true)
(check-expect (interseca-aux? (make-posn 30 10) (circle 10 "solid" "blue") (rectangle 20 20 "solid" "red")) #false)
(define (interseca-aux? p img base) (not (image=? (poner-imagen base (center base) (poner-imagen img p (empty-img base)))
                                                  (poner-imagen img p (poner-imagen base (center base) (empty-img base))))))

;; interseca?: Posn Image Posn Image -> Boolean
;; Determina si img1 en la posición p1 se interseca con img2 en la posición p2. Al decir "interseca" nos referimos a si
;; cambia el resultado de acuerdo al orden en que son dibujadas las imágenes. Si comparten el mismo color, por más que
;; estén ubicadas una sobre la otra, el resultado no cambiará.
(check-expect (interseca? (make-posn 10 10) (circle 10 "solid" "blue") (make-posn 10 10) (rectangle 20 20 "solid" "red")) #true)
(check-expect (interseca? (make-posn 30 10) (circle 10 "solid" "blue") (make-posn 10 10) (rectangle 20 20 "solid" "red")) #false)
(define (interseca? p1 img1 p2 img2) (if (< (image-area img1) (image-area img2))
                                           (interseca-aux? (rel-pos p2 p1 img1) img2 img1)
                                           (interseca-aux? (rel-pos p1 p2 img2) img1 img2)))
