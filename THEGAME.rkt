;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname THEGAME) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(require "extras.rkt")
(require racket/base)

;; Definiciones principales:
;; ========================
(define LABERINTO (bitmap "laberinto.png"))
(define FANTASMA (bitmap "fantasma.png"))
;(define JUGADOR (bitmap "dorita.png"))
(define JUGADOR (circle 10 "solid" "gray"))
(define OBJETIVO (circle 10 "solid" "blue"))
(define ALTO (image-height LABERINTO))
(define ANCHO (image-width LABERINTO))
(define CENTRO (make-posn (/ ANCHO 2) (/ ALTO 2)))
(define TIEMPO-INICIAL 1500)
(define POS-INICIAL (make-posn 15 35))
(define POS-OBJETIVO (make-posn 930 460))
(define POS-TIEMPO (make-posn (- ANCHO 50) (- ALTO 8)))
(define VIDAS-INICIAL 3)
(define COLOR-INICIAL "white")
(define TEXTO-FUENTE 72)
(define TEXTO-COLOR "black")
(define DELTA 5)

;; Estado global del programa:
;; ==========================
; state se compone de dos posn: las posiciones en pantalla del jugador y el enemigo respectivamente, y de un int que representa la cant. de ticks desde que comenzó el juego.
 (define-struct estado [jugador fantasma vidas tiempo])




;; actualizar-tiempo : Number Estado -> Estado
(define (actualizar-tiempo n e)
  (struct-copy estado e [tiempo n]))

;; actualizar-vidas : Number Estado -> Estado
;; ..................................

;; actualizar-fantasma : Posn Estado -> Estado
;; ..................................

;; actualizar-jugador : Posn Estado -> Estado
(define (actualizar-jugador p e)
  (struct-copy estado e [jugador p]))

;; Imprimir en la pantalla:
;; =======================
;; dibujar-imagen : Imagen Posn Imagen -> Imagen
(define (dibujar-imagen img pos fondo) (place-image img (posn-x pos) (posn-y pos) fondo ))

;; dibujar-laberinto : Imagen -> Imagen
(define (dibujar-laberinto fondo) (dibujar-imagen LABERINTO CENTRO fondo))

;; dibujar-jugador : Estado Imagen -> Imagen
(define (dibujar-jugador e fondo) (dibujar-imagen JUGADOR (estado-jugador e) fondo))

;; dibujar-objetivo : Imagen -> Imagen
(define (dibujar-objetivo fondo) (dibujar-imagen OBJETIVO POS-OBJETIVO fondo))

;; dibujar-tiempo : Estado Imagen -> Imagen
(define (dibujar-tiempo e fondo) (dibujar-imagen (text (string-append "Tiempo: " (number->string (estado-tiempo e))) 16 "indigo") POS-TIEMPO fondo))

;; dibujar-vidas : Estado Imagen -> Imagen
;; ..................................

;; dibujar-texto : String Imagen -> Imagen
(define (dibujar-texto s fondo) (dibujar-imagen (text s TEXTO-FUENTE TEXTO-COLOR) CENTRO fondo))

;; dibujar-fantasma : Estado Imagen -> Imagen
;; ..................................

;; fondo : Color -> Imagen
(define (fondo color) (empty-scene ANCHO ALTO color))

;; imprimir : Estado Color -> Imagen
(define (imprimir e color)
    (dibujar-tiempo
        e
        (dibujar-jugador
            e
            (dibujar-objetivo
                (dibujar-laberinto
                    (fondo
                    color))))))

;; grafica : Estado -> Imagen
(define (grafica e)
  (cond
    [(objetivo? e) (dibujar-texto "GANASTE!" (imprimir e "gold") )]
    [(fin? e) (dibujar-texto "PERDISTE!" (imprimir e "purple") )]
    [(< (estado-tiempo e) 500) (imprimir e "red")]
    [else (imprimir e COLOR-INICIAL)]))
     

;; Mover objetos:
;; =============
;; move-up : Posn Number -> Posn
(define (move-up pos dist) (if (posible-pos? (make-posn (posn-x pos) (- (posn-y pos) dist)) JUGADOR) (make-posn (posn-x pos) (- (posn-y pos) dist)) pos))

;; move-down : Posn Number -> Posn
(define (move-down pos dist) (if (posible-pos? (make-posn (posn-x pos) (+ (posn-y pos) dist)) JUGADOR) (make-posn (posn-x pos) (+ (posn-y pos) dist)) pos))

;; move-left : Posn Number -> Posn
(define (move-left pos dist) (if (posible-pos? (make-posn (- (posn-x pos) dist) (posn-y pos)) JUGADOR) (make-posn (- (posn-x pos) dist) (posn-y pos)) pos))

;; move-right : Posn Number -> Posn
(define (move-right pos dist) (if (posible-pos? (make-posn (+ (posn-x pos) dist) (posn-y pos)) JUGADOR) (make-posn (+ (posn-x pos) dist) (posn-y pos)) pos))

;; mover-fantasma : Posn -> Posn
;; ..................................

;; Posiciones:
;; ==========

(define (entre3 a b c) (and (<= a b) (<= b c)))

(define (semiancho img) (/ (image-width img) 2))
(define (semialto img) (/ (image-height img) 2))
  
;; dentro-escena? : Posn Imagen -> Boolean
(define (dentro-escena? pos img) (and (entre3 (semiancho img) (posn-x pos) (- ANCHO (semiancho img))) (entre3 (semialto img) (posn-y pos) (- ALTO (semialto img)))) )


;; posible-pos? : Posn Imagen -> Boolean
(define (posible-pos? pos img) (and (dentro-escena? pos img) (not (interseca? pos img CENTRO LABERINTO)) ) )

;; Eventos del teclado:
;; ===================
;; manejador-tecla : Estado Tecla -> Estado
(define (manejador-tecla e k) (cond
                                [(key=? k "up") (actualizar-jugador (move-up (estado-jugador e) DELTA) e)]
                                [(key=? k "down") (actualizar-jugador (move-down (estado-jugador e) DELTA) e)]
                                [(key=? k "left") (actualizar-jugador (move-left (estado-jugador e) DELTA) e)]
                                [(key=? k "right") (actualizar-jugador (move-right (estado-jugador e) DELTA) e)]
                                [else e]
                                )
  )

;; Otros handlers:
;; ==============

;; manejador-tick : Estado -> Estado
(define (manejador-tick e) (actualizar-tiempo (- (estado-tiempo e) 1) e))

;; Condiciones de Terminación:
;; ==========================
;; interseca-fantasma? : Estado -> Boolean
;; ..................................

;; objetivo? : Estado -> Boolean
(define (objetivo? e) (interseca? (estado-jugador e) JUGADOR POS-OBJETIVO OBJETIVO))

;; fin? : Estado -> Boolean
(define (fin? e ) (or (objetivo? e) (<= (estado-tiempo e) 0)))


;; estado-inicial : Estado
(define estado-inicial (make-estado POS-INICIAL POS-OBJETIVO VIDAS-INICIAL TIEMPO-INICIAL) ) 

(big-bang estado-inicial
     [to-draw grafica]
     [on-key manejador-tecla]
     [on-tick manejador-tick]
     [stop-when fin?])
