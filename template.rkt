;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname template) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Integrantes: ... (Nombre y Num. Legajo).

(require "extras.rkt")

;; Definiciones principales:
;; ========================
(define LABERINTO (bitmap "laberinto.png"))
(define FANTASMA (bitmap "fantasma.png"))
(define ALTO (image-height LABERINTO))
(define ANCHO (image-width LABERINTO))
(define CENTRO (make-posn (/ ANCHO 2) (/ ALTO 2)))
(define POS-INICIAL (make-posn 15 35))
(define POS-OBJETIVO (make-posn 930 460))

;; Estado global del programa:
;; ==========================
;; (define-struct estado ....)

;; actualizar-timer : Number Estado -> Estado
;; ..................................

;; actualizar-vidas : Number Estado -> Estado
;; ..................................

;; actualizar-fantasma : Posn Estado -> Estado
;; ..................................

;; actualizar-jugador : Posn Estado -> Estado
;; ..................................

;; Imprimir en la pantalla:
;; =======================
;; dibujar-imagen : Imagen Posn Imagen -> Imagen
;; ..................................

;; dibujar-laberinto : Imagen -> Imagen
;; ..................................

;; dibujar-jugador : Estado Imagen -> Imagen
;; ..................................

;; dibujar-objetivo : Imagen -> Imagen
;; ..................................

;; dibujar-timer : Estado Imagen -> Imagen
;; ..................................

;; dibujar-vidas : Estado Imagen -> Imagen
;; ..................................

;; dibujar-texto : String Imagen -> Imagen
;; ..................................

;; dibujar-fantasma : Estado Imagen -> Imagen
;; ..................................

;; fondo : Color -> Imagen
;; ..................................

;; imprimir : Estado Color -> Imagen
;; ..................................

;; grafica : Estado -> Imagen
;; ..................................

;; Mover objetos:
;; =============
;; move-up : Posn Number -> Posn
;; ..................................

;; move-down : Posn Number -> Posn
;; ..................................

;; move-left : Posn Number -> Posn
;; ..................................

;; move-right : Posn Number -> Posn
;; ..................................

;; mover-fantasma : Posn -> Posn
;; ..................................

;; Posiciones:
;; ==========
;; dentro-escena? : Posn Imagen -> Boolean
;; ..................................

;; posible-pos? : Posn Imagen -> Boolean
;; ..................................

;; Eventos del teclado:
;; ===================
;; manejador-tecla : Estado Tecla -> Estado
;; ..................................

;; Otros handlers:
;; ==============
;; manejador-tick : Estado -> Estado
;; ..................................

;; Condiciones de Terminación:
;; ==========================
;; interseca-fantasma? : Estado -> Boolean
;; ..................................

;; objetivo? : Estado -> Boolean
;; ..................................

;; fin? : Estado -> Boolean
;; ..................................


;; estado-inicial : Estado
;; ..................................

;; (big-bang estado-inicial
;;     [to-draw grafica]
;;     [on-key manejador-tecla]
;;     [on-tick manejador-tick]
;;     [stop-when fin?])
