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
(define OBJETIVO (circle 10 "solid" "blue")) ;imagen que representa el objetivo
(define ALTO (image-height LABERINTO)) ;altura de la ventana
(define ANCHO (image-width LABERINTO)) ;ancho de la ventana
(define CENTRO (make-posn (/ ANCHO 2) (/ ALTO 2))); centro de la pantalla
(define TIEMPO-INICIAL 1500) ;tiempo que tiene el jugador llegar al objetivo
(define POS-INICIAL (make-posn 15 35)) ;posición inicial del jugador
(define POS-OBJETIVO (make-posn 930 460)) ;posición del objetivo
(define POS-TIEMPO (make-posn (- ANCHO 50) (- ALTO 8)));posición del texto que indica el tiempo restante
(define POS-VIDAS (make-posn 50 (- ALTO 8)));posición del texto que indica las vidas restantes
(define VIDAS-INICIAL 3) ;cantidad de vidas que tiene el jugador al comenzar el juego
(define COLOR-INICIAL "white") ;color del fondo al comienzo del juego
(define TEXTO-FUENTE 72) ;tamaño del texto de los mensajes
(define TEXTO-COLOR "black") ;color del texto de los mensajes
(define DELTA-PLAYER 10) ;cantidad de píxeles que se mueve el jugador por tick
(define DELTA-FANTASMA 3) ;cantidad de píxeles que se mueve el fantasma por tick


;; Estado global del programa:
;; ==========================
; state se compone de dos posn: las posiciones en pantalla del jugador y el enemigo respectivamente, y de un int que representa la cant. de ticks desde que comenzó el juego.
 (define-struct estado [jugador fantasma vidas tiempo])




;; actualizar-tiempo : Number Estado -> Estado
;reemplaza el tiempo del estado por el número recibido
(define (actualizar-tiempo n e)
  (struct-copy estado e [tiempo n]))

;; actualizar-vidas : Number Estado -> Estado
;cambia el numero de vidas del estado por el número recibido
(define (actualizar-vidas n e)
  (struct-copy estado e [vidas n]))

(check-expect (actualizar-vidas 0 (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) ) (make-estado (make-posn 10 10) (make-posn 23 20) 0 420 ) )
(check-expect (make-estado POS-INICIAL POS-OBJETIVO 0 100) (make-estado POS-INICIAL POS-OBJETIVO 0 100))

;; actualizar-fantasma : Posn Estado -> Estado
;reemplaza la posición del fantasma del estado por el número recibido
(define (actualizar-fantasma n e)
  (struct-copy estado e [fantasma n]))

;; actualizar-jugador : Posn Estado -> Estado
;reemplaza la posición del jugador por la posición recibida
(define (actualizar-jugador p e)
  (struct-copy estado e [jugador p]))

;; Imprimir en la pantalla:
;; =======================
;; dibujar-imagen : Imagen Posn Imagen -> Imagen
;dibuja la primara imagen sobre la segunda, en la posición indicada
(define (dibujar-imagen img pos fondo) (place-image img (posn-x pos) (posn-y pos) fondo ))

;; dibujar-laberinto : Imagen -> Imagen
;dibuja el laberinto sobre un fondo
(define (dibujar-laberinto fondo) (dibujar-imagen LABERINTO CENTRO fondo))

;; dibujar-jugador : Estado Imagen -> Imagen
;dibuja al jugador sobre la imagen en su posición actual
(define (dibujar-jugador e fondo) (dibujar-imagen JUGADOR (estado-jugador e) fondo))

;; dibujar-objetivo : Imagen -> Imagen
;dibuja el objetivo sobre la imagen en su posición
(define (dibujar-objetivo fondo) (dibujar-imagen OBJETIVO POS-OBJETIVO fondo))

;; dibujar-tiempo : Estado Imagen -> Imagen
;dibuja el timer sobre la imagen
(define (dibujar-tiempo e fondo) (dibujar-imagen (text (string-append "Tiempo: " (number->string (estado-tiempo e))) 16 "indigo") POS-TIEMPO fondo))

;; dibujar-vidas : Estado Imagen -> Imagen
;dibuja la cantidad de vidas del jugador sobre la imagen
(define (dibujar-vidas e fondo) (dibujar-imagen (text (string-append "Vidas: " (number->string (estado-vidas e))) 16 "indigo") POS-VIDAS fondo) )

;; dibujar-texto : String Imagen -> Imagen
;imprime el string recibido en el centro de la imagen
(define (dibujar-texto s fondo) (dibujar-imagen (text s TEXTO-FUENTE TEXTO-COLOR) CENTRO fondo))

;; dibujar-fantasma : Estado Imagen -> Imagen
(define (dibujar-fantasma e fondo) (dibujar-imagen FANTASMA (estado-fantasma e) fondo) )

;; fondo : Color -> Imagen
;crea un fondo con el color indicado
(define (fondo color) (empty-scene ANCHO ALTO color))

;; imprimir : Estado Color -> Imagen
;dibuja los componentes del juego sobre un fondo del color indicado
(define (imprimir e color)
  (dibujar-vidas
   e
   (dibujar-tiempo
    e
    (dibujar-fantasma
     e
     (dibujar-jugador
      e
      (dibujar-objetivo
       (dibujar-laberinto
        (fondo
         color))))))))

;; grafica : Estado -> Imagen
;crea la imagen que se muestra en pantalla en base al estado actual del juego
(define (grafica e)
  (cond
    [(objetivo? e) (dibujar-texto "GANASTE!" (imprimir e "gold") )]
    [(fin? e) (dibujar-texto "PERDISTE!" (imprimir e "purple") )]
    [(< (estado-tiempo e) 500) (imprimir e "red")]
    [else (imprimir e COLOR-INICIAL)]))
     

;; Mover objetos:
;; =============
;; move-up : Posn Number -> Posn
;si es posible, mueve al jugador hacia arriba dist píxeles
(define (move-up pos dist) (if (posible-pos? (make-posn (posn-x pos) (- (posn-y pos) dist)) JUGADOR) (make-posn (posn-x pos) (- (posn-y pos) dist)) pos))

;; move-down : Posn Number -> Posn
;si es posible, mueve al jugador hacia abajo dist píxeles
(define (move-down pos dist) (if (posible-pos? (make-posn (posn-x pos) (+ (posn-y pos) dist)) JUGADOR) (make-posn (posn-x pos) (+ (posn-y pos) dist)) pos))

;; move-left : Posn Number -> Posn
;si es posible, mueve al jugador hacia la izquierda dist píxeles
(define (move-left pos dist) (if (posible-pos? (make-posn (- (posn-x pos) dist) (posn-y pos)) JUGADOR) (make-posn (- (posn-x pos) dist) (posn-y pos)) pos))

;; move-right : Posn Number -> Posn
;si es posible, mueve al jugador hacia la derecha dist píxeles
(define (move-right pos dist) (if (posible-pos? (make-posn (+ (posn-x pos) dist) (posn-y pos)) JUGADOR) (make-posn (+ (posn-x pos) dist) (posn-y pos)) pos))

;; mover-fantasma : Posn -> Posn
(define (mover-fantasma pos) pos)

;; Posiciones:
;; ==========

;; entre3? : Number Number Number -> Bool
;determina si los números están en orden creciente
(define (entre3? a b c) (and (<= a b) (<= b c)))

;; semiancho: Imagen -> Number
;devuelve la mitad del ancho de una imagen
(define (semiancho img) (/ (image-width img) 2))

;; semialto: Imagen -> Number
;devuelve la mitad del alto de una imagen
(define (semialto img) (/ (image-height img) 2))
  
;; dentro-escena? : Posn Imagen -> Boolean
;determina si una imagen está dentro de otra
(define (dentro-escena? pos img) (and (entre3? (semiancho img) (posn-x pos) (- ANCHO (semiancho img))) (entre3? (semialto img) (posn-y pos) (- ALTO (semialto img)))) )


;; posible-pos? : Posn Imagen -> Boolean
;determina si una posición (pos) de una figura (de imagen img) es posible (si está en escena y no se interseca con las paredes del laberinto)
(define (posible-pos? pos img) (and (dentro-escena? pos img) (not (interseca? pos img CENTRO LABERINTO)) ) )

;; Eventos del teclado:
;; ===================
;; manejador-tecla : Estado Tecla -> Estado
;mueve al jugador en la dirección deseada cuando éste presiona una flecha en el teclado (siempre y cuando esté permitido ese movimiento)
(define (manejador-tecla e k) (cond
                                [(key=? k "up") (actualizar-jugador (move-up (estado-jugador e) DELTA-PLAYER) e)]
                                [(key=? k "down") (actualizar-jugador (move-down (estado-jugador e) DELTA-PLAYER) e)]
                                [(key=? k "left") (actualizar-jugador (move-left (estado-jugador e) DELTA-PLAYER) e)]
                                [(key=? k "right") (actualizar-jugador (move-right (estado-jugador e) DELTA-PLAYER) e)]
                                [else e]
                                )
  )

;; Otros handlers:
;; ==============

;; manejador-tick : Estado -> Estado
;ejecuta las tres acciones que ocurren por tick: la actualización del tiempo, el movimiento del fantasma y la comprobación de si el fantasma atrapó al jugador
(define (manejador-tick e) (actualizar-tiempo
                            (- (estado-tiempo (actualizar-fantasma
                                               (mover-fantasma (estado-fantasma
                                                (if (interseca-fantasma? e) (actualizar-vidas (- (estado-vidas e) 1) e) e))) e))
                                               1) e))

;; Condiciones de Terminación:
;; ==========================
;; interseca-fantasma? : Estado -> Boolean
;comprueba si el fantasma atrapó al jugador (si se tocan)
(define (interseca-fantasma? e) (interseca? (estado-jugador e) JUGADOR (estado-fantasma e) FANTASMA))

(check-expect (interseca-fantasma? (make-estado POS-INICIAL POS-INICIAL 3 100)) #t)

;; objetivo? : Estado -> Boolean
;comprueba si el jugador llegó al objetivo
(define (objetivo? e) (interseca? (estado-jugador e) JUGADOR POS-OBJETIVO OBJETIVO))


;; fin? : Estado -> Boolean
;comprueba si el juego terminó: si el jugador alcanzó el objetivo, si se quedó sin vidas o si se quedó sin tiempo
(define (fin? e ) (or (objetivo? e) (<= (estado-tiempo e) 0) (= (estado-vidas e) 0)))


;; estado-inicial : Estado
(define estado-inicial (make-estado POS-INICIAL POS-INICIAL VIDAS-INICIAL TIEMPO-INICIAL) ) 

(big-bang estado-inicial
     [to-draw grafica]
     [on-key manejador-tecla]
     [on-tick manejador-tick]
     [stop-when fin?])
