;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname THEGAME) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; * * * * * * * * * * * * * * * * * * * * * * * *
;; *           Programación I 2016               *
;; * Licenciatura en Ciencias de la Computación  *
;; *                                             *
;; *          Trabajo Práctico n. 1              *
;; * * * * * * * * * * * * * * * * * * * * * * * *

;; Román Castellarin, C-6532/3
;; Gianni Weinand, W-0528/2

;; ============================================= ;;

(require "extras.rkt")
(require racket/base)

;; Definiciones principales:
;; ========================

; Convenciones:
; - Las distancias y las fuentes están medidas en píxeles.
; - Los intervalos de tiempo están medidas en ticks.
; - Las coordenadas se miden desde la esquina superior izquierda hacia la inferior derecha de la ventana.

; Definiciones asociadas a la ventana de visualización:
(define LABERINTO (bitmap "laberinto.png"))             ;imagen del laberinto.
(define ALTO (image-height LABERINTO))                  ;altura de la ventana.
(define ANCHO (image-width LABERINTO))                  ;ancho de la ventana.
(define CENTRO (make-posn (/ ANCHO 2) (/ ALTO 2)))      ;centro de la pantalla.
(define POS-TIEMPO (make-posn (- ANCHO 50) (- ALTO 8))) ;posición del texto que indica el tiempo restante.
(define POS-VIDAS (make-posn 50 (- ALTO 8)))            ;posición del texto que indica las vidas restantes.
(define COLOR-INICIAL "white")                          ;color del fondo al comienzo del juego.
(define TEXTO-FUENTE 72)                                ;tamaño del texto de los mensajes de finalización del juego.
(define TEXTO-COLOR "white")                            ;color del texto de los mensajes de finalización del juego.

; Definiciones asociadas a los personajes:
(define FANTASMA (bitmap "fantasma.png"))               ;imagen del fantasma.
(define JUGADOR (bitmap "personaje.png"))               ;imagen que representa al jugador.
(define OBJETIVO (bitmap "cherry.png"))                 ;imagen que representa el objetivo.
(define POS-INICIAL (make-posn 15 35))                  ;posición inicial del jugador.
(define POS-FANTASMA (make-posn ANCHO ALTO))            ;posición inicial del enemigo (sólo sirve para forzar al motor a generar una aleatoria inmediatamente).
(define POS-OBJETIVO (make-posn 930 460))               ;posición del objetivo, fija.
(define DELTA-PLAYER 10)                                ;cantidad de píxeles que se mueve el jugador por tick.
(define DELTA-FANTASMA 5)                               ;cantidad de píxeles que se mueve el fantasma por tick.

; Definiciones asociadas al motor del juego:
(define TIEMPO-INICIAL 1500)                            ;tiempo que tiene el jugador llegar al objetivo.
(define VIDAS-INICIAL 3)                                ;cantidad de vidas que tiene el jugador al comenzar el juego.

;; Estado global del programa:
;; ==========================
; jugador es un posn que representa la posición del jugador en escena.
; fantasma es un posn que representa la posición del fantasma en escena.
; vidas es un Number que contiene la cantidad de intentos restantes del jugador.
; tiempo es un Number que contiene la cantidad de ticks de tiempo que tiene el jugador para lograr su objetivo.
 (define-struct estado [jugador fantasma vidas tiempo] #:transparent)


;; actualizar-tiempo : Number Estado -> Estado
; Devuelve un estado igual al que recibe, pero actualizando el campo tiempo por el Number recibido.
(define (actualizar-tiempo n e)
  (struct-copy estado e [tiempo n]))
; Ejemplos:
(check-expect (actualizar-tiempo 0 (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) ) (make-estado (make-posn 10 10) (make-posn 23 20) 2 0) )
(check-expect (actualizar-tiempo 10 (make-estado POS-TIEMPO POS-INICIAL 19 97) ) (make-estado POS-TIEMPO POS-INICIAL 19 10) )
(check-expect (actualizar-tiempo -99 (make-estado (make-posn 0 0) (make-posn 42 0) 999 0) ) (make-estado (make-posn 0 0) (make-posn 42 0) 999 -99) )

;; actualizar-vidas : Number Estado -> Estado
; Devuelve un estado igual al que recibe, pero actualizando el campo vidas por el Number recibido.
(define (actualizar-vidas n e)
  (struct-copy estado e [vidas n]))
; Ejemplos:
(check-expect (actualizar-vidas 0 (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) ) (make-estado (make-posn 10 10) (make-posn 23 20) 0 420 ) )
(check-expect (actualizar-vidas 3 (make-estado POS-TIEMPO POS-INICIAL 19 97) ) (make-estado POS-TIEMPO POS-INICIAL 3 97) )
(check-expect (actualizar-vidas -5 (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) ) (make-estado (make-posn 10 10) (make-posn 23 20) -5 420 ) )

;; actualizar-fantasma : Posn Estado -> Estado
; Devuelve un estado igual al que recibe, pero actualizando el campo fantasma por el posn recibido.
(define (actualizar-fantasma n e)
  (struct-copy estado e [fantasma n]))
; Ejemplos:
(check-expect (actualizar-fantasma POS-INICIAL (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) ) (make-estado (make-posn 10 10) POS-INICIAL 2 420 ) )
(check-expect (actualizar-fantasma POS-OBJETIVO (make-estado POS-TIEMPO POS-INICIAL 19 97) ) (make-estado POS-TIEMPO POS-OBJETIVO 19 97) )
(check-expect (actualizar-fantasma (make-posn 50 80) (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) ) (make-estado (make-posn 10 10) (make-posn 50 80) 2 420 ) )

;; actualizar-jugador : Posn Estado -> Estado
; Devuelve un estado igual al que recibe, pero actualizando el campo jugador por el posn recibido.
(define (actualizar-jugador p e)
  (struct-copy estado e [jugador p]))
; Ejemplos:
(check-expect (actualizar-jugador POS-INICIAL (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) ) (make-estado POS-INICIAL (make-posn 23 20) 2 420 ) )
(check-expect (actualizar-jugador POS-OBJETIVO (make-estado POS-TIEMPO POS-INICIAL 19 97) ) (make-estado POS-OBJETIVO POS-INICIAL 19 97) )
(check-expect (actualizar-jugador (make-posn 50 80) (make-estado (make-posn 0 0) (make-posn 42 0) 999 0) ) (make-estado (make-posn 50 80) (make-posn 42 0) 999 0) )


;; Imprimir en la pantalla:
;; =======================
;; dibujar-imagen : Imagen Posn Imagen -> Imagen
; Toma una imagen, una posición y un fondo, y devuelve una imagen que se obtiene de dibujar la primara imagen sobre el fondo, en la posición indicada.
(define (dibujar-imagen img pos fondo) (place-image img (posn-x pos) (posn-y pos) fondo ))
; Ejemplos:
(check-expect (dibujar-imagen (circle 10 "solid" "blue") (make-posn 10 10) (rectangle 20 20 "solid" "red")) (place-image (circle 10 "solid" "blue") 10 10 (rectangle 20 20 "solid" "red")))
(check-expect (dibujar-imagen (circle 15 "solid" "red") (make-posn 2 1) (rectangle 25 78 "solid" "blue")) (place-image (circle 15 "solid" "red") 2 1 (rectangle 25 78 "solid" "blue")))
(check-expect (dibujar-imagen JUGADOR (make-posn 10 10) FANTASMA) (place-image JUGADOR 10 10 FANTASMA))


;; dibujar-laberinto : Imagen -> Imagen
; Toma un fondo y devuelve la imagen que se obtiene de dibujar el laberinto sobre el fondo.
(define (dibujar-laberinto fondo) (dibujar-imagen LABERINTO CENTRO fondo))
; Ejemplos:
(check-expect (dibujar-laberinto (rectangle ANCHO ALTO "solid" "black")) (place-image LABERINTO (/ ANCHO 2) (/ ALTO 2) (rectangle ANCHO ALTO "solid" "black")))
(check-expect (dibujar-laberinto (circle 1000 "solid" "blue")) (place-image LABERINTO (/ ANCHO 2) (/ ALTO 2) (circle 1000 "solid" "blue")) )
(check-expect (dibujar-laberinto FANTASMA) (place-image LABERINTO (posn-x CENTRO) (posn-y CENTRO) FANTASMA))

;; dibujar-jugador : Estado Imagen -> Imagen
; Toma un estado y un fondo, y devuelve la imagen que se obtiene de dibujar al jugador sobre el fondo en su posición actual.
(define (dibujar-jugador e fondo) (dibujar-imagen JUGADOR (estado-jugador e) fondo))
; Ejemplos:
(check-expect (dibujar-jugador (make-estado POS-TIEMPO POS-OBJETIVO 100 10) (rectangle ANCHO ALTO "solid" "black")) (place-image JUGADOR (posn-x POS-TIEMPO) (posn-y POS-TIEMPO) (rectangle ANCHO ALTO "solid" "black")))
(check-expect (dibujar-jugador (make-estado POS-OBJETIVO (make-posn 10 10) 100 10) LABERINTO) (place-image JUGADOR (posn-x POS-OBJETIVO) (posn-y POS-OBJETIVO) LABERINTO))
(check-expect (dibujar-jugador (make-estado (make-posn 15 25) (make-posn 12 13) 100 10) FANTASMA) (place-image JUGADOR 15 25 FANTASMA))

;; dibujar-objetivo : Imagen -> Imagen
; Toma un fondo y devuelve la imagen que se obtiene de dibujar el objetivo sobre el fondo en la posición predeterminada por POS-OBJETIVO.
(define (dibujar-objetivo fondo) (dibujar-imagen OBJETIVO POS-OBJETIVO fondo))
; Ejemplos:
(check-expect (dibujar-laberinto (rectangle ANCHO ALTO "solid" "black")) (place-image LABERINTO (/ ANCHO 2) (/ ALTO 2) (rectangle ANCHO ALTO "solid" "black")))
(check-expect (dibujar-laberinto (circle 1000 "solid" "blue")) (place-image LABERINTO (/ ANCHO 2) (/ ALTO 2) (circle 1000 "solid" "blue")) )
(check-expect (dibujar-laberinto FANTASMA) (place-image LABERINTO (posn-x CENTRO) (posn-y CENTRO) FANTASMA))

;; dibujar-tiempo : Estado Imagen -> Imagen
; Toma un estado y un fondo y devuelve la imagen que se obtiene de dibujar el tiempo restante en ticks, sobre el fondo, en la posición predeterminada por POS-TIEMPO.
(define (dibujar-tiempo e fondo) (dibujar-imagen (text (string-append "Tiempo: " (number->string (estado-tiempo e))) 16 "indigo") POS-TIEMPO fondo))
; Ejemplos:
(check-expect (dibujar-tiempo (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) LABERINTO) (place-image (text "Tiempo: 420" 16 "indigo") (posn-x POS-TIEMPO) (posn-y POS-TIEMPO) LABERINTO) )
(check-expect (dibujar-tiempo (make-estado POS-TIEMPO POS-INICIAL 19 97) JUGADOR) (place-image (text "Tiempo: 97" 16 "indigo")  (posn-x POS-TIEMPO) (posn-y POS-TIEMPO) JUGADOR) )
(check-expect (dibujar-tiempo (make-estado (make-posn 0 0) (make-posn 42 0) 999 0) (circle 200 "solid" "blue")) (place-image (text "Tiempo: 0" 16 "indigo") (posn-x POS-TIEMPO) (posn-y POS-TIEMPO) (circle 200 "solid" "blue")) )

;; dibujar-vidas : Estado Imagen -> Imagen
; Toma un estado y un fondo y devuelve la imagen que se obtiene de dibujar la cantidad de vidas del jugador sobre el fondo.
(define (dibujar-vidas e fondo) (dibujar-imagen (text (string-append "Vidas: " (number->string (estado-vidas e))) 16 "indigo") POS-VIDAS fondo) )
;Ejemplos:
(check-expect (dibujar-vidas (make-estado (make-posn 10 10) (make-posn 23 20) 2 420 ) LABERINTO) (place-image (text "Vidas: 2" 16 "indigo") (posn-x POS-VIDAS) (posn-y POS-VIDAS) LABERINTO) )
(check-expect (dibujar-vidas (make-estado POS-VIDAS POS-INICIAL 19 97) JUGADOR) (place-image (text "Vidas: 19" 16 "indigo")  (posn-x POS-VIDAS) (posn-y POS-VIDAS) JUGADOR) )
(check-expect (dibujar-vidas (make-estado (make-posn 0 0) (make-posn 42 0) 999 0) (circle 200 "solid" "blue")) (place-image (text "Vidas: 999" 16 "indigo") (posn-x POS-VIDAS) (posn-y POS-VIDAS) (circle 200 "solid" "blue")) )


;; dibujar-texto : String Imagen -> Imagen
; Toma un texto y un fondo y devuelve la imagen que se obtiene de imprimir el texto recibido centrado sobre el fondo.
(define (dibujar-texto s fondo) (dibujar-imagen (text s TEXTO-FUENTE TEXTO-COLOR) CENTRO fondo))
;Ejemplos:
(check-expect (dibujar-texto "Hello World!" LABERINTO) (place-image (text "Hello World!" TEXTO-FUENTE TEXTO-COLOR) (posn-x CENTRO) (posn-y CENTRO) LABERINTO))
(check-expect (dibujar-texto "Aguante C++ :)" JUGADOR) (place-image (text "Aguante C++ :)" TEXTO-FUENTE TEXTO-COLOR) (posn-x CENTRO) (posn-y CENTRO) JUGADOR))

;; dibujar-fantasma : Estado Imagen -> Imagen
; Toma un estado y un fondo y devuelve la imagen que se obtiene de dibujar al enemigo sobre el fondo en la posición indicada por el campo fantasma del estado.
(define (dibujar-fantasma e fondo) (dibujar-imagen FANTASMA (estado-fantasma e) fondo) )
;Ejemplos:
(check-expect (dibujar-fantasma (make-estado POS-TIEMPO POS-OBJETIVO 100 10) (rectangle ANCHO ALTO "solid" "black")) (place-image FANTASMA (posn-x POS-OBJETIVO) (posn-y POS-OBJETIVO) (rectangle ANCHO ALTO "solid" "black")))
(check-expect (dibujar-fantasma (make-estado POS-OBJETIVO (make-posn 10 10) 100 10) LABERINTO) (place-image FANTASMA 10 10 LABERINTO))
(check-expect (dibujar-fantasma (make-estado (make-posn 15 25) (make-posn 12 13) 100 10) FANTASMA) (place-image FANTASMA 12 13 FANTASMA))


;; fondo : Color -> Imagen
; Toma un color y devuelve un fondo del color indicado.
(define (fondo color) (empty-scene ANCHO ALTO color))
;Ejemplos:
(check-expect (fondo "blue") (empty-scene ANCHO ALTO "blue"))
(check-expect (fondo "red") (empty-scene ANCHO ALTO "red"))
(check-expect (fondo "pink") (empty-scene ANCHO ALTO "pink"))

;; imprimir : Estado Color -> Imagen
; Toma un estado y un color, y devuelve la imagen que se obtiene de dibujar los componentes del juego sobre un fondo del color indicado.
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


; grafica : Estado -> Imagen
; Toma un estado y devuelve la imagen que se muestra en pantalla en base al estado actual del juego.
; Las reglas para el color de fondo son las siguientes:
; - Al inicio es blanco.
; - Cuando quedan menos de 500 ticks es rojo.
; - Cuando se pierde, es púrpura.
; - Cuando se gana, es dorado.
; Además, al lograr el objetivo, se imprime el mensaje "GANASTE!", y al perder: GAME OVER!".
(define (grafica e)
  (cond
    [(objetivo? e) (dibujar-texto "GANASTE!" (imprimir e "gold") )]
    [(fin? e) (dibujar-texto "GAME OVER!" (imprimir e "purple") )]
    [(< (estado-tiempo e) 500) (imprimir e "red")]
    [else (imprimir e COLOR-INICIAL)]))
     

;; Mover objetos:
;; =============
;; move-up : Posn Number -> Posn
; Toma una posición pos y un desplazamiento dist y, si es posible, devuelve la posición que se obtiene de mover al jugador hacia arriba dist unidades, sino devuelve pos tal cual estaba.
(define (move-up pos dist) (if (posible-pos? (make-posn (posn-x pos) (- (posn-y pos) dist)) JUGADOR) (make-posn (posn-x pos) (- (posn-y pos) dist)) pos))

;; move-down : Posn Number -> Posn
; Toma una posición pos y un desplazamiento dist y, si es posible, devuelve la posición que se obtiene de mover al jugador hacia abajo dist unidades, sino devuelve pos tal cual estaba.
(define (move-down pos dist) (if (posible-pos? (make-posn (posn-x pos) (+ (posn-y pos) dist)) JUGADOR) (make-posn (posn-x pos) (+ (posn-y pos) dist)) pos))

;; move-left : Posn Number -> Posn
; Toma una posición pos y un desplazamiento dist y, si es posible, devuelve la posición que se obtiene de mover al jugador hacia la izquierda dist unidades, sino devuelve pos tal cual estaba.
(define (move-left pos dist) (if (posible-pos? (make-posn (- (posn-x pos) dist) (posn-y pos)) JUGADOR) (make-posn (- (posn-x pos) dist) (posn-y pos)) pos))

;; move-right : Posn Number -> Posn
; Toma una posición pos y un desplazamiento dist y, si es posible, devuelve la posición que se obtiene de mover al jugador hacia la derecha dist unidades, sino devuelve pos tal cual estaba.
(define (move-right pos dist) (if (posible-pos? (make-posn (+ (posn-x pos) dist) (posn-y pos)) JUGADOR) (make-posn (+ (posn-x pos) dist) (posn-y pos)) pos))

;; mover-fantasma : Posn -> Posn
; Toma una posición pos y un desplazamiento dist y, si es posible, devuelve la posición que se obtiene de mover al jugador hacia arriba dist unidades, sino devuelve pos tal cual estaba.
(define (mover-fantasma pos) (cond
                               [(> (posn-y pos) ALTO) (make-posn (random 1 ANCHO) 0)]
                               [else (make-posn (posn-x pos) (+ (posn-y pos) DELTA-FANTASMA))]
                               )
  )

;; reset-pos : Estado -> Estado
; Toma un estado y devuelve otro estado actualizando al jugador y al fantasma a sus posiciones originales y restándosele una vida al jugador.
(define (reset-pos e) (make-estado POS-INICIAL POS-FANTASMA (- (estado-vidas e) 1) (estado-tiempo e)))
;Ejemplos:
(check-expect (reset-pos (make-estado POS-INICIAL POS-INICIAL 3 1500)) (make-estado POS-INICIAL POS-FANTASMA 2 1500))
(check-expect (reset-pos (make-estado (make-posn 0 0) (make-posn 1000 1000) 1 1500)) (make-estado POS-INICIAL POS-FANTASMA 0 1500))
(check-expect (reset-pos (make-estado (make-posn 30 30) (make-posn 30 30) 30 1500)) (make-estado POS-INICIAL POS-FANTASMA 29 1500))


;; Auxiliares:

;; entre3? : Number Number Number -> Bool
; Recibe 3 Numbers y devuelve #true si los números están en orden (estrictamente) creciente, y #false en caso contrario.
(define (entre3? a b c) (and (<= a b) (<= b c)))
; Ejemplos:
(check-expect (entre3? 4 5 45) #t)
(check-expect (entre3? 4 46 45) #f)
(check-expect (entre3? 4 3 45) #f)
(check-expect (entre3? -4 5 45) #t)

;; semiancho: Imagen -> Number
; Recibe una imagen y devuelve la mitad de su ancho.
(define (semiancho img) (/ (image-width img) 2))
; Ejemplos:
(check-expect (semiancho (circle 100 "solid" "blue")) 100)
(check-expect (semiancho LABERINTO) (posn-x CENTRO))
(check-expect (semiancho (rectangle 700 20 "solid" "brown")) 350)

;; semialto: Imagen -> Number
;Recibe una imagen y devuelve la mitad de su altura.
(define (semialto img) (/ (image-height img) 2))
; Ejemplos:
(check-expect (semialto (circle 100 "solid" "blue")) 100)
(check-expect (semialto LABERINTO) (posn-y CENTRO))
(check-expect (semialto (rectangle 700 20 "solid" "brown")) 10)

;; Posiciones:
;; ==========

;; dentro-escena? : Posn Imagen -> Boolean
; Toma una posición y una imagen y devuelve #true si la imagen en dicha posición se encuentra dentro de la escena.
(define (dentro-escena? pos img) (and (entre3? (semiancho img) (posn-x pos) (- ANCHO (semiancho img))) (entre3? (semialto img) (posn-y pos) (- ALTO (semialto img)))) )



;; posible-pos? : Posn Imagen -> Boolean
; Toma una posición y una imagen y devuelve #true si la imagen en esa posición se encuentra dentro de la escena y no atravesando las paredes del laberinto, o #false en caso contrario.
(define (posible-pos? pos img) (and (dentro-escena? pos img) (not (interseca? pos img CENTRO LABERINTO)) ) )

;; Eventos del teclado:
;; ===================
;; manejador-tecla : Estado Tecla -> Estado
; Toma un estado y un evento del teclado y devuelve otro estado donde se muevió al jugador en la dirección deseada si se ha presionado una flecha del teclado (siempre y cuando esté permitido dicho movimiento).
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
; Toma un estado y devuelve otro con el 'tiempo' disminuido en una unidad si el fantasma no atrapó al jugador, en caso contrario invoca a reset-pos.
(define (manejador-tick e) (cond
                             [(interseca-fantasma? e) (reset-pos e)]
                             [else (actualizar-tiempo (- (estado-tiempo e ) 1) (actualizar-fantasma (mover-fantasma (estado-fantasma e)) e))]
                             )
  )

;; Condiciones de Terminación:
;; ==========================
;; interseca-fantasma? : Estado -> Boolean
; Toma un estado y devuelve #true si el fantasma atrapó al jugador y #false en caso contrario.
(define (interseca-fantasma? e) (interseca? (estado-jugador e) JUGADOR (estado-fantasma e) FANTASMA))
; Ejemplos:
(check-expect (interseca-fantasma? (make-estado POS-INICIAL POS-INICIAL 3 1500)) #t)
(check-expect (interseca-fantasma? (make-estado (make-posn 0 0) (make-posn 1000 1000) 3 1500)) #f)
(check-expect (interseca-fantasma? (make-estado (make-posn 30 30) (make-posn 30 30) 3 1500)) #t)

;; objetivo? : Estado -> Boolean
; Toma un estado y devuelve #true si el jugador llegó al objetivo y #false en caso contrario.
(define (objetivo? e) (interseca? (estado-jugador e) JUGADOR POS-OBJETIVO OBJETIVO))
;Ejemplos:
(check-expect (objetivo? (make-estado (make-posn -1000 -1000) POS-OBJETIVO 100 10)) #f)
(check-expect (objetivo? (make-estado POS-OBJETIVO (make-posn 10 10) 100 10)) #t)
(check-expect (objetivo? (make-estado (make-posn 100000 100000) (make-posn 12 13) 100 10)) #f)


;; fin? : Estado -> Boolean
; Toma un estado y devuelve #true si el juego terminó y #false de otro modo.
; El juego se considera terminado si se alcanzó el objetivo o se acabó el tiempo.
(define (fin? e ) (or (objetivo? e) (<= (estado-tiempo e) 0) (= (estado-vidas e) 0)))
;Ejemplos:
(check-expect (fin? (make-estado POS-OBJETIVO POS-INICIAL 3 1500)) #t)
(check-expect (fin? (make-estado (make-posn 0 0) (make-posn 1000 1000) 3 0)) #t)
(check-expect (fin? (make-estado (make-posn 30 30) (make-posn 30 30) 3 1500)) #f)
(check-expect (fin? (make-estado POS-INICIAL POS-INICIAL 3 1500)) #f)


;; estado-inicial : Estado
(define estado-inicial (make-estado POS-INICIAL POS-FANTASMA VIDAS-INICIAL TIEMPO-INICIAL) ) 

; Expresión big-bang: establece un estado incial, y asocia los tipos de eventos a sus manejadores, y lanza el programa.
(big-bang estado-inicial
     [to-draw grafica]
     [on-key manejador-tecla]
     [on-tick manejador-tick]
     [stop-when fin?])
