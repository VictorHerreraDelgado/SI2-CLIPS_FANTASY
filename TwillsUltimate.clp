(deftemplate character
    	(slot name (type STRING))
    	(slot surname (type STRING))
    	(slot species )
    	(slot actLoc)
    	(slot vPhone (type STRING))
        (slot alive )
    	(slot attack (type INTEGER))
    	(slot resistence (type INTEGER))
)

(deftemplate location
    (slot contacto (type STRING))
    (slot name (type STRING))
    (slot description(type STRING))
    (slot open (type STRING)) 
    (slot solFiltAct (type STRING))
)

(deftemplate species
    	(slot name (type STRING))
    	(slot weakness (type STRING))
    	(slot strength (type STRING))
	    (slot telepaty (type STRING))
)

(deffacts main_facts

    (character
        (name "Tadeus")
        (surname "Daemonenblut")
        (species "Vampire")
        (actLoc "SIS Insides")
	    (attack 70)
	    (resistence 400)
        (alive 1)
    )

    (character
        (name "Ariadna")
        (surname "Daemonenblut")
        (species "Vampire")
        (actLoc "Comercial Street")
	    (attack 90)
	    (resistence 900)
        (alive 1)
    )
    
    (character 
		(name "August")
		(surname "Descence")
		(species "Vampire")
		(actLoc "Colisseum")
        (attack 30)
	    (resistence 180)
        (alive 1)   
	)


    (character
        (name "Xiangus")
        (surname "Leivadrac")
        (species "Draconid")
        (actLoc "Colisseum")
	    (attack 200)
	    (resistence 1700)
        (alive 1)
    )
    
    (character 
        (name "Victor")
        (surname "HD")
        (species "Vrother")
        (actLoc "Twills Tower")
	    (vPhone "686286096")
	    (attack 700)
	    (resistence 10000)
        (alive 1)    
    )

    (character 
        (name "Vincent")
        (surname "Guepard")
        (species "Beast")
        (actLoc "SIS Insides") 
	    (attack 400)
	    (resistence 3500)
        (alive 1)   
    )
	
	




    (location 
        (name "Twills Tower")
        (contacto "yes")
	    (open "no")
        (solFiltAct "no")
    )

    (location
        (name "SIS Insides")
        (contacto "yes")
        (open "no")
        (solFiltAct "no")
    )

    (location
        (name "Comercial Street")
        (contacto "no")
		(open "yes")
        (solFiltAct "no")
    )

    (location
        (name "Colisseum")
        (contacto "yes")
	    (open "yes")
        (solFiltAct "no")
    )


	(species
		(name "Vampire")
		(telepaty "yes")
        (weakness "light")
        (strength "darkness")
	)
	
	(species
		(name "Draconid")
		(telepaty "no")
        (weakness "psiquic")
        (strength "fire")
	)
	(species
		(name "Vrother")
		(telepaty "no")
        (weakness "darkness")
        (strength "psiquic")
	)
	(species
		(name "Beast")
		(telepaty "no")
        (weakness "fire")
        (strength "fight")
	)

    (species
		(name "Human")
		(telepaty "no")
        (weakness "fight")
        (strength "light")
	)

)

(deffunction fightTipes (?spec1 ?spec2)


)

;////////////////GLOBAL///////////////////
(defglobal 
        ?*dayTime* = "night")

(defglobal
    ?*nombre* = "" 
    ?*especie* = ""
    ?*actualLoc* = ""
    ?*ataque* = 20
    ?*vidaBase* = 200 
    ?*vida* = 200
    ?*key1* = 0
    ?*key2* = 0
    ?*key3* = 0
)

(deffunction respawn_enemies()
    (do-for-all-facts ((?c character))
        (if (eq ?c:alive 0) then (modify  ?c (alive 1)))
    )
)


;///////////CUANTOS HAY VIVOS EN EL LUGAR///////////
(deffunction how_many_alive (?locName)
    (bind ?charIn 0)
    (do-for-all-facts ((?c character))
        (and (eq ?c:actLoc ?locName) (eq ?c:alive 1))
        (bind ?charIn (+ ?charIn 1))
    )
    return ?charIn
)

(deffunction appearing_enemy (?locName ?option)
    (bind ?charIn 0)
    (do-for-all-facts ((?c character))
        (and (eq ?c:actLoc ?locName) (eq ?c:alive 1))
        (if (eq ?option 1) then (return ?c:name))
        (if (eq ?option 2) then (return ?c:surname))
    )
    (return 0)

)
;option = 1 nombre
;option = 2 apellido
;/////////////////////////////////////////


(deffunction changeTime ()
    (if (eq (str-compare ?*dayTime* night ) 0) then 
        (bind ?*dayTime*  day)
    else 
        (bind ?*dayTime*  night) 
    )

)

(defrule changeCharLoc 
    (declare (salience 10))
    ?i <- (changeCharLoc ?n ?s ?loc)
    ?char <-(character (name ?n) (surname ?s) (actLoc ?oldLoc))
    (exists (location (name ?loc)))
    =>
    (printout t "Se ha movido a " ?n " " ?s " a ")
    (modify ?char (actLoc ?loc))
    (retract ?i)
)

(defrule changeCharLoc_noVal 
    (declare (salience 9))
    ?i <- (changeCharLoc ?n ?s ?loc)
    =>
    (printout t "No se ha podido hacer el cambio de localización para el personaje")
    (retract ?i)
)


;************EMERGENCE CALL******************

(deffunction activate_solar_filter (?l ?n)
    (modify ?l (solFiltAct "yes"))
    ;(printout t "Ponemos filtro solar en " ?n crlf)
)
(deffunction deactivate_solar_filter (?l ?n)
    (modify ?l (solFiltAct "no"))
    ;(printout t "Quitamos filtro solar en " ?n crlf)   
)

(defrule emergence_call_to
    (declare (salience 10))
    ?i <- (emergence_call_to ?n ?s)
    (character (name ?n) (surname ?s) (vPhone ?p))
    (test (neq ?p ""))
    =>
    (printout t "Se ha llamado al telefono " ?p " para contactar con " ?n " " ?s "." crlf )
    (retract ?i)
)

(defrule emergence_call_to_contact
    (declare (salience 9))
    ?i <- (emergence_call_to ?n ?s)
    (character (name ?n) (surname ?s) (actLoc ?l))
    (test (neq ?l ""))
    (location(name ?l) (contacto ?c))
    (test (eq ?c "yes"))
    =>
    (printout t "Se ha llamado al contacto en " ?l " para contactar con " ?n " " ?s "." crlf )
    (retract ?i)
)

(defrule emergence_call_to_telepaty
    (declare (salience 8))
    ?i <- (emergence_call_to ?n ?s)
    (character (name ?n) (surname ?s) (species ?l))
    (test (neq ?l ""))
    (species(name ?l) (telepaty ?t))
    (test (eq ?t "yes"))
    =>
    (printout t "Se ha llamado al contacto " ?n " " ?s " usando telepatia." crlf )
    (retract ?i)
)

(defrule solarFilter
    (declare (salience 10))
	?loc <- (location (name ?l) (open ?o) (solFiltAct ?sf))
    (test (eq ?o "yes"))
    =>
    (bind ?total 0)
    (bind ?lightWeak 0)
    (do-for-all-facts ((?c character)) 
        (eq ?c:actLoc ?l)
        (bind ?total (+ ?total 1))
        (bind ?w "light")
        (do-for-all-facts ((?s species))
            (eq ?c:species ?s:name)
            (if (eq ?s:weakness ?w) then (bind ?lightWeak (+ ?lightWeak 1)))
        )
    )

    
    (if (and (and (neq ?lightWeak 0) (neq ?sf "yes")) (  > (/ ?total ?lightWeak) 0.45) ) then (activate_solar_filter ?loc ?l))
    (if (and (neq ?sf "no") (or (eq ?lightWeak 0) ( < (/ ?total ?lightWeak) 0.45) )) then (deactivate_solar_filter ?loc ?l))

)



;Una vez todos los escenarios hayan sido preparados podremos dar paso a las peleas
; para ello los combatientes deben estar en el mismo lugar y en condiciones de igualdad,
; por lo tanto se intentará preparar el escenario en base a ello.
;Si todo es correcto, haremos while para la pelea, haciendo cada uno daño igual a su ataque hasta
; que se uno de los dos termine con la vida a cero.
; Preparación de un arbitro para el encuentro.
; Equipo de rescate. Dependiendo de las condiciones actuarán unos u otros.

;////////////////////////////////////////////////////////
(defrule running 
    (declare (salience 10))
    =>
    (assert (main_menu))
)

(defrule main_menu 
    (declare (salience 10))
    ?i <- (main_menu)
    =>
    (retract ?i)
    (printout t "Bienvenido a Twills Ultimate, elija una opcion tecleando su numero." crlf)
    (printout t "1:Continuar" crlf)
    (printout t "2:Nuevo juego" crlf)
    (printout t "3:Salir" crlf)
    (assert (main_menu_option (read)))
    
)


;/////////////////////////CONTINUAR//////////////////////////////
(defrule main_menu_option_continue
    ?i <- (main_menu_option ?readed)
    (test(eq ?readed 1))
    =>
    (retract ?i)
    (if (eq "" ?*nombre* ) then 
        (printout t "No existe una partida creada." crlf)
        (assert (main_menu))
    else
    (if (and (neq "" ?*nombre* ) (eq (how_many_alive "Twills Tower") 0))
    then
        (printout t "Ya has derrotado al jefe final. Te dejare ver tu estadistica final y se te devolvera al menu principal." crlf)
        (printout t "" crlf)
        (printout t "Nombre: " ?*nombre* crlf )
        (printout t "Especie: " ?*especie* crlf )
        (printout t "Vida: " ?*vida* " / " ?*vidaBase* crlf )
        (printout t "Ataque: " ?*ataque* crlf )
        (assert (main_menu))
    )
    else
    (if (and (neq "" ?*nombre* ) (neq (how_many_alive "Twills Tower") 0)) then 
        (printout t "Continua jugando" crlf)
        (assert (menu_location))
    )
    )
)

;//////////Si elige la opcion 2


;//////////////////////
(defrule main_menu_option_new
    ?i <- (main_menu_option ?readed)
    (test(eq ?readed 2))
    =>
    (printout t "Elija su especie:" crlf)
    (printout t "1:Draconid" crlf)
    (printout t "2:Vampire" crlf)
    (printout t "3:Human" crlf)
    (printout t "4:Volver al menu" crlf)
    (assert (specie_option (read)))
    (retract ?i)
)

;////Elige draconid

(defrule main_menu_character_draconid
    ?i <- (specie_option ?readed)
    (test(eq ?readed 1))
    =>
    (printout t "Desea elegir 'Draconid'?" crlf)
    (printout t "Si" crlf)
    (printout t "No" crlf)
    (assert (option (read) "Draconid"))
    (retract ?i)
)

;/////Elige Vampire

(defrule main_menu_character_vampire
    ?i <- (specie_option ?readed)
    (test(eq ?readed 2))
    =>
    (printout t "Desea elegir 'Vampire'?" crlf)
    (printout t "Si" crlf)
    (printout t "No" crlf)
    (assert (option (read) "Vampire"))
    (retract ?i)
)

;////Elige human

(defrule main_menu_character_human
    ?i <- (specie_option ?readed)
    (test(eq ?readed 3))
    =>
    (printout t "Desea elegir 'Human'?" crlf)
    (printout t "Si" crlf)
    (printout t "No" crlf)
    (assert (option (read) "Human"))
    (retract ?i)
)

(defrule main_menu_character_return
    ?i <- (specie_option ?readed)
    (test(eq ?readed 4))
    =>
    (assert (main_menu))
    (retract ?i)
)

(defrule main_menu_character_error
    ?i <- (specie_option ?readed)
    (test(and (and (neq ?readed 1) (neq ?readed 4)) (and (neq ?readed 2) (neq ?readed 3))))
    =>
    (printout t "Opcion no valida" crlf)
    (printout t "" crlf)
    (assert (main_menu_option 2 ))
    (retract ?i)
)
;/////opcion no

(defrule main_menu_character
    ?i <- (option ?readed ?spec)
    =>
    (retract ?i)
    (if (eq ?readed Si) then 
        (printout t "Ha seleccionado correctamente " ?spec crlf)
        (printout t "" crlf)
        (assert (main_menu_choose_name ?spec))
    else 
    (if (eq ?readed No) then
    (printout t "" crlf)
    (assert (main_menu_option 2))
    else 
    (printout t "No ha seleccionado la opcion correcta, volvera a la seleccion de especie " crlf)
    (printout t "" crlf)
    (assert (main_menu_option 2))
    )
    )
)   


(defrule main_menu_choose_name
    (declare (salience 10))
    ?i <- (main_menu_choose_name ?species)
    =>
    (retract ?i)
    (printout t "Elija su nombre" crlf)
    (assert (select_character_name (read) ?species))
    

)
;////////////SELECCIONA NOMBRE
(defrule select_name 
    (declare (salience 10))
    ?i <- (select_character_name ?readed ?species)
    (test (neq ?readed "") )
    =>
    (retract ?i)
    (printout t "Desea ser " ?readed)
    (printout t "?" crlf)
    (printout t "Si" crlf)
    (printout t "No" crlf)
    (assert (main_menu_confirm_name (read) ?readed ?species))
)
(defrule select_name_error 
    (declare (salience 8))
    ?i <- (select_character_name ?readed ?species)
    =>
    (retract ?i)
    (printout t "Error, nombre no valido" crlf)
    (printout t "" crlf)
    (assert (main_menu_choose_name ?species))
)

;//////////////////CONFIRMA NOMBRE
(defrule confirm_name 

    ?i <- (main_menu_confirm_name ?readed ?name ?species)

    =>
    (retract ?i)
    (if (eq ?readed Si) then 
        (respawn_enemies)
        (bind ?*especie* ?species)
        (bind ?*nombre* ?name)
        (printout t "" crlf)
        (printout t "Se llamara " ?*nombre*)
        (printout t " y su especie sera " ?*especie* crlf)
        (printout t "" crlf)
        (bind ?*actualLoc* "Hideout")
        ;Si quieres ver como aumenta su ataque y vida ilimitadamente, comenta lo de abajo
        (bind ?*ataque* 20)
        (bind ?*vidaBase* 200)
        (bind ?*vida* 200)
        (bind ?*key1* 0)
        (bind ?*key2* 0)
        (bind ?*key3* 0)
        (assert (menu_location))
    else
    (if (eq ?readed No) then
        (assert (main_menu_choose_name ?species))
    
    else
        (printout t "Error, vuelva a intentarlo." crlf)
        (assert (select_character_name ?name ?species))
    )
    )
)


(defrule main_menu_option_exit
    ?i <- (main_menu_option ?readed)
    (test(eq ?readed 3))
    =>
    (retract ?i)
)

(defrule main_menu_option_no_valid
    ?i <- (main_menu_option ?readed)
    (test (and (neq ?readed 3) (and (neq ?readed 2) (neq ?readed 1)) ) )
    =>
    (printout t "Opcion no valida, vuelva a probar" crlf)
    (printout t "" crlf)
    (retract ?i)
    (assert (main_menu))
)


;//////////////////////LOCATION MENU////////////////////
(defrule menu_location
    (declare(salience 10))
    ?i <- (menu_location)
    =>
    (retract ?i)
    (printout t "Te encuentras en " ?*actualLoc* crlf)
    (if (neq ?*actualLoc* "Hideout") then (printout t "Hay " (how_many_alive ?*actualLoc*) " enemigos." crlf ))
    
    (printout t "que desea hacer?" crlf)
    (printout t "1. Mover" crlf)
    (if (eq ?*actualLoc* "Hideout")
    then
        (printout t "2. Curar" crlf)
    else 
        (printout t "2. Luchar" crlf)
    )
    (printout t "3. Datos" crlf)
    (printout t "4. Guardar y Salir" crlf)
    (assert (game_choose (read)))
    
)






;////////ELIGE MOVERSE
(defrule menu_location_move
    ?i <- (game_choose ?readed)
    (test(eq ?readed 1))
    =>
        (retract ?i)
        (printout t "" crlf)
        (printout t "A donde desea moverse?" crlf )
        (if (neq ?*actualLoc* "Colisseum") then (printout t "1. Colisseum" crlf))
        (if (neq ?*actualLoc* "Comercial Street") then(printout t "2. Comercial Street" crlf))
        (if (neq ?*actualLoc* "SIS Insides") then(printout t "3. SIS Insides" crlf))
        (if (neq ?*actualLoc* "Hideout") then(printout t "4. Hideout" crlf))
        (if (and (neq ?*actualLoc* "Twills Tower") (and (>= ?*key2* 1) (and (>= ?*key3* 1) (>= ?*key1* 1))))then(printout t "5. Twills Tower" crlf))

        (printout t "6. Atras" crlf)
        (assert (zone_to_move (read)))
        

)


(defrule move_to_zone
    ?i <- (zone_to_move ?readed)
    =>
    (retract ?i)
    (if (eq ?readed 1)
    then
        (if (neq ?*actualLoc* "Colisseum")
        then
            (bind ?*actualLoc* "Colisseum")
            (printout t "" crlf)
            (assert (menu_location))
        else
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (printout t "" crlf)
            (assert (game_choose 1))
        )
    else
    (if (eq ?readed 2)
    then
        (if (neq ?*actualLoc* "Comercial Street")
        then
            (bind ?*actualLoc* "Comercial Street")
            (printout t "" crlf)
            (assert (menu_location))
        else
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (printout t "" crlf)
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 3)
    then
        (if (neq ?*actualLoc* "SIS Insides")
        then
            
            (bind ?*actualLoc* "SIS Insides")
            (printout t "" crlf)
            (assert (menu_location))
        else
            
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (printout t "" crlf)
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 4)
    then
        (if (neq ?*actualLoc* "Hideout")
        then
            
            (bind ?*actualLoc* "Hideout")
            (printout t "" crlf)
            (assert (menu_location))
        else
            
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (printout t "" crlf)
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 5)
    then
        (if (and (neq ?*actualLoc* "Twills Tower") (and (>= ?*key2* 1) (and (>= ?*key3* 1) (>= ?*key1* 1))))
        then
            
            (bind ?*actualLoc* "Twills Tower")
            (printout t "" crlf)
            (assert (menu_location))
        else
            
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (printout t "" crlf)
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 6)
    then
        (printout t "" crlf)
        (assert (menu_location))
    )
    else
    (if (and (and (neq ?readed 1) (neq ?readed 4)) (and (neq ?readed 2) (and (neq ?readed 3) (and (neq ?readed 5) (neq ?readed 6)))))
    then
        (printout t "No valido, vuelva a elegir la zona" crlf )
        (printout t "" crlf)
        (assert (game_choose 1))
    )   
    )
    
)



;//////////////////////CURAR o LUCHAR///////////////////////////////
(defrule menu_location_cure_battle
    ?i <- (game_choose ?readed)
    (test (eq ?readed 2))
    =>
        (retract ?i)
        (if (eq ?*actualLoc* "Hideout")
        then
            (if (< ?*vida* ?*vidaBase*)
            then
                (bind ?*vida* ?*vidaBase*)
                (printout t "Te has curado y ahora tu vida esta al maximo." crlf )
            else
                (printout t "Tu vida esta al tope, acaso quieres mas?" crlf )
            )
            (printout t "" crlf)
            (assert(menu_location))
        else
        (assert (prebattle))
        )
        

)

;//////////////////////DATOS DEL PERSONAJE///////////////////////////////
(defrule menu_location_character_data
    ?i <- (game_choose ?readed)
    (test (eq ?readed 3))
    =>
    (retract ?i)
    (printout t "" crlf)
    (printout t "Nombre: " ?*nombre* crlf )
    (printout t "Especie: " ?*especie* crlf )
    (printout t "Vida: " ?*vida* " / " ?*vidaBase* crlf )
    (printout t "Ataque: " ?*ataque* crlf )
    (printout t "Llaves: "crlf )
    (if (> ?*key1* 0)  then (printout t "Llave de Colisseum" crlf ))
    (if (> ?*key2* 0)  then (printout t "Llave de Comercial Street" crlf ))
    (if (> ?*key3* 0)  then (printout t "Llave de SIS Insides" crlf ))
    (if(and (= ?*key2* 0) (and (= ?*key3* 0) (= ?*key1* 0))) then (printout t "No tienes ninguna llave" crlf ))
    (printout t "" crlf)
    (assert(menu_location))   
)


;///////////////////////////PARTIDA GUARDADA//////////////////////////
(defrule menu_location_character_save
    ?i <- (game_choose ?readed)
    (test (eq ?readed 4))
    =>
    (retract ?i)
    (printout t "Su partida ha sido guardada. Se procede a salir del juego." crlf )
    (assert(main_menu))
)

;////////////////////ERROR DE ELECCION DE MENU//////////////////
(defrule menu_location_character_error
    ?i <- (game_choose ?readed)
    (test (and (and (neq ?readed 1) (neq ?readed 4)) (and (neq ?readed 2) (neq ?readed 3))))
    =>
    (retract ?i)
    (printout t "Opcion no valida" crlf )
    (printout t "" crlf)
    (assert(menu_location))   
)

;//////////////FIGHT-BEGINS/////////////////////////////
(defrule fight_begins 
    ?i <- (fight_begins ?at1 ?at2 ?res1 ?res2)
    =>
    (while (and (> ?res1 0) (> ?res2 0))
        (bind ?res1 (- ?res1 ?at2))
        (bind ?res2 (- ?res2 ?at1))
        (printout t "Vida 1: " ?res1 ". Vida 2: " ?res2 "." crlf)
    )
    (if (> ?res1 ?res2)  then (printout t "El ganador del combate ha sido el primer combatiente" crlf ))
    (if (> ?res2 ?res1) then (printout t "El ganador del combate ha sido el segundo combatiente" crlf) )
    (if (= ?res1 ?res2) then (printout t "Ambos combatientes han caido. No hay ganador." crlf))
    (retract ?i)
)

;///////////////FIGHT-CONDITIONS/////////////////////////
(defrule fight_cond 
    (declare (salience 10))
    ?i <- (fight_cond ?at1 ?at2 ?res1 ?res2 ?spec1 ?spec2)
    (species (name ?spec1) (weakness ?w1))
    (species (name ?spec2) (weakness ?w2))
    =>
    (assert (fight_begins ?at1 ?at2 ?res1 ?res2))
    (retract ?i)
)

(defrule fight_cond_error
    (declare (salience 9))
    ?i <- (fight_cond ?at1 ?at2 ?res1 ?res2 ?spec1 ?spec2)
    =>
    (printout t "Error, las especies no coinciden con la encontradas en la base de datos")
    (retract ?i)
)

;////////////////////STARTING FIGHT///////////////////////////

(defrule starting_fight
    (declare (salience 8))
    ?i <- (fight ?n1 ?s1 ?n2 ?s2 )
    (character (name ?n1) (surname ?s1) (species ?sp1) 
    (attack ?at1) (resistence ?res1) (actLoc ?lo1))
    (character (name ?n2) (surname ?s2) (species ?sp2)
    (attack ?at2) (resistence ?res2) (actLoc ?lo2))
    (test (eq(str-compare ?lo1 ?lo2) 0))
    =>
    (printout t "Personajes en el mismo lugar: " ?lo1 "." crlf)
    (assert (fight_cond ?at1 ?at2 ?res1 ?res2 ?sp1 ?sp2))
    (retract ?i)
)

(defrule starting_fight_no_loc
    (declare (salience 7))
    ?i <- (fight ?n1 ?s1 ?n2 ?s2)
    (character (name ?n1) (surname ?s1) (species ?sp1) 
    (attack ?at1) (resistence ?res1) (actLoc ?lo1))
    (character (name ?n2) (surname ?s2) (species ?sp2)
    (attack ?at2) (resistence ?res2) (actLoc ?lo2))
    (test (neq(str-compare ?lo1 ?lo2) 0))
    =>
    (printout t "No se puede comenzar la pelea porque los personajes no están en el mismo lugar." crlf)
    (retract ?i)
)


(defrule starting_fight_no_char
    (declare (salience 1))
    ?i <- (fight ?n1 ?s1 ?n2 ?s2)
    =>
    (printout t "Uno o ambos personajes " ?n1 " y " ?n2 " no existen." crlf )
    (retract ?i)
)

;/////////////////////////////////////////BATLLE////////////////////////
(deffunction jankenpon (?dec1 ?dec2)
    ;(if (and (eq ?dec1 contrataque) (eq ?dec2 ataque)) then (return 1) )
    ;(if (and (eq ?dec2 contrataque) (eq ?dec1 ataque)) then (return -1) )

    (if (and (eq ?dec1 2) (eq ?dec2 1)) then (return 1) )
    (if (and (eq ?dec2 2) (eq ?dec1 1)) then (return -1) )

    ;(if (and (eq ?dec1 agarre) (eq ?dec2 contrataque)) then (return 1) )
    ;(if (and (eq ?dec2 agarre) (eq ?dec1 contrataque)) then (return -1) )

    (if (and (eq ?dec1 3) (eq ?dec2 2)) then (return 1) )
    (if (and (eq ?dec2 3) (eq ?dec1 2)) then (return -1) )

    ;(if (and (eq ?dec1 ataque) (eq ?dec2 agarre)) then (return 1) )
    ;(if (and (eq ?dec2 ataque) (eq ?dec1 agarre)) then (return -1) )

    (if (and (eq ?dec1 1) (eq ?dec2 3)) then (return 1) )
    (if (and (eq ?dec2 1) (eq ?dec1 3)) then (return -1) )

    (return 0)
)
(deffunction type_advantage (?w1 ?myWek ?s1 ?myStr) 
    (if (eq  ?w1 ?myStr) then  (return 1))
    (if (eq  ?myWek ?s1) then  (return -1))
    (return 0)
)

(defrule prebattle
    ?i <- (prebattle)
    (test(neq  ?*actualLoc* "Hideout") )
    (test(neq (how_many_alive ?*actualLoc*) 0) )
    =>
    (retract ?i)
    (assert (real_battle (appearing_enemy ?*actualLoc* 1) (appearing_enemy ?*actualLoc* 2) ?*especie*))
)

(defrule prebattle_no_One
    ?i <- (prebattle)
    (test(neq  ?*actualLoc* "Hideout") )
    (test(eq (how_many_alive ?*actualLoc*) 0) )
    =>
    (retract ?i)
    (printout t "" crlf)
    (printout t "No hay enemigos en el campo." crlf)
    (printout t "" crlf)
    (assert (menu_location))
)

(defrule real_battle_begins
    (declare (salience 10))
    ?i <- (real_battle ?name ?surname ?mySpec)
    (character (name ?name) (surname ?surname) (species ?sp1) (attack ?at1) (resistence ?res1) (actLoc ?lo1))
    (species (name ?sp1) (weakness ?w1)
        (strength ?s1))
    (species (name ?mySpec) (weakness ?myWek)
        (strength ?myStr))
    (test (> ?res1 0 ))
    ;(test (neq(str-compare ?lo1 ?*actualLoc*) 0))
    =>
    (printout t "" crlf)
    (printout t "Te has encontrado con el " ?sp1 " " ?name " "?surname "." crlf)
    (printout t "Tu vida es " ?*vida* "." crlf)
    (printout t "La vida de tu rival es " ?res1 "." crlf)
    (retract ?i)
    (printout t "Elige una accion " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
    (bind ?typeAdv (type_advantage ?w1 ?myWek ?s1 ?myStr))
    (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) (mod (random) 3)))
)

(defrule real_battle_begins_error
    (declare (salience 9))
    ?i <- (real_battle ?name ?surname )
    =>
    (printout t "Error, no se puede iniciar la pelea con " ?name " " ?surname "." crlf )
    (printout t "" crlf)
    (retract ?i)

)


;////////////////////////////////////////INTO BATTLE////////////////////////////////////////////////

(defrule battle_action_continue_neutral
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?typeAdv ?choose ?eChoose)
    (test (or (eq ?choose 1) (or (eq ?choose 2) (eq ?choose 3))))
    (test (eq  (jankenpon ?choose ?eChoose) 0 ))
    =>
    (retract ?i)   
    ;(printout t "neutral" crlf)
    (printout t "Habeis hecho la usado la misma tactica y vuestros ataques se han neutralizado" crlf)
    (printout t "" crlf)
    (if (or (not (> ?*vida* 0)) (not (> ?res1 0))) 
    then 
        (assert (battle_action_end ?name ?surname ?res1))
    else
        (printout t "Tu vida es " ?*vida* "." crlf)
        (printout t "La vida de tu rival es " ?res1 "." crlf)
        (printout t "Elige una accion " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
        (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) (+ 1 (mod (random) 3)) ))
        
    ) 
    
)

(defrule battle_action_continue_win
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?typeAdv ?choose ?eChoose)
    (test (or (eq ?choose 1) (or (eq ?choose 2) (eq ?choose 3))))
    (test (eq  (jankenpon ?choose ?eChoose) 1 ))
    =>
    (retract ?i)   
    ;(printout t "win" crlf)
    (if 
        (eq ?typeAdv 1) 
    then 
        (bind ?res1 (- ?res1 (* ?*ataque* 2)))
        (printout t "Tu especie tiene ventaja sobre la de tu rival. Consigues hacerle" (* ?*ataque* 2) " de impacto." crlf)
    else
        (bind ?res1 (- ?res1  ?*ataque* ))
        (printout t "Ganas ventaja sobre tu rival y le haces "?*ataque* " de impacto." crlf)
        (printout t "" crlf)
    )

    (if (or (not (> ?*vida* 0)) (not (> ?res1 0))) 
    then 
        (assert (battle_action_end ?name ?surname ?res1 ?at1))
    else
        (printout t "Tu vida es " ?*vida* "." crlf)
        (printout t "La vida de tu rival es " ?res1 "." crlf)
        (printout t "Elige una accion " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
        (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) (+ 1 (mod (random) 3))))
    )
     
)

(defrule battle_action_continue_lost
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?typeAdv ?choose ?eChoose)
    (test (or (eq ?choose 1) (or (eq ?choose 2) (eq ?choose 3))))
    (test (eq  (jankenpon ?choose ?eChoose) -1 ))
    =>
    (retract ?i) 
    ;(printout t "lost" crlf)
    (if 
        (eq ?typeAdv -1) 
    then 
        (bind ?*vida* (- ?*vida* (* ?at1 2)))
        (printout t "Tu especie es debil a la de tu rival. Te hacen " (* ?at1 2) " de impacto." crlf)
    else
        (printout t "Pierdes la compostura y tu rival te hace " ?at1 " de impacto." crlf)
        (printout t "" crlf)
        (bind ?*vida* (- ?*vida*  ?at1 ))
    )
    (if (or (not (> ?*vida* 0)) (not (> ?res1 0))) 
    then 
        (assert (battle_action_end ?name ?surname ?res1 ?at1))
    else
        (printout t "La vida de tu rival es " ?res1 "." crlf)
        (printout t "Tu vida es " ?*vida* "." crlf)
        (printout t "Elige una accion " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
        (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) (+ 1 (mod (random) 3))))
    )   
    
)

(defrule battle_action_continue_error
    (declare (salience 8))
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?typeAdv ?choose ?eChoose)
    (test (and (neq ?choose 1) (and (neq ?choose 2) (neq ?choose 3))))
    =>
    (retract ?i)
    (printout t "Ha habido un error" crlf)
    (printout t "" crlf)
    (printout t "Elige una accion " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
    (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) (mod (random) 3)))
    
)


;/////////////////////////////////////BATTLE ENDS/////////////////////////////////////////////
(defrule battle_action_win
    (declare (salience 9))
    ?i <- (battle_action_end ?name ?surname ?res1 ?at1)
    ?char <-(character (name ?name) (surname ?surname) (resistence ?res)(alive 1))
    (test (and (> ?*vida* 0) (not (> ?res1 0))))
    =>
    (retract ?i)
    (printout t "Di molto, has ganado a " ?name " " ?surname "." crlf)
    (modify ?char (alive 0))
    (bind ?*ataque* (+ ?*ataque* ?at1))
    (bind ?*vidaBase* (+ ?*vidaBase* ?res))
    (printout t "Tus habilidades de combate han mejorado:" crlf "Ataque: " ?*ataque* "  Vida total: " ?*vidaBase* crlf)
    (printout t "" crlf)
    ;///////////////LLAVES///////////////////
    (if (and (eq ?*actualLoc* "Colisseum") (eq (how_many_alive ?*actualLoc*) 0))
    then (bind ?*key1* 1)
    (printout t "Has vencido a todos. Obtienes la llave de Colisseum" crlf))

    (if (and (eq ?*actualLoc* "SIS Insides") (eq (how_many_alive ?*actualLoc*) 0))
    then (bind ?*key3* 1)
    (printout t "Has vencido a todos. Obtienes la llave de SIS Insides" crlf))

    (if (and (eq ?*actualLoc* "Comercial Street") (eq (how_many_alive ?*actualLoc*) 0))
    then (bind ?*key2* 1)
    (printout t "Has vencido a todos. Obtienes la llave de Comercial Street" crlf))

    (if (and (neq ?*actualLoc* "Twills Tower") (and(>= ?*key2* 1) (and (>= ?*key3* 1) (>= ?*key1* 1))))
    then
    (printout t "" crlf)
    (printout t "HAS OBTENIDO LAS TRES LLAVES DESBLOQUEADO EL ACCESO A TWILLS TOWER!!!!!!!" crlf))

    (if (and (eq ?*actualLoc* "Twills Tower") (eq (how_many_alive ?*actualLoc*) 0))
    then
    (printout t "" crlf)
    (printout t "ENHORABUENA, HAS DERROTADO AL JEFE FINAL, COMO RECOMPENSA, UNA PALMADITA EN LA ESPALDA Y PA CASA" crlf)
    (printout t "" crlf)
    (assert (credits))
    else
    (assert (menu_location))
    )
    ;Assert a volver al mapa o seguir luchando
)

(defrule battle_action_loose
    (declare (salience 10))
    ?i <- (battle_action_end ?name ?surname ?res1 ?at1)
    (test (not (> ?*vida* 0)))
    =>
    (retract ?i)
    (printout t ?name " te ha dado una paliza." crlf "Vuelves a tu escondite con las fuerzas que te quedan" "." crlf);
    (printout t "" crlf)
    (bind ?*vida* ?*vidaBase*)
    (bind ?*actualLoc* "Hideout")
    (assert (menu_location))
    ;Assert a volver al mapa o seguir luchando
)

;///////////////////CHEATS////////////////////


(defrule battle_action_continue_OWMS
    (declare (salience 10))
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?typeAdv ?choose ?eChoose)
    (test (and (neq ?choose 1) (and (neq ?choose 2) (neq ?choose 3))))
    (test(eq  ?choose OWMS ))
    =>

    (retract ?i)
    (printout t "Tu : --  Omae Wo Mou Shindeiru" crlf)
    (printout t ?name ": NANI!!" crlf)
    (printout t ">_< CRITICAL HIT -99999" crlf)
    (printout t "" crlf)
    (bind ?res1 (- ?res1 99999))
    (if (or (not (> ?*vida* 0)) (not (> ?res1 0))) 
    then 
        (assert (battle_action_end ?name ?surname ?res1 ?at1))
    else
        (printout t "Tu vida es " ?*vida* "." crlf)
        (printout t "La vida de tu rival es " ?res1 "." crlf)
        (printout t "Elige una accion " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
        (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) (+ 1 (mod (random) 3)) ))
    ) 
)

(defrule battle_action_continue_ITIAH
    (declare (salience 10))
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?typeAdv ?choose ?eChoose)
    (test (and (neq ?choose 1) (and (neq ?choose 2) (neq ?choose 3))))
    (test(eq  ?choose ITIAH ))
    =>

    (retract ?i)
    (printout t "Tu: Si hay un infierno, te vere alli" crlf)
    (printout t ?name ": Me tenias en jaque desde el inicio..." crlf)
    (printout t "BOOOOOOOOOOOOOOMMMMMMMMMM    -99999" crlf)
    (printout t "" crlf)
    (bind ?res1 (- ?res1 99999))
    (bind ?*vida* (- ?*vida* 99999))
    (if (or (not (> ?*vida* 0)) (not (> ?res1 0))) 
    then 
        (assert (battle_action_end ?name ?surname ?res1 ?at1))
    else
        (printout t "Tu vida es " ?*vida* "." crlf)
        (printout t "La vida de tu rival es " ?res1 "." crlf)
        (printout t "Elige una accion " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
        (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) (+ 1 (mod (random) 3)) ))
    ) 
)


(defrule creditos
    (declare (salience 10))
    ?i <- (credits)
    =>
    (retract ?i)
    (printout t "Game by Pocholo & Canary255" crlf)
    (printout t "En colaboracion con KONAMI" crlf)
    ;Assert a volver al mapa o seguir luchando
)
