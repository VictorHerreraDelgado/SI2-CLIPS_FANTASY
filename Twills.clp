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
        (actLoc "SIS insides")
	    (attack 50)
	    (resistence 500)
        (alive 1)
    )

    (character
        (name "Ariadna")
        (surname "Daemonenblut")
        (species "Vampire")
        (actLoc "Comercial Street")
	    (attack 30)
	    (resistence 700)
        (alive 1)
    )

    (character
        (name "Xiangus")
        (surname "Leivadrac")
        (species "Draconid")
        (actLoc "Colisseum")
	    (attack 90)
	    (resistence 100)
        (alive 1)
    )
    
    (character 
        (name "Victor")
        (surname "HD")
        (species "Vrother")
        (actLoc "Twills Tower")
	    (vPhone "686286096")
	    (attack 70)
	    (resistence 1000)
        (alive 1)    
    )

    (character 
        (name "Vincent")
        (surname "Guepard")
        (species "Beast")
        (actLoc "SIS insides") 
	    (attack 80)
	    (resistence 200)
        (alive 1)   
    )
	
	(character 
		(name "August")
		(surname "Descence")
		(species "Vampire")
		(actLoc "Colisseum")
        (attack 30)
	    (resistence 100)
        (alive 1)   
	)




    (location 
        (name "Twills Tower")
        (contacto "yes")
	    (open "no")
        (solFiltAct "no")
    )

    (location
        (name "SIS insides")
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
    ?*vidaBase* = 200 
    ?*vida* = 200
    ?*key1* = 0
    ?*key2* = 0
    ?*key3* = 0
)
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
    (printout t "Ponemos filtro solar en " ?n crlf)
)
(deffunction deactivate_solar_filter (?l ?n)
    (modify ?l (solFiltAct "no"))
    (printout t "Quitamos filtro solar en " ?n crlf)   
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
    (printout t "Bienvenido, elija una opcion tecleando su numero." crlf)
    (printout t "1:Continuar" crlf)
    (printout t "2:Nuevo juego" crlf)
    (printout t "3:Salir" crlf)
    (assert (main_menu_option (read)))
    (retract ?i)
)


;/////////////////////////CONTINUAR//////////////////////////////
(defrule main_menu_option_continue
    ?i <- (main_menu_option ?readed)
    (test(eq ?readed 1))
    =>
    (if (eq 0 (str-compare "" ?*nombre*) ) then 
        (printout t "No existe una partida creada." crlf)
        (assert (main_menu))
    else (printout t "Continua jugando")
    )
    (retract ?i)
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

;/////opcion no

(defrule main_menu_character
    ?i <- (option ?readed ?spec)
    (test (or (eq ?readed No) (eq ?readed Si) ) )
    =>
    (retract ?i)
    (if (eq ?readed Si) then 
        (bind ?*especie* ?spec)
        (printout t "Ha seleccionado correctamente " ?*especie* crlf)
        (assert (main_menu_choose_name))
    else (assert (main_menu_option 2))
    )
    
)   


(defrule main_menu_choose_name
    (declare (salience 10))
    ?i <- (main_menu_choose_name)
    =>
    (printout t "Elija su nombre" crlf)
    (assert (select_character_name (read)))
    (retract ?i)

)
;////////////SELECCIONA NOMBRE
(defrule select_name 
    (declare (salience 10))
    ?i <- (select_character_name ?readed)
    (test (neq ?readed "") )
    =>
    (printout t "Desea ser " ?readed)
    (printout t "?" crlf)
    (printout t "Si" crlf)
    (printout t "No" crlf)
    (assert (main_menu_confirm_name (read) ?readed))
    (retract ?i)
)
(defrule select_name_error 
    (declare (salience 8))
    ?i <- (select_character_name ?readed)
    =>
    (printout t "Error, nombre no valido")
    (assert (main_menu_choose_name))
    (retract ?i)
)

;//////////////////CONFIRMA NOMBRE
(defrule confirm_name 

    ?i <- (main_menu_confirm_name ?readed ?name)
    (test (or (eq ?readed No) (eq ?readed Si) ) )
    =>
    (if (eq ?readed Si) then 
        (bind ?*nombre* ?name)
        (printout t "Se llamara " ?*nombre*)
        (printout t " y de especie sera " ?*especie* crlf)
        (bind ?*actualLoc* "Hideout")
        (assert (menu_location))
    else
        (assert (main_menu_choose_name))
    )
    (retract ?i)
    
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
    (retract ?i)
    (assert (main_menu))
)


;//////////////////////LOCATION MENU////////////////////
(defrule menu_location
    (declare(salience 10))
    ?i <- (menu_location)
    =>
    (printout t "Te encuentras en " ?*actualLoc* )
    (printout t ", que desea hacer?" crlf)
    (printout t "1. Mover" crlf)
    (if (eq ?*actualLoc* "Hideout")
    then
        (printout t "2. Curar" crlf)
    else 
        (printout t "2. Luchar" crlf)
    )
    (printout t "3. Datos" crlf)
    (printout t "4. Guardar" crlf)
    (assert (game_choose (read)))
    (retract ?i)
)






;////////ELIGE MOVERSE
(defrule menu_location_move
    ?i <- (game_choose ?readed)
    (test(eq ?readed 1))
    =>
        (retract ?i)
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
    (if (eq ?readed 1)
    then
        (if (neq ?*actualLoc* "Colisseum")
        then
            (retract ?i)
            (bind ?*actualLoc* "Colisseum")
            (assert (menu_location))
        else
        (retract ?i)
        (printout t "No valido, vuelva a elegir la zona" crlf )
        (assert (game_choose 1))
        )
    else
    (if (eq ?readed 2)
    then
        (if (neq ?*actualLoc* "Comercial Street")
        then
            (retract ?i)
            (bind ?*actualLoc* "Comercial Street")
            (assert (menu_location))
        else
            (retract ?i)
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 3)
    then
        (if (neq ?*actualLoc* "SIS Insides")
        then
            (retract ?i)
            (bind ?*actualLoc* "SIS Insides")
            (assert (menu_location))
        else
            (retract ?i)
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 4)
    then
        (if (neq ?*actualLoc* "Hideout")
        then
            (retract ?i)
            (bind ?*actualLoc* "Hideout")
            (assert (menu_location))
        else
            (retract ?i)
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 5)
    then
        (if (and (neq ?*actualLoc* "Twills Tower") (and (>= ?*key2* 1) (and (>= ?*key3* 1) (>= ?*key1* 1))))
        then
            (retract ?i)
            (bind ?*actualLoc* "Twills Tower")
            (assert (menu_location))
        else
            (retract ?i)
            (printout t "No valido, vuelva a elegir la zona" crlf )
            (assert (game_choose 1))
        )
    )
    else
    (if (eq ?readed 6)
    then
        (retract ?i)
        (assert (menu_location))
    )
    else
    (if (> ?readed 6)
    then
        (printout t "No valido, vuelva a elegir la zona" crlf )
        (assert (game_choose 1))
    )
    )
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



;/////////////////////////////////////////FIGHT////////////////////////
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
    (if (eq ?w1 ?myStr) then (return 1))
    (if (eq ?myWek ?s1) then (return -1))
    (return 0)
)



(defrule real_battle_begins
    (declare (salience 10))
    ?i <- (real_battle ?name ?surname )
    (character (name ?name) (surname ?surname) (species ?sp1) (attack ?at1) (resistence ?res1) (actLoc ?lo1))
    (species (name ?sp1) (weakness ?w1)
        (strength ?s1))
    (species (name ?sp1) (weakness ?myWek)
        (strength ?myStr))
    (test (> ?res1 0 ))
    (test (neq(str-compare ?lo1 ?*actualLoc*) 0))
    =>
    (printout t "Te has encontrado con el " ?sp1 " " ?name " "?surname "." crlf)
    (retract ?i)
    (printout t "Elige una acción " crlf "1:Ataque " crlf "2:Contrataque" crlf "3:Agarre" crlf)
    (bind ?typeAdv (type_advantage ?w1 ?myWek ?s1 ?myStr))
    (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read) ))
)

(defrule real_battle_begins_error
    (declare (salience 9))
    ?i <- (real_battle ?name ?surname )
    =>
    (printout t "Error, no se puede iniciar la pelea con " ?name " " ?surname "." crlf )
    (retract ?i)

)


;////////////////////////////////////////INTO BATTLE////////////////////////////////////////////////

(defrule battle_action_continue
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?typeAdv ?choose)
    (test (or (eq ?choose 1) (or (eq ?choose 2) (eq ?choose 3))))
    =>
    (retract ?i)
    (if (or (not (> ?*vida* 0)) (not (> ?res1 0))) 
    then 
        (assert (battle_action_end))
    else
        (assert (battle_action ?name ?surname ?at1 ?res1 ?typeAdv (read)))
    )    
)

(defrule battle_action_continue_error
    (declare (salience 8))
    ?i <- (battle_action ?name ?surname ?at1 ?res1 ?w1 ?myWek ?s1 ?myStr ?choose)
    (test (and (neq ?choose 1) (and (neq ?choose 2) (neq ?choose 3))))
    =>
    (printout t "Ha habido un error" crlf)
)


;/////////////////////////////////////BATTLE ENDS/////////////////////////////////////////////
(defrule battle_action_win
    (declare (salience 9))
    ?i <- (battle_action_end ?name ?surname ?res1 )
    ?char <-(character (name ?name) (surname ?surname) (alive 1))
    (test (and (> ?*vida* 0) (not (> ?res1 0)) ) )
    =>
    (printout t "Felicidades, has ganado" crlf);
    (modify ?char (alive 0))
    ;Assert a volver al mapa o seguir luchando
)

(defrule battle_action_loose
    (declare (salience 10))
    ?i <- (battle_action_end ?name ?surname ?res1)
    (test (not (> ?*vida* 0)))
    =>
    (bind ?*actualLoc* "Hideout")
    ;Assert a volver al mapa o seguir luchando
)
