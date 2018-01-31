//         __
//       >(' )
//         )/
//        /(              F# code formatter, Gilberto Vergerio
//       /  `----/
//       \  ~=- /
//     ^~^~^~^~^~^

module FSharpCodeFormatter.Formatter

open Lib

// Genero una lista di caratteri dato il codice ascii dell'inizio (compreso) e della fine (compreso)
let rec chars inizio fine =
    if inizio <= fine then (char inizio) :: chars (inizio + 1) fine
    else []

// Versione avanzata di trim_line: rimuovo tutti i caratteri ascii dallo 0 al 32 (quindi anche lo spazio)
//     0 nul    1 soh    2 stx    3 etx    4 eot    5 enq    6 ack    7 bel
//     8 bs     9 ht    10 nl    11 vt    12 np    13 cr    14 so    15 si
//    16 dle   17 dc1   18 dc2   19 dc3   20 dc4   21 nak   22 syn   23 etb
//    24 can   25 em    26 sub   27 esc   28 fs    29 gs    30 rs    31 us
//    32 sp    33  !    34  "    35  #    36  $    37  %    38  &    39  '
//    40  (    41  )    42  *    43  +    44  ,    45  -    46  .    47  /
//    48  0    49  1    50  2    51  3    52  4    53  5    54  6    55  7
//    56  8    57  9    58  :    59  ;    60  <    61  =    62  >    63  ?
//    64  @    65  A    66  B    67  C    68  D    69  E    70  F    71  G
//    ....
let rifila_linea (s : string) = s.Trim (chars 0 32 |> List.toArray)

// Versione avanzata di split_lines: splitta dividendo solo con \n, gli eventuali \r sono rimossi da rifila_linea
let dividi_linee (s : string) = s.Split ([|'\n'|], System.StringSplitOptions.None) |> Array.map rifila_linea |> List.ofArray

// Converte una stringa in una lista di caratteri
let esplodi (s : string) =
    let rec dividi n acc =
        if n = 0 then acc
        else dividi (n - 1) (s.[n - 1] :: acc)
    dividi s.Length []

// Versione avanzata di tokenize_line, per rimuovere i commenti e unire più token se appartengono alla stessa stringa
//Esempio: considerando la stringa "if bbb = \"let if elif else in then match\" then true"

//Lib.tokenize_line "if bbb = \"let if elif else in then match\" then true";;
//val it : string list = ["if"; "bbb"; "="; ""let"; "if"; "elif"; "else"; "in"; "then"; "match""; "then"; "true"]

//tokenize "if bbb = \"let if elif else in then match\" then true";;
//val it : string list = ["if"; "bbb"; "="; ""let if elif else in then match""; "then"; "true"]
let tokenize stringa =
    let rec rimuovi_commenti linea = // Funzione per rimuovere i commenti dalla linea
        match linea with
        | [] -> []
        | x::y::xs ->
            if x = '/' && y = '/' then [] // Inizia il commento
            else x :: rimuovi_commenti (y :: xs)
        | x::xs -> x :: rimuovi_commenti xs
    let rec forma_stringhe linea acc sto_unendo = // Funzione per unire in token una lista di caratteri
        match linea with
        | [] ->
            if acc = "" then []
            else [acc]
        | x::y::z::xs ->
            if x = ''' && y = '"' && z = ''' then forma_stringhe xs (acc + string x + string y + string z) sto_unendo // Non cambio lo stato
            elif x <= ' ' then // Separatore
                if sto_unendo then forma_stringhe (y :: z :: xs) (acc + string x) sto_unendo // Devo tenere la stringa intatta
                elif acc = "" then forma_stringhe (y :: z :: xs) "" sto_unendo
                else acc :: forma_stringhe (y :: z :: xs) "" sto_unendo
            elif x = '"' then forma_stringhe (y :: z :: xs) (acc + string x) (not sto_unendo) // Inizia o finisce una stringa
            else forma_stringhe (y :: z :: xs) (acc + string x) sto_unendo
        | x::xs ->
            if x <= ' ' then // Separatore
                if sto_unendo then forma_stringhe xs (acc + string x) sto_unendo // Devo tenere la stringa intatta
                elif acc = "" then forma_stringhe xs "" sto_unendo
                else acc :: forma_stringhe xs "" sto_unendo
            elif x = '"' then forma_stringhe xs (acc + string x) (not sto_unendo) // Inizia o finisce una stringa
            else forma_stringhe xs (acc + string x) sto_unendo
    let linea = esplodi stringa // Converto la stringa in lista di caratteri
    let senza_commenti = rimuovi_commenti linea // Tolgo i commenti
    forma_stringhe senza_commenti "" false // Formo le stringhe

// Lunghezza di una stringa
let lunghezza_stringa s = String.length s

// Lista di coppie (inizio, fine) per split e indent
let coppie_terminatrici = [("let", "="); ("if", "then"); ("elif", "then"); ("else", "else"); ("match", "with"); ("|", "->"); ("fun", "->"); ("in", "in")]

// Primo e secondo elemento di una coppia
let primo (a, b) = a
let secondo (a, b) = b

// Inversione degli elementi di una lista
let rec inverti lista =
    match lista with
    | [] -> []
    | x::xs -> (inverti xs) @ [x]

// Primo elemento di una lista
let primo_elemento lista =
    match lista with
    | x::xs -> x

// Ultimo elemento di una lista
let ultimo_elemento lista = primo_elemento (inverti lista)

// Rimuove il primo elemento da una lista
let rimuovi_primo_elemento lista =
    match lista with
    | [] -> []
    | x::xs -> xs

// Ricerca di un elemento in una lista
let rec ricerca lista elem =
    match lista with
    | [] -> false
    | x::xs ->
        if x = elem then true
        else ricerca xs elem

// Determina se un token è una parola chiave per iniziare una riga
let inizio_riga token =
    let rec cerca coppie token =
        match coppie with
        | [] -> false
        | x::xs -> 
            if primo x = token then true
            else cerca xs token
    cerca coppie_terminatrici token

// Determina se un token è una parola chiave per terminare una riga
let fine_riga token =
    let rec cerca coppie token =
        match coppie with
        | [] -> false
        | x::xs -> 
            if secondo x = token then true
            else cerca xs token
    cerca coppie_terminatrici token

// Converte una lista di stringhe in una stringa
let rec linea_in_stringa linea =
    match linea with
    | [] -> ""
    | x::xs ->
        if xs = [] then x // Non aggiungo lo spazio a fine stringa
        else x + " " + linea_in_stringa xs

// Versione avanzata di split
// IDEA: spezzare tutte le righe per ricomporle rispettando la width
// Esempio (width = 15):
// let rec aaa bbb =
//    if bbb > 4 then true
//    elif bbb < -7 then true
//    else false
// Viene diviso in tokens, riga per riga:
// ["let"; "rec"; "aaa"; "bbb"; "="]
// ["if"; "bbb"; ">"; "4"; "then"; "true"]
// ["elif"; "bbb"; "<"; "-7"; "then"; "true"]
// ["else"; "false"]
// Vengono spezzate le righe:
// [["let"; "rec"; "aaa"; "bbb"; "="]]
// [["if"; "bbb"; ">"; "4"; "then"]; ["true"]]
// [["elif"; "bbb"; "<"; "-7"; "then"]; ["true"]]
// [["else"]; ["false"]]
// Ruinisco le righe rispettando la width:
// [["let"; "rec"; "aaa"; "bbb"; "="]]
// [["if"; "bbb"; ">"; "4"; "then"]; ["true"]]
// [["elif"; "bbb"; "<"; "-7"; "then"]; ["true"]]
// [["else"; "false"]]
// Formo le stringhe:
// ["let rec aaa bbb ="]
// ["if bbb > 4 then"; "true"]
// ["elif bbb < -7 then"; "true"]
// ["else false"]
// Visualizzando le singole stringhe:
// let rec aaa bbb =
//   if bbb > 4 then
//      true
//   elif bbb < -7 then
//      true
//   else false
let split (w : int) (s : string) =
    let rec linee_in_stringhe linee = // Converte una lista di liste di stringhe in una lista di stringhe
        match linee with
        | [] -> []
        | x::xs -> (linea_in_stringa x) :: linee_in_stringhe xs
    let rimuovi_vuota lista = // Rimuove una lista di lista vuota
        if lista = [[]] then []
        else lista
    let rec spezza_tutto linea acc = // Spezzo la riga in righe, dove possibile
        match linea with
        | [] -> [acc]
        | x::xs ->
            if inizio_riga x && fine_riga x then (rimuovi_vuota [acc]) @ [[x]] @ (rimuovi_vuota (spezza_tutto xs [])) // La riga è formata dal singolo token
            elif inizio_riga x then (rimuovi_vuota [acc]) @ (rimuovi_vuota (spezza_tutto xs [x])) // Inizio di una riga
            elif fine_riga x && acc = [] then [[x]] @ (rimuovi_vuota (spezza_tutto xs [])) // Questo non dovrebbe mai succedere
            elif fine_riga x && ricerca coppie_terminatrici (primo_elemento acc, x) then [acc @ [x]] @ (rimuovi_vuota (spezza_tutto xs [])) // Fine di una riga
            else spezza_tutto xs (acc @ [x])
    let rec riunisci linee maxlen = // Riunisco le righe rispettando la lunghezza massima
        match linee with
        | [] -> []
        | [x] -> [x]
        | x::y::xs ->
            if fine_riga (ultimo_elemento x) && not (inizio_riga (primo_elemento y)) then // Posso provare ad unire due righe
                let lunghezza_insieme = 1 + lunghezza_stringa (linea_in_stringa x) + lunghezza_stringa (linea_in_stringa y) // Conto anche lo spazio che andrei ad aggiungere
                if lunghezza_insieme <= maxlen then [(x @ y)] @ riunisci xs maxlen // Posso unire
                else x :: riunisci (y::xs) maxlen // Devo lasciarle separate
            else x :: riunisci (y::xs) maxlen
    let rec controlla linee maxlen = // Funzione principale
        match linee with
        | [] -> []
        | x::xs ->
            let linee_spezzate = spezza_tutto (tokenize x) [] // Spezzo il più possibile
            let linee_riunite = riunisci linee_spezzate maxlen // Ricompongo
            let stringhe_unite = linee_in_stringhe linee_riunite // Trasformo le liste in stringhe
            stringhe_unite @ controlla xs maxlen
    let rec rimuovi_doppi_vuoti lista = // Funzione per rimuovere due righe vuote consecutive
        match lista with
        | [] -> []
        | [x] -> [x] // Ultimo elemento
        | x::y::xs ->
            if x <> "" || y <> "" then x :: rimuovi_doppi_vuoti (y :: xs) // Una delle due righe ha contenuto
            else rimuovi_doppi_vuoti (y :: xs)
    let linee_spezzate = controlla (dividi_linee s) w // Spezzo le linee
    rimuovi_doppi_vuoti linee_spezzate // Rimuovo linee vuote di troppo

// STRUTTURA DATI PER LO STORICO:
// Lista di coppie (indentazione, tokens)
// Esempio:
// let rec aaa bbb =
//     if bbb > 4 then true
//     elif bbb < -7 then true
//     else false
// Viene memorizzato nello storico con:
// (0, ["let"; "rec"; "aaa"; "bbb"; "="])
// (1, ["if"; "bbb"; ">"; "4"; "then"; "true"])
// (1, ["elif"; "bbb"; "<"; "-7"; "then"; "true"])
// (1, ["else"; "false"])
let rec indent (lines : string list) =
    let ultima_indentazione lista = // Restituisce l'ultimo livello di indentazione dello storico
        if lista = [] then 0
        else primo (ultimo_elemento lista)
    let trova_ultimo_if_aperto lista = // Funzione per trovare l'indentazione dell'ultimo if/elif rimasto aperto
        let rec cerca lista aperti =
            match lista with
            | [] -> aperti
            | x::xs ->
                if (secondo x) = [] then cerca xs [] // Riga vuota, azzero lo stack
                elif primo_elemento (secondo x) = "else" then cerca xs (rimuovi_primo_elemento aperti) // else: rimuovo il primo elemento dallo stack
                elif primo_elemento (secondo x) = "if" then cerca xs ((primo x) :: aperti) // if: aggiungo l'indentazione allo stack
                else cerca xs aperti
        let aperti = cerca lista [] // Ricerca dall'inizio
        in
            if aperti = [] then 0 // Questo non dovrebbe mai succedere
            else primo_elemento aperti // L'ultimo if/elif che è rimasto aperto
    let trova_ultimo_match_aperto lista = // Funzione per trovare l'indentazione dell'ultimo match rimasto aperto
        let rec cerca lista ultimo_livello =
            match lista with
            | [] -> 0
            | x::xs ->
                if (secondo x) = [] then 0 // Riga vuota: mi fermo (non dovrebbe mai succedere)
                elif primo_elemento (secondo x) = "|" && ultimo_elemento (secondo x) = "->" && primo x < ultimo_livello then primo x // Ho trovato il match giusto: mi fermo
                else cerca xs ultimo_livello
        let ultima_riga = secondo (ultimo_elemento lista)
        in
            if ultima_riga = [] then 0 // Questo non dovrebbe mai succedere
            elif primo_elemento ultima_riga = "|" || primo_elemento ultima_riga = "match" then ultima_indentazione lista // Se la riga precedente è un caso certo non faccio la ricerca
            else cerca (inverti lista) (ultima_indentazione lista) // Ricerca all'indietro
    let trova_ultimo_let lista = // Funzione per trovare l'indentazione dell'ultimo let rimasto aperto
        let rec cerca lista ultimo_livello =
            match lista with
            | [] -> 0
            | x::xs ->
                if (secondo x) = [] then 0 // Riga vuota: mi fermo (non dovrebbe mai succedere)
                elif primo_elemento (secondo x) = "in" && primo x < ultimo_livello then cerca xs (primo x) // in esplicito: ci sono altri let in mezzo, aggiorno il livello di ricerca
                elif not (inizio_riga (primo_elemento (secondo x))) && primo x < ultimo_livello then cerca xs (primo x) // in implicito: ci sono altri let in mezzo, aggiorno il livello di ricerca
                elif primo_elemento (secondo x) = "let" && ultimo_elemento (secondo x) = "=" && primo x < ultimo_livello then primo x // Ho trovato il let giusto mi fermo
                else cerca xs ultimo_livello
        cerca (inverti lista) (ultima_indentazione lista) // Ricerca all'indietro
    let trova_nuova_indentazione linea lista = // Calcolo dell'indentazione
        if lista = [] then 0 // La prima linea (lo storico è vuoto) ha indentazione 0
        else
            let lineaprec = secondo (ultimo_elemento lista)
            let indentazione =
                if linea = [] || lineaprec = [] then 0 // Riga vuota o riga dopo una riga vuota
                elif ultimo_elemento lineaprec = "=" then 1 + ultima_indentazione lista // let rimasto aperto
                elif ultimo_elemento lineaprec = "then" then 1 + ultima_indentazione lista // if/elif rimasto aperto
                elif ultimo_elemento lineaprec = "else" then 1 + ultima_indentazione lista // else rimasto aperto
                elif ultimo_elemento lineaprec = "->" then 1 + ultima_indentazione lista // match/fun rimasto aperto
                elif ultimo_elemento lineaprec = "in" then 1 + ultima_indentazione lista // in rimasto aperto
                elif primo_elemento linea = "elif" || primo_elemento linea = "else" then trova_ultimo_if_aperto lista // Cerco l'if/elif corrispondente
                elif primo_elemento linea = "|" then trova_ultimo_match_aperto lista // Cerco il match corrispondente
                elif primo_elemento lineaprec = "|" && ultimo_elemento lineaprec <> "->" then trova_ultimo_let lista // Dopo un match completo ritorno all'ultimo let
                elif primo_elemento lineaprec = "else" && ultimo_elemento lineaprec <> "else" then trova_ultimo_let lista // Dopo un else completo ritorno all'ultimo let
                elif primo_elemento lineaprec = "in" && ultimo_elemento lineaprec <> "in" then trova_ultimo_let lista // Dopo un in esplicito ritorno all'ultimo let
                elif primo_elemento lineaprec = "fun" && ultimo_elemento lineaprec <> "fun" then trova_ultimo_let lista // Dopo un fun completo ritorno all'ultimo let
                elif not (inizio_riga (primo_elemento lineaprec)) then trova_ultimo_let lista // Dopo un in implicito ritorno all'ultimo let
                else ultima_indentazione lista
            indentazione
    let rec indenta linee lista = // Funzione principale
        match linee with
            | [] -> []
            | s::ss ->
                let tokens = tokenize s // Tokenizzo la linea
                let nuova_indentazione = trova_nuova_indentazione tokens lista // Calcolo l'indentazione
                (nuova_indentazione, linea_in_stringa tokens) :: indenta ss (lista @ [(nuova_indentazione, tokens)])
    indenta lines []