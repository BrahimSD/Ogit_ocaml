
type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list


let rec concatenate dir = match dir with
| [] -> ""
|(a,b,c,_)::tl ->  let x= if b then ";d;" else ";t;" in
  if tl != [] then a ^ x ^ Digest.to_hex c ^ "\n"^ concatenate tl else  (a ^ x ^ Digest.to_hex c )


let rec hash _obj = 
  let h f  =  if ((((String.length f)-1) >= 0) && f.[(String.length f)-1] = ' ') then (f ^ "\n") else f  in
  match _obj with
| Text a -> Digest.string  (h (a))
| Directory l ->  
            let chaine = Text (concatenate l) in hash chaine  ;;

let is_known _h = 
  let ch = ".ogit/objects/" ^ Digest.to_hex (_h) in
   (Sys.file_exists ch)

let store_object _obj =
  let oc = open_out (".ogit/objects/"^ Digest.to_hex (hash _obj)) in
  let s =function 
  |Text text -> text 
  |Directory l -> concatenate l in
    Printf.fprintf oc "%s" (s _obj);
  close_out oc ; hash _obj 
  

  let read_text tx = 
    let line = ref "" in
let c = open_in tx in
try
  while true; do
    if (!line = "") then
    line := (input_line c) else 
    line :=(!line)  ^"\n"^ (input_line c)
  done; !line
with End_of_file ->
  close_in c;
   !line ;;

  let read_text_object h = read_text (".ogit/objects/" ^ Digest.to_hex (h))

let store_work_directory () = 
  let rec recursive a = 
  let elements =Sys.readdir a in
  let k = ref [] in
  for i=0 to ((Array.length elements) - 1) do
        if  (Sys.is_directory (a ^ elements.(i)) && (elements.(i)).[0] != '.' ) 
          then
          k :=   (elements.(i),true,( store_object(Directory(recursive (a^ elements.(i) ^ "/")))),Text "") :: !k
        else if ((elements.(i)).[0] != '.') 
          then
          k :=   
    (elements.(i),false,(store_object (Text (read_text (a^(elements.(i)))))),Text "") :: !k 
          done ; List.rev (!k) 
   in (store_object(Directory (recursive ("../repo/")))) ;;
 
let rec read_directory_object  _h = 
   let tr l = Directory l in 
    let lines = ref [] in
    let chan = open_in (".ogit/objects/" ^ Digest.to_hex _h) in
    let h f  = if f.[(String.length f)-1] = '\r' then
       String.sub f 0 ((String.length f) -1) else f in
    let d f =  let k = String.split_on_char ';' f in
    match k with 
    |[a;b;c] -> let (x,y) = if b = "t" then false ,Text (read_text_object (Digest.from_hex (c)))
    else  true ,  read_directory_object (Digest.from_hex (c)) in  (a,x,Digest.from_hex c,y)
    | _  -> invalid_arg ""
     
    in try
   while true; do
   lines := (d (h ( input_line chan ))):: !lines
   done; tr (List.rev !lines )
   with End_of_file ->
   close_in chan ;tr (List.rev !lines ) 


  let clean_work_directory () =
  let rec recursiv a =
 let elements =Sys.readdir a in 
 for i=0 to ((Array.length elements) - 1) do
    match  (Sys.is_directory (a ^ elements.(i)) && (elements.(i)).[0] != '.' ) with
   |true ->
     recursiv (a^ elements.(i) ^ "/")  ;
    Unix.rmdir (a^ elements.(i) ^ "/") 
   |false -> 
     if  not (Sys.is_directory (a ^ elements.(i)))
     then Sys.remove (a^ elements.(i)) 
   done ; in recursiv "../repo/"


 let restore_work _obj emplacement = 
    let rec recursive objet emplacement  =
      match objet with 
    | Directory l -> 
      let tab = Array.of_list l in 
       for i=0 to ((Array.length tab) - 1) do
              match  tab.(i) with | (a,b,_,d) -> 
                if  b then 
                   let nvemp = emplacement  ^ a ^ "/"  in 
                   let _ = Sys.command ((Printf.sprintf ("mkdir %s") nvemp)) in
                  ( if d = Directory [] then () else recursive d nvemp )
                else
                    let text = function |Text t -> t |Directory _ -> invalid_arg "c'est pas du text"  in 
                    let occ = open_out (emplacement ^ a) in
                    Printf.fprintf occ "%s" (text d) ; close_out occ 
        done ;   
                
   | Text _ ->  invalid_arg "y a pas d'information , que du text !"    
                
in  recursive  _obj emplacement ;;      

let restore_work_directory _obj = 
     restore_work _obj "" ;;
let merge_work_directory_I t  = 
    let resu = ref (true ) in
   let rec recursive objet emplacement  = 
   
   let el = (try Sys.readdir emplacement with Sys_error _-> invalid_arg "erreur" )
   in  match objet with 
     | Text _ ->  () | Directory l ->  match l with | [] -> ()
     |  ((a,b,_,d)::tl )->  (
      if ((not b) &&  (( not (Array.mem a el)) ||  (Sys.is_directory (emplacement ^ a ) ) ) ) then
     (let oc = open_out (emplacement^ a) in
     Printf.fprintf oc "%s" (match d with |Directory _ -> invalid_arg "une erreur" |Text t ->t ) ; close_out oc  )
      else if ((not b) && ((Array.mem a el))  && not (Sys.is_directory (emplacement ^ a )) ) then
      ( if not (hash ( Text ( read_text (emplacement^ a))) = hash d ) then 
       (let oc = open_out (emplacement^ a^ (Printf.sprintf ("%d") (Array.length el))) in
     Printf.fprintf oc "%s" (match d with |Directory _ -> invalid_arg "une erreur" |Text t ->t ) ;
      close_out oc ;   resu := false ))
      
      else if (b && (( not (Array.mem a el)) || not (Sys.is_directory (emplacement ^ a ) ) ) ) then
        ( let _= Sys.command ((Printf.sprintf ("mkdir %s") (emplacement ^ a ))) in
          restore_work d (emplacement ^ a ^ "")  )
      else if (b &&  (Array.mem a el) && (Sys.is_directory (emplacement ^ a )) ) then ( recursive d (emplacement ^ a ^ "/") )
     )
        ; if (tl != []) then recursive (Directory (tl)) emplacement   
    in recursive t "../repo/" ; !resu  ;; 
(* let mer = merge_work_directory_I (Directory ["fooo",false,"aaa" ,Text " 3"]);;*)
(*let vb = store_work_directory ();;*)
(* let rr = restore_work_directory (Directory ["un",false,"g",Text "hh"]);; *)

(* let n = read_directory_object ( Digest.from_hex "9ef6d96530cb3791f0dfb16d157220b6");; *)