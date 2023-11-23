open Objects
open Logs

let ogit_init () =   

let a = Sys.command ("mkdir .ogit .ogit/objects .ogit/logs || .ogit/ && touch .ogit/HEAD")  in
match a with 
|0 ->   set_head [store_commit (init_commit())]
| _ -> invalid_arg "le dossier .ogit est existe déja !";;
   
let ogit_commit _msg  = 
  set_head [store_commit (make_commit _msg (store_work_directory()))] ;;


let ogit_checkout _hash = 
let test =( try read_commit(Digest.from_hex(_hash)) with _ -> invalid_arg "hash inconnu !" )     in
clean_work_directory () ;
restore_work_directory (read_directory_object(test.content) );
let x , i , y = ref 0 ,ref 0 , "../repo/.ogit/logs/" in
let elements =Sys.readdir y in 
       while (( !i <= ((Array.length elements) - 1)) && (!x =0 )) do
     if (( Digest.to_hex ((((read_commit(Digest.from_hex(elements.(!i)))).content))) =Digest.to_hex(test.content))) 
      then (set_head[Digest.from_hex(elements.(!i))] ;x := 1 ; i := !i + 1 ;) 
    else (i := !i + 1 ; let _ = Format.printf "  rien !  " in () )
   done ;  
  ;;

  

let ogit_log () = 
  let lis ,m,c = ref [] ,ref 0 ,ref ((read_commit (List.hd (get_head()))),(List.hd (get_head())))  in 
   while (!m =0) do 
   lis :=  !c :: !lis ;
   c :=   read_commit (List.hd ((fst(!c)).parents)) ,(List.hd ((fst(!c)).parents)) ;
   if ((List.hd ((fst(!c)).parents)) = (Digest.string "" )) then m :=1 
   done ; lis :=  !c :: !lis ; 
    let tab = Array.of_list !lis in 
     for i=0 to ((Array.length tab) - 1) do

      let _ = Format.printf "la date de commit est : %s , message = %s , hash du commit = %s \n" 
      (date_fm ((fst(tab.(i))).date)) ((fst(tab.(i))).message) (Digest.to_hex(snd (tab.(i)))) in ()
done ;
;;


(* ogit_commit "deuxieme comit !! " ;; *)
(* ogit_log () ;; *)
(* ogit_checkout "44e0f2621f51486967b5e8c46ab35e25" ;; *)
ogit_init ();;

let ogit_merge _hash = failwith "TODO"


(*(Directory [".ogit",true,"",Directory["objects",true,"",Directory[]];
  "logs",true,"",Directory[(Digest.to_hex(Digest.string "")),false,"",Text ""]]) 
*)