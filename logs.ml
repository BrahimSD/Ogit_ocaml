open Objects

type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
let date_fm _d = 
    let date = Unix.gmtime _d in 
    let retour = Printf.sprintf ("%s%d:%s%d:%s%d-%s%d/%s%d/%d") (if (date.tm_hour+1 >=10 ) then "" else "0" ) (date.tm_hour +1) (if (date.tm_min >=10 ) then "" else "0" ) date.tm_min 
    (if (date.tm_sec >=10 ) then "" else "0" ) date.tm_sec 
   (if (date.tm_mday >=10 ) then "" else "0" )  date.tm_mday 
   (if (date.tm_mon+1 >=10 ) then "" else "0" ) (date.tm_mon +1) (date.tm_year +1900) 
   in retour

let get_head () =   
   let entrer = open_in ".ogit/HEAD" in
   try
   let liste = String.split_on_char ';'  (input_line entrer )   in
     (List.map (fun x -> Digest.from_hex x) liste) 
     
   with End_of_file -> close_in entrer ; [(Digest.string "")]

let set_head _l =
let re = Array.of_list _l in  
let sortie = open_out ".ogit/HEAD" in
for i =0 to ((Array.length re) -2) do
    Printf.fprintf sortie "%s"  ( Digest.to_hex(re.(i)) ^ ";") 
done;
    Printf.fprintf sortie "%s" (Digest.to_hex(re.((Array.length re) -1)));
    close_out sortie ;;


let make_commit  _s  _h =  
{
    parents = get_head();
    date = Unix.time()  ;
    message = _s ;
    content = _h 
} 

let init_commit () = make_commit "init commit" (store_work_directory())


let store_commit _c =

    let parents_=  (String.concat ";" ((List.map (fun x-> Digest.to_hex x) (_c.parents)))) in
    let ma_date_ = date_fm (_c.date) in 
    let message_ = _c.message in 
    let content_ = Digest.to_hex(_c.content) in 
    let ma_list = [parents_;ma_date_;message_;content_] in 
    let ma_chaine = String.concat "\n" ma_list in
    let fin = open_out ( "../repo/.ogit/logs/"^ Digest.to_hex(Digest.string ma_chaine)) in
     Printf.fprintf fin "%s" ma_chaine;
     close_out fin ;(Digest.string ma_chaine)


 let read_commit _h = 

    let chan = open_in ("../repo/.ogit/logs/" ^ Digest.to_hex _h) in  
    let xx = [|input_line chan;input_line chan;input_line chan;input_line chan|]  in
    let d f = List.map (fun x-> Digest.from_hex x) (String.split_on_char ';' f) in
    let rec dt f c = (let k =  String.split_on_char c f in match k  with 
    | [a;b] ->  (dt a ':') @ (dt b '/') |a -> List.map (fun y-> try (int_of_string  y) with _ -> invalid_arg (Printf.sprintf ("%s") y ) ) a )in 
    let cr l = fst (Unix.mktime { Unix.tm_hour = l.(0) ;  Unix.tm_min = l.(1); Unix.tm_sec = l.(2); 
        Unix.tm_mday = l.(3);  Unix.tm_mon = (l.(4) -1 );  Unix.tm_year = (l.(5) -1900); 
        Unix.tm_wday = 0; Unix.tm_yday = 0; Unix.tm_isdst = false }) in
    close_in chan;
    {
        parents = d (xx.(3))    ;
        date =   cr (Array.of_list (dt (xx.(2)) '-'))   ;
        message = xx.(1)  ;
        content =  Digest.from_hex(xx.(0)) 
        
    } 
    
 (* let a = Digest.to_hex(store_commit(read_commit (Digest.from_hex ("934258186f418bbb65825ae6c2d2af31"))))  *)
(*let _ = {
    parents = [Digest.string("heloo");Digest.string("whord!")];
    date = Unix.time();
    message = "mon premier code";
    content = Digest.string("ssss")}*)