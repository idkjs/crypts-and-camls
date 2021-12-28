(* open Yojson.Basic *)
(* open Library *)
(* open Sys *)

let ()=
  let parse_enemy (j:Yojson.Basic.t)=match j with
    | `Assoc obj -> let e=(new Library.enemy) in begin List.iter (fun (a:string*Yojson.Basic.t) -> match a with
        | (k,`String v) when k="name" -> e#set_name v
        | (k,`Int i) when k="health" -> e#set_health i
        | (k,`Int i) when k="damage" -> e#set_dmg i
        | _ -> raise (Yojson.Json_error "Invalid JSON")
      ) obj;e end
    | _ -> raise (Yojson.Json_error "Invalid JSON") in
  let rec parse_room (room:Library.room) (j:Yojson.Basic.t)= match j with
    | `Assoc obj -> List.iter (fun (a:string * Yojson.Basic.t) -> match a with
      | (k,`String v) when k="name" -> room#set_name v
      | (k,`List v) when k="messages" -> List.iter (fun (a:Yojson.Basic.t) -> match a with `String s -> room#add_msg s | _ -> raise (Yojson.Json_error "Invalid JSON")) v
      | (k,`List v) when k="rooms" -> List.iter (fun (a:Yojson.Basic.t) -> room#add_room (parse_room (new Library.room) a)) v
      | (k,`List v) when k="enemies" -> List.iter (fun (a:Yojson.Basic.t) -> room#add_enemy (parse_enemy a)) v
      | _ -> raise (Yojson.Json_error "Invalid JSON")
    ) obj;room
    | _ -> raise (Yojson.Json_error "Invalid JSON") in
  let j=(Yojson.Basic.from_file ("adventures/"^(Sys.argv.(1))^".json")) in
  let r=new Library.room in (parse_room r j)#enter
