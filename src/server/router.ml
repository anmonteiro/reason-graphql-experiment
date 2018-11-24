module Arg = struct
  let id: 'a. 'a -> 'a = fun x -> x

  type _ arg_typ =
    | Scalar : {
        coerce: string -> 'a
      } -> 'a arg_typ
  and 'a arg =
    { name: string; typ: 'a arg_typ}

  and (_, _) arg_list =
    | [] : ('a, 'a) arg_list
    | (::) : 'a arg * ('b, 'c) arg_list -> ('b, 'a -> 'c) arg_list

  and any_arg =
    AnyArg : _ arg -> any_arg

  let length xs =
    let rec length_loop: type a b. int -> (a, b) arg_list -> int =
      fun acc xs -> match xs with
    | [] -> acc
    | _::xs -> length_loop (acc + 1) xs
    in
    length_loop 0 xs

  let isEmpty: type a b. (a, b) arg_list -> bool = function
    | [] -> true
    | _ -> false

  let map f xs =
    let rec map_loop: type a b. 'x -> 'y list -> (a, b) arg_list -> 'y list = fun f acc xs ->
      match xs with
    | y::ys -> map_loop f (List.cons (f (AnyArg y)) acc) ys
    | [] -> List.rev acc
    in
  map_loop f [] xs

  let int = Scalar { coerce = int_of_string }
  let float = Scalar { coerce = float_of_string }
  let string = Scalar { coerce = id }
  let bool = Scalar {coerce = bool_of_string }
  let char = Scalar {coerce = (fun s -> s |> int_of_string |> char_of_int)}
end

type _ route =
  | Simple : {
      path: string;
      target: 'f;
      args: ('r, 'f) Arg.arg_list;
      (* resolve: 'r -> 'args; *)
    } -> 'r route
  (* | Nested : 'r route -> 'r list route *)

let route ~args ~path ~target =
  Simple { path; target; args;}

type labels = Text of string | Label of string

let collectLabels path =
  let rec collectFromSplit acc split =
    match split with
    | (Str.Delim ":")::(Text lbl)::xs -> collectFromSplit ((Label lbl)::acc) xs
    | (Text txt)::xs -> collectFromSplit ((Text txt)::acc) xs
    | _::xs -> collectFromSplit acc xs
    | [] -> List.rev acc
  in
  let split = Str.full_split (Str.regexp ":\\|/") path in
  collectFromSplit [] split

let matchAndVariables p (pdef, args) =
  let rec matchLoop args_acc p pdef args = match p, pdef, args with
  | x::xs, (Text y)::ys, _ ->
      if x = y then
        matchLoop args_acc xs ys args
      else (false, [])
  | x::xs, (Label y)::ys, a::ars ->
      if y = a then
        matchLoop ((y, x)::args_acc) xs ys ars
      else (false, [])
  | [], _::_, _ | _::_, [], _ | [], [], _::_ | _, Label _::_, [] -> (false, [])
  | [], [], [] -> (true, args_acc)
  in
  matchLoop [] p pdef args

type variable_map = (string * string) list

let rec eval_arglist: type a b. variable_map -> (a, b) Arg.arg_list -> b -> a =
  fun variables arglist f ->
    match arglist with
    | Arg.[] -> f
    | Arg.({name; typ; _}::xs) ->
        let value = List.assoc name variables in
        let coerced = eval_arg typ value in
        eval_arglist variables xs (f coerced)

and eval_arg: type a. a Arg.arg_typ -> string -> a =
  fun typ value ->
    let Arg.Scalar s = typ in
    s.coerce value


let visualizeLabels lbls =
  let visualizeLabel lbl = match lbl with | Text x | Label x -> x in
  String.concat "|" (List.map visualizeLabel lbls)

let rec matchRoute =
  fun routes p -> match routes with
    | [] -> None
    | x::xs -> begin match x with
        | Simple {path; args; target} ->
            if Arg.isEmpty args then begin
              if path = p then
                let res = eval_arglist [] args target in
                Some res
              else matchRoute xs p end
            else
              let labels = collectLabels path in
              let split = p |> Str.split (Str.regexp "/")
                |> List.filter (fun str -> String.length str > 0)
              in
              begin if List.length labels != List.length split then
                matchRoute xs p
              else
                let argLabels = Arg.map (fun (Arg.AnyArg ({name})) -> name) args in
                let isMatch, variables = matchAndVariables split (labels, argLabels) in
                 if isMatch then
                  let res = eval_arglist variables args target in
                  Some res
                  else matchRoute xs p
                end
    end

let matchRoute_exn rs p =
  match matchRoute rs p with
  | None -> raise Not_found
  | Some route -> route

let composePath lbls vars =
  let rec composePath_loop acc lbls =
    match lbls with
    | (Text x)::xs -> composePath_loop (acc ^ "/" ^ x) xs
    | (Label lbl)::xs ->
        let var = List.assoc lbl vars in
        composePath_loop (acc ^ "/" ^ var) xs
    | [] -> acc
  in
  composePath_loop "" lbls

let rec pathFor ?(variables=[]) rs handler =
  match rs with
  | (Simple {target; path; args})::xs ->
      if Arg.isEmpty args then begin
        let resulting_handler = eval_arglist [] args target in
        if resulting_handler = handler then
          Some path
        else
          pathFor ~variables xs handler
      end
      else if List.length variables != Arg.length args then
        pathFor ~variables xs handler
      else begin
        let lbls = collectLabels path in
        Some (composePath lbls variables)
      end
  | [] -> None

let pathFor_exn ?(variables=[]) rs handler =
  match pathFor ~variables rs handler with
  | None -> raise Not_found
  | Some x -> x

type target = Home | About | Contact | Product of int

let targetToStr = function
  | Home -> "Home"
  | About -> "About"
  | Contact -> "Contact"
  | Product id -> Printf.sprintf "Product (%d)" id

let myRoutes = [
  route ~args:Arg.[] ~path:"/" ~target:Home;
  route ~args:(Arg.[{name="id"; typ=int}]) ~path:"/users/:id" ~target:(fun _foo -> About);
  route ~args:(Arg.[{name="id"; typ=int}]) ~path:"/products/:id" ~target:(fun id -> Product id)
]

(*
 *
 * let () = Printf.eprintf "Route: %s\n" (targetToStr (matchRoute_exn myRoutes "/"))
 * let tgt = matchRoute_exn myRoutes "/users/42"
 * let () = Printf.eprintf "Route: %s\n" (targetToStr tgt)
 * let () = Printf.eprintf "Path for: '%s'\n" (pathFor_exn myRoutes Home)
 * let () = Printf.eprintf "Path for (About with variables): '%s'\n" (pathFor_exn ~variables:[("id", "Toine")] myRoutes About)
 *
 *)
(*
 * let () = Printf.eprintf "Route and path for product: '%s' '%s'\n"
 *   (targetToStr (matchRoute_exn myRoutes "/products/42"))
 *   (pathFor_exn ~variables:[("id", "42")] myRoutes (Product 42))
 *)
(* Trailing slashes *)
let () = Printf.eprintf "Trailing: '%s'\n" (targetToStr (matchRoute_exn myRoutes "/products/42//"))
(* let () = Printf.eprintf "antoine: %s\n" (visualizeLabels (collectLabels "/profucts/:var//")) *)

(* print_endline (match match_route routes "/" with
 *     | Single {target} when target = Home -> "Home"
 *     | Single {target} when target = About -> "About"
 *     | _ -> "lol") *)
(* type route(_) = {name: string}
 *
 * and route_list(_, _) =
 *   | []: route_list(a, a)
 *   | ::(route('a), route_list('b, 'c)): route_list('b, 'a => 'c);
 *
 * let routes = []; /* [["/", ["foo", `Foo]]] */ *)
