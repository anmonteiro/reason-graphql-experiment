module type SubscriptionManager = sig
  type t

  val subscriptions : t -> t (* (key * value) list *)

  val add : t -> string -> (unit -> unit) -> unit

  val create : int -> t

  val remove : t -> string -> unit

  val mem : t -> string -> bool

  val find_opt : t -> string -> (unit -> unit) option

  val iter : (string -> (unit -> unit) -> unit) -> t -> unit

  val clear : t -> unit
end

(* TODO: functorize over async / lwt – perhaps cohttp / httpaf websocket modules *)
module Make (SubscriptionManager : SubscriptionManager)  = struct
  open Websocket
  open Lwt
  module Json = Yojson.Basic.Util

  (* https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md *)
  type client_message =
    | Gql_connection_init
    | Gql_start
    | Gql_stop
    | Gql_connection_terminate
    (* not part of the protocol, used here to signal invalid messages *)
    | Invalid

  type server_message =
    | Gql_connection_error
    | Gql_connection_ack
    | Gql_data
    | Gql_error
    | Gql_complete
    (* | Gql_connection_keep_alive *)

  let client_message_of_payload payload_json =
    match payload_json |> (Json.member "type") |> Json.to_string with
    | "connection_init" -> Gql_connection_init
    | "start" -> Gql_start
    | "stop" -> Gql_stop
    | "connection_terminate" -> Gql_connection_terminate
    | _ -> Invalid

  let server_message_to_string = function
    | Gql_connection_error -> "connection_error"
    | Gql_connection_ack -> "connection_ack"
    | Gql_data -> "data"
    | Gql_error -> "error"
    | Gql_complete -> "complete"
  (* | Gql_connection_keep_alive -> "ka" *)

  let rec consume_stream stream cb =
    Lwt.catch
      (fun () ->
         Lwt_stream.next stream >>= fun x ->
         cb x;
         consume_stream stream cb)
      (fun _ -> Printf.eprintf "AMIGOS\n%!";Lwt.return_unit)

  let create_message ?(opcode=Frame.Opcode.Text) ?opId ?(payload=`Null) typ =
    let frame_payload = `Assoc [
        "type", `String (server_message_to_string typ);
        "id", begin match opId with
          | Some id -> `String id
          | None -> `Null end;
        "payload", payload
      ] in
    let json = Yojson.Basic.to_string frame_payload in
    Frame.create ~opcode ~content:json ()

  let on_recv mgr ?keepalive:_keepalive ~subscribe ~push_to_websocket frame =
    let push_to_websocket = Lazy.force push_to_websocket in
    let json = Yojson.Basic.from_string frame.Frame.content in
    match client_message_of_payload json with
    | Gql_connection_init ->
      (* TODO: allow a user-defined `on_connect` handler *)
      (* TODO: check for `graphql-ws` in the request headers, otherwise terminate connection *)
      push_to_websocket (Some (create_message Gql_connection_ack));
    | Gql_start ->
      let opId = Json.(json |> member "id" |> to_string) in
      let payload_json = Json.member "payload" json in
      let query = Json.(payload_json |> member "query" |> to_string) in
      let variables =
        try
          Json.(payload_json |> member "variables" |> to_assoc)
        with | _ -> [] in
      let operation_name = Json.(payload_json |> member "operationName" |> to_string_option)
      in
      (* TODO: unsubscribe if there's a subscription with the same id *)
      let result = subscribe ~variables:(variables :> (string * Graphql_parser.const_value) list) ?operation_name query in
      result >>= (function
          | Error message ->
            let payload = `Assoc ["message", message] in
            push_to_websocket (Some (create_message ~payload ~opId Gql_error));
            Lwt.return_unit
          | Ok (`Response json) ->
            push_to_websocket (Some (create_message ~opId ~payload:json Gql_data));
            Lwt.return_unit
          | Ok (`Stream (stream, destroy_stream)) ->
              let close_stream = (fun () -> destroy_stream ();
                                   Printf.eprintf "CLOSE CLLAED \n%!")
            in
            SubscriptionManager.add mgr opId close_stream;
            Lwt.finalize
              (fun () ->
                 consume_stream stream
                   (fun x ->
                      let Ok x | Error x = x in
                      (* XXX: OGS doesn't yet have a way of effectively killing
                       * a stream – so if we've been asked to unsubscribe, don't
                       * push the execution result to the websocket. *)
                      if SubscriptionManager.mem mgr opId then
                        push_to_websocket (Some (create_message ~opId ~payload:x Gql_data))))
              (fun () ->
                 (if SubscriptionManager.mem mgr opId then
                    push_to_websocket (Some (create_message ~opId Gql_complete)));
                 Lwt.return_unit)) |> Lwt.ignore_result
    | Gql_stop ->
      let opId = Json.(json |> member "id" |> to_string) in
      begin match SubscriptionManager.find_opt mgr opId with
        | None -> ()
        | Some unsubscribe -> unsubscribe()
      end;
      SubscriptionManager.remove mgr opId;
    | Gql_connection_terminate ->
      SubscriptionManager.iter (fun _ f -> f ()) mgr;
      SubscriptionManager.clear mgr;
      push_to_websocket (Some (create_message ~opcode:Frame.Opcode.Close Gql_connection_error))
    | Invalid ->
      let opId = Json.(json |> member "id" |> to_string) in
      let payload = `Assoc ["message", `String "Invalid message type!"] in
      push_to_websocket (Some (create_message ~opId ~payload Gql_error));
end
