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

module Make (SubscriptionManager : SubscriptionManager) : sig
  val on_recv : SubscriptionManager.t ->
    ?keepalive:int ->
    subscribe:(variables:Graphql_lwt.Schema.variables ->
               ?operation_name:string ->
               string ->
               ([< `Response of Yojson.Basic.json
                | `Stream of ((Yojson.Basic.json, Yojson.Basic.json) result Lwt_stream.t) * (unit -> unit)],
                Yojson.Basic.json) result Lwt.t) ->
    push_to_websocket:(Websocket.Frame.t option -> unit) Lazy.t ->
    Websocket.Frame.t ->
    unit
end
