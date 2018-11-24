module GQL = Graphql_lwt
let schemaDescription =
  (("{\"kind\":\"OBJECT\",\"name\":\"todo\",\"description\":null,\"fields\":[{\"name\":\"title\",\"description\":null,\"args\":[],\"type\":{\"kind\":\"NON_NULL\",\"name\":null,\"ofType\":{\"kind\":\"SCALAR\",\"name\":\"String\",\"ofType\":null}},\"isDeprecated\":false,\"deprecationReason\":null}],\"inputFields\":null,\"interfaces\":[],\"enumValues\":null,\"possibleTypes\":null}")
  [@reason.raw_literal
    "{\\\"kind\\\":\\\"OBJECT\\\",\\\"name\\\":\\\"todo\\\",\\\"description\\\":null,\\\"fields\\\":[{\\\"name\\\":\\\"title\\\",\\\"description\\\":null,\\\"args\\\":[],\\\"type\\\":{\\\"kind\\\":\\\"NON_NULL\\\",\\\"name\\\":null,\\\"ofType\\\":{\\\"kind\\\":\\\"SCALAR\\\",\\\"name\\\":\\\"String\\\",\\\"ofType\\\":null}},\\\"isDeprecated\\\":false,\\\"deprecationReason\\\":null}],\\\"inputFields\\\":null,\\\"interfaces\\\":[],\\\"enumValues\\\":null,\\\"possibleTypes\\\":null}"])
type json = Yojson.Basic.json
type 'ctx dynamic_type =
  | NonNull: {
  typ: ('ctx, 'a) GQL.Schema.typ ;
  resolve: 'ctx -> json -> 'a } -> 'ctx dynamic_type 
  | Nullable:
  {
  typ: ('ctx, 'a option) GQL.Schema.typ ;
  resolve: 'ctx -> json -> 'a option } -> 'ctx dynamic_type 
let typOfScalar =
  function
  | (("String")[@reason.raw_literal "String"]) ->
      ((Nullable
          ({
             typ = GQL.Schema.string;
             resolve =
               ((fun ctx ->
                   fun json -> Yojson.Basic.Util.to_string_option json))
           }))
      [@explicit_arity ])
  | (("Int")[@reason.raw_literal "Int"]) ->
      ((Nullable
          ({
             typ = GQL.Schema.int;
             resolve =
               ((fun ctx -> fun json -> Yojson.Basic.Util.to_int_option json))
           }))
      [@explicit_arity ])
  | (("Boolean")[@reason.raw_literal "Boolean"]) ->
      ((Nullable
          ({
             typ = GQL.Schema.bool;
             resolve =
               ((fun ctx -> fun json -> Yojson.Basic.Util.to_bool_option json))
           }))
      [@explicit_arity ])
  | _ -> raise Not_found
let generateField descr =
  let open Yojson.Basic in
    let x =
      (("{\"name\":\"title\",\"description\":null,\"args\":[],\n     \"type\":{\"kind\":\"NON_NULL\",\"name\":null,\"ofType\":{\"kind\":\"SCALAR\",\"name\":\"String\",\"ofType\":null}},\n     \"isDeprecated\":false,\"deprecationReason\":null}")
      [@reason.raw_literal
        "{\\\"name\\\":\\\"title\\\",\\\"description\\\":null,\\\"args\\\":[],\n     \\\"type\\\":{\\\"kind\\\":\\\"NON_NULL\\\",\\\"name\\\":null,\\\"ofType\\\":{\\\"kind\\\":\\\"SCALAR\\\",\\\"name\\\":\\\"String\\\",\\\"ofType\\\":null}},\n     \\\"isDeprecated\\\":false,\\\"deprecationReason\\\":null}"]) in
    let name =
      (descr |> (Util.member (("name")[@reason.raw_literal "name"]))) |>
        Util.to_string in
    match (descr |> (Util.member (("kind")[@reason.raw_literal "kind"]))) |>
            Util.to_string
    with
    | (("OBJECT")[@reason.raw_literal "OBJECT"]) ->
        GQL.Schema.obj name ~fields:(fun _ -> [])
type field_def =
  {
  name: string ;
  args: string list ;
  typ: string list ;
  key: string }
let rec fieldType typDesc =
  let open Yojson.Basic in
    match (typDesc |> (Util.member (("kind")[@reason.raw_literal "kind"])))
            |> Util.to_string
    with
    | (("NON_NULL")[@reason.raw_literal "NON_NULL"]) -> (("non_null")
        [@reason.raw_literal "non_null"]) ::
        (fieldType
           (typDesc |>
              (Util.member (("ofType")[@reason.raw_literal "ofType"]))))
    | (("SCALAR")[@reason.raw_literal "SCALAR"]) ->
        [(typDesc |> (Util.member (("name")[@reason.raw_literal "name"]))) |>
           Util.to_string]
    | (("LIST")[@reason.raw_literal "LIST"]) -> (("list")
        [@reason.raw_literal "list"]) ::
        (fieldType
           (typDesc |>
              (Util.member (("ofType")[@reason.raw_literal "ofType"]))))
    | _ ->
        raise ((Sys_error ((("fight me")[@reason.raw_literal "fight me"])))
          [@explicit_arity ])
let fieldDefOfJson descr =
  let open Yojson.Basic in
    let name =
      (descr |> (Util.member (("name")[@reason.raw_literal "name"]))) |>
        Util.to_string in
    let typ =
      (descr |> (Util.member (("type")[@reason.raw_literal "type"]))) |>
        fieldType in
    { name; typ; args = []; key = name }
let generateSchema descr =
  let open Yojson.Basic in
    match (descr |> (Util.member (("kind")[@reason.raw_literal "kind"]))) |>
            Util.to_string
    with
    | (("OBJECT")[@reason.raw_literal "OBJECT"]) -> ()
let x =
  (("{\n  \"name\": \"id\",\n  \"description\": null,\n  \"args\": [],\n  \"type\": {\n  \"kind\": \"NON_NULL\",\n  \"name\": null,\n  \"ofType\": {\n    \"kind\": \"SCALAR\",\n    \"name\": \"Int\",\n    \"ofType\": null\n  }\n},\n  \"isDeprecated\": false,\n  \"deprecationReason\": null\n}")
  [@reason.raw_literal
    "{\n  \\\"name\\\": \\\"id\\\",\n  \\\"description\\\": null,\n  \\\"args\\\": [],\n  \\\"type\\\": {\n  \\\"kind\\\": \\\"NON_NULL\\\",\n  \\\"name\\\": null,\n  \\\"ofType\\\": {\n    \\\"kind\\\": \\\"SCALAR\\\",\n    \\\"name\\\": \\\"Int\\\",\n    \\\"ofType\\\": null\n  }\n},\n  \\\"isDeprecated\\\": false,\n  \\\"deprecationReason\\\": null\n}"])
let string_of_field { name; typ } =
  let typStr = String.concat ((", ")[@reason.raw_literal ", "]) typ in
  name ^
    (((": ")[@reason.raw_literal ": "]) ^
       ((("[")[@reason.raw_literal "["]) ^
          (typStr ^ (("]")[@reason.raw_literal "]"]))))
;;print_endline
    (((Yojson.Basic.from_string x) |> fieldDefOfJson) |> string_of_field)