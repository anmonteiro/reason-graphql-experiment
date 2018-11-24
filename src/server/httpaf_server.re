open Httpaf;
open Lwt.Infix;

module Websocket = Websocket_httpaf_lwt;

module GraphQLSchema = Graphql_lwt.Schema;

let client = ref([]);

let serveStatic = (reqd, base, path) => {
  let fname = Filename.concat(base, path);
  if (Sys.file_exists(fname)) {
    if (Sys.is_directory(fname)) {
      if (Sys.file_exists(Filename.concat(fname, "index.html"))) {
        Httpaf_utils.respond_with_file(
          reqd,
          ~fname=Filename.concat(fname, "index.html"),
        );
      } else {
        Lwt.return(
          Reqd.respond_with_string(reqd, Response.create(`Not_found), "not found 1"),
        );
      };
    } else {
      Httpaf_utils.respond_with_file(reqd, ~fname);
    };
  } else if (Sys.file_exists(fname ++ ".html")) {
    Httpaf_utils.respond_with_file(reqd, ~fname=fname ++ ".html");
  } else {
    Lwt_io.printlf("Couldn't find file (%s) to serve statically", path) >>= () =>
    Lwt.return(
      Reqd.respond_with_string(
        reqd,
        Response.create(
          `Not_found,
          ~headers=Headers.of_list([("Connection", "close")]),
        ),
        "not found 2",
      ),
    );
  };
};

let json_err =
  fun
  | Ok(_) as ok => ok
  | Error(err) => Error(`String(err));

let execute_query = (ctx, schema, ~variables, ~operation_name=?, query) =>
  Lwt_result.(
    Lwt.return @@
    json_err @@
    Graphql_parser.parse(query)
    >>= (
      doc =>
        GraphQLSchema.execute(schema, ctx, ~variables, ~operation_name?, doc)
    )
  );

let execute_request = (reqd, ctx, schema) => {
  let request = Reqd.request(reqd);
  Printf.eprintf(
    "version: %s; headers: %s\n%!",
    Version.to_string(request.version),
    Headers.to_string(request.headers),
  );
  let request_body = Reqd.request_body(reqd);

  let response_content_type =
    switch (Headers.get(request.headers, "Content-Type")) {
    | Some(request_content_type) => request_content_type
    | None => "application/octet-stream"
    };

  let body_str = ref("");
  let on_eof = () => {
    let json = Yojson.Basic.from_string(body_str^);
    let query = Yojson.Basic.(json |> Util.member("query") |> Util.to_string);
    let variables =
      try (Yojson.Basic.Util.(json |> member("variables") |> to_assoc)) {
      | _ => []
      };
    Printf.eprintf("Query: %s\n%!", query);
    let vars = (variables :> list((string, Graphql_parser.const_value)));
    let result = execute_query(ctx, schema, ~variables=vars, query);
    result
    >>= (
      fun
      | Ok(response) =>
        switch (response) {
        | `Response(data) =>
          let body = Yojson.Basic.to_string(data);
          let response =
            Response.create(
              ~headers=
                Headers.of_list([
                  ("Content-Type", response_content_type),
                  ("Content-length", body |> String.length |> string_of_int),
                ]),
              `OK,
            );

          let response_body = Reqd.respond_with_streaming(reqd, response);
          Body.write_string(response_body, body);
          Lwt.return(
            Body.flush(response_body, () => Body.close_writer(response_body)),
          );
        | `Stream(_stream) => assert(false)
        }
      | Error(err) =>
        Lwt.return(
          Httpaf_utils.respond_with_error(reqd, Yojson.Basic.to_string(err)),
        )
    ) |> ignore;
  };
  let rec on_read = (request_data, ~off, ~len) => {
    let read = Httpaf.Bigstring.to_string(~off, ~len, request_data);
    body_str := body_str^ ++ read;
    Body.schedule_read(request_body, ~on_read, ~on_eof);
  };
  Body.schedule_read(request_body, ~on_read, ~on_eof);
};


/* let execute_request = (reqd, ctx, schema) => {
  let request = Reqd.request(reqd);
  Lwt_io.eprintlf(
    "version: %s; headers: %s",
    Version.to_string(request.version),
    Headers.to_string(request.headers),
  )
  |> ignore;
  let request_body = Reqd.request_body(reqd);

  let response_content_type =
    switch (Headers.get(request.headers, "Content-Type")) {
    | Some(request_content_type) => request_content_type
    | None => "application/octet-stream"
    };

  let response =
    Response.create(
      ~headers=Headers.of_list([("Content-Type", response_content_type)]),
      `OK,
    );

  let response_body = Reqd.respond_with_streaming(reqd, response);

  let rec respond = () =>
    Body.schedule_read(
      request_body,
      ~on_eof=() => Body.close_writer(response_body),
      ~on_read=
        (request_data, ~off, ~len) => {
          let body_str = Httpaf.Bigstring.to_string(~off, ~len, request_data);
          [>Lwt_io.printlf("Body: %s", body_str) |> ignore;<]
          let json = Yojson.Basic.from_string(body_str);
          let query =
            Yojson.Basic.(json |> Util.member("query") |> Util.to_string);
          let vars =
            try (Yojson.Basic.Util.(json |> member("variables") |> to_assoc)) {
            | _ => []
            };
          [>Lwt_io.printlf("Query: %s", query) |> ignore;<]
          let result =
            execute_query(
              ctx,
              schema,
              ~variables=(
                vars :> list((string, Graphql_parser.const_value))
              ),
              query,
            );
          result
          >>= (
            fun
            | Ok(response) =>
              switch (response) {
              | `Response(data) =>
                let body = Yojson.Basic.to_string(data);
                Lwt.return(Body.write_string(response_body, body));
              | `Stream(_stream) => assert(false)
              }
            | Error(err) =>
              Lwt.return(
                Httpaf_utils.respond_with_error(
                  reqd,
                  Yojson.Basic.to_string(err),
                ),
              )
          )
          |> ignore;

          respond();
        },
    );

  respond();
};
 */
module MySubscriptionManager = {
  type t = Hashtbl.t(string, unit => unit);

  let subscriptions = t => t;

  let add = Hashtbl.add;

  let create = n => Hashtbl.create(n);

  let remove = Hashtbl.remove;

  let iter = Hashtbl.iter;

  let find_opt = Hashtbl.find_opt;

  let mem = Hashtbl.mem;

  let clear = Hashtbl.clear;
};

module Subscriptions =
  Subscriptions_transport_ws_lwt.Make(MySubscriptionManager);

let subscriptions = MySubscriptionManager.create(10);

let upgrade_connection = (ctx, schema, reqd, fd) => {
  let _x = 1;
  Websocket.upgrade_connection(
    reqd,
    fd,
    ~headers=
      Httpaf.Headers.of_list([("sec-websocket-protocol", "graphql-ws")]),
    Subscriptions.on_recv(
      ~subscribe=execute_query(ctx, schema),
      ~push_to_websocket=lazy (List.hd(client^)),
      subscriptions,
    ),
  )
  >>= (
    ((_resp, _body, push_to_websocket)) =>
      Lwt_io.eprintlf("DUDDEEEEEEE...")
      >>= (
        () => {
          client := [push_to_websocket];
          /* Httpaf.Body.flush(body, () => ()) [> Httpaf.Body.close_writer(body) <]; */
          Lwt.return_unit;
        }
      )
  );
};

let mk_callback = (reqd, mk_context, schema, flow) => {
  let {Httpaf.Request.target, meth, _} as request = Reqd.request(reqd);
  let req_path = target |> Uri.of_string |> Uri.path;
  Format.printf("Req: %a\n%!", Httpaf.Request.pp_hum, request);
  let path_parts = Str.(split(regexp("/"), req_path));
  switch (meth, path_parts) {
  | (`GET, ["foo"]) =>
    let x =
      Lwt_timeout.create(1, () =>
        Reqd.respond_with_string(
          reqd,
          Response.create(
            `OK,
            ~headers=
              Headers.of_list([
                ("Connection", "close"),
                ("Content-Length", "3"),
                /* ("Content-Type", "application/json"), */
              ]),
          ),
          "foo",
        )
      );
    Lwt_timeout.start(x);
    Lwt.return_unit;

  | (`POST, ["foo"]) =>
    let response =
      Response.create(
        `OK,
        ~headers=
          Headers.of_list([
            ("Connection", "keep-alive"),
            ("Content-Length", "3"),
            /* ("Content-Type", "application/json"), */
          ]),
      );

    /* Lwt_io.eprintlf( */
    /*   "persistent? %s", */
    /*   string_of_bool(Response.persistent_connection(response)), */
    /* ); */

    let request_body = Reqd.request_body(reqd);
    let response_body = Reqd.respond_with_streaming(reqd, response);
    let rec on_read = (_request_data, ~off as _, ~len as _) => {
      Body.write_string(response_body, "yay");
      Body.close_writer(response_body);

      /* Lwt_io.eprintlf( */
      /*   "closed?: %s", */
      /*   Body.is_closed(response_body) |> string_of_bool, */
      /* ) */
      /* |> ignore; */

      Body.flush(response_body, () =>
        Body.schedule_read(request_body, ~on_eof, ~on_read)
      );
      /* Lwt_io.eprintl("done flushing") |> ignore */
    }
    and on_eof = () => Body.close_writer(response_body);

    Body.schedule_read(~on_read, ~on_eof, request_body);
    Lwt.return_unit;
  | (_, ["bar"]) =>
    let response =
      Response.create(
        `OK,
        ~headers=
          Headers.of_list([
            ("Connection", "close"),
            ("Content-Type", "text/event-stream"),
          ]),
      );
    let request_body = Reqd.request_body(reqd);
    let response_body = Reqd.respond_with_streaming(reqd, response);
    let (finished, notify) = Lwt.wait();
    let rec on_read = (_request_data, ~off as _, ~len as _) =>
      Body.flush(response_body, () =>
        Body.schedule_read(request_body, ~on_eof, ~on_read)
      )
    and on_eof = () =>
      Schema.set_interval(
        2,
        () => {
          let _ = Body.write_string(response_body, "data: some data\n\n");
          Body.flush(response_body, () => ());
          true;
        },
        () => {
          let _ = Body.write_string(response_body, "event: end\ndata: 1\n\n");
          Body.flush(
            response_body,
            () => {
              Body.close_writer(response_body);
              Lwt.wakeup_later(notify, ());
            },
          );
        },
      );
    /* Body.flush(response_body, () => ()); */
    Body.schedule_read(~on_read, ~on_eof, request_body);
    finished;
  | (`GET, ["ws"]) =>
    upgrade_connection(mk_context(request), schema, reqd, flow)
  | (`GET, ["graphql"]) => serveStatic(reqd, "./build", "graphiql.html")
  | (`POST, ["graphql"]) =>
    execute_request(reqd, mk_context(request), schema);
    Lwt.return_unit;
  | (`GET, ["lib", ..._]) => serveStatic(reqd, "./", req_path)
  | (`GET, _) => serveStatic(reqd, "./build", req_path)
  | _ =>
    Reqd.respond_with_string(reqd, Response.create(`Not_found), "");
    Lwt.return_unit;
  };
};

let mk_connection_handler = (mk_context, schema, flow) => {
  let connection_handler: (Unix.sockaddr, Lwt_unix.file_descr) => Lwt.t(unit) = {
    let request_handler: (Unix.sockaddr, Reqd.t(_)) => unit =
      (_client_address, request_descriptor) =>
        Lwt.async(() =>
          mk_callback(request_descriptor, mk_context, schema, flow)
        );

    let error_handler:
      (
        Unix.sockaddr,
        ~request: Httpaf.Request.t=?,
        _,
        Headers.t => Body.t([ | `write])
      ) =>
      unit =
      (_client_address, ~request as _=?, error, start_response) => {
        let response_body = start_response(Headers.empty);

        switch (error) {
        | `Exn(exn) =>
          Body.write_string(response_body, Printexc.to_string(exn));
          Body.write_string(response_body, "\n");

        | #Status.standard as error =>
          Body.write_string(
            response_body,
            Status.default_reason_phrase(error),
          )
        };

        Body.close_writer(response_body);
      };

    Httpaf_lwt.Server.create_connection_handler(
      ~config=?None,
      ~request_handler,
      ~error_handler,
    );
  };
  connection_handler;
};

/* let start = (~port=8080, ~ctx, schema) => {
     let server =
       Conduit_lwt_unix.serve(
         ~ctx=Conduit_lwt_unix.default_ctx,
         ~mode=`TCP(`Port(port)),
         (flow, _ic, _oc) => {
           let connection_handler = mk_connection_handler(ctx, schema, flow);
           switch (flow) {
           | TCP({fd, ip, port}) =>
             connection_handler(
               Unix.(ADDR_INET(Ipaddr_unix.to_inet_addr(ip), port)),
               fd,
             )
           | _ => assert(false)
           };
         },
       );

     Lwt_main.run(server);
   }; */

let start = (~port=8080, ~ctx, schema) => {
  let listen_address = Unix.(ADDR_INET(inet_addr_any, port));

  Lwt.async(() =>
    Lwt_io.establish_server_with_client_socket(listen_address, (addr, fd) =>
      mk_connection_handler(ctx, schema, fd, addr, fd)
    )
    >>= (_server => Lwt.return_unit)
  );

  let (forever, _) = Lwt.wait();

  Lwt_main.run(forever);
};
