open Lwt;
open Cohttp_lwt_unix;

let makeHandler = (~host, path, _conn, req: Cohttp.Request.t, body) => {
  let proxyPath = host ++ path;
  let proxyUri = Uri.of_string(proxyPath);
  let Some(host) = Uri.host(proxyUri);
  let headers = Cohttp.Header.replace(req.headers, "Host", host);
  /* Format.eprintf("REQ: %a %a\n%!", Uri.pp_hum, uri1, Uri.pp_hum,uri2); */
  Printf.eprintf("hyia: %s\n%!", host);

  (
    switch (req.meth) {
    | `GET => Printf.eprintf("AHOY\n%!"); Client.get(~headers, Uri.of_string(proxyPath))
    | `POST => Client.post(~body, ~headers, Uri.of_string(proxyPath))
    | `PUT => Client.put(~body, ~headers, Uri.of_string(proxyPath))
    | `PATCH => Client.patch(~body, ~headers, Uri.of_string(proxyPath))
    | `DELETE => Client.delete(~body, ~headers, Uri.of_string(proxyPath))
    | _ => Client.get(Uri.of_string(proxyPath))
    }
  )
  >>= (
    ((resp, body)) => {
      let headers = resp |> Response.headers;
      let status = resp |> Response.status;

      Format.eprintf("headers: %a\n%!", Cohttp_lwt_unix.Response.pp_hum, resp);
      Cohttp_lwt_unix.Server.respond(~headers, ~body, ~status, ());
    }
  );
};

let server =
  Cohttp_lwt_unix.Server.create(
    ~mode=`TCP(`Port(3000)),
    Cohttp_lwt_unix.Server.make(
      ~callback=makeHandler(~host="http://www.sapo.pt", "/"),
      (),
    ),
  );

Lwt_main.run(server);
