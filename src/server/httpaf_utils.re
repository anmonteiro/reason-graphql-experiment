open Httpaf;
open Lwt.Infix;

let respond_with_file = (reqd, ~fname) =>
  Lwt_io.with_file(fname, ~mode=Lwt_io.input, ich =>
    Lwt_io.read(ich)
    >|= (
      s =>
        Reqd.respond_with_string(
          reqd,
          Response.create(
            `OK,
            ~headers=
              Headers.of_list([
                ("Content-Length", s |> String.length |> string_of_int),
                /* ("Connection", "close"), */
              ]),
          ),
          s,
        )
    )
  );

let respond_with_error = (reqd, error_body) => {
  let body = Printf.sprintf("Error: %s", error_body);
  Reqd.respond_with_string(
    reqd,
    Response.create(
      `Internal_server_error,
      ~headers=
        Headers.of_list([
          ("Content-Length", body |> String.length |> string_of_int),
          ("Connection", "close"),
        ]),
    ),
    body,
  );
};
