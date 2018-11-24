try (Lwt_engine.(set((new libev)(~backend=Ev_backend.kqueue, ())))) {
| _ => Lwt_engine.(set((new libev)(~backend=Ev_backend.epoll, ())))
};

let setup_log = (style_renderer, level) => {
  Fmt_tty.setup_std_outputs(~style_renderer?, ());
  Logs.set_level(level);
  Logs.set_reporter(Logs_fmt.reporter());
};

let main = () => {
  setup_log(None, Some(Logs.Info));
  Httpaf_server.start(~ctx=_req => (), Schema.schema);
};

main();

/* Httpaf_server.start(~ctx=req => (), Schema.schema) |> Lwt_main.run; */
