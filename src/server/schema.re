open Shared.Types;
open Lwt.Infix;

module GQL = Graphql_lwt;

type appState = {
  nextId: int,
  todos,
};

let appState =
  ref({
    nextId: 1,
    todos: [{id: 0, title: "Add some things to do", completed: false}],
  });

let rec consume_stream = stream =>
  Lwt.catch(
    () =>
      Lwt_stream.next(stream)
      >>= (
        x => {
          let Ok(x) | Error(x) = x;
          Printf.eprintf(
            "stream response: '%s'\n%!",
            Yojson.Basic.to_string(x),
          );
          if (Lwt_stream.is_closed(stream)) {
            Lwt.return_unit;
          } else {
            consume_stream(stream);
          };
        }
      ),
    fun
    | Lwt_stream.Closed
    | Lwt_stream.Empty => Lwt.return_unit
    | _ => Lwt.return_unit,
  );

let set_interval = (s, f, destroy) => {
  let rec set_interval_loop = (s, f, n) => {
    let timeout =
      Lwt_timeout.create(s, () =>
        if (n > 0) {
          if (f()) {
            set_interval_loop(s, f, n - 1);
          };
        } else {
          destroy();
        }
      );

    Lwt_timeout.start(timeout);
  };

  set_interval_loop(s, f, 5);
};
let todo =
  GQL.Schema.(
    obj("todo", ~fields=_ =>
      [
        field("id", ~args=Arg.([]), ~typ=non_null(int), ~resolve=(_, p) =>
          p.id
        ),
        field(
          "title", ~args=Arg.([]), ~typ=non_null(string), ~resolve=((), p) =>
          p.title
        ),
        field(
          "completed", ~args=Arg.([]), ~typ=non_null(bool), ~resolve=((), p) =>
          p.completed
        ),
      ]
    )
  );

let schema =
  GQL.Schema.(
    schema(
      [
        io_field(
          "todos",
          ~args=Arg.([]),
          ~typ=non_null(list(non_null(todo))),
          ~resolve=((), ())
          /* Hack: reverse the list because we insert new todos at the head */
          => Lwt_result.return(appState^.todos |> List.rev)),
      ],
      ~mutations=[
        io_field(
          "addTodo",
          ~typ=non_null(list(non_null(todo))),
          ~args=
            Arg.[
              arg("title", ~typ=non_null(string)),
              arg'("completed", ~typ=bool, ~default=false),
            ],
          ~resolve=(_, _, title, completed) => {
            let newId = appState^.nextId;
            let newTodo = {id: newId, title, completed};
            appState :=
              {nextId: newId + 1, todos: [newTodo, ...appState^.todos]};
            Lwt_result.return(appState^.todos |> List.rev);
          },
        ),
        io_field(
          "toggleTodo",
          ~typ=non_null(list(non_null(todo))),
          ~args=Arg.[arg("id", ~typ=non_null(int))],
          ~resolve=(_, _, idToToggle) => {
            appState :=
              {
                ...appState^,
                todos:
                  List.map(
                    ({id, completed, _} as todo) =>
                      if (id == idToToggle) {
                        {...todo, completed: ! completed};
                      } else {
                        todo;
                      },
                    appState^.todos,
                  ),
              };

            Lwt_result.return(appState^.todos |> List.rev);
          },
        ),
        io_field(
          "editTodo",
          ~typ=non_null(list(non_null(todo))),
          ~args=
            Arg.[
              arg("id", ~typ=non_null(int)),
              arg("title", ~typ=non_null(string)),
            ],
          ~resolve=(_, _, idToEdit, title) => {
            appState :=
              {
                ...appState^,
                todos:
                  List.map(
                    ({id, _} as todo) =>
                      if (id == idToEdit) {
                        {...todo, title};
                      } else {
                        todo;
                      },
                    appState^.todos,
                  ),
              };
            Lwt_result.return(appState^.todos |> List.rev);
          },
        ),
        io_field(
          "removeTodo",
          ~typ=non_null(list(non_null(todo))),
          ~args=Arg.[arg("id", ~typ=non_null(int))],
          ~resolve=(_, _, idToRemove) => {
            appState :=
              {
                ...appState^,
                todos:
                  List.filter(({id, _}) => id !== idToRemove, appState^.todos),
              };
            Lwt_result.return(appState^.todos |> List.rev);
          },
        ),
      ],
      ~subscriptions=[
        subscription_field(
          "subscribe_to_user",
          ~typ=non_null(todo),
          ~args=Arg.[arg'("intarg", ~typ=int, ~default=42)],
          ~resolve=(_ctx, _intarg) => {
            let (todo_stream, push_to_todo_stream) = Lwt_stream.create();
            let destroy = () => push_to_todo_stream(None);
            set_interval(
              2,
              () => {
                let todos = appState^.todos;
                let idx = Random.int(List.length(todos));
                if (Lwt_stream.is_closed(todo_stream)) {
                  false;
                } else {
                  push_to_todo_stream(Some(List.nth(todos, idx)));
                  true;
                };
              },
              destroy,
            );
            Lwt_result.return((todo_stream, destroy));
          },
        ),
      ],
    )
  );
