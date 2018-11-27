let style = ReactDOMRe.Style.make;

let evtValue = event => event->ReactEvent.Form.target##event;

type retainedProps = string;

let component = ReasonReact.reducerComponentWithRetainedProps("Editor");

let make = (~value, ~onChange, ~placeholder, ~className="", ~clear=false, _) => {
  ...component,
  initialState: () => None,
  reducer: (action, state) => ReasonReact.Update(action(state)),
  retainedProps: value,
  willReceiveProps: ({state, retainedProps}) =>
    switch (state) {
    | None => None
    | Some(text) =>
      retainedProps === value && value !== text ? Some(text) : None
    },
  render: ({state, send}) =>
    switch (state) {
    | None =>
      <div
        style=(style(~cursor="text", ()))
        className
        onClick=(
          evt => {
            evt->ReactEvent.Mouse.stopPropagation;
            send(_ => Some(value));
          }
        )>
        (ReasonReact.string(value === "" ? placeholder : value))
      </div>
    | Some(text) =>
      <input
        value=text
        placeholder
        className
        autoFocus=true
        onChange=(
          evt => {
            let value = evtValue(evt);
            send(_ => Some(value));
          }
        )
        onClick=(evt => evt->ReactEvent.Mouse.stopPropagation)
        onKeyDown=(
          evt =>
            switch (ReactEvent.Keyboard.key(evt)) {
            | "Enter" =>
              if (text == value) {
                send(_ => None);
              } else {
                onChange(text);
                if (clear) {
                  send(_ => Some(value));
                };
              }
            | _ => ()
            }
        )
        style=(
          style(~fontFamily="inherit", ~flex="1", ~fontSize="inherit", ())
        )
        onBlur=(
          _ =>
            if (text != value) {
              onChange(text);
              if (clear) {
                send(_ => Some(value));
              };
            } else {
              send(_ => None);
            }
        )
      />
    },
};
