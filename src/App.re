[%bs.raw {|require('./App.css')|}];

type state = {
  keysPressed: list(int),
  words: list(string)
};

type action =
  | KeyPress(int)
  | SpacePress
  | UpArrowPress
  | DownArrowPress
  | BackspacePress
  | ReturnPress;

let handleKeyPress = (key, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) =>
  switch (key) {
  | 0 => self.send(SpacePress)
  | x when x > 1 && x < 10 => self.send(KeyPress(x))
  | y => Js.log(string_of_int(y) ++ " pressed")
  };

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => {keysPressed: [], words: []},

  reducer: (action, state) =>
    switch (action) {
    | KeyPress(key) => ReasonReact.Update({...state, keysPressed: List.append(state.keysPressed, [key])})
    | SpacePress => ReasonReact.Update({keysPressed: [], words: List.append(state.words, ["abc"])})
    },

  render: self => {
    let keys = List.mapi((index, key) =>
      <span key=string_of_int(index)>
        (ReasonReact.string(Js.Int.toString(key)))
      </span>,
      self.state.keysPressed
    );

    <div className="container">
      (ReasonReact.string("hello"))
      (ReasonReact.array(Array.of_list(keys)))
      <Keyboard
        onKeyPress={(_event, key) => handleKeyPress(key, self)}
        onUpArrowPress={_event => self.send(UpArrowPress)}
        onDownArrowPress={_event => self.send(DownArrowPress)}
        onReturnPress={_event => self.send(ReturnPress)}
        onBackspacePress={_event => self.send(BackspacePress)}
      />
    </div>
  },
};
