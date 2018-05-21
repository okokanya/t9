[%bs.raw {|require('./App.css')|}];

open Axios;
let axiosInst = Instance.create(makeConfig(~baseURL="http://localhost:5000", ()));

type state = {
  keysPressed: list(int),
  words: list(string),
  combinations: list(string),
  chosenOption: int
};

type action =
  | GetCombinations(list(string))
  | KeyPress(int)
  | SpacePress
  | UpArrowPress
  | DownArrowPress
  | BackspacePress
  | ReturnPress;

let requestCombinations = (key, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  let keysPressed = Belt_List.reduce(self.state.keysPressed, "", (acc, num) => acc ++ string_of_int(num));
  Js.Promise.(
    Instance.get(axiosInst, "/?n=" ++ keysPressed ++ string_of_int(key))
    |> then_((response) => resolve(self.send(GetCombinations(Array.to_list(response##data##combinations)))))
    |> catch((error) => resolve(Js.log(error)))
  );
};

let handleKeyPress = (key, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  switch (key) {
  | 0 => self.send(SpacePress)
  | x when x > 1 && x < 10 => {
    let _request = requestCombinations(key, self);
    self.send(KeyPress(x))
  };
  | y => Js.log(string_of_int(y) ++ " pressed")
  };
};

let initialState: state = {keysPressed: [], words: [], combinations: [""], chosenOption: 0};

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => initialState,

  reducer: (action, state) =>
    switch (action) {
    | KeyPress(key) => ReasonReact.Update({ ...state, keysPressed: List.append(state.keysPressed, [key]) })
    | SpacePress => ReasonReact.Update({ ...initialState, words: List.append(state.words, [List.nth(state.combinations, state.chosenOption)]) })
    | DownArrowPress => {
      let newOption = switch state.chosenOption {
      | x when x < List.length(state.combinations) - 1 => x + 1
      | _ => List.length(state.combinations) - 1
      };
      ReasonReact.Update({ ...state, chosenOption: newOption })
    };
    | UpArrowPress => {
      let newOption = switch state.chosenOption {
      | x when x > 0 => x - 1
      | _ => 0
      };
      ReasonReact.Update({ ...state, chosenOption: newOption })
    }
    | BackspacePress
    | ReturnPress => ReasonReact.Update({...state, words: List.append(state.words, ["abc"])})
    | GetCombinations(combinations) => ReasonReact.Update({ ...state, combinations: combinations })
    },

  render: self => {
    Js.log(self.state);
    let keys = List.mapi((index, key) =>
      <span key=string_of_int(index)>
        (ReasonReact.string(Js.Int.toString(key)))
      </span>,
      self.state.keysPressed
    );

    <div className="container">
      (ReasonReact.string(List.nth(self.state.combinations, self.state.chosenOption)))
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
