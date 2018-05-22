[%bs.raw {|require('./App.css')|}];

open Axios;
let axiosInst = Instance.create(makeConfig(~baseURL="http://localhost:5000", ()));

type action =
  | GetCombinations(list(string))
  | GetSuggests(list(string))
  | KeyPress(int)
  | SpacePress
  | UpArrowPress
  | DownArrowPress
  | BackspacePress
  | ReturnPress;

type actionState =
  | Idle
  | Combinations
  | Suggests;

type state = {
  keysPressed: list(int),
  words: list(string),
  combinations: list(string),
  suggests: list(string),
  chosenOption: int,
  actionState: actionState
};

let requestCombinations = (keysPressed: string, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  Js.Promise.(
    Instance.get(axiosInst, "/?n=" ++ keysPressed)
    |> then_((response) => resolve(self.send(GetCombinations(Array.to_list(response##data##combinations)))))
    |> catch((error) => resolve(Js.log(error)))
  );
};

let requestSuggests = (prefix: string, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  Js.log("request suggests");
  Js.Promise.(
    Instance.get(axiosInst, "/?q=" ++ prefix)
    |> then_((response) => resolve(self.send(GetSuggests(Array.to_list(response##data##suggests)))))
    |> catch((error) => resolve(Js.log(error)))
  );
};

let handleKeyPress = (key, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  switch key {
  | 0 => self.send(SpacePress)
  | x when x > 1 && x < 10 => {
    let keysPressed = Belt_List.reduceReverse(self.state.keysPressed, "", (acc, num) => acc ++ string_of_int(num));
    let _request = requestCombinations(keysPressed ++ string_of_int(key), self);
    self.send(KeyPress(x))
  };
  | y => Js.log(string_of_int(y) ++ " pressed")
  };
};

let handleReturnPress = (self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  if (self.state.actionState == Combinations) {
    let prefix = List.nth(self.state.words, List.length(self.state.words) -1) ++ List.nth(self.state.combinations, self.state.chosenOption);
    let _request = requestSuggests(prefix, self);
  };
  self.send(ReturnPress);
};

let handleBackspacePress = (self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  if (self.state.actionState == Combinations && List.length(self.state.keysPressed) > 1) {
    let keysPressed = Belt_List.reduceReverse(List.tl(self.state.keysPressed), "", (acc, num) => acc ++ string_of_int(num));
    let _request = requestCombinations(keysPressed, self);
  };
  self.send(BackspacePress);
};

let addCombinationToCurrent = state => {
  let combinedWord = List.hd(state.words) ++ List.nth(state.combinations, state.chosenOption);
  List.append([combinedWord], List.tl(state.words));
};

let addSuggestionToCurrent = state => {
  let word = List.nth(state.suggests, state.chosenOption);
  List.append([word], List.tl(state.words));
};

let addNewWord = state =>
  List.append([""], state.words);

let removeLastChar = string =>
  StdLabels.String.sub(string, 0, StdLabels.String.length(string) - 1);

let initialState: state = {
  keysPressed: [],
  words: [""],
  combinations: [""],
  suggests: [""],
  chosenOption: 0,
  actionState: Idle
};

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => initialState,

  reducer: (action, state) =>
    switch (action) {
    | KeyPress(key) => ReasonReact.Update({
      ...state,
      keysPressed: List.append([key], state.keysPressed),
      actionState: Combinations
    })

    | SpacePress => {
      let nextWords = switch(state.actionState) {
      | Combinations => List.append([""], addCombinationToCurrent(state))
      | _ => addNewWord(state)
      };
      ReasonReact.Update({ ...initialState, actionState: Idle, words: nextWords });
    }
    
    | DownArrowPress => {
      let newOption = switch (state.chosenOption + 1) {
      | x when x < List.length(state.combinations) => x
      | _ => List.length(state.combinations) - 1
      };
      ReasonReact.Update({ ...state, chosenOption: newOption })
    }

    | UpArrowPress => {
      let newOption = switch (state.chosenOption - 1) {
      | x when x >= 0 => x
      | _ => 0
      };
      ReasonReact.Update({ ...state, chosenOption: newOption })
    }

    | BackspacePress => {
      let nextKeysPressed = switch state.actionState {
        | Combinations when List.length(state.keysPressed) <= 1 => []
        | Combinations => List.tl(state.keysPressed)
        | _ => state.keysPressed
      };
      let nextWords = switch state.actionState {
        | Combinations when state.keysPressed != [] => state.words
        | _ when state.words == [""] => state.words
        | _ => {
          let word = List.hd(state.words);
          switch word {
            | "" => {
              let nextWords = List.tl(state.words);
              [removeLastChar(List.hd(nextWords)), ...List.tl(nextWords)];
            }
            | _ => [removeLastChar(word), ...List.tl(state.words)];
          }
        }
      };
      ReasonReact.Update({ ...initialState, actionState: state.actionState, keysPressed: nextKeysPressed, words: nextWords });
    }

    | ReturnPress => {
      let nextActionState = switch(state.actionState) {
      | Idle => Idle
      | Combinations => Suggests
      | Suggests => Idle
      };

      let nextWords = switch(state.actionState) {
      | Combinations => addCombinationToCurrent(state)
      | Suggests => addSuggestionToCurrent(state)
      | _ => state.words
      };
      ReasonReact.Update({ ...initialState, actionState: nextActionState, words: nextWords });
    }
    
    | GetCombinations(combinations) => ReasonReact.Update({ ...state, combinations })
    | GetSuggests(suggests) => ReasonReact.Update({ ...state, suggests })
    },

  render: self => {
    <div>
      <Display
        words=self.state.words
        combinations=self.state.combinations
        suggests=self.state.suggests
        actionState=self.state.actionState
        chosenOption=self.state.chosenOption
      />
      <div className="container">
        <Keyboard
          onKeyPress={(_event, key) => handleKeyPress(key, self)}
          onUpArrowPress={_event => self.send(UpArrowPress)}
          onDownArrowPress={_event => self.send(DownArrowPress)}
          onReturnPress={_event => handleReturnPress(self)}
          onBackspacePress={_event => handleBackspacePress(self)}
        />
      </div>
    </div>
  },
};
