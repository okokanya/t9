[%bs.raw {|require('./App.css')|}];

open Axios;
let axiosInst = Instance.create(makeConfig(~baseURL="http://localhost:5000", ()));

type action =
  | GetCombinations(list(string))
  | GetSuggestions(list(string))
  | KeyPress(int)
  | SpacePress
  | UpArrowPress
  | DownArrowPress
  | BackspacePress
  | ReturnPress;

type actionState =
  | Idle
  | Combinations
  | Suggestions;

type state = {
  keysPressed: list(int),
  words: list(string),
  combinations: list(string),
  suggestions: list(string),
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

let requestSuggestions = (prefix: string, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  Js.Promise.(
    Instance.get(axiosInst, "/?q=" ++ prefix)
    |> then_((response) => resolve(self.send(GetSuggestions(Array.to_list(response##data##suggestions)))))
    |> catch((error) => resolve(Js.log(error)))
  );
};

let handleKeyPress = (key, self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  switch key {
  | 0 => self.send(SpacePress)
  | x when x > 1 && x < 10 && List.length(self.state.keysPressed) < 9 =>
      let keysPressed = Belt_List.reduceReverse(self.state.keysPressed, "", (acc, num) => acc ++ string_of_int(num));
      let _request = requestCombinations(keysPressed ++ string_of_int(key), self);
      self.send(KeyPress(x))
  | y => Js.log(string_of_int(y) ++ " pressed")
  };
};

let handleReturnPress = (self: ReasonReact.self(state, ReasonReact.noRetainedProps, action)) => {
  if (self.state.actionState == Combinations) {
    let prefix = List.hd(self.state.words) ++ List.nth(self.state.combinations, self.state.chosenOption);
    let _request = requestSuggestions(prefix, self);
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
  let word = List.nth(state.suggestions, state.chosenOption);
  List.tl(state.words)
  |> List.append([word])
  |> List.append([""])
};

let addNewWord = state =>
  List.append([""], state.words);

let removeLastChar = string =>
  StdLabels.String.sub(string, 0, StdLabels.String.length(string) - 1);

let initialState: state = {
  keysPressed: [],
  words: [""],
  combinations: [""],
  suggestions: [""],
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

      | SpacePress =>
        let nextWords = switch(state.actionState) {
          | Combinations => List.append([""], addCombinationToCurrent(state))
          | _ => addNewWord(state)
        };
        ReasonReact.Update({ ...initialState, actionState: Idle, words: nextWords });
      
      | DownArrowPress =>
        let newOption = switch (state.chosenOption + 1) {
          | x when x < List.length(state.combinations) || x < List.length(state.suggestions) => x
          | _ => List.length(state.combinations) - 1
        };
        ReasonReact.Update({ ...state, chosenOption: newOption })

      | UpArrowPress =>
        let newOption = switch (state.chosenOption - 1) {
          | x when x >= 0 => x
          | _ => 0
        };
        ReasonReact.Update({ ...state, chosenOption: newOption })

      | BackspacePress =>
        let nextKeysPressed = switch state.actionState {
          | Combinations when List.length(state.keysPressed) <= 1 => []
          | Combinations => List.tl(state.keysPressed)
          | _ => state.keysPressed
        };
        let nextCombinations = switch (List.length(state.keysPressed)) {
          | 0 => []
          | 1 => []
          | _ => state.combinations
        };
        let nextWords = switch state.actionState {
          | Combinations when state.keysPressed != [] => state.words
          | _ when state.words == [""] => state.words
          | _ =>
            switch (List.hd(state.words)) {
              | "" =>
                let nextWords = List.tl(state.words);
                switch (List.hd(nextWords)) {
                  | "" => nextWords
                  | x => [removeLastChar(x), ...List.tl(nextWords)]
                };
              | x => [removeLastChar(x), ...List.tl(state.words)];
            }
        };
        ReasonReact.Update({ ...initialState, actionState: state.actionState, keysPressed: nextKeysPressed, words: nextWords, combinations: nextCombinations });

      | ReturnPress =>
        let nextActionState = switch(state.actionState) {
          | Idle => Idle
          | Combinations => Suggestions
          | Suggestions => Idle
        };
        let nextWords = switch(state.actionState) {
          | Combinations => addCombinationToCurrent(state)
          | Suggestions => addSuggestionToCurrent(state)
          | _ => state.words
        };
        ReasonReact.Update({ ...initialState, actionState: nextActionState, words: nextWords });
      
      | GetCombinations(combinations) => ReasonReact.Update({ ...state, combinations })
      | GetSuggestions(suggestions) =>
        let nextActionState =
          switch suggestions {
            | [] => Idle
            | _ => Suggestions
          };
        ReasonReact.Update({ ...state, suggestions, actionState: nextActionState })
  },

  render: self => {
    <div>
      <Display
        words=self.state.words
        combinations=self.state.combinations
        suggestions=self.state.suggestions
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
