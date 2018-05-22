let component = ReasonReact.statelessComponent("Keyboard");

let numpadLetters = [
  "",
  "abc",
  "def",
  "ghi",
  "jkl",
  "mno",
  "pqrs",
  "tuv",
  "wxyz"
];

let make = (~onKeyPress, ~onUpArrowPress, ~onDownArrowPress, ~onReturnPress, ~onBackspacePress, _children) => {
  ...component,

  render: _self => {
    let numpad = List.mapi((index, letters) =>
      <button key=string_of_int(index) onClick={event => onKeyPress(event, index + 1)}>
        <span className="number">(ReasonReact.string(string_of_int(index + 1)))</span>
        (ReasonReact.string(letters))
      </button>, numpadLetters
    );

    <div className="grid-container">
      <button onClick={onBackspacePress} className="c-button center">
          <span className="number">(ReasonReact.string("C"))</span>
      </button>
      <button onClick={onReturnPress} className="center-button center">
        <svg width="60" height="60">
          <rect
            x="5"
            y="5"
            rx="10"
            ry="10"
            width="50"
            height="50"
            fill="none"
            stroke="black"
            strokeWidth="5"
          />
        </svg>
      </button>
      <button onClick={onUpArrowPress} className="arrow-up center">
        <svg height="25" width="30">
          <polygon points="15,0 30,30 0,30" fill="black" />
        </svg>
      </button>
      <button onClick={onDownArrowPress} className="arrow-down center">
        <svg height="25" width="30">
          <polygon points="0,0 30,0 15,25" fill="black" />
        </svg>
      </button>

      (ReasonReact.array(Array.of_list(numpad)))

      <button>
        <span className="number">(ReasonReact.string("*"))</span>
      </button>
      <button onClick={event => onKeyPress(event, 0)}>
        <span className="number">(ReasonReact.string("0"))</span>
        <svg width="22" height="20">
          <polyline stroke="#000000" fill="none" strokeWidth="2" points="2 12 2 18 20 18 20 12" />
        </svg>
      </button>
      <button>
        <span className="number">(ReasonReact.string("#"))</span>
      </button>
    </div>
  }
};
