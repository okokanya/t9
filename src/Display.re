let component = ReasonReact.statelessComponent("Display");

let make = (~words, ~combinations, ~suggestions, ~chosenOption, _children) => {
  ...component,

  render: _self => {
    let options = list => List.mapi((index, option) => {
      let className = index == chosenOption ? "black" : "";
      <div key=string_of_int(index) className=className >(ReasonReact.string(option))</div>
    }, list);

    let optionsStyle = ReactDOMRe.Style.make(~transform="translateY(-" ++ string_of_int(chosenOption * 44) ++ "px)", ());

    let headerWord = word => {
      <div className="word">
        <div className="chars">
          <div>(ReasonReact.string(word))</div>
        </div>
        <div className="chars" style=optionsStyle>(ReasonReact.array(Array.of_list(options(combinations))))</div>
      </div>
    };

    let suggestionItems = () => {
      <div className="word">
        <div className="chars" style=optionsStyle>
          (ReasonReact.array(Array.of_list(options(suggestions))))
        </div>
      </div>
    };

    let word = word => <div className="word"><div className="chars"><div>(ReasonReact.string(word))</div></div></div>;
    let wordsList = List.rev_map(a => a, List.mapi((index, item) => {
      switch index {
      | 0 when List.length(combinations) > 1 => headerWord(item);
      | 0 when List.length(suggestions) > 1 => suggestionItems()
      | _ => word(item)
      };
    }, words));
    <div className="display">
      <div className="line" />
      (ReasonReact.array(Array.of_list(wordsList)))
    </div>
  }
};
