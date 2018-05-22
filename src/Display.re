let component = ReasonReact.statelessComponent("Display");

let make = (~words, ~combinations, ~suggests, ~actionState, ~chosenOption, _children) => {
  ...component,

  render: _self => {
    let options = () => List.mapi((index, option) => {
      let className = index == chosenOption ? "black" : "";
      <div key=string_of_int(index) className=className >(ReasonReact.string(option))</div>
    }, combinations);
    let optionsStyle = ReactDOMRe.Style.make(~transform="translateY(-" ++ string_of_int(chosenOption * 44) ++ "px)", ());
    let headerWord = word => {
      <div className="word">
        <div className="chars">
          <div>(ReasonReact.string(word))</div>
        </div>
        <div className="chars" style=optionsStyle>(ReasonReact.array(Array.of_list(options())))</div>
      </div>
    };
    let word = word => <div className="word"><div className="chars"><div>(ReasonReact.string(word))</div></div></div>;
    let words = List.rev_map(item => {
      switch item {
      | x when x == List.hd(words) => headerWord(item)
      | _ => word(item)
      };
    }, words);
    <div className="display">
      <div className="line" />
      (ReasonReact.array(Array.of_list(words)))
    </div>
  }
};
