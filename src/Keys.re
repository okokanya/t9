type trie;
[@bs.module] external words : array(string) = "an-array-of-english-words";
[@bs.module] external trie : array(string) => trie = "trie-prefix-tree";
[@bs.send] external getWords : (trie, bool) => array(string) = "";
[@bs.send] external getPrefix : (trie, string, bool) => array(string) = "";
let dict = trie(words);

let keyboard: list(list(string)) = [
  ["a", "b", "c"],
  ["d", "e", "f"],
  ["g", "h", "i"],
  ["j", "k", "l"],
  ["m", "n", "o"],
  ["p", "q", "r", "s"],
  ["t", "u", "v"],
  ["w", "x", "y", "z"]
];

let rec concatValues = (list1, list2) =>
  switch list1 {
  | [] => []
  | [head, ...tail] => [ List.map(char => head ++ char, list2), ...concatValues(tail, list2) ]
  };

let combineKeys = (list1, list2) =>
  list1
  |> concatValues(list2)
  |> List.flatten;

let getCombinations = (numbers: list(int)): list(string) =>
  Belt.List.reduceReverse(numbers, [], (acc, num) => List.length(acc) !== 0 ? combineKeys(acc, List.nth(keyboard, num - 2)) : List.nth(keyboard, num - 2));

let findCombinations = input =>
  input
  |> Js.String.split("")
  |> Array.map(int_of_string)
  |> Array.to_list
  |> getCombinations
  |> Array.of_list;

let findSuggests = (string, combinations) =>
  switch combinations {
  | [||] => getPrefix(dict, string, false)
  | _ => Belt.Array.reduce(combinations, [||], (acc, prefix) => Array.append(acc, getPrefix(dict, string ++ prefix, false)))
  };
