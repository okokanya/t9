open Express;

[@bs.module] external cors : unit => Express.Middleware.t = "";

let app = express();

module Combinations {
  let findCombinations = nullableStringOfNumbers => {
    switch (nullableStringOfNumbers) {
      | None => [||]
      | Some(n) => Keys.findCombinations(Js.String.make(n));
    }
  };
  let getCombinations = query =>
    query
      |. Js.Dict.get("n")
      |. findCombinations
};

module Suggestions {
  let findSuggestions = (nullableString, combinations) => {
    switch (nullableString) {
      | None => [||]
      | Some(s) => Keys.findSuggestions(Js.String.make(s), combinations)
    }
  };
  let getSuggestions = (query, combinations) =>
    query
      |. Js.Dict.get("q")
      |. findSuggestions(combinations)
};

let handleRequest = query => {
  let charCombinations = Combinations.getCombinations(query);
  let wordSuggestions = Suggestions.getSuggestions(query, charCombinations);
  let answer = Js.Dict.empty();
  Js.Dict.set(answer, "combinations", Json.Encode.stringArray(charCombinations));
  Js.Dict.set(answer, "suggestions", Json.Encode.stringArray(wordSuggestions));
  answer;
};

let sendData = req => {
  req
    |. Request.query
    |. handleRequest
    |. Js.Json.object_
    |. Response.sendJson
};

App.use(app, cors());

App.get(app, ~path="/") @@
Middleware.from((next, req) =>
  switch (Request.baseUrl(req)) {
    | "" => sendData(req);
    | _ => next(Next.route)
  }
);

let onListen = e =>
  switch (e) {
    | exception (Js.Exn.Error(e)) =>
      Js.log(e);
      Node.Process.exit(1);
    | _ => Js.log @@ "Listening at http://127.0.0.1:5000"
  };

let server = App.listen(app, ~port=5000, ~onListen, ());
