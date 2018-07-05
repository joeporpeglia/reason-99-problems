[%%debugger.chrome];

open Belt;
open Solutions;

let tests = [
  ("Problem 1.1", last(["a", "b", "c", "d"]) == Some("d")),
  ("Problem 1.2", last([]) == None),
  ("Problem 2.1", lastTwo(["a", "b", "c", "d"]) == Some(("c", "d"))),
  ("Problem 2.2", lastTwo(["a"]) == None),
  ("Problem 3.1", at(3, ["a", "b", "c", "d"]) == Some("c")),
  ("Problem 3.2", at(5, ["a", "b", "c", "d"]) == None),
  ("Problem 4.1", length(["a", "b", "c"]) == 3),
  ("Problem 4.2", length([]) == 0),
  ("Problem 5.1", rev(["a", "b", "c"]) == ["c", "b", "a"]),
  ("Problem 6.1", isPalindrome(["x", "a", "m", "a", "x"])),
  ("Problem 6.2", ! isPalindrome(["a", "b"])),
  (
    "Problem 7.1",
    flatten([
      One("a"),
      Many([One("b"), Many([One("c"), One("d")]), One("e")]),
    ])
    == ["a", "b", "c", "d", "e"],
  ),
  (
    "Problem 8.1",
    compress([
      "a",
      "a",
      "a",
      "a",
      "b",
      "c",
      "c",
      "a",
      "a",
      "d",
      "e",
      "e",
      "e",
      "e",
    ])
    == ["a", "b", "c", "a", "d", "e"],
  ),
  (
    "Problem 9.1",
    pack([
      "a",
      "a",
      "a",
      "a",
      "b",
      "c",
      "c",
      "a",
      "a",
      "d",
      "d",
      "e",
      "e",
      "e",
      "e",
    ])
    == [
         ["a", "a", "a", "a"],
         ["b"],
         ["c", "c"],
         ["a", "a"],
         ["d", "d"],
         ["e", "e", "e", "e"],
       ],
  ),
];

List.forEach(tests, ((name, passed)) =>
  Js.log(name ++ ": " ++ (passed ? "passed" : "failed"))
);
