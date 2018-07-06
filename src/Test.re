open Belt;
open Solutions;

let tests = [
  ("Problem 1.1", P1.last(["a", "b", "c", "d"]) == Some("d")),
  ("Problem 1.2", P1.last([]) == None),
  ("Problem 2.1", P2.lastTwo(["a", "b", "c", "d"]) == Some(("c", "d"))),
  ("Problem 2.2", P2.lastTwo(["a"]) == None),
  ("Problem 3.1", P3.at(3, ["a", "b", "c", "d"]) == Some("c")),
  ("Problem 3.2", P3.at(5, ["a", "b", "c", "d"]) == None),
  ("Problem 4.1", P4.length(["a", "b", "c"]) == 3),
  ("Problem 4.2", P4.length([]) == 0),
  ("Problem 5.1", P5.rev(["a", "b", "c"]) == ["c", "b", "a"]),
  ("Problem 6.1", P6.isPalindrome(["x", "a", "m", "a", "x"])),
  ("Problem 6.2", ! P6.isPalindrome(["a", "b"])),
  (
    "Problem 7.1",
    P7.flatten([
      P7.One("a"),
      P7.Many([
        P7.One("b"),
        P7.Many([P7.One("c"), P7.One("d")]),
        P7.One("e"),
      ]),
    ])
    == ["a", "b", "c", "d", "e"],
  ),
  (
    "Problem 8.1",
    P8.compress([
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
    P9.pack([
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
  (
    "Problem 10.1",
    P10.encode([
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
    == [(4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")],
  ),
  (
    "Problem 11.1",
    P11.encode([
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
    == [
         P11.Many((4, "a")),
         P11.One("b"),
         P11.Many((2, "c")),
         P11.Many((2, "a")),
         P11.One("d"),
         P11.Many((4, "e")),
       ],
  ),
  (
    "Problem 12.1",
    P12.decode([
      P11.Many((4, "a")),
      P11.One("b"),
      P11.Many((2, "c")),
      P11.Many((2, "a")),
      P11.One("d"),
      P11.Many((4, "e")),
    ])
    == ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"],
  ),
  (
    "Problem 13.1",
    P13.encode([
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
    == [
         P13.Many((4, "a")),
         P13.One("b"),
         P13.Many((2, "c")),
         P13.Many((2, "a")),
         P13.One("d"),
         P13.Many((4, "e")),
       ],
  ),
];

List.forEach(tests, ((name, passed)) =>
  Js.log(name ++ ": " ++ (passed ? "passed" : "failed"))
);
