open Belt;
open Solutions;

let tests = [
  (
    "Problem 1",
    P1.last(["a", "b", "c", "d"]) == Some("d") && P1.last([]) == None,
  ),
  (
    "Problem 2",
    P2.lastTwo(["a", "b", "c", "d"]) == Some(("c", "d"))
    && P2.lastTwo(["a"]) == None,
  ),
  (
    "Problem 3",
    P3.at(3, ["a", "b", "c", "d"]) == Some("c")
    && P3.at(5, ["a", "b", "c", "d"]) == None,
  ),
  ("Problem 4", P4.length(["a", "b", "c"]) == 3 && P4.length([]) == 0),
  ("Problem 5", P5.rev(["a", "b", "c"]) == ["c", "b", "a"]),
  (
    "Problem 6",
    P6.isPalindrome(["x", "a", "m", "a", "x"])
    && ! P6.isPalindrome(["a", "b"]),
  ),
  (
    "Problem 7",
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
    "Problem 8",
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
    "Problem 9",
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
    "Problem 10",
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
    "Problem 11",
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
    "Problem 12",
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
    "Problem 13",
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
  (
    "Problem 14",
    P14.duplicate(["a", "b", "c", "c", "d"])
    == ["a", "a", "b", "b", "c", "c", "c", "c", "d", "d"],
  ),
  (
    "Problem 15",
    P15.replicate(["a", "b", "c"], 3)
    == ["a", "a", "a", "b", "b", "b", "c", "c", "c"],
  ),
  (
    "Problem 16",
    P16.drop(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3)
    == ["a", "b", "d", "e", "g", "h", "j"],
  ),
  (
    "Problem 17",
    P17.split(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3)
    == (["a", "b", "c"], ["d", "e", "f", "g", "h", "i", "j"])
    && P17.split(["a", "b", "c", "d"], 5) == (["a", "b", "c", "d"], []),
  ),
  (
    "Problem 18",
    P18.slice(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 2, 6)
    == ["c", "d", "e", "f", "g"],
  ),
  (
    "Problem 19",
    P19.rotate(["a", "b", "c", "d", "e", "f", "g", "h"], 3)
    == ["d", "e", "f", "g", "h", "a", "b", "c"]
    &&
    P19.rotate(["a", "b", "c", "d", "e", "f", "g", "h"], -2) == [
                                                                    "g",
                                                                    "h",
                                                                    "a",
                                                                    "b",
                                                                    "c",
                                                                    "d",
                                                                    "e",
                                                                    "f",
                                                                  ],
  ),
  ("Problem 20", P20.removeAt(1, ["a", "b", "c", "d"]) == ["a", "c", "d"]),
];

List.forEach(tests, ((name, passed)) =>
  Js.log(name ++ ": " ++ (passed ? "passed" : "failed"))
);
