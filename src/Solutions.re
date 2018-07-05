open Belt;

let rec last = items =>
  switch (items) {
  | [] => None
  | [lastItem] => Some(lastItem)
  | [_, ...rest] => last(rest)
  };

let rec lastTwo = items =>
  switch (items) {
  | [] => None
  | [_] => None
  | [y, z] => Some((y, z))
  | [_, ...rest] => lastTwo(rest)
  };

let rec at = (index, items) =>
  switch (items) {
  | [] => None
  | [a, ...rest] => index <= 1 ? Some(a) : at(index - 1, rest)
  };

let length = items => List.reduce(items, 0, (total, _) => total + 1);

let rev = items =>
  List.reduce(items, [], (reversed, item) => [item, ...reversed]);

let isPalindrome = chars => chars == rev(chars);

type node('a) =
  | One('a)
  | Many(list(node('a)));

let rec flatten = items =>
  switch (items) {
  | [] => []
  | [One(item), ...rest] => [item, ...flatten(rest)]
  | [Many(subItems), ...rest] =>
    List.concat(flatten(subItems), flatten(rest))
  };

let compress = items => {
  let rec compress = (prev, compressed, rest) =>
    switch (rest) {
    | [] => compressed
    | [head, ...tail] =>
      head == prev ?
        compress(prev, compressed, tail) :
        compress(head, [head, ...compressed], tail)
    };

  switch (items) {
  | [] => []
  | [head, ...tail] => rev(compress(head, [head], tail))
  };
};

let pack = items => {
  let rec pack = (group, groups, rest) =>
    switch (rest) {
    | [] => [group, ...groups]
    | [head, ...tail] =>
      head === List.headExn(group) ?
        pack([head, ...group], groups, tail) :
        pack([head], [group, ...groups], tail)
    };

  switch (items) {
  | [] => []
  | [head, ...tail] => rev(pack([head], [], tail))
  };
};
