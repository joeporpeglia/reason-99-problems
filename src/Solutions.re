open Belt;

module P1 = {
  let rec last = items =>
    switch (items) {
    | [] => None
    | [lastItem] => Some(lastItem)
    | [_, ...rest] => last(rest)
    };
};

module P2 = {
  let rec lastTwo = items =>
    switch (items) {
    | [] => None
    | [_] => None
    | [y, z] => Some((y, z))
    | [_, ...rest] => lastTwo(rest)
    };
};

module P3 = {
  let rec at = (index, items) =>
    switch (items) {
    | [] => None
    | [a, ...rest] => index <= 1 ? Some(a) : at(index - 1, rest)
    };
};

module P4 = {
  let length = items => List.reduce(items, 0, (total, _) => total + 1);
};

module P5 = {
  let rev = items =>
    List.reduce(items, [], (reversed, item) => [item, ...reversed]);
};

module P6 = {
  let isPalindrome = chars => chars == P5.rev(chars);
};

module P7 = {
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
};

module P8 = {
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
    | [head, ...tail] => P5.rev(compress(head, [head], tail))
    };
  };
};

module P9 = {
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
    | [head, ...tail] => P5.rev(pack([head], [], tail))
    };
  };
};

module P10 = {
  let encode = items =>
    List.map(P9.pack(items), group =>
      switch (group) {
      | [] => raise(Invalid_argument("A packed group should not be empty"))
      | [char, ..._] => (P4.length(group), char)
      }
    );
};

module P11 = {
  type rle('a) =
    | One('a)
    | Many((int, 'a));

  let encodeModified = items =>
    List.map(P9.pack(items), group =>
      switch (group) {
      | [] => raise(Invalid_argument("A packed group should not be empty"))
      | [char] => One(char)
      | [char, ..._] => Many((P4.length(group), char))
      }
    );
};

module P12 = {
  let rec decode = items =>
    switch (items) {
    | [] => []
    | [P11.One(char), ...rest] => [char, ...decode(rest)]
    | [P11.Many((count, char)), ...rest] =>
      List.concat(List.make(count, char), decode(rest))
    };
};
