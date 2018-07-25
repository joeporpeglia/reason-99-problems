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
  let isPalindrome = chars => chars == List.reverse(chars);
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
    | [head, ...tail] => List.reverse(compress(head, [head], tail))
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
    | [head, ...tail] => List.reverse(pack([head], [], tail))
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

  let encode = items =>
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

module P13 = {
  type rle('a) =
    | One('a)
    | Many((int, 'a));

  let encodeGroup = ((count, char) as group) =>
    count > 1 ? Many(group) : One(char);

  let encode = chars => {
    let rec encode = (group, groups, chars) => {
      let (count, char) = group;
      switch (chars) {
      | [] => [encodeGroup(group), ...groups]
      | [head, ...tail] =>
        head == char ?
          encode((count + 1, char), groups, tail) :
          encode((1, head), [encodeGroup(group), ...groups], tail)
      };
    };

    switch (chars) {
    | [] => []
    | [head, ...tail] => List.reverse(encode((1, head), [], tail))
    };
  };
};

module P14 = {
  let rec duplicate = chars =>
    switch (chars) {
    | [] => []
    | [head, ...tail] => [head, head, ...duplicate(tail)]
    };
};

module P15 = {
  let rec replicate = (chars, times) =>
    switch (chars) {
    | [] => []
    | [head, ...tail] =>
      List.concat(List.make(times, head), replicate(tail, times))
    };
};

module P16 = {
  let drop = (chars, skipIndex) => {
    let rec drop = (chars, cursor) =>
      switch (chars) {
      | [] => []
      | [head, ...tail] =>
        cursor == skipIndex ?
          drop(tail, 1) : [head, ...drop(tail, cursor + 1)]
      };

    drop(chars, 1);
  };
};

module P17 = {
  let split = (chars, size) => {
    let rec shift = (destination, source, size) =>
      switch (source) {
      | [] => (List.reverse(destination), [])
      | [head, ...tail] =>
        size == 1 ?
          (List.reverse([head, ...destination]), tail) :
          shift([head, ...destination], tail, size - 1)
      };

    shift([], chars, size);
  };
};

module P18 = {
  let rec slice = (chars, start, stop) => {
    let rec shift = (destination, source, size) =>
      switch (source) {
      | [] => List.reverse(destination)
      | [head, ...tail] =>
        size == 1 ?
          List.reverse(destination) :
          shift([head, ...destination], tail, size - 1)
      };

    switch (chars) {
    | [] => []
    | [_, ...tail] =>
      start == 0 ?
        shift([], chars, stop - start) : slice(tail, start - 1, stop)
    };
  };
};

module P19 = {
  let rotate = (chars, dist) => {
    let rec shift = (shifted, remaining, count) =>
      switch (count) {
      | 0 => List.concat(remaining, List.reverse(shifted))
      | _ =>
        switch (remaining) {
        | [] => shift([], shifted, count - 1)
        | [head, ...tail] => shift([head, ...shifted], tail, count - 1)
        }
      };

    let dist =
      if (dist < 0) {
        List.length(chars) + dist;
      } else {
        dist;
      };

    switch (chars) {
    | [] => []
    | _ => shift([], chars, dist)
    };
  };
};

module P20 = {
  let rec removeAt = (index, chars) =>
    switch (chars) {
    | [] => []
    | [head, ...tail] =>
      switch (index) {
      | 0 => tail
      | _ => [head, ...removeAt(index - 1, tail)]
      }
    };
};
