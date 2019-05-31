module ASet : sig
  type 'a t
  val empty : 'a t                    (* 空集合 *)
  val elt : 'a -> 'a t                (* 'a の値を集合の要素にする *)
  val singleton : 'a t -> 'a t        (* 単一集合 *)
  val union : 'a t -> 'a t -> 'a t    (* 和集合 *)
  val inter : 'a t -> 'a t -> 'a t    (* 積集合 *)
  val diff : 'a t -> 'a t -> 'a t     (* 差集合 *)
  val insert : 'a t -> 'a t -> 'a t   (* 要素の付け加え *)
  val eq : 'a t -> 'a t -> bool       (* 集合の同一性関係 *)
  val mem : 'a t -> 'a t -> bool      (* 要素関係 *)
  val sub : 'a t -> 'a t -> bool      (* 部分集合関係 *)
end = struct

  type 'a t = Empty
            | Element of 'a
            | S of 'a t list

  let empty = Empty

  let elt a = Element a

  let eq x y =
    let rec hojo a y = match y with
        [] -> []
      | first :: rest ->
        if a = first then hojo a rest
        else first :: hojo a rest
    in
    let rec hojo2 x y = match x with
        [] -> y = []
      | first :: rest -> hojo2 rest (hojo first y)
    in
    match (x, y) with
      (Empty, Empty) -> true
    | (Empty, S (b)) -> false
    | (S (a), Empty) -> false
    | (S (a), S (b)) -> hojo2 a b && hojo2 b a
    | _ -> false

  let singleton x = S ([x])

  let union x y = match (x, y) with
      (Empty, Empty) -> Empty
    | (Empty, S (b)) -> S (b)
    | (S (a), Empty) -> S (a)
    | (S (a), S (b)) -> S (a @ b)
    | _ -> Empty

  let inter x y =
    let rec hojo e lst = match lst with
        [] -> []
      | first :: rest ->
        if e = first then [first]
        else hojo e rest
    in
    let empconv x =
      if x = S ([]) then empty
      else x
    in
    let rec hojo2 lst1 lst2 acc = match lst1 with
        [] -> acc
      | first :: rest -> hojo2 rest lst2 ((hojo first lst2) @ acc)
    in
    match (x, y) with
      (Empty, _) -> Empty
    | (_, Empty) -> Empty
    | (S (a), S (b)) ->
      empconv(S (hojo2 a b []))
    | _ -> Empty

  let diff x y =
    let rec hojo e lst = match lst with
        [] -> []
      | first :: rest ->
        if e = first then hojo e rest
        else first :: (hojo e rest)
    in
    let rec hojo2 lst1 lst2 = match lst2 with
        [] -> lst1
      | first :: rest -> hojo2 (hojo first lst1) rest
    in
    let empconv x =
      if x = S ([]) then empty
      else x
    in
    match (x, y) with
      (x, Empty) -> x
    | (Empty, _) -> Empty
    | (S (a), S (b)) -> empconv (S (hojo2 a b))
    | _ -> Empty

  let mem x y =
    let rec listmem a lst = match lst with
        [] -> false
      | first :: rest ->
        if first = a then true
        else listmem a rest
    in
    match y with
      S (a) -> if listmem x a then true
      else false
    | _ -> false

  let insert x y =
    union (singleton x) y

  let sub x y =
    let rec listmem a lst = match lst with
        [] -> false
      | first :: rest ->
        if first = a then true
        else listmem a rest
    in
    let rec listsub lst1 lst2 = match lst1 with
        [] -> true
      | first :: rest ->
        (listmem first lst2) && listsub rest lst2
    in
    match (x, y) with
      (S (a), S(b)) -> listsub a b
    | (Empty, _) -> true
    | _ -> false
end
