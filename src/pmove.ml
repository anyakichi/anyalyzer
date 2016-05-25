type t =
  | Move  of Square.t * Square.t * bool
  | Move' of Square.t * Square.t * Koma.t
  | Drop  of Square.t * Koma.t
