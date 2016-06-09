module Make (Atom : Decompress_checksum.ATOM) (Scalar : Decompress_checksum.SCALAR with type elt = Atom.t) : Decompress_checksum.S
  with type atom = Atom.t
   and type buffer = Scalar.t = struct
    let zero_char = Char.chr 0
    let max_32 = 1 lsl 32
  type t = {
      crc : int32;
      length : int;
  }
  type atom = Atom.t
  type buffer = Scalar.t
  let init () = { crc = Int32.of_int 0; length = 0; }
  let make crc length = { crc=(Int32.of_int crc); length; }
  let eq crc1 crc2 = (crc1 = crc2)
  let neq crc1 crc2 = (crc1 <> crc2)
  let get { crc; length; } =
      (* NOTE: Crc lib treating Int32 as unsigned *)
      let c = Int32.to_int crc in
      let crc = if c < 0 then max_32 + c else c in
      (crc, length)
  let add_length length len =
      let l = length + len in
      l mod max_32
  let update buff off len {crc;length} =
      let arr = Array.make len zero_char in
      for i = 0 to len - 1 do
          arr.(i) <- Char.chr @@ Atom.code (Scalar.get buff i)
      done;
      let str = Stringext.of_array arr in
      let next_crc = Crc.Crc32.string ~crc str 0 len and
      next_length = add_length length len in
      { crc=next_crc; length=next_length}
end

