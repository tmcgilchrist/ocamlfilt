(* Code taken from https://github.com/cfcs/ocaml-punycode under AGPL license.
   Needs re-writing from scratch under ISC license and provide correct
   attribution.
*)
type illegal_ascii_label =
  | Illegal_label_size of string
  | Label_contains_illegal_character of string
  | Label_starts_with_illegal_character of char
  | Label_ends_with_hyphen of string

type punycode_decode_error =
  | Overflow_error
  | Invalid_domain_name of string
  | Illegal_label of illegal_ascii_label

type punycode_encode_error =
  | Malformed_utf8_input of string
  | Overflow
  | Invalid_domain_name of string
  | Illegal_label of illegal_ascii_label

(* Code taken from https://github.com/dbuenzli/uutf under ISC licence.
   Please split into standalone module and provide correct attribution.
 *)

type decode_result =
  | Just of Uchar.t
  | Malformed of string

let unsafe_byte s j = Char.code (Bytes.unsafe_get s j)
let malformed s j l = Malformed (Bytes.sub_string s j l)

let utf_8_len = [| (* uchar byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]

let r_utf_8 s j l =
  (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
  let uchar c = Just (Uchar.unsafe_of_int c) in
  match l with
  | 1 -> uchar (unsafe_byte s j)
  | 2 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      if b1 lsr 6 != 0b10 then malformed s j l else
      uchar (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F))
  | 3 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      let b2 = unsafe_byte s (j + 2) in
      let c = ((b0 land 0x0F) lsl 12) lor
              ((b1 land 0x3F) lsl 6) lor
              (b2 land 0x3F)
      in
      if b2 lsr 6 != 0b10 then malformed s j l else
      begin match b0 with
      | 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then malformed s j l else uchar c
      | 0xED -> if b1 < 0x80 || 0x9F < b1 then malformed s j l else uchar c
      | _ -> if b1 lsr 6 != 0b10 then malformed s j l else uchar c
      end
  | 4 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      let b2 = unsafe_byte s (j + 2) in let b3 = unsafe_byte s (j + 3) in
      let c = (((b0 land 0x07) lsl 18) lor
               ((b1 land 0x3F) lsl 12) lor
               ((b2 land 0x3F) lsl 6) lor
               (b3 land 0x3F))
      in
      if b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then malformed s j l else
      begin match b0 with
      | 0xF0 -> if b1 < 0x90 || 0xBF < b1 then malformed s j l else uchar c
      | 0xF4 -> if b1 < 0x80 || 0x8F < b1 then malformed s j l else uchar c
      | _ -> if b1 lsr 6 != 0b10 then malformed s j l else uchar c
      end
  | _ -> assert false

let fold_utf_8 ?(pos = 0) ?len f acc s =
  let rec loop acc f s i last =
    if i > last then acc else
      let need = Array.unsafe_get utf_8_len (unsafe_byte s i) in
      if need = 0 then loop (f acc i (malformed s i 1)) f s (i + 1) last else
      let rem = last - i + 1 in
      if rem < need then f acc i (malformed s i rem) else
      loop (f acc i (r_utf_8 s i need)) f s (i + need) last
  in
    let len = match len with None -> String.length s - pos | Some l -> l in
    let last = pos + len - 1 in
    loop acc f (Bytes.unsafe_of_string s) pos last

let cut sep s =
  let unsafe_get = String.get in
  let unsafe_string_sub s first len =
    let b = Bytes.create len in
    Bytes.(unsafe_blit (unsafe_of_string s) first b 0 len);
    Bytes.unsafe_to_string b
  in

  let sep_len = 1 in
  let s_len = String.length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - 1 in
  let rec check_sep i k =
    if k > max_sep_idx then
      let r_start = i + sep_len in
      Some (unsafe_string_sub s 0 i,
            unsafe_string_sub s r_start (s_len - r_start))
    else
      if unsafe_get s (i + k) = unsafe_get sep k
      then check_sep i (k + 1)
      else rscan (i - 1)
  and rscan i =
    if i < 0 then None else
    if unsafe_get s i = unsafe_get sep 0 then check_sep i 1 else rscan (i - 1)
  in
  rscan (max_s_idx - max_sep_idx)

(* constants from RFC 3492 *)
let initial_n = 0x80
let initial_bias = 72
let delimiter_int = 0x2D
let base = 36
let damp = 700
let tmin = 1
let tmax = 26
let skew = 38
let punycode_max_int = 0x7f_ff_ff_ff

let uchar_list_to_string (lst : Uchar.t list) : string =
  let b = Buffer.create 64 in
  let () = List.iter (Buffer.add_utf_8_uchar b) lst in
  Buffer.contents b

let decode_digit (cp : char) : int =
  let cp_int = Char.code cp in
  match cp with
  | '\x00' .. '\x39' (* 00..57 ported from cp - 48 < 10 *)
    -> cp_int - 22
  | '\x3a' .. '\x5a' (* 58..90 ported from cp - 65 < 26 *)
    -> cp_int - 65
  | '\x5b' .. '\x7a' (* 91..122 ported from cp - 97 < 26 *)
    -> cp_int - 97
  | '\x7b' .. '\xFF' (* 123..255 else / default case *)
    -> base

let encode_digit (d : int) (uppercase_flag : bool) : int =
  (* d + 22 + 75 * (d < 26) - ((flag != 0) << 5) *)
  match uppercase_flag , char_of_int d with
  | false , '\x00' .. '\x19' -> (* 00..25: lowercase a..z *)
     d + 0x61 (* d + 'a' *)
  | true , '\x00' .. '\x19' -> (* 00..25: uppercase a..z *)
     d + 0x41 (* d + 'A' *)
  | false , '\x1a' .. '\x23' -> (* 26..35: lowercase 0..9 *)
     (d - 26) + 0x30 (* d-26 + '0' *)
  | _ , ch ->
    failwith (__LOC__ ^ ": trying to decode invalid digit: "
              ^ (Char.escaped ch))

let adapt delta numpoints firsttime =
  let delta = match firsttime with
    | true -> (delta / damp)
    | false -> (delta lsr 1)
  in
  let delta = delta + (delta / numpoints) in
  let (delta, k) =
    let rec foo ~delta ~k =
      if delta > (((base - tmin) * tmax) / 2) then
        let delta = delta / (base - tmin) in
        let k = k + base in
        foo ~delta ~k
      else
        delta , k
    in
    foo ~delta ~k:0
  in
  k + (base - tmin + 1) * delta / (delta + skew)

(* Add binding operators for Result monad *)
let (let*) = Result.bind
let (let+) f r= Result.map r f

module ImmutArray = struct
  type backing = Uchar.t
  type 'a t = { arr: Uchar.t array
              ; len: int} constraint 'a = backing

  let get t idx =
    if idx < t.len then t.arr.(idx)
    else failwith "ImmutArray: index out of bounds"

  let len {len ; _} = len

  let insert_left t left_off uchar =
    let fresh = Array.make (t.len + 1) (Uchar.of_int 0) in
    Array.blit t.arr 0 fresh 0 left_off ;
    fresh.(left_off) <- uchar ;
    Array.blit t.arr left_off fresh (left_off + 1) (t.len - left_off) ;
    { arr = fresh ; len = t.len + 1 }

  let to_list {arr ; len } = Array.sub arr 0 len |> Array.to_list

  let fold_left cb initial_acc t =
    let acc = ref initial_acc in
    for i = 0 to t.len -1 do
      acc := cb !acc t.arr.(i)
    done;
    !acc

  let of_utf8 malformed_error utf8_str callback initial_acc =
    let array = Array.make (String.length utf8_str) (Uchar.of_int 0) in
    let ptr = ref 0 in
    let+ callback_acc = fold_utf_8
      (fun acc _codepoint_idx -> function
         | Malformed s -> Result.error (malformed_error s)
         | Just c ->
           let* acc in
           array.(!ptr) <- c ;
           incr ptr;
           Ok (callback acc c)
      )
      (Result.ok (initial_acc)) utf8_str
    in
    ({arr = array ; len = !ptr }, callback_acc)

  let of_utf8_save_basic_codepoints malformed_error utf8_str =
    of_utf8 malformed_error utf8_str
      (fun (basic_codepoints, basic_len) c ->
         if Uchar.to_int c < 0x80
         then (c::basic_codepoints), succ basic_len
         else basic_codepoints, basic_len
      ) ([], 0)

  let to_utf8 t =
    let buf = Buffer.create (len t) in
    fold_left (fun () uchar -> Buffer.add_utf_8_uchar buf uchar) () t;
    Buffer.contents buf
end

(* TODO preserve character case. Module names begin with uppercase and need to retain
        that naming to conform with OCaml compiler conventions.  *)
let decode input
  : (Uchar.t ImmutArray.t, punycode_decode_error) Result.t =
  let basic_codepoints, complex_codepoints =
    (* string component after RIGHTMOST dash is the complex codepoints.
       If no dash is found, the label is assumed to be "basic". *)
    match cut "-" input with
    | None -> input, ""
    | Some basic_complex_pair -> basic_complex_pair
  in
  let initial_value_output_len, initial_value_output =
    ImmutArray.of_utf8 (fun s -> s) basic_codepoints (fun () _b -> ()) ()
    |> Result.get_ok |> fun (foo,()) -> ImmutArray.len foo, foo
  in
  let max_ic = String.length complex_codepoints in

  let rec f ~ic ~n ~i ~w ~k ~value_output ~value_output_len ~bias =
    if ic >= max_ic then Result.ok value_output
    else
      let oldi = i in
      let rec ff_inner ~ic ~w ~k ~i = (* while ic < input.length *)
        if ic >= max_ic then Result.error Overflow_error
        else
          let digit = decode_digit complex_codepoints.[ic] in
          let ic = ic + 1 in
          (* digit >= base && raise RangeError *)
          (* digit > Math.floor((punycode_max_int - i) / w) && raise Overflow *)
          let i = i + (digit * w) in
          let t = match k with
            (* t = k <= bias ? tmin : k >= bias + tmax ? tmax : k - bias *)
            | _ when k <= bias -> tmin
            | _ when k >= bias + tmax -> tmax
            | _ -> k - bias
          in
          if digit < t then Result.ok (n , i , oldi, ic)
          else (* if not, keep looping over characters: *)
            (* if w > Math.floor(punycode_max_int / (base -t )) then raise Overflow *)
            let w = w * (base - t)
            and k = k + base in
            ff_inner ~ic ~w ~k ~i
      in
      let* (n , i, oldi, ic) = ff_inner ~ic ~w ~k ~i in
      let out = value_output_len + 1 in (* "out" == new length of output *)
      let bias = adapt (i-oldi) out (oldi = 0) in
      let n = n + (i / out) in
      let* () = begin if n > punycode_max_int
          then Result.error Overflow_error
          else Result.ok ()
        end
      in
      let i = i mod out in
      let value_output =
        let insertion_point = i in
        ImmutArray.insert_left value_output insertion_point (Uchar.of_int n) in
      f ~n ~i:(i+1) ~value_output ~value_output_len:out ~ic ~w ~k ~bias
  in
  f ~n:initial_n ~i:0 ~w:1 ~k:base ~ic:0
    ~value_output:initial_value_output
    ~value_output_len:initial_value_output_len
    ~bias:initial_bias

let encode input_utf8 : (string, punycode_encode_error) Result.t =
  let* (input, (basic_codepoints, basic_codepoints_len)) = ImmutArray.of_utf8_save_basic_codepoints
    (fun s -> Malformed_utf8_input s) input_utf8 in
  (* restore order after fold_utf_8 which reverses it: *)
  let initial_value_output : Uchar.t list =
    (* initial value of output is prefixed with the ASCII characters and '-'*)
    if basic_codepoints <> []
    then Uchar.of_int delimiter_int :: basic_codepoints
    else [] (* if the string only contains complex characters, no '-' prefix *)
  in
  (* main encoding loop: *)
  let rec f ~n ~h ~delta ~bias ~(value_output:Uchar.t list)
    : ('a list , punycode_encode_error) Result.t =
    if h >= ImmutArray.len input then Result.ok value_output
    else
      let m = ImmutArray.fold_left (* find lowest codepoint that in input: *)
          (fun m -> fun uchar ->
             let char = Uchar.to_int uchar in
             if char >= n then min char m else m)
          punycode_max_int input in
      let* () = match m - n > ((punycode_max_int - delta) / (h+1)) with
        | true -> Result.error Overflow
        | false -> Result.ok ()
      in
      let delta = delta + ( (m - n) * (h + 1) ) in
      let n : int = m in
      let rec f_inner ~ic ~delta ~bias ~(n:int) ~h
          ~(value_output:Uchar.t list) =
        (* input.each_with_index do |char, j| *)
        if ic >= ImmutArray.len input
        then Result.ok ((delta , bias , n , h , value_output))
        else
        let char = ImmutArray.get input ic in
        let* delta = match delta + 1 with
          | new_delta when Uchar.to_int char < n -> Result.ok new_delta
          | new_delta when new_delta > punycode_max_int
            -> Result.error Overflow
          | _ -> Result.ok delta
        in
        let (value_output, bias, delta, h) =
          begin
            if Uchar.to_int char = n then
              (* while true do *)
              let value_output =
                let rec fff ~(value_output:Uchar.t list) ~k ~q =
                  let t = match 0 with
                    | _ when k <= bias -> tmin
                    | _ when k >= bias + tmax -> tmax
                    | _ -> k - bias
                  in
                  if q < t
                  then
                    (Uchar.of_int @@ encode_digit q false)::value_output
                  else
                    let value_output =
                      ( Uchar.of_int @@
                        encode_digit (t + ((q-t) mod (base - t))) false
                        (* TODO should be false? *)
                      )::value_output
                    in
                    let q = (q-t) / (base -t) in
                    let k = k + base in
                    fff ~value_output ~k ~q
                in
                fff ~value_output ~k:base ~q:delta
              in
              let bias = adapt delta (h + 1)
                  (h = basic_codepoints_len) in
              let delta = 0 in
              let h = h+1 in
              (value_output , bias , delta , h)
            else
              (value_output , bias , delta , h)
          end
        in
        f_inner ~ic:(ic+1) ~delta ~bias ~n ~h ~value_output
      in
      let* (delta , bias , n , h , value_output) = f_inner ~ic:0 ~delta ~bias ~n ~h ~value_output in
      f ~n:(n+1) ~h ~delta:(delta+1) ~bias ~value_output
  in
  let+ result = f ~n:initial_n
    ~h:basic_codepoints_len
    ~delta:0
    ~bias:initial_bias
    ~value_output:initial_value_output in
  List.rev result |> uchar_list_to_string
