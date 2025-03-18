let caml_prefix = "caml"
let caml_prefix_len = 4

let hex c =
  let c = Char.code c in
  if (c >= Char.code '0' && c <= Char.code '9') then
    c - Char.code '0'
  else if (c >= Char.code 'a' && c <= Char.code 'f') then
    c - Char.code 'a' + 10
  else
    c - Char.code 'A' + 10

let (.%[;..]) = Bigarray.Genarray.get
let (.%{;..}) = Bigarray.Genarray.get
let (.%(;..)) = Bigarray.Genarray.get

let ( .%[]<- ) = Bytes.set
let ( .%[] ) = Bytes.get
let ( .%{}<- ) = Bytes.set

let get_opt str i =
  try Some (String.get str i) with
  | Invalid_argument _ -> None

let is_digit (c : Char.t) : bool =
  (Char.code c - Char.code '0') < 10

let is_ascii (c : Char.t ) =
  ((Char.code c >= Char.code 'a') && (Char.code c <= Char.code 'z')) ||
    ((Char.code c >= Char.code 'A') && (Char.code c <= Char.code 'Z'))

let is_xdigit (c : Char.t ) =
  if is_digit c then true
  else if ((Char.code c >= Char.code 'a') && (Char.code c <= Char.code 'f')) then true
  else ((Char.code c >= Char.code 'A') && (Char.code c <= Char.code 'F'))

(* let match_after str sep = *)
(*   let index = String.index_from_opt str caml_prefix_len sep in *)
(*   match index with *)
(*   | Some s -> begin *)
(*     let n = get_opt str (s + 1) in *)
(*     let n_1 = get_opt str (s + 2) in *)
(*     Printf.printf "str: %s index: %i n: %c n_2: %c\n" str (Option.get index) (Option.get n) (Option.get n_1); *)
(*     match n with *)
(*     | Some n -> is_ascii n || (Char.equal sep n)  *)
(*     | None -> false *)
(*     end *)
(*   | None -> false *)

let is_ocaml_classic str =
  let c = get_opt str caml_prefix_len in
  match c with
  | Some c ->
    (* 1. Contains '$' without following digits -> false *)
    (* 2. Contains '$' with following '$' or 'alpha' -> true *)
    (* 3. Contains '.' with following '$' or 'alpha' -> true *)
    (* 4. Contains '__' -> true *)

    let index = String.index_from_opt str caml_prefix_len '.' in
    let separator_found = match index with
    | Some s -> begin
        let n = get_opt str (s + 1) in
        match n with
        | Some n -> is_ascii n || (Char.equal '$' n) || (Char.equal '.' n)
        | None -> false
      end
    | None -> false
    in
    String.starts_with ~prefix:caml_prefix str &&
      (Char.equal c (Char.uppercase_ascii c)) &&
        separator_found
  | None -> false

let demangle_classic_sym str =
  if not(String.starts_with ~prefix:caml_prefix str) then
    None
  else begin
    let j = ref 0 in
    let i = ref caml_prefix_len in
    let len = String.length str in
    let result = Bytes.create len in
    while (!i < len) do
      if (str.[!i] == '_' && str.[!i + 1] == '_') then
        begin
          (* "__" -> "." *)
          result.%[!j] <- '.';
          j := !j + 1;
          i := !i + 2
        end
      else if (Char.equal str.[!i] '$'
                 && is_xdigit str.[!i + 1]
                      && is_xdigit str.[!i + 2]) then
        begin
          (* "$xx" is a hex-encoded character *)
          let a = Char.chr (((hex str.[!i + 1]) lsl 4) lor (hex str.[!i + 2] )) in
          result.%[!j] <- a;
          j := !j + 1;
          i := !i + 3
        end
      else (
          (* regular characters *)
          result.%[!j] <- str.[!i];
                  j:= !j + 1;
                  i:= !i + 1 )
    done;
    Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)
  end

let is_ocaml_classic_5_4 str =
  let c = get_opt str caml_prefix_len in
  match c with
  | Some c ->
    let index = String.index_from_opt str caml_prefix_len '$' in
    let separator_found = match index with
    | Some s -> begin
        let n = get_opt str (s + 1) in
        match n with
        | Some n ->
          if (Char.equal '$' n) then
            (Char.equal '$' (String.get str (s + 2)))
          else
            is_ascii n
        | None -> false
      end
    | None -> false
    in
    String.starts_with ~prefix:caml_prefix str &&
      (Char.equal c (Char.uppercase_ascii c)) &&
        separator_found
  | None -> false

let demangle_classic_5_4_sym str =
  if not(String.starts_with ~prefix:caml_prefix str) then
    None
  else begin
      let j = ref 0 in
      let i = ref caml_prefix_len in
      let len = String.length str in
      let result = Bytes.create len in
      while (!i < len) do
        if (Char.equal str.[!i] '$' &&
            Char.equal str.[!i + 1] '$' &&
            Char.equal str.[!i + 2] '$' &&
            is_xdigit str.[!i + 3] &&
            is_xdigit str.[!i + 4]) then (
          (* "$$$xx" is a separator plus hex-encoded character *)
          let a = (((hex str.[!i + 3]) lsl 4) lor (hex str.[!i + 4])) in
          let a = Char.chr a in
          result.%[!j] <- '.';
          j := !j + 1;
          result.%[!j] <- a;
          j := !j + 1;
          i := !i + 5)
        else if (Char.equal str.[!i] '$' &&
                 Char.equal str.[!i + 1] '$' &&
                 is_xdigit str.[!i + 2] &&
                 is_xdigit str.[!i + 3]) then (
          (* "$$xx" is a separator plus hex-encoded character *)
          let a = (((hex str.[!i + 2]) lsl 4) lor (hex str.[!i + 3])) in
          let a = Char.chr a in
          result.%[!j] <- a;
          j := !j + 1;
          i := !i + 4)
        else if (Char.equal str.[!i] '$' && is_digit(str.[!i + 1]) && is_digit(str.[!i + 2])) then (
          (* "$27" is a hex-encoded character *)
          let a = (((hex str.[!i + 1]) lsl 4) lor (hex str.[!i + 2])) in
          result.%[!j] <- Char.chr a;
          j := !j + 1;
          i := !i + 3)
        else if (str.[!i] == '$') then (
          (* "$" -> "." *)
          result.%[!j] <- '.';
          j := !j + 1;
          i := !i + 1 )
        else if (str.[!i] == '_' && str.[!i + 1] == '_') then (
          (* "__" -> "." *)
          result.%[!j] <- '.';
          j := !j + 1;
          i := !i + 2)
        else (
          (* regular characters *)
          result.%[!j] <- str.[!i];
          j:= !j + 1;
          i:= !i + 1)
      done;
      Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)
    end
