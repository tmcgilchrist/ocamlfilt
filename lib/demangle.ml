let caml_prefix = "caml"
let caml_prefix_len = 4

let hex c =
  let c = Char.code c in
  if c >= Char.code '0' && c <= Char.code '9' then c - Char.code '0'
  else if c >= Char.code 'a' && c <= Char.code 'f' then c - Char.code 'a' + 10
  else c - Char.code 'A' + 10

let ( .%[;..] ) = Bigarray.Genarray.get
let ( .%{;..} ) = Bigarray.Genarray.get
let ( .%(;..) ) = Bigarray.Genarray.get
let ( .%[]<- ) = Bytes.set
let ( .%[] ) = Bytes.get
let ( .%{}<- ) = Bytes.set

let get_opt str i =
  try Some (String.get str i) with Invalid_argument _ -> None

let is_digit (c : Char.t) : bool =
  Char.code c >= Char.code '0' &&
  Char.code c <= Char.code '9'

let is_ascii (c : Char.t) =
  (Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z')
  || (Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z')

let is_xdigit (c : Char.t) =
  if is_digit c then true
  else if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'f' then true
  else Char.code c >= Char.code 'A' && Char.code c <= Char.code 'F'

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
      let separator_found =
        match index with
        | Some s -> (
            let n = get_opt str (s + 1) in
            match n with
            | Some n -> is_ascii n || Char.equal '$' n || Char.equal '.' n
            | None -> false)
        | None -> false
      in
      String.starts_with ~prefix:caml_prefix str
      && Char.equal c (Char.uppercase_ascii c)
      && separator_found
  | None -> false

let demangle_classic_sym str =
  if not (String.starts_with ~prefix:caml_prefix str) then None
  else
    let j = ref 0 in
    let i = ref caml_prefix_len in
    let len = String.length str in
    let result = Bytes.create len in
    while !i < len do
      if str.[!i] == '_' && str.[!i + 1] == '_' then (
        (* "__" -> "." *)
        result.%[!j] <- '.';
        j := !j + 1;
        i := !i + 2)
      else if
        Char.equal str.[!i] '$'
        && is_xdigit str.[!i + 1]
        && is_xdigit str.[!i + 2]
      then (
        (* "$xx" is a hex-encoded character *)
        (* TODO Guard against shell escaping from STDIN *)
        let a = Char.chr ((hex str.[!i + 1] lsl 4) lor hex str.[!i + 2]) in
        result.%[!j] <- a;
        j := !j + 1;
        i := !i + 3)
      else (
        (* regular characters *)
        result.%[!j] <- str.[!i];
        j := !j + 1;
        i := !i + 1)
    done;
    Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)

let is_ocaml_classic_5_4 str =
  let c = get_opt str caml_prefix_len in
  match c with
  | Some c ->
      let index = String.index_from_opt str caml_prefix_len '$' in
      let separator_found =
        match index with
        | Some s -> (
            let n = get_opt str (s + 1) in
            match n with
            | Some n ->
                if Char.equal '$' n then Char.equal '$' (String.get str (s + 2))
                else is_ascii n
            | None -> false)
        | None -> false
      in
      String.starts_with ~prefix:caml_prefix str
      && Char.equal c (Char.uppercase_ascii c)
      && separator_found
  | None -> false

let demangle_classic_5_4_sym str =
  if not (String.starts_with ~prefix:caml_prefix str) then None
  else
    let j = ref 0 in
    let i = ref caml_prefix_len in
    let len = String.length str in
    let result = Bytes.create len in
    while !i < len do
      if
        Char.equal str.[!i] '$'
        && Char.equal str.[!i + 1] '$'
        && Char.equal str.[!i + 2] '$'
        && is_xdigit str.[!i + 3]
        && is_xdigit str.[!i + 4]
      then (
        (* "$$$xx" is a separator plus hex-encoded character *)
        let a = (hex str.[!i + 3] lsl 4) lor hex str.[!i + 4] in
        let a = Char.chr a in
        result.%[!j] <- '.';
        j := !j + 1;
        result.%[!j] <- a;
        j := !j + 1;
        i := !i + 5)
      else if
        Char.equal str.[!i] '$'
        && Char.equal str.[!i + 1] '$'
        && is_xdigit str.[!i + 2]
        && is_xdigit str.[!i + 3]
      then (
        (* "$$xx" is a separator plus hex-encoded character *)
        let a = (hex str.[!i + 2] lsl 4) lor hex str.[!i + 3] in
        let a = Char.chr a in
        result.%[!j] <- a;
        j := !j + 1;
        i := !i + 4)
      else if
        Char.equal str.[!i] '$'
        && is_digit str.[!i + 1]
        && is_digit str.[!i + 2]
      then (
        (* "$27" is a hex-encoded character *)
        let a = (hex str.[!i + 1] lsl 4) lor hex str.[!i + 2] in
        result.%[!j] <- Char.chr a;
        j := !j + 1;
        i := !i + 3)
      else if str.[!i] == '$' then (
        (* "$" -> "." *)
        result.%[!j] <- '.';
        j := !j + 1;
        i := !i + 1)
      else if str.[!i] == '_' && str.[!i + 1] == '_' then (
        (* "__" -> "." *)
        result.%[!j] <- '.';
        j := !j + 1;
        i := !i + 2)
      else (
        (* regular characters *)
        result.%[!j] <- str.[!i];
        j := !j + 1;
        i := !i + 1)
    done;
    Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)

let is_ocaml_runlength str =
  String.starts_with str ~prefix:"_OM" || String.starts_with str ~prefix:"_OC"

(* Transform a string with escape sequences into one without them
   e.g. some$27_988 -> some'_988  *)
let unescape str =
  match String.index_opt str '$' with
  | Some _i ->
      let j = ref 0 in
      let i = ref 0 in
      let len = String.length str in
      let result = Bytes.create len in
      while !i < len do
        if
          Char.equal str.[!i] '$'
          && is_xdigit str.[!i + 1]
          && is_xdigit str.[!i + 2]
        then (
          (* "$xx" is a hex-encoded character *)
          (* TODO Guard against shell escaping from STDIN *)
          let a = Char.chr ((hex str.[!i + 1] lsl 4) lor hex str.[!i + 2]) in
          result.%[!j] <- a;
          j := !j + 1;
          i := !i + 3)
        else (
          (* regular characters *)
          result.%[!j] <- str.[!i];
          j := !j + 1;
          i := !i + 1)
      done;
      Bytes.extend result 0 (!j - len) |> Bytes.to_string
  | None -> str

let take_int i str =
  (* let len = String.length str in *)
  let rec go i str acc =
    (* if i < len then begin *)
    let a = str.[i] in
    if is_digit a then go (i + 1) str (acc ^ String.make 1 a) else (acc, i)
    (* end *)
    (* else *)
    (*   (acc, i) *)
  in
  go i str ""

let demangle_run_length str =
  if not (is_ocaml_runlength str) then None
  else
    let prefix = String.sub str 0 3 in
    match prefix with
    | "_OC"->
        let len = String.length str in
        let result = Bytes.create (len + 1) in
        let j = ref 0 in
        let i = ref 3 in
        while !i < len - 1 do
          let index, start_at = take_int !i str in
          (* Printf.printf "index=%s" index; *)
          let size = if String.equal index "" then 0 else int_of_string index in
          (* Printf.printf "start_at=%i size=%i char=%c\n" start_at size str.[!i]; *)
          if size == 0 then
            i := len (* Short circuit return. *)
          else
            let i_prime = !i + size + (start_at - !i) in
            if i_prime < len - 1 then (
              (* Module or function path *)
              Bytes.blit_string str start_at result !j size;
              j := !j + size;
              i := !i + size + (start_at - !i);
              result.%[!j] <- '.';
              j := !j + 1)
           else (
             (* Anonymous function *)
             result.%[!j] <- '<';
             result.%[!j+1] <- 'f';
             result.%[!j+2] <- 'u';
             result.%[!j+3] <- 'n';
             j := !j + 4;
             Bytes.blit_string str start_at result !j size;
             j := !j + size;
             i := i_prime;
             result.%[!j] <- '>';
             j := !j + 1)
          (* Printf.printf "j=%i i=%i %s\n" !j !i (Bytes.to_string result); *)
        done;
        Some (Bytes.to_string result)

    | "_OM" ->
        let len = String.length str in
        let result = Bytes.create len in
        let j = ref 0 in
        let i = ref 3 in
        while !i < len - 1 do
          let index, start_at = take_int !i str in
          (* Printf.printf "index=%s" index; *)
          let size = if String.equal index "" then 0 else int_of_string index in
          (* Printf.printf "start_at=%i size=%i char=%c\n" start_at size str.[!i]; *)
          if size == 0 then i := len (* Short circuit return. *)
          else
            let unescaped_str = unescape (String.sub str start_at size) in
            let size' = String.length unescaped_str in
            (* Printf.printf "%s length=%i\n" unescaped_str size'; *)
            Bytes.blit_string unescaped_str 0 result !j size';
            (* Bytes.blit_string str start_at result !j size; *)
            j := !j + size';
            i := !i + size + (start_at - !i);

            if !i < len - 1 then (
              result.%[!j] <- '.';
              j := !j + 1)
          (* Printf.printf "j=%i i=%i %s\n" !j !i (Bytes.to_string result); *)
        done;
        Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)
    | _s -> Some _s
