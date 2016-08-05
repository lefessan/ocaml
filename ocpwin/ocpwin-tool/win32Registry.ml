open StringCompat

type hroot =
  | HKEY_CLASSES_ROOT
  | HKEY_CURRENT_CONFIG
  | HKEY_CURRENT_USER
  | HKEY_LOCAL_MACHINE
  | HKEY_USERS

type dwType =
  | REG_SZ
  | REG_EXPAND_SZ
  | REG_DWORD
  | REG_QWORD
  | REG_BINARY
  | REG_NONE

type rkey_value = string array * string * dwType

external read_registry_key :
  hroot ->
    string array ->
      string ->
        dwType ref ->
          (* string to read (result) *)  bytes ->
            (* size read *) int
            = "win32_read_registry_value_ml"

external write_registry_key :
  hroot ->
    string array ->
        string ->
          dwType ->
          string -> unit
            = "win32_write_registry_value_ml"

external delete_registry_key :
  hroot ->
    string array ->
        string -> bool
            = "win32_delete_registry_value_ml"


let read_key hroot (s2, s3, _) =
  let len = 8192 in
  let s = Bytes.create len in
  let dwType = ref REG_SZ in
  let res_len = read_registry_key hroot s2 s3 dwType s in
  if res_len < 0 then raise Not_found;
  !dwType, Bytes.sub_string s 0 res_len

let write_key hroot (s2,s3,dwType) s =
  write_registry_key hroot s2 s3 dwType s

let delete_key hroot (s2,s3,_) =
  delete_registry_key hroot s2 s3

let string_of_rkey (path, entry, _) =
  String.concat "\\" (Array.to_list path @ [entry])

let read_standard_config key =
  try
(*    if !verbose then
       Printf.fprintf stderr "Trying HKCU\\%s...\n%!"
       (string_of_reg_key key); *)
    read_key HKEY_CURRENT_USER key
  with _ ->
(*    if !verbose then
       Printf.fprintf stderr "Trying HKCU\\%s...\n%!"
       (string_of_reg_key key); *)
     read_key HKEY_LOCAL_MACHINE key

external broadcast_setting_change : string -> unit =
  "win32_broadcast_setting_change_ml"
