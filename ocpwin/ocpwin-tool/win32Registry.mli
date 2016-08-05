
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

val string_of_rkey : rkey_value -> string
val read_standard_config : rkey_value -> dwType * string

val read_key : hroot -> rkey_value -> dwType * string
val write_key : hroot -> rkey_value -> string -> unit
val delete_key : hroot -> rkey_value -> bool

val broadcast_setting_change : string -> unit
