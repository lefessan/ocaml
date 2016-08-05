(* [detect_visual_studio x64p] checks in the Win32 registry if Visual
   Studio 9.0 is installed, and returns [Some (vc_install_dir,
   sdk_install_dir)] if true, and sets environment variables
   accordingly. *)
val detect_visual_studio : bool -> (string * string) option

(* same as [detect_visual_studio], but executed only once *)
val detect_visual_studio_cached : bool -> (string * string) option
