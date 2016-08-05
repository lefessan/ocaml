(* Requires 'ocpwin-cygwin-exec' installed in ../tools/ *)

(* ./ocpwin-make-nsi INSTALL_PREFIX TARGET_VERSION :
     creates a directory 'original_files' containing the files copied from
   INSTALL_PREFIX, compress them, and copy them into an installer created with
   NSIS from a file 'install.nsi' in the current directory. *)

let ocpwin_cygwin_exec = "../tools/ocpwin-cygwin-exec"

let nsi_directory = NsiWorker.detect_nsi ()
let cygwin = CygwinWorker.detect_cygwin ()

let nsis =
  match nsi_directory with
    None -> failwith "nsis not installed"
  | Some nsis_dir -> Filename.concat nsis_dir "makensis.exe"

let _ = if Sys.argv.(1) <> "-nsis" then begin

    let install_prefix = Sys.argv.(1) in
    let target_version = Sys.argv.(2) in
    let target_systems =
      let target_systems = ref [] in
      for i = 3 to Array.length Sys.argv - 1 do
	target_systems := Sys.argv.(i) :: !target_systems
      done;
      List.rev !target_systems
    in
(* Copy files to install in original_files *)
    assert (OnlyWin32.command [| ocpwin_cygwin_exec; "rm"; "-rf"; "original_files"; "files"; "uninstall_lines.nsi" |] = 0);

    let copy_rec relpath =
      assert (OnlyWin32.command [| ocpwin_cygwin_exec; "cp"; "-dpR";
			       install_prefix ^ "/" ^ relpath;
			       "original_files" ^ "/" ^ relpath;
			     |] = 0);
      ()
    in
    let copy relpath =
      assert (OnlyWin32.command [| ocpwin_cygwin_exec; "cp";
			       install_prefix ^ "/" ^ relpath;
			       "original_files" ^ "/" ^ relpath;
			     |] = 0);
      ()
    in
    let version_relpath = Printf.sprintf "roots/ocamlwin-%s"
         target_version in
    assert (OnlyWin32.command [|
	    ocpwin_cygwin_exec; "mkdir"; "-p"; "original_files" ^ "/" ^ version_relpath |] = 0);
    assert (OnlyWin32.command [|
	    ocpwin_cygwin_exec; "mkdir"; "-p"; "original_files/bin" |] = 0);

    copy_rec "bin-ocamlpro";
    copy_rec "bin-install";
(*    copy_rec (Printf.sprintf "%s/man" version_relpath); *)
    copy (Printf.sprintf "%s/Changes.txt" version_relpath);
    copy (Printf.sprintf "%s/License.txt" version_relpath);
    copy (Printf.sprintf "%s/Readme.general.txt" version_relpath);
    copy (Printf.sprintf "%s/Readme.windows.txt" version_relpath);
    List.iter (fun system ->
      let bin_dir = Printf.sprintf "bin-%s" system in
      copy_rec (Printf.sprintf "%s/bin-%s"  version_relpath system);
      copy_rec (Printf.sprintf "%s/lib-%s"  version_relpath system);
      copy_rec bin_dir;
	      ) target_systems   ;

    if Sys.file_exists "config.nsh" then Sys.remove  "config.nsh";
    let config_nsh = File.string_of_file "config.nsh.in" in

    let nsubst, config_nsh = StringSubst.subst
	(StringSubst.subst_of_list
           [
            "TARGET_VERSION", target_version;
            "SOURCE_PREFIX", install_prefix;
	    "TARGET_SYSTEMS", String.concat "-" target_systems
          ]) config_nsh in

    File.file_of_string "config.nsh" config_nsh;
    InstallMerge.merge_windows "original_files";
    InstallPre.pre_install "original_files";
  end

let _ =
  let cmd = Printf.sprintf "\"%s\" install.nsi" nsis in
  Printf.fprintf stderr "cmd = [%s]\n%!" cmd;
  assert (Sys.command cmd = 0)

