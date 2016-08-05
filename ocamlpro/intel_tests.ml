open Intel_proc
open Intel_assembler

open StringCompat

open Elf
open ElfTypes.RAW

let char_hex n =
  Char.unsafe_chr (n + if n < 10 then Char.code '0' else (Char.code 'a' - 10))

let to_hex d =
  let len = String.length d in
  let result = Bytes.create (len*3) in
  for i = 0 to len-1 do
    let x = Char.code d.[i] in
    Bytes.unsafe_set result (i*3) (char_hex (x lsr 4));
    Bytes.unsafe_set result (i*3+1) (char_hex (x land 0x0f));
    Bytes.unsafe_set result (i*3+2) ' ';
  done;
  Bytes.to_string result

let tests () =
  let good = open_out "intel_assembler.good" in
  let bad = open_out "intel_assembler.bad" in
  let ntests = ref 0 in
  let nfail = ref 0 in
  let nok = ref 0 in
  let ndiff = ref 0 in
  let nnot = ref 0 in
  let nweird = ref 0 in
  let test code =
    incr ntests;
    let code = [ code ] in
    let b = Buffer.create 10000 in
    List.iter (fun ins -> Intel_gas.bprint_instr b !arch64 (Ins ins)) code;
    let s = Buffer.contents b in
    let oc = open_out "test.s" in
    output_string oc ".text\n";
    output_string oc s;
    close_out oc;

    let ret = Ccomp.command (String.concat " "
          [Config.asm; "-o";
           Filename.quote "test.o";
           Filename.quote "test.s"]) in
    if ret <> 0 then begin
      Printf.fprintf good "Warning: gas could not compile %s\n%!" s;
      Printf.eprintf "for %s%!" s;
      incr nnot;
    end else begin
      let elf = Elf.ElfReader.RAW.read "test.o" in
      let found = ref false in
      Array.iter (fun se ->
        if se.section_name = ".text" then begin
          found := true;
          let hex1 = to_hex se.section_content in
          try
            let x = assemble_section {
                sec_name = ".text";
                sec_instrs = Array.of_list (List.map (fun ins -> Ins ins) code);
              } in
            let hex2 = to_hex (contents x) in
            if hex1 <> hex2 then begin
              Printf.fprintf bad "### Difference:%s gas translation: %s\n%!" s hex1;
              Printf.fprintf bad " our translation: %s\n%!" hex2;
              incr ndiff;
            end else begin
              Printf.fprintf good "### OK: %s%!" s;
              incr nok
            end
          with e ->
            let s = Printexc.get_backtrace () in
            Printf.fprintf bad "### Our translation failed with %s\n%s\n%!"
              (Printexc.to_string e) s;
            incr nfail
        end
      ) elf.elf_sections;
      if not !found then begin
        incr nweird;
        Printf.fprintf bad
          "Warning: could not find .text section in elf file\n%!";
      end
    end;
  in

  let test0 ins () = test ins in
  let test2 srcs dsts f () =
    List.iter (fun dst ->
      List.iter (fun src ->
        match dst, src with
          Mem _, Mem _ -> ()
        | _ ->
          test (f src dst)
      ) srcs;
    ) dsts
  in
  let tests = ref [] in
  let add name test f =  tests := (name, test f) :: !tests in



  if !arch64 then begin
    let reg64s = [
      RAX ; RBX ; RDI ; RSI ; RDX ; RCX ; RBP ; RSP ; R8 ; R9 ; R10 ; R11 ; R12 ; R13 ; R14 ; R15
    ] in

    let mem64s = ref [] in
    let mem64 addr = mem64s := Mem(QWORD, M64 addr) :: !mem64s in
    List.iter (fun r1 ->
      List.iter (fun scale ->
        match r1, scale with
          RSP, (2|4|8) -> ()
        | _ -> mem64 (Some (r1, scale, None), (None, 0L))
      ) [1;2;4;8];
    ) reg64s;
    let m64 = !mem64s in

    let xmms = List.map (fun n -> Regf (XMM n)) [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] in
    let r64 =  List.map (fun r -> Reg64 r) reg64s in
    let rm64 = m64 @ r64 in
    let xm64 = m64 @ xmms in

    let test_rm64_rm64 = test2 rm64 rm64 in
    let test_m64_r64 = test2 m64 r64 in
    let test_rm64_r64 = test2 rm64 r64 in
    let test_xmm_xmm = test2 xmms xmms in
    let test_m64_xmm = test2 m64 xmms in
    let test_xm64_r64 = test2 xm64 r64 in
    let test_rm64_xmm = test2 rm64 xmms in
    let _test_rm64 f () = List.iter (fun dst -> test (f dst)) rm64 in
    let test_r64 f () = List.iter (fun dst -> test (f dst)) r64 in

    add "ADD" test_rm64_rm64 (fun src dst -> ADD (src, dst));
    add "SUB" test_rm64_rm64 (fun src dst -> SUB (src, dst));
    add "XOR" test_rm64_rm64 (fun src dst -> XOR (src, dst));
    add "OR" test_rm64_rm64 (fun src dst -> OR (src, dst));
    add "AND" test_rm64_rm64 (fun src dst -> AND (src, dst));
    add "CMP" test_rm64_rm64 (fun src dst -> CMP (src, dst));
    add "LEA" test_m64_r64 (fun src dst -> LEA (src, dst));

    add "IMUL" test_rm64_r64 (fun src dst -> IMUL (src, Some dst));
    add "MOV" test_rm64_rm64 (fun src dst -> MOV (src, dst));
    add "BSWAP" test_r64 (fun dst -> BSWAP dst);

    add "CVTSD2SS" test_xmm_xmm (fun src dst -> CVTSD2SS (src, dst));
    add "CVTSS2SD" test_m64_xmm (fun src dst -> CVTSS2SD (src, dst));
    add "CVTSI2SD" test_rm64_xmm (fun src dst -> CVTSI2SD (src, dst));
    add "CVTSD2SI" test_xm64_r64 (fun src dst -> CVTSD2SI (src, dst));
    add "CVTTSD2SI" test_xm64_r64 (fun src dst -> CVTTSD2SI (src,dst));

    add "CQTO" test0 CQTO;

  end else begin

    let reg32s = [ RAX ; RBX ; RDI ; RSI ; RDX ; RCX ; RBP ; RSP ]  in
    let reg32s = List.map (fun r -> R32 r) reg32s in

    let mem32s = ref [] in
    let mem32 addr = mem32s := Mem(DWORD, M32 addr) :: !mem32s in
    List.iter (fun r1 ->
      List.iter (fun scale ->
        match r1, scale with
          R32 RSP, (2|4|8) -> ()
        | _ -> mem32 (Some (r1, scale, None), (None, 0L))
      ) [1;2;4;8];
    ) reg32s;
    let m32 = !mem32s in

    let r32 =  List.map (fun r -> Reg32 r) reg32s in
    let rm32 = m32 @ r32 in

    let test_rm32_rm32 = test2 rm32 rm32 in
    let test_m32_r32 = test2 m32 r32 in
    let test_rm32_r32 = test2 rm32 r32 in
    let _test_rm32 f () = List.iter (fun dst -> test (f dst)) rm32 in
    let test_r32 f () = List.iter (fun dst -> test (f dst)) r32 in

    add "ADD" test_rm32_rm32 (fun src dst -> ADD (src, dst));
    add "SUB" test_rm32_rm32 (fun src dst -> SUB (src, dst));
    add "XOR" test_rm32_rm32 (fun src dst -> XOR (src, dst));
    add "OR" test_rm32_rm32 (fun src dst -> OR (src, dst));
    add "AND" test_rm32_rm32 (fun src dst -> AND (src, dst));
    add "CMP" test_rm32_rm32 (fun src dst -> CMP (src, dst));
    add "LEA" test_m32_r32 (fun src dst -> LEA (src, dst));

    add "IMUL" test_rm32_r32 (fun src dst -> IMUL (src, Some dst));
    add "MOV" test_rm32_rm32 (fun src dst -> MOV (src, dst));
    add "BSWAP" test_r32 (fun dst -> BSWAP dst);

  end;

  add "HLT" test0 HLT;
  add "CDQ" test0 CDQ;
  add "LEAVE" test0 LEAVE;
  add "RET" test0 RET;

  let found = ref false in
  List.iter (fun name ->
    List.iter (fun (ins, test) ->
      if ins = name then begin found := true;
        Printf.eprintf "%s %!" ins; test ();
      end
    ) !tests
  ) !ocamlasm_tests;
  if not !found then
    List.iter (fun (ins, test) ->
      Printf.eprintf "%s %!" ins; test ();
    ) !tests;
  Printf.eprintf "\n%!";

  close_out good;
  close_out bad;

  Printf.eprintf "%d/%d tests ok\n%!" !nok !ntests;
  if !nfail <> 0 then Printf.eprintf "%d tests failed\n%!" !nfail;
  if !ndiff <> 0 then Printf.eprintf "%d tests differ\n%!" !ndiff;
  if !nnot <> 0 then Printf.eprintf "%d tests aborted (gas failed)\n%!" !nnot;
  if !nweird <> 0 then Printf.eprintf "%d tests weird (gas elf file incorrect)\n%!" !nweird;

  ()

let init () =
  if !ocamlasm_verbose then Clflags.verbose := true;
  if !ocamlasm_unit then tests ()
