
open StringCompat

module EndianSig = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

module type DataEncoding = sig
  val buf_int8 : Buffer.t -> int -> unit
  val buf_int8_64 : Buffer.t -> int64 -> unit
  val buf_int16_64 : Buffer.t -> int64 -> unit
  val buf_int32_64 : Buffer.t -> int64 -> unit
  val buf_int64 : Buffer.t -> int64 -> unit

  val get_int8_64 : string -> int -> int64 * int
  val get_int16_64 : string -> int -> int64 * int
  val get_int32_64 : string -> int -> int64 * int
  val get_int64 : string -> int -> int64 * int
  val get_uint8_64 : string -> int -> int64 * int
  val get_uint16_64 : string -> int -> int64 * int
  val get_uint32_64 : string -> int -> int64 * int
  val get_uint64 : string -> int -> int64 * int

  val get_int : string -> int -> int * int
  val get_uint : string -> int -> int * int
  val get_int8 : string -> int -> int * int
  val get_uint8 : string -> int -> int * int
  val get_int16 : string -> int -> int * int
  val get_uint16 : string -> int -> int * int

  val get_int8_32 : string -> int -> int32 * int
  val get_int16_32 : string -> int -> int32 * int
  val get_int32 : string -> int -> int32 * int

  val str_int8_32 : bytes -> int -> int32 -> unit
  val str_int16_32 : bytes -> int -> int32 -> unit
  val str_int32 : bytes -> int -> int32 -> unit
end
end
module LittleEndian: sig

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

include EndianSig.DataEncoding
end = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

  (* to test DataEncoding... *)
  let test msg signed buf get =
(*
    for i = 1 to 1_000_000 do
      let i = Random.int 0x3ffffff in
      let i = if signed && Random.int 10 < 5 then -i else i in
      let i = Int64.of_int i in
      let b = Buffer.create 10 in
      buf b i;
      let s = Buffer.contents b in
      let ii, _ = get s 0 in
      if ( i <> ii) then begin
        Printf.fprintf stderr "%s: %Lx <> %Lx\n" msg i ii;
        for i = 0 to String.length s - 1 do
          Printf.fprintf stderr "%02x" (int_of_char s.[i])
        done;
        Printf.fprintf stderr "\n%!";
        exit 1
      end
    done
*)
()

  let buf_int8 b i =
    Buffer.add_char b (char_of_int (i land 0xff))

  let buf_int8_64 b iL =
    buf_int8 b (Int64.to_int iL)

  let buf_int16_64 b iL =
    buf_int8_64 b iL;
    buf_int8_64 b (Int64.shift_right iL 8)

  let buf_int32_64 b iL =
    buf_int16_64 b iL;
    buf_int16_64 b (Int64.shift_right iL 16)

  let buf_int64 b iL =
    buf_int32_64 b iL;
    buf_int32_64 b (Int64.shift_right iL 32)

  let get_uint8_64 s pos = Int64.of_int (int_of_char s.[pos])

  let get_uint16_64 s pos =
    let c1 = get_uint8_64 s pos in
    let c2 = get_uint8_64 s (pos+1) in
    Int64.logor c1 (Int64.shift_left c2 8), pos+2

  let get_uint32_64 s pos =
    let c1,pos = get_uint16_64 s pos in
    let c2,pos = get_uint16_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 16), pos

  let get_uint64 s pos =
    let c1,pos = get_uint32_64 s pos in
    let c2,pos = get_uint32_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 32), pos

  let get_int8_64 s pos =
    let i = int_of_char s.[pos] in
    let i = if i > 127 then i - 256 else i in
    Int64.of_int i

  let get_int16_64 s pos =
    let c1 = get_uint8_64 s pos in
    let c2 = get_int8_64 s (pos+1) in
    Int64.logor c1 (Int64.shift_left c2 8), pos+2

  let get_int32_64 s pos =
    let c1,pos = get_uint16_64 s pos in
    let c2,pos = get_int16_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 16), pos

  let get_int64 s pos =
    let c1,pos = get_uint32_64 s pos in
    let c2,pos = get_int32_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 32), pos

  let _ =
    test "LittleEndian.get_int32_64" true buf_int32_64 get_int32_64;
    test "LittleEndian.get_uint32_64" false buf_int32_64 get_uint32_64

  let get_uint8_64 s pos = get_uint8_64 s pos, pos+1
  let get_int8_64 s pos = get_int8_64 s pos, pos+1


  let get_int s pos =
    let intL, pos = get_int32_64 s pos in
    Int64.to_int intL, pos

  let get_uint s pos =
    let intL, pos = get_uint32_64 s pos in
    Int64.to_int intL, pos

  let get_int8 s pos =
    let intL, pos = get_int8_64 s pos in
    Int64.to_int intL, pos

  let get_uint8 s pos =
    let intL, pos = get_uint8_64 s pos in
    Int64.to_int intL, pos

  let get_int16 s pos =
    let intL, pos = get_int16_64 s pos in
    Int64.to_int intL, pos

  let get_uint16 s pos =
    let intL, pos = get_uint16_64 s pos in
    Int64.to_int intL, pos



  let get_int8_32 s pos =
    Int32.of_int (Char.code s.[pos]), pos+1
  let get_int16_32 s pos =
    let c2, pos = get_int8_32 s pos in
    let c1, pos = get_int8_32 s pos in
    Int32.logor c2 (Int32.shift_left c1 8), pos

  let get_int32 s pos =
    let c2, pos = get_int16_32 s pos in
    let c1, pos = get_int16_32 s pos in
    Int32.logor c2 (Int32.shift_left c1 16), pos

  let str_int8_32 s pos v =
    Bytes.set s pos (char_of_int ((Int32.to_int v) land 0xff))

  let str_int16_32 s pos v =
    str_int8_32 s pos v;
    str_int8_32 s (pos+1) (Int32.shift_right_logical v 8)

  let str_int32 s pos v =
    str_int16_32 s pos v;
    str_int16_32 s (pos+2) (Int32.shift_right_logical v 16);
    ()
end
module BigEndian: sig

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

include EndianSig.DataEncoding
end = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)


  (* to test DataEncoding... *)
  let test msg signed buf get =
(*
    for i = 1 to 1_000_000 do
      let i = Random.int 0x3ffffff in
      let i = if signed && Random.int 10 < 5 then -i else i in
      let i = Int64.of_int i in
      let b = Buffer.create 10 in
      buf b i;
      let s = Buffer.contents b in
      let ii, _ = get s 0 in
      if ( i <> ii) then begin
        Printf.fprintf stderr "%s: %Lx <> %Lx\n" msg i ii;
        for i = 0 to String.length s - 1 do
          Printf.fprintf stderr "%02x" (int_of_char s.[i])
        done;
        Printf.fprintf stderr "\n%!";
        exit 1
      end
    done
*)
()

  let buf_int8 b i =
    Buffer.add_char b (char_of_int (i land 0xff))

  let buf_int8_64 b iL =
    buf_int8 b (Int64.to_int iL)

  let buf_int16_64 b iL =
    buf_int8_64 b (Int64.shift_right iL 8);
    buf_int8_64 b iL;
    ()

  let buf_int32_64 b iL =
    buf_int16_64 b (Int64.shift_right iL 16);
    buf_int16_64 b iL;
    ()

  let buf_int64 b iL =
    buf_int32_64 b (Int64.shift_right iL 32);
    buf_int32_64 b iL;
    ()

  let get_uint8_64 s pos = Int64.of_int (int_of_char s.[pos])

  let get_uint16_64 s pos =
    let c2 = get_uint8_64 s pos in
    let c1 = get_uint8_64 s (pos+1) in
    Int64.logor c1 (Int64.shift_left c2 8), pos+2

  let get_uint32_64 s pos =
    let c2,pos = get_uint16_64 s pos in
    let c1,pos = get_uint16_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 16), pos

  let get_uint64 s pos =
    let c2,pos = get_uint32_64 s pos in
    let c1,pos = get_uint32_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 32), pos

  let get_int8_64 s pos =
    let i = int_of_char s.[pos] in
    let i = if i > 127 then i - 256 else i in
    Int64.of_int i

  let get_int16_64 s pos =
    let c2 = get_int8_64 s pos in
    let c1 = get_uint8_64 s (pos+1) in
    Int64.logor c1 (Int64.shift_left c2 8), pos+2

  let get_int32_64 s pos =
    let c2,pos = get_int16_64 s pos in
    let c1,pos = get_uint16_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 16), pos

  let get_int64 s pos =
    let c2,pos = get_int32_64 s pos in
    let c1,pos = get_uint32_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 32), pos

  let _ =
    test "BigEndian.get_int32_64" true buf_int32_64 get_int32_64;
    test "BigEndian.get_uint32_64" false buf_int32_64 get_uint32_64

  let get_uint8_64 s pos = get_uint8_64 s pos, pos+1
  let get_int8_64 s pos = get_int8_64 s pos, pos+1

  let get_int s pos =
    let intL, pos = get_int32_64 s pos in
    Int64.to_int intL, pos

  let get_uint s pos =
    let intL, pos = get_uint32_64 s pos in
    Int64.to_int intL, pos

  let get_int8 s pos =
    let intL, pos = get_int8_64 s pos in
    Int64.to_int intL, pos

  let get_uint8 s pos =
    let intL, pos = get_uint8_64 s pos in
    Int64.to_int intL, pos

  let get_int16 s pos =
    let intL, pos = get_int16_64 s pos in
    Int64.to_int intL, pos

  let get_uint16 s pos =
    let intL, pos = get_uint16_64 s pos in
    Int64.to_int intL, pos









  let get_int8_32 s pos =
    Int32.of_int (Char.code s.[pos]), pos+1
  let get_int16_32 s pos =
    let c1, pos = get_int8_32 s pos in
    let c2, pos = get_int8_32 s pos in
    Int32.logor c2 (Int32.shift_left c1 8), pos

  let get_int32 s pos =
    let c1, pos = get_int16_32 s pos in
    let c2, pos = get_int16_32 s pos in
    Int32.logor c2 (Int32.shift_left c1 16), pos

  let str_int8_32 s pos v =
    Bytes.set s pos (char_of_int ((Int32.to_int v) land 0xff))

  let str_int16_32 s pos v =
    str_int8_32 s (pos+1) v;
    str_int8_32 s pos (Int32.shift_right_logical v 8)

  let str_int32 s pos v =
    str_int16_32 s (pos+2) v;
    str_int16_32 s pos (Int32.shift_right_logical v 16);
    ()
end
module ElfTypes = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)


module IntMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

type uchar = int64
type addr = int64  (* unsigned *)
type half = int64  (* unsigned 16 bits *)
type off = int64   (* unsigned *)
type sword = int64 (* signed 32 bits *)
type word = int64  (* unsigned *)
type sxword = int64 (* signed 64 bits *)
type xword = int64 (* unsigned 64 bits *)


type byte_order = LittleEndian | BigEndian
type word_size = ARCH32 | ARCH64

type encoding = {
  byte_order : byte_order;
  word_size : word_size;
}

type elf_data_type =
  | ELF_DATA_TYPE_BYTE
  | ELF_DATA_TYPE_ADDR
  | ELF_DATA_TYPE_DYN
  | ELF_DATA_TYPE_EHDR
  | ELF_DATA_TYPE_HALF
  | ELF_DATA_TYPE_OFF
  | ELF_DATA_TYPE_PHDR
  | ELF_DATA_TYPE_RELA
  | ELF_DATA_TYPE_REL
  | ELF_DATA_TYPE_SHDR
  | ELF_DATA_TYPE_SWORD
  | ELF_DATA_TYPE_SYM
  | ELF_DATA_TYPE_WORD
  | ELF_DATA_TYPE_SXWORD
  | ELF_DATA_TYPE_XWORD
  | ELF_DATA_TYPE_VDEF
  | ELF_DATA_TYPE_VNEED
  | ELF_DATA_TYPE_NUM

type elf_type =
  | ET_NONE    (* No file type *)
  | ET_REL     (* relocatable file *)
  | ET_EXEC    (* executable file *)
  | ET_DYN     (* shared object file *)
  | ET_CORE    (* core file *)
  | ET_NUM of int
  | ET_OS of int (* TODO *)
  | ET_PROC of int (* TODO *)

type elf_data_encoding =
  | ELFDATANONE
  | ELFDATA2LSB
  | ELFDATA2MSB
  | ELFDATANUM of int

type elf_file_class =
  | ELFCLASSNONE
  | ELFCLASS32
  | ELFCLASS64
  | ELFCLASSNUM of int

type machine =
    EM_NONE          (* No machine *)
  | EM_M32           (* AT&T WE 32100 *)
  | EM_SPARC         (* SPARC *)
  | EM_386           (* Intel 80386 *)
  | EM_68K           (* Motorola 68000 *)
  | EM_88K           (* Motorola 88000 *)
  | EM_486           (* Intel i486 (DO NOT USE THIS ONE) *)
  | EM_860           (* Intel 80860 *)
  | EM_MIPS          (* MIPS I Architecture *)
  | EM_S370          (* IBM System/370 Processor *)
  | EM_MIPS_RS3_LE   (* MIPS RS3000 Little-endian *)
  | EM_SPARC64       (* SPARC 64-bit *)
  | EM_PARISC        (* Hewlett-Packard PA-RISC *)
  | EM_VPP500        (* Fujitsu VPP500 *)
  | EM_SPARC32PLUS   (* Enhanced instruction set SPARC *)
  | EM_960           (* Intel 80960 *)
  | EM_PPC           (* PowerPC *)
  | EM_PPC64         (* 64-bit PowerPC *)
  | EM_S390          (* IBM System/390 Processor *)
  | EM_SPU           (* Cell SPU *)
  | EM_V800          (* NEC V800 *)
  | EM_FR20          (* Fujitsu FR20 *)
  | EM_RH32          (* TRW RH-32 *)
  | EM_RCE           (* Motorola RCE *)
  | EM_ARM           (* Advanced RISC Machines ARM *)
  | EM_ALPHA         (* Digital Alpha *)
  | EM_SH            (* Hitachi SH *)
  | EM_SPARCV9       (* SPARC Version 9 *)
  | EM_TRICORE       (* Siemens TriCore embedded processor *)
  | EM_ARC           (* Argonaut RISC Core, Argonaut Technologies Inc. *)
  | EM_H8_300        (* Hitachi H8/300 *)
  | EM_H8_300H       (* Hitachi H8/300H *)
  | EM_H8S           (* Hitachi H8S *)
  | EM_H8_500        (* Hitachi H8/500 *)
  | EM_IA_64         (* Intel IA-64 processor architecture *)
  | EM_MIPS_X        (* Stanford MIPS-X *)
  | EM_COLDFIRE      (* Motorola ColdFire *)
  | EM_68HC12        (* Motorola M68HC12 *)
  | EM_MMA           (* Fujitsu MMA Multimedia Accelerator *)
  | EM_PCP           (* Siemens PCP *)
  | EM_NCPU          (* Sony nCPU embedded RISC processor *)
  | EM_NDR1          (* Denso NDR1 microprocessor *)
  | EM_STARCORE      (* Motorola Star*Core processor *)
  | EM_ME16          (* Toyota ME16 processor *)
  | EM_ST100         (* STMicroelectronics ST100 processor *)
  | EM_TINYJ         (* Advanced Logic Corp. TinyJ embedded processor family *)
  | EM_X86_64        (* AMD x86-64 architecture *)
  | EM_PDSP          (* Sony DSP Processor *)
  | EM_FX66          (* Siemens FX66 microcontroller *)
  | EM_ST9PLUS       (* STMicroelectronics ST9+ 8/16 bit microcontroller *)
  | EM_ST7           (* STMicroelectronics ST7 8-bit microcontroller *)
  | EM_68HC16        (* Motorola MC68HC16 Microcontroller *)
  | EM_68HC11        (* Motorola MC68HC11 Microcontroller *)
  | EM_68HC08        (* Motorola MC68HC08 Microcontroller *)
  | EM_68HC05        (* Motorola MC68HC05 Microcontroller *)
  | EM_SVX           (* Silicon Graphics SVx *)
  | EM_ST19          (* STMicroelectronics ST19 8-bit microcontroller *)
  | EM_VAX           (* Digital VAX *)
  | EM_CRIS          (* Axis Communications 32-bit embedded processor *)
  | EM_JAVELIN       (* Infineon Technologies 32-bit embedded processor *)
  | EM_FIREPATH      (* Element 14 64-bit DSP Processor *)
  | EM_ZSP           (* LSI Logic 16-bit DSP Processor *)
  | EM_MMIX          (* Donald Knuth's educational 64-bit processor *)
  | EM_HUANY         (* Harvard University machine-independent object files *)
  | EM_PRISM         (* SiTera Prism *)
  | EM_AVR           (* Atmel AVR 8-bit microcontroller *)
  | EM_FR30          (* Fujitsu FR30 *)
  | EM_D10V          (* Mitsubishi D10V *)
  | EM_D30V          (* Mitsubishi D30V *)
  | EM_V850          (* NEC v850 *)
  | EM_M32R          (* Mitsubishi M32R *)
  | EM_MN10300       (* Matsushita MN10300 *)
  | EM_MN10200       (* Matsushita MN10200 *)
  | EM_PJ            (* picoJava *)
  | EM_OPENRISC      (* OpenRISC 32-bit embedded processor *)
  | EM_ARC_A5        (* ARC Cores Tangent-A5 *)
  | EM_XTENSA        (* Tensilica Xtensa Architecture *)
  | EM_VIDEOCORE     (* Alphamosaic VideoCore processor *)
  | EM_TMM_GPP       (* Thompson Multimedia General Purpose Processor *)
  | EM_NS32K         (* National Semiconductor 32000 series *)
  | EM_TPC           (* Tenor Network TPC processor *)
  | EM_SNP1K         (* Trebia SNP 1000 processor *)
  | EM_ST200         (* STMicroelectronics (www.st.com) ST200 microcontroller *)
  | EM_IP2K          (* Ubicom IP2xxx microcontroller family *)
  | EM_MAX           (* MAX Processor *)
  | EM_CR            (* National Semiconductor CompactRISC microprocessor *)
  | EM_F2MC16        (* Fujitsu F2MC16 *)
  | EM_MSP430        (* Texas Instruments embedded microcontroller msp430 *)
  | EM_BLACKFIN      (* Analog Devices Blackfin (DSP) processor *)
  | EM_SE_C33        (* S1C33 Family of Seiko Epson processors *)
  | EM_SEP           (* Sharp embedded microprocessor *)
  | EM_ARCA          (* Arca RISC Microprocessor *)
  | EM_UNICORE       (* Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University *)
  | EM_NUM of int

type elf_osabi =
  | ELFOSABI_NONE
  | ELFOSABI_SYSV
  | ELFOSABI_HPUX
  | ELFOSABI_NETBSD
  | ELFOSABI_LINUX
  | ELFOSABI_SOLARIS
  | ELFOSABI_AIX
  | ELFOSABI_IRIX
  | ELFOSABI_FREEBSD
  | ELFOSABI_TRU64
  | ELFOSABI_MODESTO
  | ELFOSABI_OPENBSD
  | ELFOSABI_OPENVMS
  | ELFOSABI_NSK
  | ELFOSABI_AROS
  | ELFOSABI_ARM
  | ELFOSABI_STANDALONE
  | ELFOSABI_NUM of int

type elf_section_header_flag =
  | SHF_WRITE       (* writable data during execution *)
  | SHF_ALLOC       (* section is in memory during execution *)
  | SHF_EXECINSTR   (* executable machine instructions *)
  | SHF_NUM of int
(*
    | SHF_MERGE
    | SHF_STRINGS
    | SHF_INFO_LINK
    | SHF_LINK_ORDER
    | SHF_OS_NONCONFORMING
    | SHF_GROUP
    | SHF_TLS
    | SHF_MASKOS
    | SHF_MASKPROC
*)


  let standard_sections = [
    ".bss", "Data segment initialized at 0";
    ".comment", "Version Control information";
    ".data", "Initialized data";
    ".debug", "Debug data";
    ".dynamic", "Dynamic linking tables";
    ".dynstr",  "A String table for .dynamic section";
    ".dynsym", "A Symbol table for dynamic linking";
    ".fini", "Code to run on exit";
    ".got", "mach. dep. Global offset table";
    ".hash", "A Symbol hash table";
    ".init", "Code to run on entry";
    ".interp", "Program interpreter path name";
    ".line", "????";
    ".note", "Note section";
    ".plt", "mach. dep. Procedure linkage table";
    ".rel{name}", "Relocations for section {name} (.text for e.g.)";
    ".rela{name}", "Relocations for section {name} (.text for e.g.)";
    ".rodata",  "A Read-only data (constants and literals)";
    ".shstrtab", "Section name string table";
    ".strtab", "String table";
    ".symtab",  "Linker symbol table";
    ".text", "Executable code";
  ]


  type elf_symbol_bind_type =
    | STB_LOCAL  (* local symbol *)
    | STB_GLOBAL (* global symbol *)
    | STB_WEAK   (* low precedence global symbol *)
    | STB_OS of int
    | STB_PROC of int
    | STB_NUM of int

  type elf_symbol_type =
    | STT_NOTYPE     (* no type associated (e.g. absolute symbold) *)
    | STT_OBJECT     (* data object *)
    | STT_FUNC       (* function entry point *)
    | STT_SECTION    (* symbol asssociated with a section *)
    | STT_FILE       (* source file associated with the object file *)
          (* must have STB_LOCAL binding, SHN_ABS for section index and before all local symbols of the file *)
    | STT_OS of int
    | STT_PROC of int
    | STT_NUM of int


  type segment_type = (* p_type *)
    | PT_NULL           (* 0 : unused entry *)
    | PT_LOAD           (* 1 : loadable segment *)
    | PT_DYNAMIC        (* 2 : dynamic linking tables *)
    | PT_INTERP         (* 3 : program interpreter path name *)
    | PT_NOTE           (* 4 : note sections *)
    | PT_SHLIB          (* 5 : reserved *)
    | PT_PHDR           (* 6 : program header table *)
    | PT_OS of int      (* 0x6000_0000 - 0x6FFF_FFFF *)
    | PT_PROC of int    (* 0x7000_0000 - 0x7FFF_FFFF *)
    | PT_NUM of int

  type segment_attribute = (* p_flags *)
    | PF_X              (* 1 : execute *)
    | PF_W              (* 2 : write *)
    | PF_R              (* 3 : read *)
    | PF_OS of int      (* 0x00FF_0000 *)
    | PF_PROC of int    (* 0xFF00_0000 *)
    | PF_NUM of int

module RAW = struct

type elf_section_type =
  | SHT_NULL        (* useless section *)
  | SHT_PROGBITS    (* can only be used by the program itself *)
  | SHT_SYMTAB      (* symbol table (for linking) *)
  | SHT_STRTAB      (* string table *)
  | SHT_RELA        (* relocation entries with explicit addends *)
  | SHT_HASH        (* symbol hash table, required for dynamic linking *)
  | SHT_DYNAMIC     (* info for dynamic linking *)
  | SHT_NOTE        (* note on the file *)
  | SHT_NOBITS      (* contains nothing in the file, but probably allocated in memory *)
  | SHT_REL         (* relocation entries without explicit addends *)
  | SHT_SHLIB       (* reserved ? *)
  | SHT_DYNSYM      (* symbol table (for linking) *)
  | SHT_INIT_ARRAY
  | SHT_FINI_ARRAY
  | SHT_PREINIT_ARRAY
  | SHT_GROUP
  | SHT_SYMTAB_SHNDX
  | SHT_NUM of int
  | SHT_OS of int (* TODO *)
  | SHT_PROC of int (* TODO *)
  | SHT_USER of int (* TODO *)

  type section_header = {
    sh_name : word;               (* name of section, index in String Table *)
    sh_type : elf_section_type;   (* section type and semantics *)
    sh_flags : elf_section_header_flag list;  (* 1-bit flags *)
    sh_addr : addr;               (* required position in memory, or 0 *)
    sh_offset : off;              (* position of the section *)
    sh_size : word;               (* size of section *)
    sh_link : word;               (* index of a link to another section header in the section header table *)
    (* sh_type => associated section
       - SHT_DYNAMIC String table used by entries in this section
       - SHT_HASH Symbol table to which the hash table applies
       - SHT_REL
       - SHT_RELA  Symbol table referenced by relocations
       - SHT_SYMTAB
       - SHT_DYNSYM String table used by entries in this section
    *)
    sh_info : word;               (* extra info *)
    (* sh_type => meaining of sh_info
       - SHT_REL
       - SHT_RELA Section index of section to which the relocations apply
       - SHT_SYMTAB
       - SHT_DYNSYM Index of first non-local symbol (i.e., number of local symbols)
    *)
    sh_addralign : word;          (* alignment needed by this section 0,1,2,4,8, 16, etc *)
    sh_entsize : word;            (* entry size if the section is a table *)
  }

  type program_header = {
    p_type : segment_type;      (* type of segment  *)
    p_flags : word;     (* segment attributes *)
    p_offset : off;     (* offset in file *)
    p_vaddr : addr;     (* virtual address in memory *)
    p_paddr : addr;     (* reserved *)
    p_filesz : xword;   (* size of segment in file *)
    p_memsz : xword;    (* size of segment in memory *)
    p_align : xword;    (* alignment of segment *)
  }

  type program = {
    program_content : string; (* the content of the corresponding segment *)
    program_header : program_header;
  }

  type header = {
    e_ident : string;          (* 16 bytes ELF identification (magic string) *)
    e_file_class : elf_file_class;
    e_data_encoding : elf_data_encoding;
    e_file_version : int;
    e_osabi : elf_osabi;
    e_abi_version : int;

    e_type      : elf_type;  (* object file type *)
    e_machine   : machine;   (* machine type *)
    e_version   : word;      (* object file version (should be 1) *)
    e_entry     :   addr;    (* entry point address (or 0) *)
    e_phoff     :   off;     (* program header table position, or 0 *)
    e_shoff     :   off;     (* section header table position, or 0 *)
    e_flags     :   word;    (* processor specific flags *)
    e_ehsize    :   half;    (* elf header size in bytes *)
    e_phentsize :   half;    (* size of one entry in program header table *)
    e_phnum     :   half;    (* number of entries in program header table *)
    e_shentsize :   half;    (* size of one entry in section header table *)
    e_shnum     :   half;    (* number of entries in section header table *)
    e_shstrndx  :   half;    (* section name string table index in
                             section header table, or SHN_UNDEF *)
  }

  type t = {
    elf_content : string; (* the complete file *)
    elf_header : header;
    elf_programs : program array;  (* needed for execution *)
    elf_sections : section array;
  }

  and section = {
    section_name : string;
    section_content : string;
(*    section_interp : section_interp; *)
    section_header : section_header;
  }

end

module ABSTRACT = struct

  type symbol_section =
    | SYM_SHN_UNDEF
    | SYM_SHN_ABS
    | SYM_SHN_COMMON
    | SYM_SHN of string

  type elf_symbol = {
    st_name : string; (* st_name : word *)
    st_value : addr;  (* symbol value : absolute and relocatable address
                     relocatable files: alignment for commons,
                     or offset from begin of section for defined symbols
                     executable/shared files: virtual address for defined
                     relocatable symbols      *)
    st_size : word;   (* size of object (e.g. common)
                         0 if no associated size, or size unknown *)
(*    st_info : uchar;                   (* type and binding attributes *) *)
    st_bind : elf_symbol_bind_type;       (* st_info bind value *)
    st_type : elf_symbol_type;            (* st_info type value *)
(*    st_other : uchar;                  (* reserved *) *)
   (* st_shndx : half; *)
       (* section table index, where the symbol is defined
          if the symbol is undefined, it is SHN_UNDEF
          if the symbol is absolute, it is SHN_ABS
          if the symbol is common, it is SHN_COMMON
          *)
    st_section : symbol_section;
  }

  type elf64_rel = {
    rel_offset : addr;     (* address of reference
                              relocatable file: offset since beginning of section
                              executable/shared: virtual address
                           *)
    rel_info : xword;      (* symbol index and type of relocation *)
    rel_info_symbol : half; (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rel_info_type : half;   (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
  }

  type elf64_rela = {
    rela_offset : addr;     (* address of reference *)
    rela_info : xword;      (* symbold index and type of relocation *)
    rela_info_symbol : half; (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rela_info_type : half;   (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
    rela_addend : sxword;   (* constant part of expression (to be added before storing) *)
  }

  type elf32_rel = {
    rel_offset : addr;     (* address of reference
                              relocatable file: offset since beginning of section
                              executable/shared: virtual address
                           *)
    rel_info : xword;      (* symbol index and type of relocation *)
    rel_info_symbol : half; (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rel_info_type : half;   (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
  }

  type elf32_rela = {
    rela_offset : addr;     (* address of reference *)
    rela_info : word;      (* symbold index and type of relocation *)
    rela_info_symbol : half; (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rela_info_type : half;   (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
    rela_addend : sword;   (* constant part of expression (to be added before storing) *)
  }

  type note_section = { (* or note_segment *)
    notes : xword array;
    note_namez : xword; (* size of name *)
    note_descsz : xword; (* size of description *)
    note_type : xword;  (* type, originator dependent *)
    note_name : string; (* 0-terminated name, size does not include final 0, 8-padded in file *)
    note_desc : string; (* note content, 8-padded in file *)
  }

  type dynamic_tag =
      DT_NULL      (* 0 ignored Marks the end of the dynamic array *)
    | DT_NEEDED    (* 1 d_val The string table offset of the name of a needed library. *)
    | DT_PLTRELSZ  (*  2 d_val Total size, in bytes, of the relocation entries associated with
                      the procedure linkage table. *)
    | DT_PLTGOT    (*  3 d_ptr Contains an address associated with the linkage table. The
                       specific meaning of this field is processor-dependent. *)
    | DT_HASH      (*  4 d_ptr Address of the symbol hash table, described below. *)
    | DT_STRTAB    (* 5 d_ptr Address of the dynamic string table. *)
    | DT_SYMTAB    (* 6 d_ptr Address of the dynamic symbol table. *)
    | DT_RELA      (* 7 d_ptr Address of a relocation table with Elf64_Rela entries. *)
    | DT_RELASZ    (* 8 d_val Total size, in bytes, of the DT_RELA relocation table. *)
    | DT_RELAENT   (* 9 d_val Size, in bytes, of each DT_RELA relocation entry. *)
    | DT_STRSZ     (* 10 d_val Total size, in bytes, of the string table. *)
    | DT_SYMENT    (* 11 d_val Size, in bytes, of each symbol table entry. *)
    | DT_INIT      (* 12 d_ptr Address of the initialization function. *)
    | DT_FINI      (*  13 d_ptr Address of the termination function. *)
    | DT_SONAME    (* 14 d_val The string table offset of the name of this shared object. *)
    | DT_RPATH     (* 15 d_val The string table offset of a shared library search path string. *)
    | DT_SYMBOLIC  (* 16 ignored The presence of this dynamic table entry modifies the
                      symbol resolution algorithm for references within the
                      library. Symbols defined within the library are used to
                      resolve references before the dynamic linker searches the
                      usual search path. *)
    | DT_REL       (* 17 d_ptr Address of a relocation table with Elf64_Rel entries. *)
    | DT_RELSZ     (* 18 d_val Total size, in bytes, of the DT_REL relocation table. *)
    | DT_RELENT    (* 19 d_val Size, in bytes, of each DT_REL relocation entry. *)
    | DT_PLTREL    (* 20 d_val Type of relocation entry used for the procedure linkage
                      table. The d_val member contains either DT_REL or DT_RELA. *)
    | DT_DEBUG     (* 21 d_ptr Reserved for debugger use. *)
    | DT_TEXTREL   (* 22 ignored The presence of this dynamic table entry signals that the
                      relocation table contains relocations for a non-writable
                      segment. *)
    | DT_JMPREL    (* 23 d_ptr Address of the relocations associated with the procedure
                      linkage table. *)
    | DT_BIND_NOW   (* 24 ignored The presence of this dynamic table entry signals that the
                       dynamic loader should process all relocations for this object
                       before transferring control to the program. *)
    | DT_INIT_ARRAY (* 25 d_ptr Pointer to an array of pointers to initialization functions. *)
    | DT_FINI_ARRAY (* 26 d_ptr Pointer to an array of pointers to termination functions. *)
    | DT_INIT_ARRAYSZ (* 27 d_val Size, in bytes, of the array of initialization functions. *)
    | DT_FINI_ARRAYSZ (* 28 d_val Size, in bytes, of the array of termination functions. *)
    | DT_OS of int    (* 0x60000000 - 0x6FFF_FFFF Defines a range of dynamic table tags that are reserved for
                         environment-specific use. *)
    | DT_PROC of int  (* 0x7000_0000 - 0x7FFF_FFFF Defines a range of dynamic table tags that are reserved for
                         processor-specific use. *)
    | DT_NUM of int

  type dynamic = { (* ".dynamic" *)
    d_tag : sxword;  (* dynamic tag *)
    d_val : xword; (* for integer values, or d_ptr for virtual pointers *)
  }

  (* ".hash" section *)
  type hash_section = {
    hash_words : word array;
    hash_nbuckets : word;
    hash_nchain: word; (* also the number of symbols entries stored *)
    hash_buckets : word array;
    hash_chains : word array;
    (* the bucket contains the symbol number associated with the hash value, and the next symbol in the
       collision list is found in the chain at the index of the symbol number, until STN_UNDEF is found. *)
  }

(*
  unsigned long
  elf64_hash(const unsigned char *name)
  {
  unsigned long h = 0, g;
  while ( *name ) {
  h = (h << 4) + *name++;
  if (g = h & 0xf0000000)
  h ^= g >> 24;
  h &= 0x0f f f f f f f ;
  }
  return h;
  }
      *)

(*
  let elf64_hash name =
    let h = ref 0L in
(*    let g = ref 0L in *)
    for i = 0 to String.length name - 1 do
      let c = int_of_char name.[i] in
      h := Int64.add (Int64.shift_left !h 4) (Int64.of_int c);
      assert false;
    done;
    !h
*)

  type dynamic_segment = dynamic array

  type elf32_relocation =
    | R_386_NONE
    | R_386_32        (* direct,       S + A     *)
    | R_386_PC32      (* PC-relative,  S + A - P *)
    | R_386_GOT32     (* GOT entry,    G + A     *)
    | R_386_PLT32     (* PLT entry,    L + A - P *)
    | R_386_COPY
    | R_386_GLOB_DAT  (* create GOT entry, S *)
    | R_386_JMP_SLOT  (* create PLT entry, S *)
    | R_386_RELATIVE  (* rel. to program base, B + A *)
    | R_386_GOTOFF    (* offset to GOT, S + A - GOT *)
    | R_386_GOTPC     (* GOT + A - P *)
    | R_386_32PLT     (* L + A       *)
(* GNU extensions for LD *)
    | R_386_16       (* = 20 *) (* 16-bit direct,      S + A     *)
    | R_386_PC16     (* = 21 *) (* 16-bit PC-relative, S + A - P *)
    | R_386_8        (* = 22 *) (* 8-bit direct,       S + A     *)
    | R_386_PC8      (* = 23 *) (* 8-bit PC-relative,  S + A - P *)
    | R_386_UNKNOWN of int

  type elf64_relocation =
    | R_X86_64_NONE        (*  =   0 *)
    | R_X86_64_64          (*  =   1 *)    (* S + A     *)
    | R_X86_64_PC32        (*  =   2 *)    (* S + A - P *)
    | R_X86_64_GOT32       (*  =   3 *)    (* G + A     *)
    | R_X86_64_PLT32       (*  =   4 *)    (* L + A - P *)
    | R_X86_64_COPY        (*  =   5 *)    (*           *)
    | R_X86_64_GLOB_DAT    (*  =   6 *)    (* S         *)
    | R_X86_64_JUMP_SLOT   (*  =   7 *)    (* S         *)
    | R_X86_64_RELATIVE    (*  =   8 *)    (* B + A     *)
    | R_X86_64_GOTPCREL    (*  =   9 *)    (* G + GOT + A - P *)
    | R_X86_64_32          (*  =  10 *)    (* S + A     *)
    | R_X86_64_32S         (*  =  11 *)    (* S + A     *)
    | R_X86_64_16          (*  =  12 *)    (* S + A     *)
    | R_X86_64_PC16        (*  =  13 *)    (* S + A - P *)
    | R_X86_64_8           (*  =  14 *)    (* S + A     *)
    | R_X86_64_PC8         (*  =  15 *)    (* S + A - P *)
    | R_X86_64_DPTMOD64    (*  =  16 *)
    | R_X86_64_DTPOFF64    (*  =  17 *)
    | R_X86_64_TPOFF64     (*  =  18 *)
    | R_X86_64_TLSGD       (*  =  19 *)
    | R_X86_64_TLSLD       (*  =  20 *)
    | R_X86_64_DTPOFF32    (*  =  21 *)
    | R_X86_64_GOTTPOFF    (*  =  22 *)
    | R_X86_64_TPOFF32     (*  =  23 *)
    | R_X86_64_PC64        (*  =  24 *)    (* S + A - P   *)
    | R_X86_64_GOTOFF64    (*  =  25 *)    (* S + A - GOT *)
    | R_X86_64_GOTPC32     (*  =  26 *)    (* GOT + A - P *)
    | R_X86_64_SIZE32      (*  =  32 *)
    | R_X86_64_SIZE64      (*  =  33 *)
    | R_X86_64_UNKNOWN of int

  type 'relocation rela_reloc = {
    r_offset : int64;
    r_sym : int64;
    r_type : 'relocation;
    r_addend : int64;
  }

  type 'relocation rela = {
    rela_relocs : 'relocation rela_reloc array;
    rela_symbols : string;
    rela_target : string;
  }

  type section_bits = {
    sht_flags : elf_section_header_flag list;
    sht_addr : int64;
    sht_align : int;
    sht_content : string;
  }

  type section_desc =
    | SHT_NULL
    | SHT_PROGBITS of section_bits
    | SHT_NOBITS of section_bits
    | SHT_STRTAB of string
    | SHT_SYMTAB of elf_symbol array
    | SHT_RELA32 of elf32_relocation rela
    | SHT_RELA64 of elf64_relocation rela
    | SHT_REL32 of elf32_relocation rela
    | SHT_REL64 of elf64_relocation rela
    | SHT_UNKNOWN of RAW.section

  type section = {
    s_name : string;
    s_desc : section_desc;
  }

  type program_desc =
    | ProgramUnknown of RAW.program

  type program = {
    p_num : int;
    p_desc : program_desc;
  }

  type t = {
    e_file_class : elf_file_class;
    e_data_encoding : elf_data_encoding;
    e_file_version : int;
    e_osabi : elf_osabi;
    e_abi_version : int;

    e_type      : elf_type;  (* object file type *)
    e_machine   : machine;   (* machine type *)
    e_version   : word;      (* object file version (should be 1) *)
    e_entry     :   addr;    (* entry point address (or 0) *)
(*
    e_phoff     :   off;     (* program header table position, or 0 *)
    e_shoff     :   off;     (* section header table position, or 0 *)
*)
    e_flags     :   word;    (* processor specific flags *)
(*
  e_ehsize    :   half;    (* elf header size in bytes *)
  e_phentsize :   half;    (* size of one entry in program header table *)
  e_phnum     :   half;    (* number of entries in program header table *)
  e_shentsize :   half;    (* size of one entry in section header table *)
  e_shnum     :   half;    (* number of entries in section header table *)
  e_shstrndx  :   half;    (* section name string table index in
                              section header table, or SHN_UNDEF *)
*)
    mutable e_sections : section StringMap.t;
    mutable e_programs : program StringMap.t;
  }

end
end
module ElfPrinter: sig

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

type indent = string

module RAW : sig
  val to_ocaml : indent -> ElfTypes.RAW.t -> string
end


module ABSTRACT : sig
  val to_ocaml : indent -> ElfTypes.ABSTRACT.t -> string
end

val string_of_data_encoding : ElfTypes.elf_data_encoding -> string
end = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open ElfTypes

let compact = ref true

type indent = string

  let string_of_type = function
    | ET_NONE -> "ET_NONE"
    | ET_REL -> "ET_REL"
    | ET_EXEC -> "ET_EXEC"
    | ET_DYN -> "ET_DYN"
    | ET_CORE -> "ET_CORE"
    | ET_NUM n -> Printf.sprintf "ET_NUM %d" n
    | ET_OS n -> Printf.sprintf "ET_OS %d" n
    | ET_PROC n -> Printf.sprintf "ET_PROC %d" n

  let string_of_file_class = function
    | ELFCLASSNONE -> "ELFCLASSNONE"
    | ELFCLASS32 -> "ELFCLASS32"
    | ELFCLASS64 -> "ELFCLASS64"
    | ELFCLASSNUM n -> Printf.sprintf "ELFCLASSNUM %d" n


  let string_of_data_encoding = function
    | ELFDATANONE -> "ELFDATANONE"
    | ELFDATA2LSB -> "ELFDATA2LSB"
    | ELFDATA2MSB -> "ELFDATA2MSB"
    | ELFDATANUM n -> Printf.sprintf "ELFDATANUM %d" n

  let string_of_osabi = function
    | ELFOSABI_NONE -> "ELFOSABI_NONE"
    | ELFOSABI_SYSV -> "ELFOSABI_SYSV"
    | ELFOSABI_HPUX -> "ELFOSABI_HPUX"
    | ELFOSABI_NETBSD -> "ELFOSABI_NETBSD"
    | ELFOSABI_LINUX -> "ELFOSABI_LINUX"
    | ELFOSABI_SOLARIS -> "ELFOSABI_SOLARIS"
    | ELFOSABI_AIX -> "ELFOSABI_AIX"
    | ELFOSABI_IRIX -> "ELFOSABI_IRIX"
    | ELFOSABI_FREEBSD -> "ELFOSABI_FREEBSD"
    | ELFOSABI_TRU64 -> "ELFOSABI_TRU64"
    | ELFOSABI_MODESTO -> "ELFOSABI_MODESTO"
    | ELFOSABI_OPENBSD -> "ELFOSABI_OPENBSD"
    | ELFOSABI_OPENVMS -> "ELFOSABI_OPENVMS"
    | ELFOSABI_NSK -> "ELFOSABI_NSK"
    | ELFOSABI_AROS -> "ELFOSABI_AROS"
    | ELFOSABI_ARM -> "ELFOSABI_ARM"
    | ELFOSABI_STANDALONE -> "ELFOSABI_STANDALONE"
    | ELFOSABI_NUM n -> Printf.sprintf "ELFOSABI %d" n

  let string_of_machine = function
    | EM_NONE        -> "EM_NONE"
    | EM_M32         -> "EM_M32"
    | EM_SPARC       -> "EM_SPARC"
    | EM_386         -> "EM_386"
    | EM_68K         -> "EM_68K"
    | EM_88K         -> "EM_88K"
    | EM_486         -> "EM_486"
    | EM_860         -> "EM_860"
    | EM_MIPS        -> "EM_MIPS"
    | EM_S370        -> "EM_S370"
    | EM_MIPS_RS3_LE -> "EM_MIPS_RS3_LE"
    | EM_SPARC64     -> "EM_SPARC64"
    | EM_PARISC      -> "EM_PARISC"
    | EM_VPP500      -> "EM_VPP500"
    | EM_SPARC32PLUS -> "EM_SPARC32PLUS"
    | EM_960         -> "EM_960"
    | EM_PPC         -> "EM_PPC"
    | EM_PPC64       -> "EM_PPC64"
    | EM_S390        -> "EM_S390"
    | EM_SPU         -> "EM_SPU"
    | EM_V800        -> "EM_V800"
    | EM_FR20        -> "EM_FR20"
    | EM_RH32        -> "EM_RH32"
    | EM_RCE         -> "EM_RCE"
    | EM_ARM         -> "EM_ARM"
    | EM_ALPHA       -> "EM_ALPHA"
    | EM_SH          -> "EM_SH"
    | EM_SPARCV9     -> "EM_SPARCV9"
    | EM_TRICORE     -> "EM_TRICORE"
    | EM_ARC         -> "EM_ARC"
    | EM_H8_300      -> "EM_H8_300"
    | EM_H8_300H     -> "EM_H8_300H"
    | EM_H8S         -> "EM_H8S"
    | EM_H8_500      -> "EM_H8_500"
    | EM_IA_64       -> "EM_IA_64"
    | EM_MIPS_X      -> "EM_MIPS_X"
    | EM_COLDFIRE    -> "EM_COLDFIRE"
    | EM_68HC12      -> "EM_68HC12"
    | EM_MMA         -> "EM_MMA"
    | EM_PCP         -> "EM_PCP"
    | EM_NCPU        -> "EM_NCPU"
    | EM_NDR1        -> "EM_NDR1"
    | EM_STARCORE    -> "EM_STARCORE"
    | EM_ME16        -> "EM_ME16"
    | EM_ST100       -> "EM_ST100"
    | EM_TINYJ       -> "EM_TINYJ"
    | EM_X86_64      -> "EM_X86_64"
    | EM_PDSP        -> "EM_PDSP"
    | EM_FX66        -> "EM_FX66"
    | EM_ST9PLUS     -> "EM_ST9PLUS"
    | EM_ST7         -> "EM_ST7"
    | EM_68HC16      -> "EM_68HC16"
    | EM_68HC11      -> "EM_68HC11"
    | EM_68HC08      -> "EM_68HC08"
    | EM_68HC05      -> "EM_68HC05"
    | EM_SVX         -> "EM_SVX"
    | EM_ST19        -> "EM_ST19"
    | EM_VAX         -> "EM_VAX"
    | EM_CRIS        -> "EM_CRIS"
    | EM_JAVELIN     -> "EM_JAVELIN"
    | EM_FIREPATH    -> "EM_FIREPATH"
    | EM_ZSP         -> "EM_ZSP"
    | EM_MMIX        -> "EM_MMIX"
    | EM_HUANY       -> "EM_HUANY"
    | EM_PRISM       -> "EM_PRISM"
    | EM_AVR         -> "EM_AVR"
    | EM_FR30        -> "EM_FR30"
    | EM_D10V        -> "EM_D10V"
    | EM_D30V        -> "EM_D30V"
    | EM_V850        -> "EM_V850"
    | EM_M32R        -> "EM_M32R"
    | EM_MN10300     -> "EM_MN10300"
    | EM_MN10200     -> "EM_MN10200"
    | EM_PJ          -> "EM_PJ"
    | EM_OPENRISC    -> "EM_OPENRISC"
    | EM_ARC_A5      -> "EM_ARC_A5"
    | EM_XTENSA      -> "EM_XTENSA"
    | EM_VIDEOCORE   -> "EM_VIDEOCORE"
    | EM_TMM_GPP     -> "EM_TMM_GPP"
    | EM_NS32K       -> "EM_NS32K"
    | EM_TPC         -> "EM_TPC"
    | EM_SNP1K       -> "EM_SNP1K"
    | EM_ST200       -> "EM_ST200"
    | EM_IP2K        -> "EM_IP2K"
    | EM_MAX         -> "EM_MAX"
    | EM_CR          -> "EM_CR"
    | EM_F2MC16      -> "EM_F2MC16"
    | EM_MSP430      -> "EM_MSP430"
    | EM_BLACKFIN    -> "EM_BLACKFIN"
    | EM_SE_C33      -> "EM_SE_C33"
    | EM_SEP         -> "EM_SEP"
    | EM_ARCA        -> "EM_ARCA"
    | EM_UNICORE     -> "EM_UNICORE"
    | EM_NUM n -> Printf.sprintf "EM_NUM %d" n

  let info_of_machine = function
    | EM_NONE        -> "No machine"
    | EM_M32         -> "AT&T WE 32100"
    | EM_SPARC       -> "SPARC"
    | EM_386         -> "Intel 80386"
    | EM_68K         -> "Motorola 68000"
    | EM_88K         -> "Motorola 88000"
    | EM_486         -> "Intel i486 (DO NOT USE THIS ONE)"
    | EM_860         -> "Intel 80860"
    | EM_MIPS        -> "MIPS I Architecture"
    | EM_S370        -> "IBM System/370 Processor"
    | EM_MIPS_RS3_LE -> "MIPS RS3000 Little-endian"
    | EM_SPARC64     -> "SPARC 64-bit"
    | EM_PARISC      -> "Hewlett-Packard PA-RISC"
    | EM_VPP500      -> "Fujitsu VPP500"
    | EM_SPARC32PLUS -> "Enhanced instruction set SPARC"
    | EM_960         -> "Intel 80960"
    | EM_PPC         -> "PowerPC"
    | EM_PPC64       -> "64-bit PowerPC"
    | EM_S390        -> "IBM System/390 Processor"
    | EM_SPU         -> "Cell SPU"
    | EM_V800        -> "NEC V800"
    | EM_FR20        -> "Fujitsu FR20"
    | EM_RH32        -> "TRW RH-32"
    | EM_RCE         -> "Motorola RCE"
    | EM_ARM         -> "Advanced RISC Machines ARM"
    | EM_ALPHA       -> "Digital Alpha"
    | EM_SH          -> "Hitachi SH"
    | EM_SPARCV9     -> "SPARC Version 9"
    | EM_TRICORE     -> "Siemens TriCore embedded processor"
    | EM_ARC         -> "Argonaut RISC Core, Argonaut Technologies Inc."
    | EM_H8_300      -> "Hitachi H8/300"
    | EM_H8_300H     -> "Hitachi H8/300H"
    | EM_H8S         -> "Hitachi H8S"
    | EM_H8_500      -> "Hitachi H8/500"
    | EM_IA_64       -> "Intel IA-64 processor architecture"
    | EM_MIPS_X      -> "Stanford MIPS-X"
    | EM_COLDFIRE    -> "Motorola ColdFire"
    | EM_68HC12      -> "Motorola M68HC12"
    | EM_MMA         -> "Fujitsu MMA Multimedia Accelerator"
    | EM_PCP         -> "Siemens PCP"
    | EM_NCPU        -> "Sony nCPU embedded RISC processor"
    | EM_NDR1        -> "Denso NDR1 microprocessor"
    | EM_STARCORE    -> "Motorola Star*Core processor"
    | EM_ME16        -> "Toyota ME16 processor"
    | EM_ST100       -> "STMicroelectronics ST100 processor"
    | EM_TINYJ       -> "Advanced Logic Corp. TinyJ embedded processor family"
    | EM_X86_64      -> "AMD x86-64 architecture"
    | EM_PDSP        -> "Sony DSP Processor"
    | EM_FX66        -> "Siemens FX66 microcontroller"
    | EM_ST9PLUS     -> "STMicroelectronics ST9+ 8/16 bit microcontroller"
    | EM_ST7         -> "STMicroelectronics ST7 8-bit microcontroller"
    | EM_68HC16      -> "Motorola MC68HC16 Microcontroller"
    | EM_68HC11      -> "Motorola MC68HC11 Microcontroller"
    | EM_68HC08      -> "Motorola MC68HC08 Microcontroller"
    | EM_68HC05      -> "Motorola MC68HC05 Microcontroller"
    | EM_SVX         -> "Silicon Graphics SVx"
    | EM_ST19        -> "STMicroelectronics ST19 8-bit microcontroller"
    | EM_VAX         -> "Digital VAX"
    | EM_CRIS        -> "Axis Communications 32-bit embedded processor"
    | EM_JAVELIN     -> "Infineon Technologies 32-bit embedded processor"
    | EM_FIREPATH    -> "Element 14 64-bit DSP Processor"
    | EM_ZSP         -> "LSI Logic 16-bit DSP Processor"
    | EM_MMIX        -> "Donald Knuth's educational 64-bit processor"
    | EM_HUANY       -> "Harvard University machine-independent object files"
    | EM_PRISM       -> "SiTera Prism"
    | EM_AVR         -> "Atmel AVR 8-bit microcontroller"
    | EM_FR30        -> "Fujitsu FR30"
    | EM_D10V        -> "Mitsubishi D10V"
    | EM_D30V        -> "Mitsubishi D30V"
    | EM_V850        -> "NEC v850"
    | EM_M32R        -> "Mitsubishi M32R"
    | EM_MN10300     -> "Matsushita MN10300"
    | EM_MN10200     -> "Matsushita MN10200"
    | EM_PJ          -> "picoJava"
    | EM_OPENRISC    -> "OpenRISC 32-bit embedded processor"
    | EM_ARC_A5      -> "ARC Cores Tangent-A5"
    | EM_XTENSA      -> "Tensilica Xtensa Architecture"
    | EM_VIDEOCORE   -> "Alphamosaic VideoCore processor"
    | EM_TMM_GPP     -> "Thompson Multimedia General Purpose Processor"
    | EM_NS32K       -> "National Semiconductor 32000 series"
    | EM_TPC         -> "Tenor Network TPC processor"
    | EM_SNP1K       -> "Trebia SNP 1000 processor"
    | EM_ST200       -> "STMicroelectronics (www.st.com) ST200 microcontroller"
    | EM_IP2K        -> "Ubicom IP2xxx microcontroller family"
    | EM_MAX         -> "MAX Processor"
    | EM_CR          -> "National Semiconductor CompactRISC microprocessor"
    | EM_F2MC16      -> "Fujitsu F2MC16"
    | EM_MSP430      -> "Texas Instruments embedded microcontroller msp430"
    | EM_BLACKFIN    -> "Analog Devices Blackfin (DSP) processor"
    | EM_SE_C33      -> "S1C33 Family of Seiko Epson processors"
    | EM_SEP         -> "Sharp embedded microprocessor"
    | EM_ARCA        -> "Arca RISC Microprocessor"
    | EM_UNICORE     -> "Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University"
    | EM_NUM n -> Printf.sprintf "unknown EM_NUM %d" n

  let string_of_section_header_flag = function
    | SHF_WRITE -> "SHF_WRITE"
    | SHF_ALLOC ->   "SHF_ALLOC"
    | SHF_EXECINSTR -> "SHF_EXECINSTR"
    | SHF_NUM n -> Printf.sprintf "SHF_NUM %d" n

  let string_of_symbol_bind_type = function
    | STB_LOCAL -> "STB_LOCAL"
    | STB_GLOBAL -> "STB_GLOBAL"
    | STB_WEAK -> "STB_WEAK"
    | STB_OS n -> Printf.sprintf "STB_OS %d" n
    | STB_PROC n -> Printf.sprintf "STB_PROC %d" n
    | STB_NUM n -> Printf.sprintf "STB_NUM %d" n

  let string_of_symbol_type = function
    | STT_NOTYPE -> "STT_NOTYPE"
    | STT_OBJECT -> "STT_OBJECT"
    | STT_FUNC -> "STT_FUNC"
    | STT_SECTION -> "STT_SECTION"
    | STT_FILE -> "STT_FILE"
    | STT_OS n -> Printf.sprintf "STT_OS %d" n
    | STT_PROC n -> Printf.sprintf "STT_PROC %d" n
    | STT_NUM n -> Printf.sprintf "STT_NUM %d" n

let segment_type = function
  | PT_NULL -> "PT_NULL"
  | PT_LOAD -> "PT_LOAD"
  | PT_DYNAMIC -> "PT_DYNAMIC"
  | PT_INTERP -> "PT_INTERP"
  | PT_NOTE -> "PT_NOTE"
  | PT_SHLIB -> "PT_SHLIB"
  | PT_PHDR -> "PT_PHDR"
  | PT_OS n -> Printf.sprintf "PT_OS %x" n
  | PT_PROC n -> Printf.sprintf "PT_PROC %x" n
  | PT_NUM n -> Printf.sprintf "PT_NUM %x" n

module RAW = struct
  open ElfTypes.RAW


  let string_of_section_type = function
    | SHT_NULL -> "SHT_NULL"
    | SHT_PROGBITS -> "SHT_PROGBITS"
    | SHT_SYMTAB -> "SHT_SYMTAB"
    | SHT_STRTAB -> "SHT_STRTAB"
    | SHT_RELA -> "SHT_RELA"
    | SHT_HASH -> "SHT_HASH"
    | SHT_DYNAMIC -> "SHT_DYNAMIC"
    | SHT_NOTE -> "SHT_NOTE"
    | SHT_NOBITS -> "SHT_NOBITS"
    | SHT_REL -> "SHT_REL"
    | SHT_SHLIB -> "SHT_SHLIB"
    | SHT_DYNSYM -> "SHT_DYNSYM"
    | SHT_INIT_ARRAY -> "SHT_INIT_ARRAY"
    | SHT_FINI_ARRAY -> "SHT_FINI_ARRAY"
    | SHT_PREINIT_ARRAY -> "SHT_PREINIT_ARRAY"
    | SHT_GROUP -> "SHT_GROUP"
    | SHT_SYMTAB_SHNDX -> "SHT_SYMTAB_SHNDX"
    | SHT_NUM n -> Printf.sprintf "SHT_NUM %d" n
    | SHT_OS n -> Printf.sprintf "SHT_OS %d" n
    | SHT_PROC n -> Printf.sprintf "SHT_PROC %d" n
    | SHT_USER n -> Printf.sprintf "SHT_USER %d" n

  let section_header b indent sh =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s sh_name = %Lx\n" indent sh.sh_name;
    Printf.bprintf b "%s sh_type = %s;\n" indent (string_of_section_type sh.sh_type);
    Printf.bprintf b "%s sh_flags = [" indent;
      List.iter (fun f ->
        Printf.bprintf b " %s;" (string_of_section_header_flag f))
        sh.sh_flags;
      Printf.bprintf b "];\n";

    Printf.bprintf b "%s sh_addr = %Lx;\n" indent sh.sh_addr;
    Printf.bprintf b "%s sh_offset = %Ld;\n" indent sh.sh_offset;
    Printf.bprintf b "%s sh_size = %Ld;\n" indent sh.sh_size;
    Printf.bprintf b "%s sh_link = %Ld;\n" indent sh.sh_link;
    Printf.bprintf b "%s sh_info = %Ld;\n" indent sh.sh_info;
    Printf.bprintf b "%s sh_addralign = %Ld;\n" indent sh.sh_addralign;
    Printf.bprintf b "%s sh_entsize = %Ld;\n" indent sh.sh_entsize;
    Printf.bprintf b "%s}" indent

  let section b indent s =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s section_name = %S;\n" indent s.section_name;
    Printf.bprintf b "%s section_content = string[%d];\n" indent
      (String.length s.section_content);
    Printf.bprintf b "%s section_header = " indent;
    section_header b (indent ^ "  ") s.section_header;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s}" indent

  let header b indent h =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s  e_ident = %S; (* magic number *)\n" indent h.e_ident;
    Printf.bprintf b "%s  e_file_class = %s;\n" indent (string_of_file_class h.e_file_class);
    Printf.bprintf b "%s e_data_encoding = %s;\n" indent (string_of_data_encoding h.e_data_encoding);
    Printf.bprintf b "%s e_file_version = %d;\n" indent h.e_file_version;
    Printf.bprintf b "%s e_osabi = %s;\n" indent (string_of_osabi h.e_osabi);
    Printf.bprintf b "%s e_abi_version = %d;\n" indent h.e_abi_version;
    Printf.bprintf b "%s e_type = %s;\n" indent (string_of_type h.e_type);
    Printf.bprintf b "%s e_machine = %s;\n" indent (string_of_machine h.e_machine);
    Printf.bprintf b "%s e_version = %Ld;\n"  indent h.e_version;
    Printf.bprintf b "%s e_entry = %Ld; (* entry point addr *)\n"
      indent h.e_entry;
    Printf.bprintf b "%s e_phoff = %Ld; (*program header table offset*)\n"
      indent h.e_phoff;
    Printf.bprintf b "%s e_shoff = %Ld; (*section header table offset*)\n"
      indent h.e_shoff;
    Printf.bprintf b "%s e_flags = %Ld;\n" indent h.e_flags;
    Printf.bprintf b "%s e_ehsize = %Ld; (*elf header size*)\n" indent h.e_ehsize;
    Printf.bprintf b "%s e_phentsize = %Ld; (*program header table entry size*)\n" indent h.e_phentsize;
    Printf.bprintf b "%s e_phnum = %Ld; (*program header table number of entries*)\n" indent h.e_phnum;
    Printf.bprintf b "%s e_shentsize = %Ld; (*section header table entry size*)\n" indent h.e_shentsize;
    Printf.bprintf b "%s e_shnum = %Ld; (*section header table number of entries*)\n" indent h.e_shnum;
    Printf.bprintf b "%s e_shstrndx = %Ld; (*string table section index in section header table*)\n" indent h.e_shstrndx;
    Printf.bprintf b "%s}" indent



  let to_ocaml indent elf =
    let b = Buffer.create 1000 in
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s elf_content = string[%d];\n"
      indent (String.length elf.elf_content);
    Printf.bprintf b "%s elf_header = " indent;
    header b (indent ^ "    ") elf.elf_header;
    Printf.bprintf b ";\n";

    Printf.bprintf b "%s elf_programs = [| (* %d programs *)\n"
      indent (Array.length elf.elf_programs);
    Printf.bprintf b "%s  |];\n" indent;

    Printf.bprintf b "%s elf_sections = [| (* %d sections *)\n"
      indent (Array.length elf.elf_sections);
    Array.iteri (fun i s ->
      Printf.bprintf b "%s   (* section %d *) " indent i;
      section b (indent ^ "     ") s;
      Printf.bprintf b ";\n";
    ) elf.elf_sections;
    Printf.bprintf b "%s  |];\n" indent;
    Buffer.contents b

end

module ABSTRACT = struct

  open ElfTypes.ABSTRACT

  let symbol b indent st =
    if !compact then begin
      Printf.bprintf b "{";
      Printf.bprintf b " st_name = %S;" st.st_name;
      Printf.bprintf b " st_value = 0x%Lx;"  st.st_value;
      Printf.bprintf b " st_size = %Ld;\n"  st.st_size;
      Printf.bprintf b "%s st_bind = %s;" indent
        (string_of_symbol_bind_type st.st_bind);
      Printf.bprintf b " st_type = %s;"
        (string_of_symbol_type st.st_type);
      (*    Printf.bprintf b "%s st_other = %s;\n" indent st.st_other; *)
      Printf.bprintf b " st_section = %s; }"
        (match st.st_section with
         | SYM_SHN_UNDEF -> "SYM_SHN_UNDEF"
         | SYM_SHN_ABS -> "SYM_SHN_ABS"
         | SYM_SHN_COMMON -> "SYM_SHN_COMMON"
         | SYM_SHN s -> Printf.sprintf "SYM_SHN %S" s
        );
    end  else  begin
      Printf.bprintf b "{\n";
      Printf.bprintf b "%s st_name = %S;\n" indent st.st_name;
      Printf.bprintf b "%s st_value = 0x%Lx;\n" indent st.st_value;
      Printf.bprintf b "%s st_size = %Ld;\n" indent st.st_size;
      Printf.bprintf b "%s st_bind = %s;\n" indent
        (string_of_symbol_bind_type st.st_bind);
      Printf.bprintf b "%s st_type = %s;\n" indent
        (string_of_symbol_type st.st_type);
      (*    Printf.bprintf b "%s st_other = %s;\n" indent st.st_other; *)
      Printf.bprintf b "%s st_section = %s;\n" indent
        (match st.st_section with
         | SYM_SHN_UNDEF -> "SYM_SHN_UNDEF"
         | SYM_SHN_ABS -> "SYM_SHN_ABS"
         | SYM_SHN_COMMON -> "SYM_SHN_COMMON"
         | SYM_SHN s -> Printf.sprintf "SYM_SHN %S" s
        );
      Printf.bprintf b "%s}" indent
    end

  let prog_bits b indent sht =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s sht_flags = [" indent;
      List.iter (fun f ->
        Printf.bprintf b " %s;" (string_of_section_header_flag f))
        sht.sht_flags;
      Printf.bprintf b "];\n";
    Printf.bprintf b "%s sht_addr = 0x%Lx;\n" indent sht.sht_addr;
(*    Printf.bprintf b "%s sht_size = %d;\n" indent sht.sht_size; *)
    Printf.bprintf b "%s sht_align = %d;\n" indent sht.sht_align;
    Printf.bprintf b "%s sht_content = string[%d];\n" indent
      (String.length sht.sht_content);
    Printf.bprintf b "%s}" indent

  let string_of_elf64_relocation = function
    | R_X86_64_NONE         -> "R_X86_64_NONE"
    | R_X86_64_64           -> "R_X86_64_64"
    | R_X86_64_PC32         -> "R_X86_64_PC32"
    | R_X86_64_GOT32        -> "R_X86_64_GOT32"
    | R_X86_64_PLT32        -> "R_X86_64_PLT32"
    | R_X86_64_COPY         -> "R_X86_64_COPY"
    | R_X86_64_GLOB_DAT     -> "R_X86_64_GLOB_DAT"
    | R_X86_64_JUMP_SLOT    -> "R_X86_64_JUMP_SLOT"
    | R_X86_64_RELATIVE     -> "R_X86_64_RELATIVE"
    | R_X86_64_GOTPCREL     -> "R_X86_64_GOTPCREL"
    | R_X86_64_32           -> "R_X86_64_32"
    | R_X86_64_32S          -> "R_X86_64_32S"
    | R_X86_64_16           -> "R_X86_64_16"
    | R_X86_64_PC16         -> "R_X86_64_PC16"
    | R_X86_64_8            -> "R_X86_64_8"
    | R_X86_64_PC8          -> "R_X86_64_PC8"
    | R_X86_64_DPTMOD64     -> "R_X86_64_DPTMOD64"
    | R_X86_64_DTPOFF64     -> "R_X86_64_DTPOFF64"
    | R_X86_64_TPOFF64      -> "R_X86_64_TPOFF64"
    | R_X86_64_TLSGD        -> "R_X86_64_TLSGD"
    | R_X86_64_TLSLD        -> "R_X86_64_TLSLD"
    | R_X86_64_DTPOFF32     -> "R_X86_64_DTPOFF32"
    | R_X86_64_GOTTPOFF     -> "R_X86_64_GOTTPOFF"
    | R_X86_64_TPOFF32      -> "R_X86_64_TPOFF32"
    | R_X86_64_PC64         -> "R_X86_64_PC64"
    | R_X86_64_GOTOFF64     -> "R_X86_64_GOTOFF64"
    | R_X86_64_GOTPC32      -> "R_X86_64_GOTPC32"
    | R_X86_64_SIZE32       -> "R_X86_64_SIZE32"
    | R_X86_64_SIZE64       -> "R_X86_64_SIZE64"
    | R_X86_64_UNKNOWN n    -> Printf.sprintf "R_X86_64_UNKNOWN %d" n

  let string_of_elf32_relocation = function
    | R_386_NONE -> "R_386_NONE"
    | R_386_32 -> "R_386_32"
    | R_386_PC32 -> "R_386_PC32"
    | R_386_GOT32 -> "R_386_GOT32"
    | R_386_PLT32 -> "R_386_PLT32"
    | R_386_COPY -> "R_386_COPY"
    | R_386_GLOB_DAT -> "R_386_GLOB_DAT"
    | R_386_JMP_SLOT -> "R_386_JMP_SLOT"
    | R_386_RELATIVE -> "R_386_RELATIVE"
    | R_386_GOTOFF   -> "R_386_GOTOFF"
    | R_386_GOTPC    -> "R_386_GOTPC"
    | R_386_32PLT    -> "R_386_32PLT"
    | R_386_16 -> "R_386_16"
    | R_386_PC16 -> "R_386_PC16"
    | R_386_8    -> "R_386_8"
    | R_386_PC8  -> "R_386_PC8"
    | R_386_UNKNOWN n -> Printf.sprintf "R_386_UNKNOWN %d" n

  let relocations e string_of_reloc_type b relA indent
      { rela_symbols; rela_relocs; rela_target } =

    Printf.bprintf b "SHT_REL%s {\n" (if relA then "A" else "");
    Printf.bprintf b "%s rela_symbols = %S;\n" indent rela_symbols;
    let symbols = StringMap.find rela_symbols e.e_sections in
    Printf.bprintf b "%s rela_target = %S;\n" indent rela_target;
    Printf.bprintf b "%s rela_relocs = [| (* %d relocs *)\n" indent (Array.length rela_relocs);
    Printf.bprintf b "(* if unspecified, r_addend = 0 *)\n";
    Array.iter (fun r ->
      if !compact then begin
        Printf.bprintf b " {" ;
        Printf.bprintf b " r_offset = %Ld;" r.r_offset;
        Printf.bprintf b " r_sym = %Ld;" r.r_sym;
        (match symbols.s_desc with
           SHT_SYMTAB syms ->
           Printf.bprintf b "(* %S *)" syms.(Int64.to_int r.r_sym).st_name
         | _ -> ());
        Printf.bprintf b " r_type = %s;" (string_of_reloc_type r.r_type);
        if r.r_addend <> 0L then
          Printf.bprintf b " r_addend = %Ld;" r.r_addend;
        Printf.bprintf b " };\n"
      end else begin
        Printf.bprintf b "%s {\n" indent;
        Printf.bprintf b "%s  r_offset = %Ld;\n" indent r.r_offset;
        Printf.bprintf b "%s  r_sym = %Ld;" indent r.r_sym;
        (match symbols.s_desc with
           SHT_SYMTAB syms ->
           Printf.bprintf b "(* %S *)" syms.(Int64.to_int r.r_sym).st_name
         | _ -> ());
        Printf.bprintf b "\n";
        Printf.bprintf b "%s  r_type = %s;\n" indent
          (string_of_reloc_type r.r_type);
        Printf.bprintf b "%s  r_addend = %Ld;\n" indent r.r_addend;
        Printf.bprintf b "%s };\n" indent
      end
    ) rela_relocs;
    Printf.bprintf b "|]"

  let section_desc e b indent desc =
    match desc with
    | SHT_STRTAB s ->
      Printf.bprintf b "SHT_STRTAB %d\n" (String.length s)
    | SHT_NOBITS bits ->
      Printf.bprintf b "SHT_NOBITS";
      prog_bits b indent bits
    | SHT_PROGBITS bits ->
      Printf.bprintf b "SHT_PROGBITS";
      prog_bits b indent bits
    | SHT_SYMTAB syms ->
      Printf.bprintf b "SHT_SYMTAB [| (* %d symbols *)\n" (Array.length syms);
      Array.iter (fun sym ->
        Printf.bprintf b "%s" indent;
        symbol b (indent ^ "  ") sym;
        Printf.bprintf b ";\n"
      ) syms;
      Printf.bprintf b "|]"
    | SHT_REL32 rela ->
      relocations e string_of_elf32_relocation b false indent rela
    | SHT_RELA32 rela ->
      relocations e string_of_elf32_relocation b true indent rela
    | SHT_REL64 rela ->
      relocations e string_of_elf64_relocation b false indent rela
    | SHT_RELA64 rela ->
      relocations e string_of_elf64_relocation b true indent rela
    | SHT_NULL ->
      Printf.bprintf b "SHT_NULL"
    | SHT_UNKNOWN sec ->
      Printf.bprintf b "SHT_UNKNOWN ";
      RAW.section b (indent ^ "    ") sec


  let section e b indent s =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s s_name = %S;\n" indent s.s_name;
    Printf.bprintf b "%s s_desc = " indent;
    section_desc e b (indent ^ "    ") s.s_desc;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s}" indent

let to_ocaml indent e =
  let b = Buffer.create 1000 in

    Printf.bprintf b "{\n";
(*    Printf.bprintf b "%s  e_ident = %S; (* magic number *)\n" indent e.e_ident; *)
    Printf.bprintf b "%s  e_file_class = %s;\n" indent (string_of_file_class e.e_file_class);
    Printf.bprintf b "%s e_data_encoding = %s;\n" indent (string_of_data_encoding e.e_data_encoding);
    Printf.bprintf b "%s e_file_version = %d;\n" indent e.e_file_version;
    Printf.bprintf b "%s e_osabi = %s;\n" indent (string_of_osabi e.e_osabi);
    Printf.bprintf b "%s e_abi_version = %d;\n" indent e.e_abi_version;
    Printf.bprintf b "%s e_type = %s;\n" indent (string_of_type e.e_type);
    Printf.bprintf b "%s e_machine = %s;\n" indent (string_of_machine e.e_machine);
    Printf.bprintf b "%s e_version = %Ld;\n"  indent e.e_version;
    Printf.bprintf b "%s e_entry = %Ld; (* entry point addr *)\n"
      indent e.e_entry;
    Printf.bprintf b "%s e_flags = %Ld;\n" indent e.e_flags;

    Printf.bprintf b "%s e_sections = {|\n" indent;
    StringMap.iter (fun name s ->
      Printf.bprintf b "%s  " indent;
      section e b (indent ^ "    ") s;
      Printf.bprintf b ";\n";
    ) e.e_sections;
    Printf.bprintf b "%s|};\n" indent;

    Printf.bprintf b "%s}" indent;


  Buffer.contents b

end

end
module ElfUtils = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open ElfTypes

let wrap lf bf =
  fun e b pos ->
    if e.byte_order = LittleEndian then lf b pos else bf b pos

let get_half = wrap LittleEndian.get_uint16_64 BigEndian.get_uint16_64
let get_addr32 : encoding -> string -> int -> int64 * int= wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_addr64 = wrap LittleEndian.get_int64 BigEndian.get_int64
let get_off32 = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_off64 = wrap LittleEndian.get_int64 BigEndian.get_int64
let get_sword = wrap LittleEndian.get_int32_64 BigEndian.get_int32_64
let get_word32 = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_word64 = wrap LittleEndian.get_int64 BigEndian.get_int64
let get_uchar = wrap LittleEndian.get_uint8_64 BigEndian.get_uint8_64
let get_char = wrap LittleEndian.get_int8_64 BigEndian.get_int8_64

let get_off_by_class en s pos =
  if en.word_size = ARCH32 then
    get_off32 en s pos else
    get_off64 en s pos

let get_word_by_class en s pos =
  if en.word_size = ARCH32 then
    get_word32 en s pos else
    get_word64 en s pos

let get_addr_by_class (en: encoding) (s: string) pos =
  if en.word_size = ARCH32 then
    get_addr32 en s pos else
    get_addr64 en s pos

let wrap lf bf =
  fun e b v ->
    if e.byte_order = LittleEndian then lf b v else bf b v

(* TODO: use buf_uint16_64 instead of buf_int16_64 *)
let buf_half64 = wrap LittleEndian.buf_int16_64 BigEndian.buf_int16_64
let buf_half e b v = buf_half64 e b (Int64.of_int v)

let buf_word32 = wrap LittleEndian.buf_int32_64 BigEndian.buf_int32_64
let buf_word64 = wrap LittleEndian.buf_int64 BigEndian.buf_int64

let buf_addr32 = wrap LittleEndian.buf_int32_64 BigEndian.buf_int32_64
let buf_addr64 = wrap LittleEndian.buf_int64 BigEndian.buf_int64
let buf_addr_by_class e b v =
  if e.word_size = ARCH32 then
    buf_addr32 e b v
  else
    buf_addr64 e b v
let buf_word_by_class e b v =
  if e.word_size = ARCH32 then
    buf_word32 e b v
  else
    buf_word64 e b v
let buf_off_by_class = buf_word_by_class
let buf_off32 = buf_word32
let buf_off64 = buf_word64

let shn_undef = 0       (* undefined *)
let shn_abs = 0xFFF1    (* absolute value *)
let shn_common = 0xFFF2 (* common section *)

let shn_undefL = 0L       (* undefined *)
let shn_absL = 0xFFF1L    (* absolute value *)
let shn_commonL = 0xFFF2L (* common section *)



let get_encoding e_data_encoding e_file_class =
    let byte_order = match e_data_encoding with
      | ELFDATA2LSB -> LittleEndian
      | ELFDATA2MSB -> BigEndian
      | _ ->
        failwith "unsupported data encoding"
    in
    let word_size = match e_file_class with
      | ELFCLASS32 -> ARCH32
      | ELFCLASS64 -> ARCH64
      | _ -> failwith "unsupported file class"
    in
    { byte_order; word_size }
end
module ElfReader: sig

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

module RAW : sig
  val read : string -> ElfTypes.RAW.t
end

module ABSTRACT : sig
  val read : string -> ElfTypes.ABSTRACT.t
  val of_raw : ElfTypes.RAW.t -> ElfTypes.ABSTRACT.t
end

end = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open ElfTypes
open ElfUtils

let debug = ref false

let type_of_int v =
  match Int64.to_int v with
    | 0 -> ET_NONE
    | 1 -> ET_REL
    | 2 -> ET_EXEC
    | 3 -> ET_DYN
    | 4 -> ET_CORE
    | n ->
      if n >= 0xff00 && n <= 0xffff then
        ET_PROC (n - 0xff00)
      else
        if n >= 0xfe00 && n <= 0xfeff then
          ET_OS (n - 0xfe00)
      else
        ET_NUM n

let to_bits v =
  let bits = ref [] in
  let bit = ref 1 in
  for _i = 0 to 63 do
    if v land !bit <> 0 then bits := !bit :: !bits;
    bit := !bit lsl 1
  done;
  !bits

let section_header_flag_of_int v =
  match v with
      1 -> SHF_WRITE
    | 2 -> SHF_ALLOC
    | 4 -> SHF_EXECINSTR
    | n -> SHF_NUM n

let section_header_flags_of_int en v =
  let bits = to_bits (Int64.to_int v) in
  List.map section_header_flag_of_int bits

let file_class_of_int v =
  match v with
    | 0 -> ELFCLASSNONE
    | 1 -> ELFCLASS32
    | 2 -> ELFCLASS64
    | n -> ELFCLASSNUM n


let data_encoding_of_int = function
    | 0 -> ELFDATANONE
    | 1 -> ELFDATA2LSB
    | 2 -> ELFDATA2MSB
    | n -> ELFDATANUM n

let osabi_of_int = function
  |  0   -> ELFOSABI_SYSV
  |  1   -> ELFOSABI_HPUX
  |  2   -> ELFOSABI_NETBSD
  |  3   -> ELFOSABI_LINUX
  |  6   -> ELFOSABI_SOLARIS
  |  7   -> ELFOSABI_AIX
  |  8   -> ELFOSABI_IRIX
  |  9   -> ELFOSABI_FREEBSD
  |  10  -> ELFOSABI_TRU64
  |  11  -> ELFOSABI_MODESTO
  |  12  -> ELFOSABI_OPENBSD
  |  13  -> ELFOSABI_OPENVMS
  |  14  -> ELFOSABI_NSK
  |  15  -> ELFOSABI_AROS
  |  97  -> ELFOSABI_ARM
  |  255 -> ELFOSABI_STANDALONE
  |  n   -> ELFOSABI_NUM n

let machine_of_int v =
  match Int64.to_int v with
      0    -> EM_NONE
    | 1    -> EM_M32
    | 2    -> EM_SPARC
    | 3    -> EM_386
    | 4    -> EM_68K
    | 5    -> EM_88K
    | 6    -> EM_486
    | 7    -> EM_860
    | 8    -> EM_MIPS
    | 9    -> EM_S370
    | 10   -> EM_MIPS_RS3_LE
    | 11   -> EM_SPARC64
    | 15   -> EM_PARISC
    | 17   -> EM_VPP500
    | 18   -> EM_SPARC32PLUS
    | 19   -> EM_960
    | 20   -> EM_PPC
    | 21   -> EM_PPC64
    | 22   -> EM_S390
    | 23   -> EM_SPU
    | 36   -> EM_V800
    | 37   -> EM_FR20
    | 38   -> EM_RH32
    | 39   -> EM_RCE
    | 40   -> EM_ARM
    | 41   -> EM_ALPHA
    | 42   -> EM_SH
    | 43   -> EM_SPARCV9
    | 44   -> EM_TRICORE
    | 45   -> EM_ARC
    | 46   -> EM_H8_300
    | 47   -> EM_H8_300H
    | 48   -> EM_H8S
    | 49   -> EM_H8_500
    | 50   -> EM_IA_64
    | 51   -> EM_MIPS_X
    | 52   -> EM_COLDFIRE
    | 53   -> EM_68HC12
    | 54   -> EM_MMA
    | 55   -> EM_PCP
    | 56   -> EM_NCPU
    | 57   -> EM_NDR1
    | 58   -> EM_STARCORE
    | 59   -> EM_ME16
    | 60   -> EM_ST100
    | 61   -> EM_TINYJ
    | 62   -> EM_X86_64
    | 63   -> EM_PDSP
    | 66   -> EM_FX66
    | 67   -> EM_ST9PLUS
    | 68   -> EM_ST7
    | 69   -> EM_68HC16
    | 70   -> EM_68HC11
    | 71   -> EM_68HC08
    | 72   -> EM_68HC05
    | 73   -> EM_SVX
    | 74   -> EM_ST19
    | 75   -> EM_VAX
    | 76   -> EM_CRIS
    | 77   -> EM_JAVELIN
    | 78   -> EM_FIREPATH
    | 79   -> EM_ZSP
    | 80   -> EM_MMIX
    | 81   -> EM_HUANY
    | 82   -> EM_PRISM
    | 83   -> EM_AVR
    | 84   -> EM_FR30
    | 85   -> EM_D10V
    | 86   -> EM_D30V
    | 87   -> EM_V850
    | 88   -> EM_M32R
    | 89   -> EM_MN10300
    | 90   -> EM_MN10200
    | 91   -> EM_PJ
    | 92   -> EM_OPENRISC
    | 93   -> EM_ARC_A5
    | 94   -> EM_XTENSA
    | 95   -> EM_VIDEOCORE
    | 96   -> EM_TMM_GPP
    | 97   -> EM_NS32K
    | 98   -> EM_TPC
    | 99   -> EM_SNP1K
    | 100  -> EM_ST200
    | 101  -> EM_IP2K
    | 102  -> EM_MAX
    | 103  -> EM_CR
    | 104  -> EM_F2MC16
    | 105  -> EM_MSP430
    | 106  -> EM_BLACKFIN
    | 107  -> EM_SE_C33
    | 108  -> EM_SEP
    | 109  -> EM_ARCA
    | 110  -> EM_UNICORE
    | n -> EM_NUM n

let segment_type_of_int = function
  | 0 -> PT_NULL
  | 1 -> PT_LOAD
  | 2 -> PT_DYNAMIC        (* 2 : dynamic linking tables *)
  | 3 -> PT_INTERP         (* 3 : program interpreter path name *)
  | 4 -> PT_NOTE           (* 4 : note sections *)
  | 5 -> PT_SHLIB          (* 5 : reserved *)
  | 6 -> PT_PHDR           (* 6 : program header table *)
  | n ->
    if n >= 0x6000_0000 && n <= 0x6FFF_FFFF then
      PT_OS ( n - 0x6000_0000 )
    else
    if n >= 0x7000_0000 && n <= 0x7FFF_FFFF then
      PT_OS ( n - 0x7000_0000 )
    else
      PT_NUM n

let symbol_bind_type_of_int = function
  | 0 -> STB_LOCAL
  | 1 -> STB_GLOBAL
  | 2 -> STB_WEAK
  | n ->
    if n >= 10 && n <= 12 then STB_OS (n-10) else
    if n >= 13 && n <= 15 then STB_PROC (n - 13) else
    STB_NUM n

let symbol_type_of_int = function
  | 0 -> STT_NOTYPE
  | 1 -> STT_OBJECT
  | 2 -> STT_FUNC
  | 3 -> STT_SECTION
  | 4 -> STT_FILE
  | n ->
    if n >= 10 && n <= 12 then STT_OS (n-10) else
    if n >= 13 && n <= 15 then STT_PROC (n - 13) else
    STT_NUM n

module RAW = struct

  open ElfTypes.RAW


let section_type_of_int nL =
  match Int64.to_int nL with
    | 0 -> SHT_NULL
    | 1 -> SHT_PROGBITS
    | 2 -> SHT_SYMTAB
    | 3 -> SHT_STRTAB
    | 4 -> SHT_RELA
    | 5 -> SHT_HASH
    | 6 -> SHT_DYNAMIC
    | 7 -> SHT_NOTE
    | 8 -> SHT_NOBITS
    | 9 -> SHT_REL
    | 10 -> SHT_SHLIB
    | 11 -> SHT_DYNSYM
      (*    | SHT_INIT_ARRAY -> "SHT_INIT_ARRAY"
            | SHT_FINI_ARRAY -> "SHT_FINI_ARRAY"
            | SHT_PREINIT_ARRAY -> "SHT_PREINIT_ARRAY"
            | SHT_GROUP -> "SHT_GROUP"
            | SHT_SYMTAB_SHNDX -> "SHT_SYMTAB_SHNDX"
      *)
    | _ ->
      if nL >= 0x60000000L && nL <= 0x6fffffffL then
        SHT_OS (Int64.to_int (Int64.sub nL 0x60000000L))
      else
      if nL >= 0x70000000L && nL <= 0x7fffffffL then
        SHT_PROC (Int64.to_int (Int64.sub nL 0x70000000L))
      else
        if nL >= 0x80000000L && nL <= 0xffffffffL then
          SHT_USER (Int64.to_int (Int64.sub nL 0x80000000L))
        else
            (*    | SHT_OS n -> Printf.sprintf "SHT_OS %d" n *)
          SHT_NUM (Int64.to_int nL)

  let get_elf_header s pos =
  let e_ident = String.sub s pos 16 in

  if int_of_char e_ident.[0] <> 0x7f ||
    e_ident.[1] <> 'E' ||
    e_ident.[2] <> 'L' ||
    e_ident.[3] <> 'F' then
    Printf.kprintf failwith "Bad magic number [%S]" e_ident;
  let e_file_class = file_class_of_int (int_of_char e_ident.[4]) in
  let e_data_encoding = data_encoding_of_int (int_of_char e_ident.[5]) in
  let e_file_version = int_of_char e_ident.[6] in
  let e_osabi = osabi_of_int (int_of_char e_ident.[7]) in
  let e_abi_version = int_of_char e_ident.[8] in
  (* other bytes are for padding *)

(*  Printf.eprintf "e_data_encoding %s\n%!"
    (string_of_data_encoding e_data_encoding); *)
  let en = get_encoding e_data_encoding e_file_class in
  let e_type, pos = get_half en s (pos+16) in
  let e_machine, pos = get_half en s pos in
  let e_machine = machine_of_int e_machine in
  let e_version, pos = get_word32 en s pos in
  let e_entry, pos = get_addr_by_class en s pos in
  let e_phoff, pos = get_off_by_class en s pos in
  let e_shoff, pos = get_off_by_class en s pos in
  let e_flags, pos = get_word32 en s pos in
  let e_ehsize, pos = get_half en s pos in
  let e_phentsize, pos = get_half en s pos in
  let e_phnum, pos = get_half en s pos in
  let e_shentsize, pos = get_half en s pos in
  let e_shnum, pos = get_half en s pos in
  let e_shstrndx, pos = get_half en s pos in
  en, {
    e_ident = e_ident;
    e_file_class = e_file_class;
    e_data_encoding = e_data_encoding;
    e_file_version = e_file_version;
    e_osabi = e_osabi;
    e_abi_version = e_abi_version;
    e_type = type_of_int e_type;
    e_machine = e_machine;
    e_version = e_version;
    e_entry = e_entry;
    e_phoff = e_phoff;
    e_shoff = e_shoff;
    e_flags = e_flags;
    e_ehsize = e_ehsize;
    e_phentsize = e_phentsize;
    e_phnum = e_phnum;
    e_shentsize = e_shentsize;
    e_shnum = e_shnum;
    e_shstrndx = e_shstrndx;
  }, pos

  let get_section_header en s pos section_size section_num =
    (*  Printf.eprintf "get_section_header %d + %d * %d\n%!"
        pos section_size section_num; *)
    let pos = pos + section_size * section_num in
    let sh_name, pos = get_word32 en s pos in
    let sh_type, pos = get_word32 en s pos in
    let sh_flags, pos = get_word_by_class en s pos in
    let sh_addr, pos = get_addr_by_class en s pos in
    let sh_offset, pos = get_off_by_class en s pos in
    let sh_size, pos = get_word_by_class en s pos in
    let sh_link, pos = get_word32 en s pos in
    let sh_info, pos = get_word32 en s pos in
    let sh_addralign, pos = get_word_by_class en s pos in
    let sh_entsize, pos = get_word_by_class en s pos in

    if !debug then begin
      Printf.eprintf "----------------------------------------\n";
      Printf.eprintf "sh_name %Ld\n" sh_name;
      Printf.eprintf "sh_type %Ld\n" sh_type;
      Printf.eprintf "sh_flags %Ld\n" sh_flags;
      Printf.eprintf "sh_addr %Ld\n" sh_addr;
      Printf.eprintf "sh_offset %Ld\n" sh_offset;
      Printf.eprintf "sh_size %Ld\n" sh_size;
      Printf.eprintf "sh_link %Ld\n" sh_link;
      Printf.eprintf "sh_info %Ld\n" sh_info;
      Printf.eprintf "sh_addralign %Ld\n" sh_addralign;
      Printf.eprintf "sh_entsize %Ld\n" sh_entsize;
    end;

    let sh_type = section_type_of_int sh_type in
    let sh_flags = section_header_flags_of_int en sh_flags in
    {
      sh_name = sh_name;
      sh_type = sh_type;
      sh_flags = sh_flags;
      sh_addr = sh_addr;
      sh_offset = sh_offset;
      sh_size = sh_size;
      sh_link = sh_link;
      sh_info = sh_info;
      sh_addralign = sh_addralign;
      sh_entsize;
    }

let get_string strtab pos =
  let pos = Int64.to_int pos in
  try
    let rec iter strtab pos0 pos =
      if strtab.[pos] = '\000' then String.sub strtab pos0 (pos-pos0)
      else iter strtab pos0 (pos+1)
    in
    iter strtab pos pos
  with e ->
    Printf.eprintf "Error: get_string[%d] in strtab[%d]\n%!"
      pos (String.length strtab);
    raise e

let get_section en string_table elf_sections_contents i sh =
  let s = elf_sections_contents.(i) in
  let section_content = s in
  let section_name = get_string string_table sh.sh_name in
  if !debug then
    Printf.eprintf "section_name = %S\n%!" section_name;
    {
      section_name;
      section_header = sh;
      section_content;
    }


let get_program en s pos program_size program_num =
  let pos = pos + program_size * program_num in
  let program_header, pos =
    if en.word_size = ARCH32 then
      let p_type, pos = get_word32 en s pos in
      let p_offset, pos = get_off32 en s pos in
      let p_vaddr, pos = get_addr32 en s pos in
      let p_paddr, pos = get_addr32 en s pos in
      let p_filesz, pos = get_word32 en s pos in
      let p_memsz, pos = get_word32 en s pos in
      let p_flags, pos = get_word32 en s pos in
      let p_align, pos = get_word32 en s pos in

(*
      Printf.eprintf "----------------------------------------\n";
      Printf.eprintf "p_type %Lx\n" p_type;
      Printf.eprintf "p_offset %Ld\n" p_offset;
      Printf.eprintf "p_vaddr %Ld\n" p_vaddr;
      Printf.eprintf "p_paddr %Ld\n" p_paddr;
      Printf.eprintf "p_filesz %Ld\n" p_filesz;
      Printf.eprintf "p_memsz %Ld\n" p_memsz;
      Printf.eprintf "p_flags %Ld\n" p_flags;
      Printf.eprintf "p_align %Ld\n" p_align;
*)

      let p_type = segment_type_of_int (Int64.to_int p_type) in
      {
        p_type; p_offset; p_vaddr; p_paddr; p_filesz; p_memsz; p_flags; p_align
      }, pos
    else
      let p_type, pos = get_word32 en s pos in
      let p_flags, pos = get_word32 en s pos in
      let p_offset, pos = get_off64 en s pos in
      let p_vaddr, pos = get_addr64 en s pos in
      let p_paddr, pos = get_addr64 en s pos in
      let p_filesz, pos = get_word64 en s pos in
      let p_memsz, pos = get_word64 en s pos in
      let p_align, pos = get_word64 en s pos in

(*
      Printf.eprintf "----------------------------------------\n";
      Printf.eprintf "p_type %Lx\n" p_type;
      Printf.eprintf "p_offset %Ld\n" p_offset;
      Printf.eprintf "p_vaddr %Ld\n" p_vaddr;
      Printf.eprintf "p_paddr %Ld\n" p_paddr;
      Printf.eprintf "p_filesz %Ld\n" p_filesz;
      Printf.eprintf "p_memsz %Ld\n" p_memsz;
      Printf.eprintf "p_flags %Ld\n" p_flags;
      Printf.eprintf "p_align %Ld\n" p_align;
*)
      let p_type = segment_type_of_int (Int64.to_int p_type) in
      {
        p_type; p_offset; p_vaddr; p_paddr; p_filesz; p_memsz; p_flags; p_align
      }, pos
  in
  let program_content =
    if program_header.p_filesz = 0L then "" else begin
      let p_offset = Int64.to_int program_header.p_offset in
      let p_filesz = Int64.to_int program_header.p_filesz in
(*      Printf.eprintf "sub [%d] %d/%d\n%!" (String.length s)
        p_offset p_filesz; *)
      String.sub s p_offset p_filesz
    end
  in
  {
    program_content; program_header;
  }

let elf_of_file_content s =
  let en, elf_header, pos = get_elf_header s 0 in
  if Int64.of_int pos <> elf_header.e_ehsize then begin
    Printf.eprintf "Warning: header size %d <> expected size %Ld\n%!"
      pos elf_header.e_ehsize;
  end;

(* read sections *)

  let elf_section_header_table =
    if elf_header.e_shoff = 0L then [||] else
      let pos = Int64.to_int elf_header.e_shoff in
      let nsections = Int64.to_int elf_header.e_shnum in
      let section_size = Int64.to_int elf_header.e_shentsize in
      Array.init nsections (get_section_header en s pos section_size
      )
  in
  let elf_sections_contents = Array.map (fun sh ->
    if sh.sh_offset <> 0L && sh.sh_type <> SHT_NOBITS then begin
(*      Printf.eprintf "section at %Ld len %Ld\n%!" sh.sh_offset sh.sh_size; *)
      String.sub s (Int64.to_int sh.sh_offset) (Int64.to_int sh.sh_size)
    end else ""
  ) elf_section_header_table in

  let string_table_index = Int64.to_int elf_header.e_shstrndx in
  let string_table =  elf_sections_contents.(string_table_index) in
  let elf_sections = Array.mapi
    (get_section en string_table elf_sections_contents)
    elf_section_header_table
  in

(* read segments *)

  let elf_programs =
    if elf_header.e_phoff = 0L then [||] else
      let pos = Int64.to_int elf_header.e_phoff in
      let nprograms = Int64.to_int elf_header.e_phnum in
      let program_size = Int64.to_int elf_header.e_phentsize in
      Array.init nprograms (get_program en s pos program_size
      )
  in

  {
    elf_content = s ;
    elf_header = elf_header;
    elf_programs = elf_programs;
    elf_sections = elf_sections;
  }

let read filename =

  (* string_of_binfile *)
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let s = Bytes.create len in
  really_input ic s 0 len;
  close_in ic;

  elf_of_file_content (Bytes.to_string s)


let encoding r =
  get_encoding r.elf_header.e_data_encoding r.elf_header.e_file_class


end

module ABSTRACT = struct

  module R = ElfTypes.RAW
  open ElfTypes.ABSTRACT

  let get_section r i =
    r.R.elf_sections.(Int64.to_int i).R.section_name

  let get_symbol_table_entry r string_table en s pos =
    if !debug then
      Printf.eprintf "get_symbol_table_entry[%d]\n%!" pos;
    if en.word_size = ARCH32 then
      let st_name, pos = get_word32 en s pos in
      let st_name = RAW.get_string string_table st_name in
      let st_value, pos = get_addr32 en s pos in
      let st_size, pos = get_word32 en s pos in
      let st_info, pos = get_uchar en s pos in
      let st_other, pos = get_uchar en s pos in
      let st_shndx, pos = get_half en s pos in
      let st_section =
        if st_shndx = shn_undefL then SYM_SHN_UNDEF
        else if st_shndx = shn_absL then SYM_SHN_ABS
        else if st_shndx = shn_commonL then SYM_SHN_COMMON
        else SYM_SHN (get_section r st_shndx)
      in
      let st_bind =
        let st_info = Int64.to_int st_info in
        symbol_bind_type_of_int ( (st_info lsr 4) land 0xf ) in
      let st_type =
        let st_info = Int64.to_int st_info in
        symbol_type_of_int ( st_info land 0xf ) in
      {
        st_name = st_name;
        st_value = st_value;
        st_size = st_size;
        st_bind = st_bind;
        st_type = st_type;
(*        st_info = st_info; *)
(*        st_other = st_other; *)
        st_section = st_section;
      }
    else
      let st_name, pos = get_word32 en s pos in
      let st_name2 = RAW.get_string string_table st_name in
      if !debug then
        Printf.eprintf "read symbol %Ld -> %S\n%!" st_name st_name2;
      let st_name = st_name2 in

      let st_info, pos = get_uchar en s pos in
      let st_other, pos = get_uchar en s pos in
      let st_shndx, pos = get_half en s pos in
      let st_section =
        if st_shndx = shn_undefL then SYM_SHN_UNDEF
        else if st_shndx = shn_absL then SYM_SHN_ABS
        else if st_shndx = shn_commonL then SYM_SHN_COMMON
        else SYM_SHN (get_section r st_shndx)
      in
      let st_value, pos = get_addr64 en s pos in
      let st_size, pos = get_word64 en s pos in
      let st_bind =
        let st_info = Int64.to_int st_info in
        symbol_bind_type_of_int ( (st_info lsr 4) land 0xf ) in
      let st_type =
        let st_info = Int64.to_int st_info in
        symbol_type_of_int ( st_info land 0xf ) in

      {
        st_name = st_name;
        st_value = st_value;
        st_size = st_size;
(*        st_info = st_info; *)
        st_bind = st_bind;
        st_type = st_type;
(*        st_other = st_other; *)
        st_section = st_section;
      }


  let rtype64_of_int r_type =
    match Int64.to_int r_type with
    |   0 -> R_X86_64_NONE
    |   1 -> R_X86_64_64
    |   2 -> R_X86_64_PC32
    |   3 -> R_X86_64_GOT32
    |   4 -> R_X86_64_PLT32
    |   5 -> R_X86_64_COPY
    |   6 -> R_X86_64_GLOB_DAT
    |   7 -> R_X86_64_JUMP_SLOT
    |   8 -> R_X86_64_RELATIVE
    |   9 -> R_X86_64_GOTPCREL
    |  10 -> R_X86_64_32
    |  11 -> R_X86_64_32S
    |  12 -> R_X86_64_16
    |  13 -> R_X86_64_PC16
    |  14 -> R_X86_64_8
    |  15 -> R_X86_64_PC8
    |  16 -> R_X86_64_DPTMOD64
    |  17 -> R_X86_64_DTPOFF64
    |  18 -> R_X86_64_TPOFF64
    |  19 -> R_X86_64_TLSGD
    |  20 -> R_X86_64_TLSLD
    |  21 -> R_X86_64_DTPOFF32
    |  22 -> R_X86_64_GOTTPOFF
    |  23 -> R_X86_64_TPOFF32
    |  24 -> R_X86_64_PC64
    |  25 -> R_X86_64_GOTOFF64
    |  26 -> R_X86_64_GOTPC32
    |  32 -> R_X86_64_SIZE32
    |  33 -> R_X86_64_SIZE64
    | n -> R_X86_64_UNKNOWN n

  let rtype32_of_int r_type =
    match Int64.to_int r_type with
    | 0 -> R_386_NONE
    | 1 -> R_386_32
    | 2 -> R_386_PC32
    | 3 -> R_386_GOT32
    | 4 -> R_386_PLT32
    | 5 -> R_386_COPY
    | 6 -> R_386_GLOB_DAT
    | 7 -> R_386_JMP_SLOT
    | 8 -> R_386_RELATIVE
    | 9 -> R_386_GOTOFF
    | 10 -> R_386_GOTPC
    | 11 -> R_386_32PLT
    | 20 -> R_386_16
    | 21 -> R_386_PC16
    | 22 -> R_386_8
    | 23 -> R_386_PC8
    | n -> R_386_UNKNOWN n


      let get_rel64 get_addend sh section_name en c =
        let reloc_size = Int64.to_int sh.R.sh_entsize in
        let nrelocs = Int64.to_int sh.R.sh_size / reloc_size in
        let rela_symbols = section_name sh.R.sh_link in
        let rela_target = section_name sh.R.sh_info in
        let rela_relocs = Array.init nrelocs (fun i ->
            let pos = i * reloc_size in
            let r_offset, pos = get_addr64 en c pos in
            let rela_info, pos = get_word64 en c pos in
            let r_addend, pos = get_addend en c pos in
            let rela_sym = Int64.shift_right rela_info 32 in
            let r_sym = Int64.logand rela_sym 0xffff_ffffL in
            let r_type = Int64.logand rela_info 0xffff_ffffL in
            let r_type = rtype64_of_int r_type in
            {
              r_offset;
              r_sym;
              r_type;
              r_addend;
            }
          ) in

        { rela_symbols; rela_relocs; rela_target }

      let get_rel32 get_addend sh section_name en c =
        let reloc_size = Int64.to_int sh.R.sh_entsize in
        let nrelocs = Int64.to_int sh.R.sh_size / reloc_size in
        let rela_symbols = section_name sh.R.sh_link in
        let rela_target = section_name sh.R.sh_info in

        let rela_relocs = Array.init nrelocs (fun i ->
            let pos = i * reloc_size in
            let r_offset, pos = get_addr32 en c pos in
            let rela_info, pos = get_word32 en c pos in
            let r_addend, pos = get_addend en c pos in

            let rela_sym = Int64.shift_right rela_info 8 in
            let r_sym = Int64.logand rela_sym 0xff_ffffL in
            let r_type = Int64.logand rela_info 0xffL in
            let r_type = rtype32_of_int r_type in
            {
              r_offset;
              r_sym;
              r_type;
              r_addend;
            }
          ) in
        { rela_symbols; rela_relocs; rela_target }




  let of_raw r =
    let h = r.R.elf_header in
    let e = {
      (*      e_ident = h.R.e_ident; *)
      e_file_class = h.R.e_file_class;
      e_data_encoding = h.R.e_data_encoding;
      e_file_version = h.R.e_file_version;
      e_osabi = h.R.e_osabi;
      e_abi_version = h.R.e_abi_version;
      e_type = h.R.e_type;
      e_machine = h.R.e_machine;
      e_version = h.R.e_version;
      e_entry = h.R.e_entry;
      e_flags = h.R.e_flags;
      e_sections = StringMap.empty;
      e_programs = StringMap.empty;
    } in

    let en = RAW.encoding r in
    let section_content n =
      r.R.elf_sections.(n).R.section_content
    in
    let section_name n =
      r.R.elf_sections.(Int64.to_int n).R.section_name
    in
    let parse_rel sh c =
      match sh.R.sh_entsize with
      | 24L -> SHT_RELA64 (get_rel64 get_word64 sh section_name en c )
      | 16L ->
        SHT_REL64 (get_rel64 (fun en _ pos -> (0L, pos)) sh section_name en c )
      | 12L -> SHT_RELA32 (get_rel32 get_word32 sh section_name en c )
      | 8L ->
        SHT_REL32 (get_rel32 (fun en _ pos -> (0L, pos)) sh section_name en c )
      | _ ->
        Printf.kprintf failwith "Unexpected REL/RELA size %Ld" sh.R.sh_entsize
    in
    let string_table_index = Int64.to_int h.R.e_shstrndx in
    let string_table =  section_content string_table_index in
    Array.iteri (fun i s ->
      let s_name = s.R.section_name in
      let c = s.R.section_content in
      let sh = s.R.section_header in
      let prog_bits sh c =
        {
          sht_flags = sh.R.sh_flags;
          sht_addr = sh.R.sh_addr;
          (*          sht_size = Int64.to_int sh.R.sh_size; *)
          sht_align = Int64.to_int sh.R.sh_addralign;
          sht_content = c;
        } in
      let s_desc = match sh.R.sh_type with
        | R.SHT_NOBITS -> SHT_NOBITS (prog_bits sh c)
        | R.SHT_PROGBITS -> SHT_PROGBITS (prog_bits sh c)
        | R.SHT_STRTAB -> SHT_STRTAB s.R.section_content
        | R.SHT_SYMTAB ->
          let string_table =
            if sh.R.sh_link = 0L then string_table else
              section_content (Int64.to_int sh.R.sh_link)
          in
          let symbol_entry_size = Int64.to_int sh.R.sh_entsize in
          let nsymbols =
            if symbol_entry_size = 0 then 0 else
              Int64.to_int sh.R.sh_size / symbol_entry_size
          in
          let symbols = Array.init nsymbols (fun i ->
              get_symbol_table_entry r string_table en c
                (i * symbol_entry_size)
            ) in
          SHT_SYMTAB symbols
        | R.SHT_NULL -> SHT_NULL
        | R.SHT_REL
        | R.SHT_RELA
          -> parse_rel sh c
        | R.SHT_HASH
        | R.SHT_DYNAMIC
        | R.SHT_NOTE
        | R.SHT_SHLIB
        | R.SHT_DYNSYM
        | R.SHT_INIT_ARRAY
        | R.SHT_FINI_ARRAY
        | R.SHT_PREINIT_ARRAY
        | R.SHT_GROUP
        | R.SHT_SYMTAB_SHNDX
        | R.SHT_NUM _
        | R.SHT_OS _
        | R.SHT_PROC _
        | R.SHT_USER _
          -> SHT_UNKNOWN s
      in

      let ss = { s_name; s_desc } in
      e.e_sections <- StringMap.add s_name ss e.e_sections
    ) r.R.elf_sections;
    e

  let read filename =
    of_raw (RAW.read filename)

end
end
module ElfWriter: sig

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

type popular_system =
  | S_386_Linux
  | S_X64_Linux

val create :
  ElfTypes.elf_file_class ->
  ElfTypes.elf_data_encoding ->
  ElfTypes.elf_osabi -> ElfTypes.machine -> ElfTypes.ABSTRACT.t
val create_popular : popular_system -> ElfTypes.ABSTRACT.t
val to_string : ElfTypes.ABSTRACT.t -> string
val to_file : string -> ElfTypes.ABSTRACT.t -> unit
end = struct

(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)
(*
Elf32_Addr  4   Unsigned program address
Elf32_Half  2   Unsigned medium integer
Elf32_Off   4   Unsigned file offset
Elf32_Sword 4   Signed large integer
Elf32_Word  4   Unsigned large integer
*)

open StringCompat
open ElfTypes
open ElfTypes.ABSTRACT
open ElfUtils

let debug = ref false

type tmp_section = {
  sh_num : int;
  sh_content : string;
  sh_header : RAW.section_header;
  mutable sh_pos : int;
}

type tmp_strtab = {
  mutable tmpstr_map : int StringMap.t;
  tmpstr_buf : Buffer.t;
}

type tmp = {
  tmp_elf : ElfTypes.ABSTRACT.t;
  tmp_en : ElfTypes.encoding;
  tmp_buf : Buffer.t;
  mutable tmp_pos : int;
  mutable tmp_nsections : int;
  mutable tmp_map : int StringMap.t;
  mutable tmp_sections : tmp_section IntMap.t;
  mutable tmp_contents : string list;
  tmp_strtab : tmp_strtab;
  tmp_shstrtab : tmp_strtab;
}

let create
    e_file_class e_data_encoding
    e_osabi e_machine =
  {
    e_file_class;
    e_data_encoding;
    e_file_version = 1;
    e_osabi;
    e_abi_version = 0;
    e_type = ET_REL;
    e_machine;
    e_version = 1L;
    e_entry = 0L; (* entry point addr *)
    e_flags = 0L;
    e_sections = StringMap.empty;
    e_programs = StringMap.empty;
  }

type popular_system =
  | S_386_Linux
  | S_X64_Linux

let create_popular = function
  | S_X64_Linux -> create
                     ELFCLASS64
                     ELFDATA2LSB
                     ELFOSABI_SYSV
                     EM_X86_64
  | S_386_Linux -> create
                     ELFCLASS32
                     ELFDATA2LSB
                     ELFOSABI_SYSV
                     EM_386

let new_strtab () = {
  tmpstr_map = StringMap.empty;
  tmpstr_buf = Buffer.create 111;
}

let get_string tab str =
  try
    StringMap.find str tab.tmpstr_map
  with Not_found ->
    (* TODO add compression *)
    if !debug then
      Printf.eprintf "new  string %S\n%!" str;
    let pos = Buffer.length tab.tmpstr_buf in
    tab.tmpstr_map <- StringMap.add str pos tab.tmpstr_map;
    Buffer.add_string tab.tmpstr_buf str;
    Buffer.add_char tab.tmpstr_buf '\000';
    pos

let section_num tmp name =
  try
    StringMap.find name tmp.tmp_map
  with Not_found ->
    let num = tmp.tmp_nsections in
    tmp.tmp_nsections <- tmp.tmp_nsections + 1;
    tmp.tmp_map <- StringMap.add name num tmp.tmp_map;
    if !debug then
      Printf.eprintf "Section %d : %S\n%!" num name;
    num

let int_of_symbol_bind = function
  | STB_LOCAL -> 0
  | STB_GLOBAL -> 1
  | STB_WEAK -> 2
  | STB_OS n -> n+10
  | STB_PROC n -> n+13
  | STB_NUM n -> n

let int_of_symbol_type = function
  | STT_NOTYPE -> 0
  | STT_OBJECT -> 1
  | STT_FUNC -> 2
  | STT_SECTION -> 3
  | STT_FILE -> 4
  | STT_OS n -> n+10
  | STT_PROC n -> n+13
  | STT_NUM n -> n

let buf_symbol_table_entry tmp b st =
  if !debug then
    Printf.eprintf "buf_symbol_table_entry[%d]\n%!"
      (Buffer.length b);
  let en = tmp.tmp_en in

  let st_shndx = match st.st_section with
    | SYM_SHN_UNDEF -> shn_undefL
    | SYM_SHN_ABS -> shn_absL
    | SYM_SHN_COMMON -> shn_commonL
    | SYM_SHN s -> Int64.of_int (section_num tmp s) in
  let st_bind = int_of_symbol_bind st.st_bind in
  let st_type = int_of_symbol_type st.st_type in
  let st_info = (st_bind lsl 4) lor st_type in
  let st_name = get_string tmp.tmp_strtab st.st_name in
  if !debug then
    Printf.eprintf "symbol = %S -> %d\n%!" st.st_name st_name;
  if en.word_size = ARCH32 then begin
(*
Elf32_Word    st_name;  (* 4 *)
Elf32_Addr    st_value; (* 4 *)
Elf32_Word    st_size;  (* 4 *)
unsigned char st_info;  (* 1 *)
unsigned char st_other; (* 1 *)
Elf32_Half    st_shndx; (* 2 *)
} Elf32_Sym;
*)
    buf_word32 en b (Int64.of_int st_name);
    buf_addr32 en b st.st_value;
    buf_word32 en b st.st_size;
    Buffer.add_char b (char_of_int st_info);
    Buffer.add_char b '\000' (* st_other *);
    buf_half64 en b st_shndx;

  end else begin
(*
Elf64_Word st_name;  (* 4 *)
unsigned char st_info; (* 1 *)
unsigned char st_other; (* 1 *)
Elf64_Half st_shndx; (* 2 *)
Elf64_Addr st_value; (* 8 *)
Elf64_Xword st_size; (* 8 *)
*)

    buf_word32 en b (Int64.of_int st_name);
    Buffer.add_char b (char_of_int st_info);
    Buffer.add_char b '\000' (* st_other *);
    buf_half64 en b st_shndx;
    buf_addr64 en b st.st_value;
    buf_addr64 en b st.st_size;
    ()
  end

let buf_rel32 en rela buf_addend =
  let b = Buffer.create 10000 in
  Array.iter (fun r ->
    let r_type = match r.r_type with
      | R_386_NONE -> 0
      | R_386_32 -> 1
      | R_386_PC32 -> 2
      | R_386_GOT32 -> 3
      | R_386_PLT32 -> 4
      | R_386_COPY -> 5
      | R_386_GLOB_DAT -> 6
      | R_386_JMP_SLOT -> 7
      | R_386_RELATIVE -> 8
      | R_386_GOTOFF -> 9
      | R_386_GOTPC -> 10
      | R_386_32PLT -> 11
      | R_386_16 -> 20
      | R_386_PC16 -> 21
      | R_386_8 -> 22
      | R_386_PC8 -> 23
      | R_386_UNKNOWN n -> n
    in
    let r_info =
      Int64.logor (Int64.of_int r_type) (Int64.shift_left r.r_sym 8)
    in
    buf_addr32 en b r.r_offset;
    buf_word32 en b r_info;
    buf_addend en b r.r_addend;
  ) rela.rela_relocs;
  Buffer.contents b
(*
typedef struct {
Elf32_Addr r_offset; (* 4 *)
Elf32_Word  r_info;  (* 4 *)
Elf32_Sword  r_addend; (* 4 *)
} Elf32_Rela;

r_info =  (r_sym lsl 8) lor r_type
*)

let buf_rel64 en rela buf_addend =
  let b = Buffer.create 10000 in
  Array.iter (fun r ->
    let r_type = match r.r_type with
      |  R_X86_64_NONE ->    0
      |  R_X86_64_64 ->    1
      |  R_X86_64_PC32 ->    2
      |  R_X86_64_GOT32 ->    3
      |  R_X86_64_PLT32 ->    4
      |  R_X86_64_COPY ->    5
      |  R_X86_64_GLOB_DAT ->    6
      |  R_X86_64_JUMP_SLOT ->    7
      |  R_X86_64_RELATIVE ->    8
      |  R_X86_64_GOTPCREL ->    9
      |  R_X86_64_32 ->   10
      |  R_X86_64_32S ->   11
      |  R_X86_64_16 ->   12
      |  R_X86_64_PC16 ->   13
      |  R_X86_64_8 ->   14
      |  R_X86_64_PC8 ->   15
      |  R_X86_64_DPTMOD64 ->   16
      |  R_X86_64_DTPOFF64 ->   17
      |  R_X86_64_TPOFF64 ->   18
      |  R_X86_64_TLSGD ->   19
      |  R_X86_64_TLSLD ->   20
      |  R_X86_64_DTPOFF32 ->   21
      |  R_X86_64_GOTTPOFF ->   22
      |  R_X86_64_TPOFF32 ->   23
      |  R_X86_64_PC64 ->   24
      |  R_X86_64_GOTOFF64 ->   25
      |  R_X86_64_GOTPC32 ->   26
      |  R_X86_64_SIZE32 ->   32
      |  R_X86_64_SIZE64 ->   33
      |  R_X86_64_UNKNOWN n ->  n
    in
    let r_info =
      Int64.logor
        (Int64.of_int r_type) (Int64.shift_left r.r_sym 32)
    in
    buf_addr64 en b r.r_offset;
    buf_word64 en b r_info;
    buf_word64 en b r.r_addend;
  ) rela.rela_relocs;
  Buffer.contents b


let add_section tmp s_name sh_type =
  let en = tmp.tmp_en in
  let sh_num = section_num tmp s_name in
  let sh_name = get_string tmp.tmp_shstrtab s_name in
  let sh_offset = tmp.tmp_pos in
  if !debug then
    Printf.eprintf "Section %S saved at %d\n%!" s_name sh_offset;
  let sh_content = match sh_type with
    | SHT_NULL -> ""
    | SHT_PROGBITS sht -> sht.sht_content
    | SHT_NOBITS sht -> sht.sht_content
    | SHT_STRTAB s -> s
    | SHT_SYMTAB syms ->
      let b = Buffer.create 10000 in
      Array.iter (fun s ->
        buf_symbol_table_entry tmp b s
      ) syms;
      Buffer.contents b
    | SHT_REL32 rela ->
      buf_rel32 en rela (fun en b addend -> assert (addend = 0L))
    | SHT_RELA32 rela -> buf_rel32 en rela buf_word32
    | SHT_REL64 rela ->
      buf_rel64 en rela (fun en b addend -> assert (addend = 0L))
    | SHT_RELA64 rela -> buf_rel64 en rela buf_word64
    | SHT_UNKNOWN sec -> sec.RAW.section_content
  in
  let sh_header = {
    RAW.sh_name = Int64.of_int sh_name;
    sh_size = Int64.of_int (String.length sh_content);
    sh_type = RAW.SHT_NULL;
    sh_flags = [];
    sh_addr = 0L;
    sh_offset = Int64.of_int sh_offset;
    sh_link = 0L;
    sh_info = 0L;
    sh_addralign = 0L;
    sh_entsize = 0L;
  } in
  let progbits sh_header sht sh_type =
    { sh_header with
      RAW.sh_flags = sht.sht_flags;
      sh_addr = sht.sht_addr;
      sh_addralign = Int64.of_int sht.sht_align;
      sh_type = sh_type;
    } in

  let after_last_local_symbol symtab =
    let rec iter symtab n =
      if n = 0 || symtab.(n-1).st_bind = STB_LOCAL then n else
        iter symtab (n-1)
    in
    iter symtab (Array.length symtab)
  in

  let sh_header =
    match sh_type with
    | SHT_NULL -> { sh_header with
                    RAW.sh_type = RAW.SHT_NULL;
                    sh_offset = 0L;
                  }
    | SHT_UNKNOWN sec ->
      { sec.RAW.section_header with
        RAW.sh_offset = sh_header.RAW.sh_offset;
      }
    | SHT_PROGBITS sht -> progbits sh_header sht RAW.SHT_PROGBITS
    | SHT_NOBITS sht -> progbits sh_header sht RAW.SHT_NOBITS
    | SHT_STRTAB s ->
      { sh_header with RAW.sh_type = RAW.SHT_STRTAB }
    | SHT_SYMTAB symtab ->
      { sh_header with
        RAW.sh_type = RAW.SHT_SYMTAB;
        RAW.sh_link = Int64.of_int (section_num tmp ".strtab");
        sh_addralign = (
          if en.word_size = ARCH32 then 8L else 8L);
        sh_entsize = (
          if en.word_size = ARCH32 then 16L else 24L);
        sh_info = Int64.of_int (after_last_local_symbol symtab);
      }
    | SHT_REL32 rela ->
      { sh_header with
        RAW.sh_type = RAW.SHT_REL;
        RAW.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        RAW.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        RAW.sh_entsize = 8L;
        RAW.sh_addralign = 8L;
      }
    | SHT_RELA32 rela ->
      { sh_header with
        RAW.sh_type = RAW.SHT_RELA;
        RAW.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        RAW.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        RAW.sh_entsize = 12L;
        RAW.sh_addralign = 8L;
      }
    | SHT_REL64 rela ->
      { sh_header with
        RAW.sh_type = RAW.SHT_REL;
        RAW.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        RAW.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        RAW.sh_entsize = 16L;
        RAW.sh_addralign = 8L;
      }
    | SHT_RELA64 rela ->
      { sh_header with
        RAW.sh_type = RAW.SHT_RELA;
        RAW.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        RAW.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        RAW.sh_entsize = 24L;
        RAW.sh_addralign = 8L;
      }
  in
  let section = {
    sh_num; sh_content; sh_header; sh_pos = 0;
  } in
  tmp.tmp_pos <- tmp.tmp_pos + String.length sh_content;
  tmp.tmp_sections <- IntMap.add section.sh_num section tmp.tmp_sections;
  tmp.tmp_contents <- sh_content :: tmp.tmp_contents

let string_table tab =
  SHT_STRTAB (Buffer.contents tab.tmpstr_buf)

let int_of_file_class = function
    | ELFCLASSNONE -> 0
    | ELFCLASS32 -> 1
    | ELFCLASS64 -> 2
    | ELFCLASSNUM n -> n

let int_of_data_encoding = function
    | ELFDATANONE -> 0
    | ELFDATA2LSB -> 1
    | ELFDATA2MSB -> 2
    | ELFDATANUM n -> n

let int_of_osabi = function
  |  ELFOSABI_NONE -> 0
  |  ELFOSABI_SYSV -> 0
  |  ELFOSABI_HPUX -> 1
  |  ELFOSABI_NETBSD -> 2
  |  ELFOSABI_LINUX -> 3
  |  ELFOSABI_SOLARIS -> 6
  |  ELFOSABI_AIX -> 7
  |  ELFOSABI_IRIX -> 8
  |  ELFOSABI_FREEBSD -> 9
  |  ELFOSABI_TRU64 -> 10
  |  ELFOSABI_MODESTO -> 11
  |  ELFOSABI_OPENBSD -> 12
  |  ELFOSABI_OPENVMS -> 13
  |  ELFOSABI_NSK -> 14
  |  ELFOSABI_AROS -> 15
  |  ELFOSABI_ARM -> 97
  |  ELFOSABI_STANDALONE -> 255
  |  ELFOSABI_NUM n -> n

let int_of_type = function
    | ET_NONE -> 0
    | ET_REL -> 1
    | ET_EXEC -> 2
    | ET_DYN -> 3
    | ET_CORE -> 4
    | ET_PROC n -> n + 0xff00
    | ET_OS n -> n + 0xfe00
    | ET_NUM n -> n

let int_of_machine = function
    |  EM_NONE ->  0
    |  EM_M32 ->  1
    |  EM_SPARC ->  2
    |  EM_386 ->  3
    |  EM_68K ->  4
    |  EM_88K ->  5
    |  EM_486 ->  6
    |  EM_860 ->  7
    |  EM_MIPS ->  8
    |  EM_S370 ->  9
    |  EM_MIPS_RS3_LE ->  10
    |  EM_SPARC64 ->  11
    |  EM_PARISC ->  15
    |  EM_VPP500 ->  17
    |  EM_SPARC32PLUS ->  18
    |  EM_960 ->  19
    |  EM_PPC ->  20
    |  EM_PPC64 ->  21
    |  EM_S390 ->  22
    |  EM_SPU ->  23
    |  EM_V800 ->  36
    |  EM_FR20 ->  37
    |  EM_RH32 ->  38
    |  EM_RCE ->  39
    |  EM_ARM ->  40
    |  EM_ALPHA ->  41
    |  EM_SH ->  42
    |  EM_SPARCV9 ->  43
    |  EM_TRICORE ->  44
    |  EM_ARC ->  45
    |  EM_H8_300 ->  46
    |  EM_H8_300H ->  47
    |  EM_H8S ->  48
    |  EM_H8_500 ->  49
    |  EM_IA_64 ->  50
    |  EM_MIPS_X ->  51
    |  EM_COLDFIRE ->  52
    |  EM_68HC12 ->  53
    |  EM_MMA ->  54
    |  EM_PCP ->  55
    |  EM_NCPU ->  56
    |  EM_NDR1 ->  57
    |  EM_STARCORE ->  58
    |  EM_ME16 ->  59
    |  EM_ST100 ->  60
    |  EM_TINYJ ->  61
    |  EM_X86_64 ->  62
    |  EM_PDSP ->  63
    |  EM_FX66 ->  66
    |  EM_ST9PLUS ->  67
    |  EM_ST7 ->  68
    |  EM_68HC16 ->  69
    |  EM_68HC11 ->  70
    |  EM_68HC08 ->  71
    |  EM_68HC05 ->  72
    |  EM_SVX ->  73
    |  EM_ST19 ->  74
    |  EM_VAX ->  75
    |  EM_CRIS ->  76
    |  EM_JAVELIN ->  77
    |  EM_FIREPATH ->  78
    |  EM_ZSP ->  79
    |  EM_MMIX ->  80
    |  EM_HUANY ->  81
    |  EM_PRISM ->  82
    |  EM_AVR ->  83
    |  EM_FR30 ->  84
    |  EM_D10V ->  85
    |  EM_D30V ->  86
    |  EM_V850 ->  87
    |  EM_M32R ->  88
    |  EM_MN10300 ->  89
    |  EM_MN10200 ->  90
    |  EM_PJ ->  91
    |  EM_OPENRISC ->  92
    |  EM_ARC_A5 ->  93
    |  EM_XTENSA ->  94
    |  EM_VIDEOCORE ->  95
    |  EM_TMM_GPP ->  96
    |  EM_NS32K ->  97
    |  EM_TPC ->  98
    |  EM_SNP1K ->  99
    |  EM_ST200 ->  100
    |  EM_IP2K ->  101
    |  EM_MAX ->  102
    |  EM_CR ->  103
    |  EM_F2MC16 ->  104
    |  EM_MSP430 ->  105
    |  EM_BLACKFIN ->  106
    |  EM_SE_C33 ->  107
    |  EM_SEP ->  108
    |  EM_ARCA ->  109
    |  EM_UNICORE ->  110
    |  EM_NUM n ->  n

let buf_header tmp =
  let h = tmp.tmp_elf in
  let b = tmp.tmp_buf in
  let en = tmp.tmp_en in
  Buffer.add_string b "\127ELF";
  Buffer.add_char b (char_of_int (int_of_file_class h.e_file_class));
  Buffer.add_char b (char_of_int (int_of_data_encoding h.e_data_encoding));
  Buffer.add_char b (char_of_int h.e_file_version);
  Buffer.add_char b (char_of_int (int_of_osabi h.e_osabi));
  Buffer.add_char b (char_of_int h.e_abi_version);
  for _i = 9 to 15 do
    Buffer.add_char b '\000'
  done;

  let e_shoff = tmp.tmp_pos in
  let e_ehsize = if en.word_size = ARCH32 then 52 else 64 in
  let e_phentsize = 0 in
  let e_phnum = 0 in
  let e_shentsize = if en.word_size = ARCH32 then 40 else 64 in
  let e_shnum = tmp.tmp_nsections in

  buf_half en b (int_of_type h.e_type);
  buf_half en b (int_of_machine h.e_machine);
  buf_word32 en b h.e_version;
  buf_addr_by_class en b h.e_entry;
  buf_off_by_class en b 0L;
  buf_off_by_class en b (Int64.of_int e_shoff);
  buf_word32 en b h.e_flags;
  buf_half en b e_ehsize;
  buf_half en b e_phentsize;
  buf_half en b e_phnum;
  buf_half en b e_shentsize;
  buf_half en b e_shnum;
  buf_half en b (section_num tmp ".shstrtab");
  ()

(*
  let flags = match s_type with
    | SHT_PROGBITS bits | SHT_NOBITS bits -> bits.sht_flags

    | SHT_UNKNOWN sec -> sec.RAW.section_header.RAW.sh_flags
    | _ -> []
  in
*)

let int_of_section_type = function
  | RAW.SHT_NULL -> 0
  | RAW.SHT_PROGBITS -> 1
  | RAW.SHT_SYMTAB -> 2
  | RAW.SHT_STRTAB -> 3
  | RAW.SHT_RELA -> 4
  | RAW.SHT_HASH -> 5
  | RAW.SHT_DYNAMIC -> 6
  | RAW.SHT_NOTE -> 7
  | RAW.SHT_NOBITS -> 8
  | RAW.SHT_REL -> 9
  | RAW.SHT_SHLIB -> 10
  | RAW.SHT_DYNSYM -> 11
  | _ -> assert false

let int_of_section_header_flags flags =
  List.fold_left (fun acc flag ->
    match flag with
    | SHF_WRITE -> acc lor 1
    | SHF_ALLOC -> acc lor 2
    | SHF_EXECINSTR -> acc lor 4
    | SHF_NUM n -> acc lor n
  ) 0 flags

let buf_section_header tmp sh =
  let b = tmp.tmp_buf in
  let en = tmp.tmp_en in
  buf_word32 en b sh.RAW.sh_name;
  buf_word32 en b (Int64.of_int (int_of_section_type sh.RAW.sh_type));
  buf_word_by_class en b (Int64.of_int (int_of_section_header_flags sh.RAW.sh_flags));
  buf_addr_by_class en b sh.RAW.sh_addr;
  buf_off_by_class en b sh.RAW.sh_offset;
  buf_word_by_class en b sh.RAW.sh_size;
  buf_word32 en b sh.RAW.sh_link;
  buf_word32 en b sh.RAW.sh_info;
  buf_word_by_class en b sh.RAW.sh_addralign;
  buf_word_by_class en b sh.RAW.sh_entsize;
  ()

let buf_section tmp s =
  if !debug then
    Printf.eprintf "Buffering %d -> %d\n%!"
      s.sh_num (Buffer.length tmp.tmp_buf);
  Buffer.add_string tmp.tmp_buf s.sh_content

let to_string e =

  let en = get_encoding e.e_data_encoding e.e_file_class in

  let buf = Buffer.create 10000 in
  let tmp = {
    tmp_elf = e;
    tmp_en = en;
    tmp_buf = buf;
    tmp_pos = if en.word_size = ARCH32 then 52 else 64;
    tmp_nsections = 0;
    tmp_sections = IntMap.empty;
    tmp_contents = [];
    tmp_map = StringMap.empty;
    tmp_strtab = new_strtab ();
    tmp_shstrtab = new_strtab ();
  } in

  add_section tmp "" SHT_NULL;
  StringMap.iter (fun name s ->
    match name with
    | ""
    | ".strtab"
    | ".shstrtab" -> () (* automatically generated *)
    | _ ->
      add_section tmp name s.s_desc) e.e_sections;
  add_section tmp ".strtab" (string_table tmp.tmp_strtab);

  (* Must be done before saving the content ! *)
  let _ = get_string tmp.tmp_shstrtab  ".shstrtab" in
  add_section tmp ".shstrtab" (string_table tmp.tmp_shstrtab);

  buf_header tmp;
(*
  List.iter (fun content ->
    Buffer.add_string tmp.tmp_buf content)
    (List.rev tmp.tmp_contents);
*)
(*
  for i = 0 to tmp.tmp_nsections - 1 do
    buf_section tmp (IntMap.find i tmp.tmp_sections)
  done; *)
  for i = 0 to tmp.tmp_nsections - 1 do
    let b = tmp.tmp_buf in
    let s = IntMap.find i tmp.tmp_sections in
    s.sh_pos <- Buffer.length b;
    Buffer.add_string b s.sh_content
  done;
  for i = 0 to tmp.tmp_nsections - 1 do
    let s = IntMap.find i tmp.tmp_sections in
    buf_section_header tmp { s.sh_header with
                             RAW.sh_offset = Int64.of_int s.sh_pos }
  done;

  Buffer.contents buf

let to_file filename t =
  let s = to_string t in
  let oc = open_out_bin filename in
  output_string oc s;
  close_out oc
end
