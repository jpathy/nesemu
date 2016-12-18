open Core_kernel

let of_file file =
  if not Sys.unix then failwith "tests not supported on this platform";
  let ic = open_in file in
  Bigstring.map_file ~shared:false (Unix.descr_of_in_channel ic) (in_channel_length ic)

let run_tests cpu_file =
  let open Nes in
  let cart = Cartridge.of_bigarray @@ of_file cpu_file in
  Mapper.register_mapper (Some cart);
  let ppu = Ppu.init () in
  let mem = Mem.init ppu in
  let cpu = Cpu.init mem in
  let open Integer in
  let rec loop () =
    Cpu.step cpu mem |> ignore;
    if U8.((Mem.read8 mem (U16.of_int 0x6001) === (of_int 0xDE)) &&
    (Mem.read8 mem (U16.of_int 0x6002) === (of_int 0xB0)) &&
    (Mem.read8 mem (U16.of_int 0x6003) === (of_int 0x61))) then
      begin
      let status = Mem.read8 mem (U16.of_int 0x6000) in
      if U8.(status <<< (of_int 0x80)) then
        Printf.printf "status code : 0x%02X\n" (U8.to_int status)
      else
        loop()
      end
    else
      loop()
  in
  match loop () with
  | exception e -> Printf.printf "Error : %s" (Printexc.to_string e)
  | _ ->
      let rec result addr bs =
        let v = Mem.read8 mem addr in
        if U8.(v === zero) then ()
        else
          begin
          Buffer.add_char bs (Char.unsafe_chr @@ U8.to_int v);
          result U16.(addr+one) bs
          end
      in
      let bs = Buffer.create 1 in
      result U16.(of_int 0x6004) bs; Printf.printf "%s" (Buffer.contents bs)

(* Cmdline interface *)
open Cmdliner

let cpu_file =
  let doc="Run blargg cpu test roms and verify" in
  Arg.(value & opt non_dir_file "test.nes" & info ["cpu-test"] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Test functionality by running nes testroms" in
  let man = [
    `S "DESCRIPTION";
    `P "None";
  ] in
  Term.(const run_tests $ cpu_file),
  Term.info "nes_test" ~version:"0.1" ~doc ~man

let _ = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
