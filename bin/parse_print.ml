open Core

let loop filename () =
  In_channel.with_file filename ~f:(fun file_ic ->
      Lys_parsing.Lex_and_parse.parse_and_print (Lexing.from_channel file_ic))

let () =
  Command.basic_spec ~summary:"Parse and display"
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command_unix.run
