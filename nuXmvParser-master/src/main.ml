(* Copyright (c) 2019 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)

(** @author Daniel Larraz *)

let read_input_from_file filename =
  match NuxmvInput.from_file filename with
  | Ok _ -> Format.printf "No errors found!@."

  | Error (NuxmvInput.UnexpectedChar (pos, c)) ->
      Format.eprintf "%a: error: unexpected character ‘%c’@."
        NuxmvInput.pp_print_position pos c

  | Error (NuxmvInput.SyntaxError pos) ->
      Format.eprintf "%a: syntax error@." NuxmvInput.pp_print_position pos

  | exception (Sys_error msg) ->
      Format.eprintf "%s@." msg

  | exception e ->
      Format.eprintf "%s@." (Printexc.to_string e)

  
let main () =
  if Array.length Sys.argv >= 2 then
    read_input_from_file Sys.argv.(1)
  else
    Format.printf "Usage: %s <input.smv>@." Sys.argv.(0)


let () = main ()

