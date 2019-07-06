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
  match VmtInput.from_file filename with

  | Ok res -> Format.printf "%s @." res

  | Error (VmtInput.UnexpectedChar (pos, c)) ->
      Format.eprintf "%a: error: unexpected character ‘%c’@."
        Position.pp_print_position pos c

  | Error (VmtInput.SyntaxError pos) ->
      Format.eprintf "%a: syntax error@." Position.pp_print_position pos
  
  | Error (VmtInput.IdentifierAlreadyExists (pos, str)) ->
      Format.eprintf "%a: Identifier %s already exists in the scope@." 
        Position.pp_print_position pos str
  
  | Error (VmtInput.InvalidArgCount (pos, i1, i2)) ->
      Format.eprintf "%a: Invalid argument count (%d given but %d expected)@." 
        Position.pp_print_position pos i2 i1  
  
  | Error (VmtInput.InvalidOperator (pos, str)) ->
      Format.eprintf "%a: Invalid operator '%s'@." 
        Position.pp_print_position pos str
  
  | Error (VmtInput.InvalidType (pos, str)) ->
      Format.eprintf "%a: Invalid type '%s'@." 
        Position.pp_print_position pos str
  
  | Error (VmtInput.InvalidTypeWithOperator (pos, str1, str2)) ->
      Format.eprintf "%a: Operator '%s' doesn't support type '%s'@." 
        Position.pp_print_position pos str2 str1
  
  | Error (VmtInput.MissingIdentifier (pos, str)) ->
      Format.eprintf "%a: Identifier '%s' is missing@." 
        Position.pp_print_position pos str
  
  | Error (VmtInput.MissingTerm pos) ->
      Format.eprintf "%a: Term is required@." 
        Position.pp_print_position pos
  
  | Error (VmtInput.NonMatchingTypes (pos, str1, str2)) ->
      Format.eprintf "%a: Types '%s' and '%s' don't match in the expression@." 
        Position.pp_print_position pos str1 str2
  
  | Error (VmtInput.NotSupported (pos, str)) ->
      Format.eprintf "%a: Functionality '%s' is parsable but not supported@." 
        Position.pp_print_position pos str
      
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

