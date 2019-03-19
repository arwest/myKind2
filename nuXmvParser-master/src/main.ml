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
  (* | Ok res -> (match res with
              | [] -> Format.printf "AST is empty@."
              | NuxmvAst.CustomModule _ :: t -> Format.printf "AST is not empty@.") *)
  | Ok res -> Format.printf res

  | Error (NuxmvInput.UnexpectedChar (pos, c)) ->
      Format.eprintf "%a: error: unexpected character ‘%c’@."
        Position.pp_print_position pos c

  | Error (NuxmvInput.SyntaxError pos) ->
      Format.eprintf "%a: syntax error@." Position.pp_print_position pos

  | Error (NuxmvInput.LtlUseError pos) ->
      Format.eprintf "%a: ltl expression use error@." 
        Position.pp_print_position pos

  | Error (NuxmvInput.NextExprError pos) ->
      Format.eprintf "%a: next expression use error@." 
        Position.pp_print_position pos
  
  | Error (NuxmvInput.DoubleNextExprError pos) ->
      Format.eprintf "%a: Cannot use next expression in side a next expression@." 
        Position.pp_print_position pos
  
  | Error (NuxmvInput.RangeLowerValueError pos) ->
      Format.eprintf "%a: Lower bound of range must be less than or equal to upper bound@." 
        Position.pp_print_position pos

  | Error (NuxmvInput.ExpectedTypeError pos) ->
      Format.eprintf "%a: Given type not in the list of allowed types for operation@." 
        Position.pp_print_position pos

  | Error (NuxmvInput.NonMatchingTypeError pos) ->
      Format.eprintf "%a: Two types of operation are not equal@." 
        Position.pp_print_position pos

  | Error (NuxmvInput.MissingVariableError pos) ->
      Format.eprintf "%a: Missing variable in environment when called@." 
        Position.pp_print_position pos
  
  | Error (NuxmvInput.AssignTypeError pos) ->
      Format.eprintf "%a: Type of variable being assigned and the value being assigned to do not match@." 
        Position.pp_print_position pos

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

