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


type position = { fname : string; line: int; col: int }

val pp_print_position: Format.formatter -> position -> unit

type input = unit

type parse_error =
  | UnexpectedChar of position * char
  | SyntaxError of position

val from_channel: in_channel -> (input, parse_error) result

val from_file: string -> (input, parse_error) result

