open Ppx_mermaid
open Mermaid_defs
type i_list = int list [@@deriving mermaid_ast, show]
type ii_list = i_list list[@@deriving mermaid_ast, show]
;;
let test_ii_list = [[1;2;3];[4;5;6]];;
let mtree = mt_of_ii_list test_ii_list;;
print_endline(MT.show_node mtree);;
print_endline(Defs.mermaid_str_of_mt mtree);;

