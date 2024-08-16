module NodeShape = struct
type t = 
  | RECT 
  | ROUND_EDGE
  | STADIUM
  | SUBROUTINE
  | CYLINDRICAL 
  | CIRCLE
  | ASYMMETRIC 
  | RHOMBUS
  | HEXAGON
  | PARALLELOGRAM
  | PARALLELOGRAM_ALT
  | TREPEZOID
  | TREPEZOID_ALT
  | DOUBLE_CIRCLE
[@@deriving show]
let sytle_str_of = function
  | RECT -> ("[", "]")
  | ROUND_EDGE -> ("(", ")")
  | STADIUM -> ("([", "])")
  | SUBROUTINE -> ("[[", "]]")
  | CYLINDRICAL -> ("[(", ")]")
  | CIRCLE -> ("((", "))")
  | ASYMMETRIC -> (">", "]")
  | RHOMBUS -> ("{", "}")
  | HEXAGON -> ("{{", "}}")
  | PARALLELOGRAM -> ("/", "/")
  | PARALLELOGRAM_ALT -> ("\\", "\\")
  | TREPEZOID -> ("/", "\\")
  | TREPEZOID_ALT -> ("\\", "/")
  | DOUBLE_CIRCLE -> ("((", "))")
[@@deriving show]
end 

module EdgeShape = struct
type edge_shape = 
  | ARROW 
  | LINE 
  | DOTTED 
  | THICK
  | INVISIBLE
[@@deriving show]
let style_str_of = function
  | ARROW -> "-->"
  | LINE -> "---"
  | DOTTED -> "-."
  | THICK -> "==.->>"
  | INVISIBLE -> "~~~"
[@@deriving show]

let style_str_with_txt shape str =
  let style_str = style_str_of shape in
  if str = "" then style_str else style_str ^ "|" ^ str ^ "|" 
end

module MermaidTr = struct 

type node = {
  shape : NodeShape.t; 
  text  : string; 
  typ_text  : string;
  edge  : edge list;
} [@@deriving show]

and edge = {
  dst   : node;
  text  : string; 
  shape : EdgeShape.edge_shape;
} [@@deriving show]

let default_node = {
  shape = NodeShape.RECT;
  typ_text = "";
  text = ""; (* only useful for base types like int *)
  edge = [];
}  

let default_edge = {
  dst = default_node;
  text = "";
  shape = EdgeShape.ARROW;
}
end

module type MermaidStyleCfg = sig 
  val node_style : string -> NodeShape.t
  val edge_style : string -> string -> EdgeShape.edge_shape 
  (* edge style between two types *)
end 

module DefaultMermaidStyle : MermaidStyleCfg = struct
  let node_style = function
    | _ -> NodeShape.RECT
  let edge_style a b = 
    match a, b with
    | _, _ -> EdgeShape.ARROW
end



let rec transform_mt_style (cfg : (module MermaidStyleCfg)) (n : MermaidTr.node) : MermaidTr.node = 
  let module Cfg = (val cfg) in
  let new_shape = Cfg.node_style n.typ_text in
  let new_edge = List.map (fun e -> 
    let new_children = transform_mt_style cfg e.MermaidTr.dst in
    let new_shape = Cfg.edge_style n.typ_text e.MermaidTr.dst.typ_text in
    {e with shape = new_shape; dst = new_children}
  ) n.edge in
  {n with shape = new_shape; edge = new_edge}

let nid = ref 0
let next_nid() = 
let cur_id = !nid in
nid := !nid + 1;
cur_id

(** mermaid_string_of mermaid tree *)
let rec _mermaid_str_of_mt ?(mermaid_style : (module MermaidStyleCfg) option) (n : MermaidTr.node) 
         : (string * int) = 
  let mermaid_style = if mermaid_style = None then (module DefaultMermaidStyle : MermaidStyleCfg) 
                      else Option.get mermaid_style in
  let n = transform_mt_style mermaid_style n in
  let cur_id = next_nid() in
  let (l_shape_s, r_shape_s) = NodeShape.sytle_str_of n.shape in
  let ret = if n.text = "" then 
    ref("node_id_" ^ string_of_int cur_id ^ l_shape_s ^ "\"" ^ n.typ_text ^"\"" ^ r_shape_s ^ "\n")
  else 
    ref("node_id_" ^ string_of_int cur_id ^ l_shape_s ^ "\"" ^ n.typ_text  ^ " : " ^ n.text ^ "\"" ^ r_shape_s ^ "\n")
  in 
  List.iter (fun e -> 
     let (child_s, child_id) = _mermaid_str_of_mt ~mermaid_style e.MermaidTr.dst in
     ret := !ret ^ child_s;
     let edge_txt = EdgeShape.style_str_with_txt e.shape e.text in
     ret := !ret ^ "node_id_" ^ string_of_int cur_id ^ edge_txt ^ "node_id_" ^ string_of_int child_id ^ "\n"
  ) n.edge;
  (!ret, cur_id)

let mermaid_str_of_mt ?(mermaid_style : (module MermaidStyleCfg) option) (n : MermaidTr.node) : string = 
  let mermaid_style = if mermaid_style = None then (module DefaultMermaidStyle : MermaidStyleCfg) 
                      else Option.get mermaid_style in
  let (s, _) = _mermaid_str_of_mt ~mermaid_style:mermaid_style n in
  "graph TD\n" ^ s