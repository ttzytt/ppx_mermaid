open Ppxlib
open Ast_helper


module MT = Mermaid_defs.Defs.MermaidTr

let expr_of_lit_str str loc = Exp.constant (Pconst_string (str, loc, None))

let rec mermaid_tree_expr_of_type ?(type_decl_name : string option) (typ : core_type) = 
    let loc = typ.ptyp_loc in
    let type_decl_name = if type_decl_name = None then "" else Option.get type_decl_name in
    let epxr_of_tdecl_name = expr_of_lit_str type_decl_name loc in
    match typ with
    | [%type: int] -> [%expr fun i -> { Ppx_mermaid.MT.default_node with typ_text = "int"; 
                                          text = string_of_int i }]
    | [%type: string] -> [%expr fun i -> { Ppx_mermaid.MT.default_node with typ_text = "string";
                                                                text = i}]
    | [%type: bool] -> [%expr fun i -> { Ppx_mermaid.MT.default_node with typ_text = "bool";
                                                text = string_of_bool(i)}] 
    | [%type: float] -> [%expr fun i -> { Ppx_mermaid.MT.default_node with typ_text = "float";
                                                text = string_of_float(i)}]
    | [%type: [%t? t] list] -> 
      [%expr fun lst -> 
      let type_text = if [%e epxr_of_tdecl_name] = "" then "list" else
                      "list< " ^ [%e epxr_of_tdecl_name] ^ " >" in
      { Ppx_mermaid.MT.default_node with typ_text = type_text;
        edge = List.map 
        (fun x -> {Ppx_mermaid.MT.default_edge with dst = [%e mermaid_tree_expr_of_type t] x}) lst;
      }]
    | { ptyp_desc = Ptyp_constr (lid, args); _ } ->
      (* We generate the name with "_stringify" on the end *)
      let new_name = Expansion_helpers.mangle_lid (Prefix "mt_of") lid.txt in
      let new_lid = { lid with txt = new_name } in
      (* We turn it into an identifier hoping we have already generated 
         the function either by hand or by using the ppx *)
      let fn = Exp.ident new_lid in
      (* We construct an application of this function to an argument.
         Note that we assume the function may be higher-order if the type
         contains parameters e.g. int list *)
      let app =
        if args = [] then fn else
        let apply_arg_lst = List.map (fun x -> Nolabel, mermaid_tree_expr_of_type x ) args in
        Exp.apply fn apply_arg_lst  
      in
      [%expr fun x -> [%e app] x]

    | _ -> Location.raise_errorf ~loc "No support for this type: %s"
          (string_of_core_type typ)

let generate_impl ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map 
  (fun typ_decl -> 
    (* let t = int, then t is that type name *)
    let typ_name = typ_decl.ptype_name.txt in
    let typ_name_expr = expr_of_lit_str typ_name loc in

    match typ_decl with
    | {ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_record _; ptype_manifest; _ } -> (
      match ptype_manifest with
      | Some t -> 
          let mermaid_func = mermaid_tree_expr_of_type ~type_decl_name:typ_name t in
          let func_name = 
            if typ_decl.ptype_name.txt = "t" then {loc; txt="mt_of"}
            else {loc; txt = "mt_of_" ^ typ_decl.ptype_name.txt}
          in 
          [%stri let [%p Pat.var func_name] =  [%e mermaid_func]]
      | None -> 
          let mermaid_func = [%expr fun _ -> { MT_.default_node with 
                              typ_text = [%e typ_name_expr]; text = "[opaque_type]"}] in
          let func_name = 
            if typ_decl.ptype_name.txt = "t" then {loc; txt="mt_of"}
            else {loc; txt = "mt_of_" ^ typ_decl.ptype_name.txt}
          in 
          [%stri let [%p Pat.var func_name] = [%e mermaid_func]]
      )
    | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type")
  type_decls

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let mermaid_ast = Deriving.add "mermaid_ast" ~str_type_decl:impl_generator