open Ocaml5_parser
open Parsetree
(* open Sugar *)

let exists_attributes l name =
  List.exists (fun a -> String.equal a.attr_name.Location.txt name) l

let rec clear_ct_attr ct =
  let aux = function
    | Ptyp_tuple cts -> Ptyp_tuple (List.map clear_ct_attr cts)
    | Ptyp_constr (name, cts) -> Ptyp_constr (name, List.map clear_ct_attr cts)
    | _ as t -> t
  in
  { ct with ptyp_attributes = []; ptyp_desc = aux ct.ptyp_desc }

let layout_ t =
  let _ = Format.flush_str_formatter () in
  Pprintast.pattern Format.str_formatter t;
  Format.flush_str_formatter ()

let dest_to_pat pat =
  {
    ppat_desc = pat;
    ppat_loc = Location.none;
    ppat_loc_stack = [];
    ppat_attributes = [];
  }

let desc_to_ct t =
  {
    ptyp_desc = t;
    ptyp_loc = Location.none;
    ptyp_loc_stack = [];
    ptyp_attributes = [];
  }

let get_if_rec flag =
  match flag with Asttypes.Recursive -> true | Asttypes.Nonrecursive -> false

let mk_idloc names =
  match Longident.unflatten names with
  | None -> failwith "die"
  | Some id -> Location.mknoloc id

let desc_to_ocamlexpr desc =
  {
    pexp_desc = desc;
    pexp_loc = Location.none;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let desc_to_structure_item desc = { pstr_desc = desc; pstr_loc = Location.none }
let mk_ty_constr name args = desc_to_ct @@ Ptyp_constr (mk_idloc [ name ], args)
let mk_ty_list ct = mk_ty_constr "list" [ ct ]

let get_ty_from_type_dedeclaration { ptype_name; ptype_params; _ } =
  let type_name = ptype_name.Location.txt in
  let args = List.map fst ptype_params in
  mk_ty_constr type_name args

let string_to_pattern name = dest_to_pat @@ Ppat_var (Location.mknoloc name)
let tuple_to_pattern pats = dest_to_pat @@ Ppat_tuple pats

let string_dataconstr_to_pattern (name, args) =
  let c = Location.mknoloc (Longident.Lident name) in
  let res =
    match args with
    | [] -> Ppat_construct (c, None)
    | [ arg ] -> Ppat_construct (c, Some ([], arg))
    | _ -> Ppat_construct (c, Some ([], tuple_to_pattern args))
  in
  dest_to_pat res

let string_record_to_pattern args =
  let closed_flag =
    if List.exists (fun (_, x) -> match x with None -> true | _ -> false) args
    then Asttypes.Open
    else Asttypes.Closed
  in
  let args =
    List.filter_map
      (fun (lab, pat) ->
        match pat with None -> None | Some pat -> Some (lab, pat))
      args
  in
  dest_to_pat @@ Ppat_record (args, closed_flag)

let string_to_ct name =
  desc_to_ct @@ Ptyp_constr (Location.mknoloc @@ Longident.Lident name, [])

let ty_var = string_to_ct "string"
let mk_arrty ct1 ct2 = desc_to_ct @@ Ptyp_arrow (Asttypes.Nolabel, ct1, ct2)
let typed_to_pattern (pat, ct) = dest_to_pat @@ Ppat_constraint (pat, ct)
let typed_to_expr (expr, ct) = desc_to_ocamlexpr @@ Pexp_constraint (expr, ct)

let structure_item_get_typedefs (structure : structure_item) =
  match structure.pstr_desc with Pstr_type (_, tyds) -> Some tyds | _ -> None

let structure_get_typedefs (structure : structure) =
  List.concat @@ List.filter_map structure_item_get_typedefs structure

let structure_item_get_valuedefs (structure : structure_item) =
  match structure.pstr_desc with
  | Pstr_value (flag, vbs) -> Some (flag, vbs)
  | _ -> None

let structure_get_valuedefs (structure : structure) =
  List.filter_map structure_item_get_valuedefs structure

let mklam argpat body =
  desc_to_ocamlexpr @@ Pexp_fun (Asttypes.Nolabel, None, argpat, body)

let mkvar name = desc_to_ocamlexpr @@ Pexp_ident (mk_idloc [ name ])
let mktuple es = desc_to_ocamlexpr @@ Pexp_tuple es

let mk_vb (lhs, rhs) =
  {
    pvb_pat = string_to_pattern lhs;
    pvb_expr = rhs;
    pvb_attributes = [];
    pvb_loc = Location.none;
  }

let mk_valuedef vbs =
  desc_to_structure_item @@ Pstr_value (Asttypes.Nonrecursive, vbs)

let mk_rec_valuedef vbs =
  desc_to_structure_item @@ Pstr_value (Asttypes.Recursive, vbs)

let st_to_pattern (name, ct) = typed_to_pattern (string_to_pattern name, ct)

let mk_multi_rec_funcdef_args l =
  let l =
    List.map
      (fun (fname, args, body) ->
        let args = List.map st_to_pattern args in
        let body = List.fold_right mklam args body in
        mk_vb (fname, body))
      l
  in
  mk_rec_valuedef l

let mk_rec_funcdef_args fname args body =
  mk_multi_rec_funcdef_args [ (fname, args, body) ]

let mk_rec_funcdef fname arg body = mk_rec_funcdef_args fname [ arg ] body

let mk_funcdef_args fname args body =
  let args = List.map st_to_pattern args in
  let body = List.fold_right mklam args body in
  mk_valuedef [ mk_vb (fname, body) ]

let mk_funcdef fname (argname, ct) body =
  let argpat = typed_to_pattern (string_to_pattern argname, ct) in
  mk_valuedef [ mk_vb (fname, mklam argpat body) ]

let mk_match_case (lhs, exp) = { pc_lhs = lhs; pc_guard = None; pc_rhs = exp }

let mk_match case_target cases =
  desc_to_ocamlexpr @@ Pexp_match (case_target, List.map mk_match_case cases)

let mk_ite (e1, e2, e3) = desc_to_ocamlexpr @@ Pexp_ifthenelse (e1, e2, Some e3)

let layout_ct t =
  let _ = Format.flush_str_formatter () in
  Pprintast.core_type Format.str_formatter t;
  Format.flush_str_formatter ()

let ct_eq ct ct' = String.equal (layout_ct ct) (layout_ct ct')

let mk_op_apply (op, args) =
  desc_to_ocamlexpr
  @@ Pexp_apply (mkvar op, List.map (fun x -> (Asttypes.Nolabel, x)) args)

let mk_typed_map e f = mk_op_apply ("#->", [ e; f ])
let mk_list_map_apply f e = mk_op_apply ("List.map", [ f; e ])
let mk_list_concat_apply l = mk_op_apply ("List.concat", [ l ])
let mk_list_union_apply a b = mk_op_apply ("@", [ a; b ])
let mk_list_subtract_apply a b = mk_op_apply ("typed_subtract", [ a; b ])

let mk_construct (name, args) =
  let arg =
    match args with [] -> None | [ x ] -> Some x | _ -> Some (mktuple args)
  in
  desc_to_ocamlexpr
  @@ Pexp_construct (Location.mknoloc (Longident.Lident name), arg)

let mk_empty_list = mk_construct ("[]", [])
let mk_list_cons_apply a b = mk_construct ("::", [ a; b ])
let mk_list_literal l = List.fold_right mk_list_cons_apply l mk_empty_list
(* let mk_construct (name, args) = *)
(*   desc_to_ocamlexpr *)
(*   @@ Pexp_construct (Location.mknoloc (Longident.Lident name), args) *)

open Convention
open Sugar

let pcd_args_to_args pcd_args =
  match pcd_args with
  | Pcstr_tuple cts ->
      List.mapi
        (fun idx ct -> (mk_tmp_name idx (layout_ct (clear_ct_attr ct)), ct))
        cts
  | Pcstr_record lds ->
      List.map (fun ld -> (ld.pld_name.Location.txt, ld.pld_type)) lds

let tyd_to_cs tyd =
  match tyd.ptype_kind with
  | Ptype_variant cs -> cs
  | _ -> _failatwith __FILE__ __LINE__ "unimp"

type constrh = {
  cargs : (string * core_type) list;
  cmkpat : pattern option list -> pattern;
  cmke : expression option list -> expression;
}

let mk_first_match target_e (f : constrh -> pattern * expression)
    (tyd : type_declaration) : expression =
  let cs =
    List.map
      (fun { pcd_name; pcd_args; _ } ->
        let name = pcd_name.Location.txt in
        match pcd_args with
        | Pcstr_tuple cts ->
            let cargs =
              List.mapi
                (fun idx ct ->
                  (mk_tmp_name idx (layout_ct (clear_ct_attr ct)), ct))
                cts
            in
            let cmkpat pats =
              let pats =
                List.map
                  (fun pat ->
                    match pat with
                    | None -> string_to_pattern "_"
                    | Some pat -> pat)
                  pats
              in
              string_dataconstr_to_pattern (name, pats)
            in
            let cmke es =
              let es =
                List.map
                  (fun pat ->
                    match pat with None -> mkvar "_" | Some pat -> pat)
                  es
              in
              mk_construct (name, es)
            in
            let lhs, e = f { cargs; cmkpat; cmke } in
            (lhs, e)
        | Pcstr_record lds ->
            let cargs =
              List.map (fun ld -> (ld.pld_name.Location.txt, ld.pld_type)) lds
            in
            let cmkpat pats =
              let pats =
                List.map (fun ((lab, _), pat) ->
                    let lab = Location.mknoloc (Longident.Lident lab) in
                    (lab, pat))
                @@ List.combine cargs pats
              in
              string_dataconstr_to_pattern
                (name, [ string_record_to_pattern pats ])
            in
            let cmke es =
              let es =
                List.map
                  (fun x ->
                    match x with
                    | Some x -> x
                    | None -> _failatwith __FILE__ __LINE__ "never")
                  es
              in
              let es =
                List.map (fun ((lab, _), pat) ->
                    let lab = Location.mknoloc (Longident.Lident lab) in
                    (lab, pat))
                @@ List.combine cargs es
              in
              mk_op_apply (name, [ desc_to_ocamlexpr @@ Pexp_record (es, None) ])
            in
            let lhs, e = f { cargs; cmkpat; cmke } in
            (lhs, e))
      (tyd_to_cs tyd)
  in
  mk_match target_e cs

let match_ct_switch (ct : core_type) (default, l) =
  List.fold_left
    (fun res (ct', res') -> if ct_eq ct ct' then res' else res)
    default l

let split_ct_attr (ct : core_type) =
  let ct' = { ct with ptyp_attributes = [] } in
  if exists_attributes ct.ptyp_attributes "bound" then (Some "bound", ct')
  else if exists_attributes ct.ptyp_attributes "free" then (Some "free", ct')
  else (None, ct')
