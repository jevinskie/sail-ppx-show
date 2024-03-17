(* open Stdppx *)
open Ppxlib

let () = Driver.enable_checks ()

let expand ~ctxt env_var =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match Sys.getenv env_var with
  | value -> Ast_builder.Default.estring ~loc value
  | exception Not_found ->
      let ext =
        Location.error_extensionf ~loc "The environement variable %s is unbound"
          env_var
      in
      Ast_builder.Default.pexp_extension ~loc ext

let my_extension =
  Extension.V3.declare "get_env" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "get_env"

(*
let transform a b c =
  let module Metaopt (Item : Ppxlib.payload) = struct
    let map (super : Item.t Ast.map) (item : Item.t) : Item.t =
      Ppxlib.Ast_helper.with_default_loc (Item.to_loc item) @@ fun () ->
      match Option.bind (Item.destruct_extension item) Options.handle with
      | None -> super item
  end in
  let map = object end in
  b a [c;] *)

(* let map = object (self)
     inherit Ppxlib.Ast_traverse.map

     method! structure s = self#structure s
     method! signature s = self#signature s

     (* method! structure s =
       transform self#structure Ocaml_common.Ast_mapper.default_mapper s *)
     (* method! structure _ = Ocaml_common.Ast_mapper.default_mapper.structure *)

     (* method! signature s =
       transform self#structure Ocaml_common.Ast_mapper.default_mapper s *)
   end *)

(* let () =
   Ppxlib.Driver.register_transformation "get_env"
     ~preprocess_impl:map#structure
     ~preprocess_intf:map#signature *)

(* open Ppxlib.Asttypes
   open Ppxlib.Parsetree
   open Ocaml_common.Ast_mapper

   let test_mapper argv =
     { default_mapper with
       expr = fun mapper expr ->
         match expr with
         | { pexp_desc = Pexp_extension ({ txt = "test"; _ }, PStr []); _} ->
           Ast_helper.Exp.constant Pconst_integer "42" 'z'
         | other -> default_mapper.expr mapper other; }

   let () =
     register "get_env" test_mapper *)

(* open Ppxlib
   class mapper =
     object (self)
       inherit  Ast_traverse.map
       method! signature signature =
         (signature |> (List.map (Ppxlib.Ast_helperSignature.mapSignatureItem self))) |>
           List.concat
       method! structure structure =
         (structure |> (List.map (Structure.mapStructureItem self))) |>
           List.concat
     end
   let structure_mapper s = (new mapper)#structure s
   let signature_mapper s = (new mapper)#signature s
   ;;Ppxlib.Driver.register_transformation ~preprocess_impl:structure_mapper
       ~preprocess_intf:signature_mapper "decco" *)

(* open Stdppx
   open Ppxlib

   let rec last = function
     | x :: [] -> x
     | _ :: xs -> last xs
     | [] -> failwith "no element"

   let side_print_ctxt =
     object
       inherit Ast_traverse.map_with_expansion_context_and_errors as super

       method! structure ctxt st =
         let orig = super#structure ctxt st in
         let orig_struct, orig_err = (fst orig, snd orig) in
         let new_stucture_item =
           let loc = { (last orig_struct).pstr_loc with loc_ghost = true } in
           [%stri let answer = 42]
         in
         let new_struct = orig_struct @ [ new_stucture_item ] in
         (new_struct, orig_err)

       method! signature ctxt sg = super#signature ctxt sg
     end

   let () =
     Driver.V2.(
       register_transformation
         ~impl:(fun ctxt structure ->
           let structure, errors = side_print_ctxt#structure ctxt structure in
           List.map errors ~f:(fun error ->
               Ast_builder.Default.pstr_extension
                 ~loc:(Location.Error.get_location error)
                 (Location.Error.to_extension error)
                 [])
           @ structure)
         ~intf:(fun ctxt signature ->
           let signature, errors = side_print_ctxt#signature ctxt signature in
           List.map errors ~f:(fun error ->
               Ast_builder.Default.psig_extension
                 ~loc:(Location.Error.get_location error)
                 (Location.Error.to_extension error)
                 [])
           @ signature)
         "sail_ppx_show") *)

(* let add_deriving_show_to_type =
     let expr = Ppxlib.Ast_builder.Default.estring ~loc:Location.none "show" in
     let deriver =
       {
         attr_name = { txt = "deriving"; loc = Location.none };
         attr_payload =
           PStr [ { pstr_desc = Pstr_eval (expr, []); pstr_loc = Location.none } ];
         attr_loc = Location.none;
       }
     in
     let extension =
       Ppxlib.Extension.V3.declare "add_deriving_show_to_type"
         Extension.Context.type_declaration
         Ast_pattern.(ptyp __)
         (fun ~ctxt typ ->
           let loc = Expansion_context.Extension.extension_point_loc ctxt in
           { typ with pexp_attributes = deriver :: typ.ptyp_attributes })
     in
     Context_free.Rule.extension extension

   let () =
     Driver.register_transformation "add_deriving_show"
       ~rules:[ Context_free.Rule.extension add_deriving_show_to_type ] *)

(* let add_deriving_show_to_type =
     let deriver =
       Attribute.declare "type_declaration_deriving_show"
         Attribute.Context.type_declaration
         Ast_pattern.(pstr nil)
         ()
     in
     let extension =
       Extension.declare "add_deriving_show_to_type"
         Extension.Context.type_declaration
         Ast_pattern.(ptyp __)
         (fun ~ctxt typ ->
           let loc = Expansion_context.Extension.extension_point_loc ctxt in
           let show_deriving =
             {
               attr_name = Location.mkloc "deriving" loc;
               attr_payload =
                 PStr
                   [
                     Ast_builder.Default.pstr_eval ~loc
                       (Ast_builder.Default.pexp_ident ~loc
                          (Location.mkloc (Longident.Lident "show") loc))
                       [];
                   ];
               attr_loc = loc;
             }
           in
           { typ with ptype_attributes = show_deriving :: typ.ptype_attributes })
     in
     Context_free.Rule.extension extension

   let () =
     Driver.register_transformation "add_deriving_show"
       ~rules:[ Context_free.Rule.extension add_deriving_show_to_type ] *)
(*
let add_deriving_show_to_type =
  let extension =
    Extension.V3.declare "add_deriving_show_to_type"
      Extension.Context.Type_declaration
      Ast_pattern.(ptyp __)
      (fun ~ctxt typ ->
        let loc = Expansion_context.Extension.extension_point_loc ctxt in
        let show_deriving =
          {
            attr_name = Location.mkloc "deriving" loc;
            attr_payload =
              PStr
                [
                  Ast_builder.Default.pstr_eval ~loc
                    (Ast_builder.Default.pexp_construct ~loc
                       (Location.mkloc (Longident.Lident "show") loc)
                       None)
                    [];
                ];
            attr_loc = loc;
          }
        in
        { typ with ptype_attributes = show_deriving :: typ.ptype_attributes })
  in
  Context_free.Rule.extension extension

let () =
  Driver.register_transformation "add_deriving_show"
    ~rules:[ Context_free.Rule.extension add_deriving_show_to_type ] *)

(* let add_deriving_show_to_type =
     let transform ~ctxt type_decl =
       let loc = Location ctxt in
       let show_deriving =
         {
           attr_name = Location.mkloc "deriving" loc;
           attr_payload =
             PStr
               [
                 Ast_builder.Default.pstr_eval ~loc
                   (Ast_builder.Default.pexp_construct ~loc
                      (Location.mkloc (Longident.Lident "show") loc)
                      None)
                   [];
               ];
           attr_loc = loc;
         }
       in
       {
         type_decl with
         ptype_attributes = show_deriving :: type_decl.ptype_attributes;
       }
     in
     Context_free.Rule.Type_declaration.V2.lift transform

   let () =
     Driver.register_transformation "add_deriving_show"
       ~rules:[ add_deriving_show_to_type ] *)

(* open Stdppx *)
(* open Ppxlib

   (* let () = Driver.enable_checks () *)

   let add_deriving_show_to_type =
     let transform ~ctxt type_decl =
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       let show_deriving =
         {
           attr_name = Location.mkloc "deriving" loc;
           attr_payload =
             PStr
               [
                 Ast_builder.Default.pstr_eval ~loc
                   (Ast_builder.Default.pexp_construct ~loc
                      (Location.mkloc (Longident.Lident "show") loc)
                      None)
                   [];
               ];
           attr_loc = loc;
         }
       in
       {
         type_decl with
         ptype_attributes = show_deriving :: type_decl.ptype_attributes;
       }
     in
     Context_free.Rule.Type_declaration.V2.lift transform

   let () =
     Driver.register_transformation "add_deriving_show"
       ~rules:[ add_deriving_show_to_type ] *)

(* let add_show_deriver =
     Deriving.add "show"
       ~extension:(fun ~ctxt:_ ~derivee_path:_ ~derivee_ast_type:_ -> [])
       ~str_type_decl:(fun ~ctxt:_ ~path:_ ~rec_flag:_ ~type_decls:_ ->
         List.map
           (fun ({ ptype_attributes; ptype_loc; _ } as type_decl) ->
             let deriving_show =
               {
                 attr_name = { txt = "deriving"; loc = ptype_loc };
                 attr_payload =
                   PStr
                     [
                       Ast_builder.Default.pstr_eval ~loc:ptype_loc
                         (Ast_builder.Default.pexp_ident ~loc:ptype_loc
                            { txt = Lident "show"; loc = ptype_loc })
                         [];
                     ];
                 attr_loc = ptype_loc;
               }
             in
             {
               type_decl with
               ptype_attributes = deriving_show :: ptype_attributes;
             })
           type_decls)
       ~sig_type_decl:(fun ~ctxt:_ ~path:_ ~rec_flag:_ ~type_decls:_ -> [])

   let () = Driver.register_transformation "add_deriving_show" *)

(* let generator =
     let extension ~ctxt:_ type_decl =
       let loc = type_decl.ptype_loc in
       let deriving_attr =
         {
           attr_name = { txt = "deriving"; loc };
           attr_payload =
             PStr
               [
                 Ast_builder.Default.pstr_eval ~loc
                   (Ast_builder.Default.pexp_construct ~loc
                      { txt = Lident "show"; loc }
                      None)
                   [];
               ];
           attr_loc = loc;
         }
       in
       {
         type_decl with
         ptype_attributes = deriving_attr :: type_decl.ptype_attributes;
       }
     in
     Deriving.Generator.make_noarg (fun ~loc ~path:"foo" type_decls ->
         List.map f (extension ctxt) type_decls)

   let () =
     Deriving.add "show" ~str_type_decl:generator
     |> Driver.register_transformation "add_deriving_show" *)
