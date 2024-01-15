(* open Ppxlib *)

(* let expand ~ctxt env_var =
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
   let () = Driver.register_transformation ~rules:[ rule ] "get_env" *)

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

open Stdppx
open Ppxlib

let pprint_ctxt ctxt =
  let tool_name = Expansion_context.Base.tool_name ctxt in
  let input_name = Expansion_context.Base.input_name ctxt in
  let file_path =
    Code_path.file_path @@ Expansion_context.Base.code_path @@ ctxt
  in
  Printf.printf "tool_name: %s\ninput_name: %s\nfile_path: %s\n" tool_name
    input_name file_path

let side_print_ctxt =
  object
    inherit Ast_traverse.map_with_expansion_context_and_errors as super

    method! structure ctxt st =
      pprint_ctxt ctxt;
      let orig = super#structure ctxt st in
      let strct = fst orig in
      let loc = Ast_helper.default_loc in
      let strct_new = strct :: Ast_builder.Default.eint ~loc:!loc 1 in
      let errs = snd orig in
      (strct_new, errs)

    method! signature ctxt sg =
      pprint_ctxt ctxt;
      super#signature ctxt sg
  end

let () =
  Driver.V2.(
    register_transformation
      ~preprocess_impl:(fun ctxt structure ->
        let structure, errors = side_print_ctxt#structure ctxt structure in
        List.map errors ~f:(fun error ->
            Ast_builder.Default.pstr_extension
              ~loc:(Location.Error.get_location error)
              (Location.Error.to_extension error)
              [])
        @ structure)
      ~preprocess_intf:(fun ctxt signature ->
        let signature, errors = side_print_ctxt#signature ctxt signature in
        List.map errors ~f:(fun error ->
            Ast_builder.Default.psig_extension
              ~loc:(Location.Error.get_location error)
              (Location.Error.to_extension error)
              [])
        @ signature)
      "print_ctxt")
