(* This file is copyright 2012 by John Toman. Permission is granted for use in the 
   instruction of the course CMSC430 at the University of Maryland.
   Some tweaks by Jeff Foster
*)

(** Assembler for a subset of the Lua instructions. The Lua VM references all constants by a numeric index in a constant list that is defined on a per function basis. Instructions that reference a constant therefore do so by numeric index. This module automatically hoists out all constants that appear in an instruciton and replaces it with the corresponding index into the constant list.

It may be possible to use this module without reading the "No Frills" guide, and the core meaning and documentation of each instruction is recreated in the documentation of this module, but the "No Frills" guide is the definitive source, and in cases of doubt should be referenced. In particular, the treatment of global variables is absent in this documentation, and the explanations of SetGlobal and GetGlobal should be referenced to understand how they work.
*)

(** {6 Lua Primitives} *)

type lua_const = [
  | `L_String of string (** A lua string *)
  | `L_Double of float (** A "number", which is a double on most platforms *)
  | `L_Bool of bool (** A boolean value *)
  | `L_Nill (** The nil primitive *)
];;
(* Primitive values supported by the Lua VM *)

type rk_source = [ 
  lua_const
  | `Register of int
];;
(* Some instructions allow for direct references to constants to appear in the source operands along with registers.
    In the "No Frills" guide, an instruction uses an rk_source when when the operands in the RTL representation of the instructions use RK(A) or RK(B)
*)

(** {6 Lua Instructions} *)


(** {7 Arithmetic Operations} *)

type arith_op_types = Arith_Add | Arith_Sub | Arith_Mul | Arith_Div;;

(** The type of arithmetic operation for an arithmetic operation *)

type arith_op = {
  op_type: arith_op_types; (** The operator used in this operation. *)
  left_src: rk_source; (** The left operand, that is, the value on the left of the operator *)
  right_src: rk_source; (** The right operand, that is, the value on the right of the operator *)
  dest: int (** The destination register in which to store the result of this arithmetic operation *)
};;
(* A common representation of an arithmetic expression. *)

(** {7 Comparisons} *)

type comparison_type = 
    Comp_Eq
  | Comp_Lt
  | Comp_Le

(** The comparisons possible in a Comparison instruction. The names are self-explanatory *)

type comparison_op = {
  skip_if_not: bool; (** A comparison instruction will skip the next instruction if the result of the comparison is NOT equal to this field *)
  right_operand: rk_source; (** The operand on the right side of the comparison *)
  left_operand: rk_source; (** The operand of the left side of the comparison *)
  comp_type: comparison_type (** The type of comparison that this comparison_op encodes.*)
}

(** A common representation of comparison operations. See page 35 for details on how Lua encodes conditionals. *) 

(** {7 Supported Instructions} *)

type return_val = 
  | No_Value (** Return no value from the function *)
  | Return_One of int (** Return one value from the function: the contents of the specified register *)
  | Return_Many of int * int (** [Return_Many (r,n)] return [n] values, starting at register [r] *)
(** An abstraction away from the complexities of returning values from a function (variable number of return values is not supported). *)

type lua_ops = 
  | Load_Const of int * lua_const (** Loads the specified constant into the named register *)
  | Get_Global of int * string  (** Loads the global variable identified by the string into the specified register *)
  | Set_Global of string * int (** [Set_Global (global_name,src_reg)] sets the global variable with the name [global_name] to the value in register [src_reg] *)
  | Concat of int * int * int (** [Concat (dst,start,end)] String concatenates a range of registers starting at [start] and ending with [end] and places the result in [dst] *)
  | Jump of int (** Performs an unconditional jump by adding the displacement (which may be negative) to the PC *)
  | Cmp of comparison_op (** Comparison instruction that incorporates the LT, EQ, and LE instructions. See {!Assembler.comparison_op} for an explanation of the fields *)
  | Move of int * int  (** [Move (dst,src)] sets the contents of register [dst] to that of register [src] *)
  | Arith of arith_op (** An arithmetic operation *)
  | Call of int * int * int (** [Call (fn,args,ret)] calls the function loaded into register [fn]. There are [args-1] arguments passed to this function. These arguments reside in registers [fn+1] to [fn+(args-1)]. There are [ret-1] return values saved. These return value(s) function are stored in [fn] to [fn + (ret - 2)]. *)
  (* [destination register] * [function index] *)
  | Closure of int * int (** [Closure (dst,function_index)] loads the closure at [function_index] the current function unit's child functions into [dst]. See {!Assembler.function_unit} for details on child functions. *)
  (* [dest register] * [table reg] * [table key] *)
  | Load_Table of int * int * rk_source (** [Load_Table (dst,table,key)] loads value associated with [key] in the table whose reference is stored in the register [table] into the register [dst]. [key] may be a number (for array lookups), or a string (for hash tables). *)
  (* [table reg] * [table key] * [table_value] *)
  | Set_Table of int * rk_source * rk_source (** [Set_Table (table,k,v)] associates the key [k] with the value [v] in the table whose reference is stored in the register [table]. [k] may be an integer or a string, and [v] may be any value, including a register whose contents is a reference to another table, in which case [k] is associated with a reference to the table. *)
  | Set_List of int * int * int (** [Set_List (table,page,n)] sets a range of numerical indices in the table to values in the registers [table+1] to [table+n] inclusive. The numerical indices set in the table are [(page-1)*50] to [(page-1)*50+n]. Note that page must be offset by 1, a [page] value of 0 will not work as expected.*)
  (* [table reg] * [array size] * [hash size] *)
  | Make_Table of int * int * int (** [Make_Table (dst,n_array,n_hash)] initializes a new table in the register [dst] that has [n_array] array slots pre-allocated and [n_hash]  hash slots pre-allocated. These values are hints, and using more hash slots than specified in [n_hash] or using array indices greater than [n_array] is not an error *)
  | Return of return_val (** Returns a value from the function. *)

(** Representations of the subset of instructions supported by the assembler. The names used here are almost exactly the same as those used in the "No Frills" guide. *)

(** {6 API} *)

type function_unit = {
  instructions: lua_ops list; (** The list of instructions that comprise the body of this function. *)
  num_params: int; (** The number of parameters accepted by this function. If this is the top-level function, that is, the function_unit passed to the {!Assembler.assemble} function, then this value is ignored *)
  child_functions: function_unit list; (** The list of child functions. Within the body of the function. Within the body of this function, these functions can be loaded via numeric index. There is no way to directly reference a parent or any of it's parents functions, that is, child_functions are accessible only within the body of the current function *)
}

(** A representation of a function unit (or function block as it is called in the "No Frills" guide). Varargs functions are not supported *)

val assemble: function_unit -> out_channel -> unit
(** [assemble f c] assembles the function [f] into bytecode and writes it to [c]. [c] must be a writable channel that was opened in binary mode. *) 
