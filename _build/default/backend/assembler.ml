(* This file is copyright 2012 by John Toman. Permission is granted for use in the 
   instruction of the course CMSC430 at the University of Maryland.
   Some tweaks by Jeff Foster
*)
(* hack needed to get the size of size_t and ints on the current system *)
external get_size_t_size : unit -> int = "get_size_t_size"
external get_int_size : unit -> int = "get_int_size"

let int_size = get_int_size ()
let size_t_size = get_size_t_size ()

type lua_const = [
  | `L_String of string
  | `L_Double of float
  | `L_Bool of bool
  | `L_Nill
]

type rk_source = [ lua_const | `Register of int ]

type return_val = 
  | No_Value
  | Return_One of int
  | Return_Many of int * int

type arith_op_types = Arith_Add | Arith_Sub | Arith_Mul | Arith_Div

type arith_op = {
  op_type: arith_op_types;
  left_src: rk_source;
  right_src: rk_source;
  dest: int
}

type comparison_type = 
    Comp_Eq
  | Comp_Lt
  | Comp_Le

type comparison_op = {
  skip_if_not: bool;
  right_operand: rk_source;
  left_operand: rk_source;
  comp_type: comparison_type
}

type lua_ops = 
  | Load_Const of int * lua_const
  | Get_Global of int * string
  | Set_Global of string * int
  | Concat of int * int * int
  | Jump of int
  | Cmp of comparison_op
  | Move of int * int
  | Arith of arith_op
  | Call of int * int * int
  (* [destination register] * [function index] *)
  | Closure of int * int
  (* [dest register] * [table reg] * [table key] *)
  | Load_Table of int * int * rk_source
  (* [table reg] * [table key] * [table_value] *)
  | Set_Table of int * rk_source * rk_source
  | Set_List of int * int * int
  (* [table reg] * [array size] * [hash size] *)
  | Make_Table of int * int * int
  | Return of return_val

type function_unit = {
  instructions: lua_ops list;
  num_params: int;
  child_functions: function_unit list;
}
type instruction_type = 
  | I_ABC
  | I_ABx
  | I_AsBx

type instruction = {
  a: int;
  b: int;
  c: int;
  op_code: int;
  instr_type: instruction_type 
}

module type GENINT = sig
	type t
	val to_int: t -> int
	val logand: t -> t -> t
	val shift_right: t -> int -> t
	val of_int: int -> t
end

module IntWriter(I : sig include GENINT val size : int end) : sig
	val write: I.t -> out_channel -> unit
end = struct
  let mask = I.of_int 255;;
  let rec write_loop value byte_index channel =
      if byte_index = I.size then ()
      else
         begin
          output_byte channel
            (I.to_int
               (I.logand (I.shift_right value (byte_index * 8)) mask));
          write_loop value (byte_index + 1) channel
        end
       ;;
  let write v channel = write_loop v 0 channel	
end

module Int64Writer = IntWriter(struct include Int64 let size = 8 end)
module Int32Writer = IntWriter(struct include Int32 let size = 4 end)

let write_int64 i chan = Int64Writer.write i chan
let write_int32 i chan = Int32Writer.write i chan
let write_byte i chan = output_byte chan i

let write_integer = 
  if int_size == 4 then
    fun i chan -> write_int32 (Int32.of_int i) chan
  else
    fun i chan -> write_int64 (Int64.of_int i) chan

(* assumes that the size of an integer is the same
size as size_t *)
let write_size_t = 
  if size_t_size == 4 then
    fun i chan -> write_int32 (Int32.of_int i) chan
  else
    fun i chan -> write_int64 (Int64.of_int i) chan
 
let write_float f chan = 
   write_int64 (Int64.bits_of_float f) chan

let write_string s chan =
  write_size_t ((String.length s) + 1) chan;
  String.iter (fun c ->
    write_byte (int_of_char c) chan
  ) s;
  write_byte 0 chan

let mask_of_int i = 
  let rec loop index accum = 
    if index = i then accum
    else
      loop (index+1) (Int32.logor accum (Int32.shift_left Int32.one index)) in
   loop 0 Int32.zero

let blit_instruction blit_list =
  let rec loop starting_position accum = function
    | [] -> accum
    | (value,width)::t ->
      let blitee = Int32.shift_left 
        (Int32.logand (Int32.of_int value) (mask_of_int width)) starting_position in
      loop (starting_position + width) (Int32.logor blitee accum) t in
   loop 0 Int32.zero blit_list

let instruction_to_int32 instr = match instr.instr_type with
  | I_ABC ->
    blit_instruction [
      (instr.op_code,6);
      (instr.a, 8);
      (instr.c, 9);
      (instr.b, 9);
    ]
  | I_ABx ->
    blit_instruction [
      (instr.op_code,6);
      (instr.a, 8);
      (instr.b, 18)
    ]
  | I_AsBx ->
      blit_instruction [
	(instr.op_code,6);
	(instr.a, 8);
	(instr.b + 131071, 18);
      ]

let write_instruction instr chan = 
  write_int32 (instruction_to_int32 instr) chan

let rk_to_const = function
  | `L_String _ as s -> s
  | `L_Double _ as d -> d
  | `L_Bool _ as b -> b
  | `L_Nill as n -> n
  | `Register _ -> failwith "Error, attempt to convert register to a constant"

let build_constants instruction_list =
  let module L_ValCompare = struct
    type t = lua_const
    let compare a b = Pervasives.compare a b
  end in
  let module L_ValSet = Set.Make(L_ValCompare) in
  let add_src_to_set c_set = function
    | `Register _ -> c_set
    | v -> L_ValSet.add (rk_to_const v) c_set in
  let accum_func const_set instr = match instr with
    | Load_Const (_,l_const) -> L_ValSet.add l_const const_set
    | Arith a_op -> List.fold_left add_src_to_set const_set [ a_op.right_src; a_op.left_src ]
    | Get_Global (_,name) ->
      L_ValSet.add (`L_String name) const_set
    | Set_Global (name,_) ->
      L_ValSet.add (`L_String name) const_set
    | Set_Table (_,k,v) -> List.fold_left add_src_to_set const_set [ k ; v ]
    | Load_Table (_,_,k) -> add_src_to_set const_set k
    | Cmp cmp -> List.fold_left add_src_to_set const_set [cmp.left_operand; cmp.right_operand]
    | _ -> const_set in
  let constant_set = List.fold_left accum_func L_ValSet.empty instruction_list in
  L_ValSet.elements constant_set
  
let get_constant_index l c = 
  let rec loop i = function
    | [] -> raise Not_found
    | h::_ when h = c -> i
    | _::t -> loop (i+1) t in
  loop 0 l

let op_code_of_arith = function
  | Arith_Add -> 12
  | Arith_Sub -> 13
  | Arith_Mul -> 14
  | Arith_Div -> 15

let compile_instruction constant_list =     
  let select_src_code = function
   | `Register r -> r
   | v -> 256 + (get_constant_index constant_list (rk_to_const v)) in 
  function
  | Load_Const (target_reg,const) ->
    {
      a = target_reg;
      c = 0; 
      b = (get_constant_index constant_list const);
      op_code = 1;
      instr_type = I_ABx
    }
  | Move (dst,src) ->
    {
      a = dst;
      b = src;
      op_code = 0;
      c = 0;
      instr_type = I_ABC
    }
  | Arith a_op ->
    {
      a = a_op.dest;
      b = (select_src_code a_op.left_src);
      c = (select_src_code a_op.right_src);
      instr_type = I_ABC;
      op_code = (op_code_of_arith a_op.op_type)
    }
  | Return return_val ->
    begin
      match return_val with
        | No_Value -> 
          {
            a = 0;
            b = 1;
            c = 0;
            op_code = 30;
            instr_type = I_ABC
          }
        | Return_One r ->
          {
            a = r;
            b = 2;
            c = 0;
            op_code = 30;
            instr_type = I_ABC
          }
        | Return_Many (start,num) -> 
	  {
	    a = start;
	    b = num + 1;
	    c = 0;
	    op_code = 30;
	    instr_type = I_ABC
	  }
    end
  | Get_Global (target, name) ->
    {
      a = target;
      b = (get_constant_index constant_list (`L_String name));
      op_code = 5;
      c = 0;
      instr_type = I_ABx
    }
  | Set_Global (name, target) ->
      { 
      a = target;
      b = (get_constant_index constant_list (`L_String name));
      op_code = 7;
      c = 0;
      instr_type = I_ABx
      }
  | Jump target ->
      {
	a = 0;
	b = target;
	c = 0;
	op_code = 22;
	instr_type = I_AsBx
      }
  | Concat (dst, src1, src2) ->
      {
	a = dst;
	b = src1;
	c = src2;
	op_code = 21;
	instr_type = I_ABC
      }
  | Cmp t ->
      {
	a = if t.skip_if_not then 1 else 0;
	b = select_src_code t.left_operand;
	c = select_src_code t.right_operand;
	op_code = (match t.comp_type with
	  | Comp_Eq -> 23
	  | Comp_Lt -> 24
	  | Comp_Le -> 25);
	instr_type = I_ABC; 

      }
  | Call (function_reg,arg_length,return_val_length) ->
    {
      a = function_reg;
      b = arg_length;
      c = return_val_length;
      instr_type = I_ABC;
      op_code = 28
    }
  | Closure (dst,function_ind) ->
    {
      a = dst;
      b = function_ind;
      c = 0;
      instr_type = I_ABx;
      op_code = 36
    }
  | Load_Table (dst,table_reg,key) ->
    {
      a = dst;
      b = table_reg;
      c = select_src_code key;
      instr_type = I_ABC;
      op_code = 6;
    }
  | Set_Table (table_reg,key,value) ->
    {
      a = table_reg;
      b = select_src_code key;
      c = select_src_code value;
      instr_type = I_ABC;
      op_code = 9;
    }
  | Set_List (table_reg,page,size) ->
    {
      a = table_reg;
      c = page;
      b = size;
      op_code = 34;
      instr_type = I_ABC
    }
  | Make_Table (table_reg, array_size, hash_size) ->
    (* The following is adapted from the luaO_int2fb function in the
       lobject.c file in the lua source code. 
    *)
    let encode_byte a = 
      let rec loop (e,x) =
	if x <= 16 then
	  (e,x)
	else
	  loop (e+1, ((x + 1) lsr 1)) in
      let e,x = loop (0,a) in
      if x < 8 then x
      else ((e+1) lsl 3) lor (x - 8) in
    {
      a = table_reg;
      b = (encode_byte array_size);
      c = (encode_byte hash_size);
      instr_type = I_ABC;
      op_code = 10
    }

let get_max_reg i_list = 
  List.fold_left (fun curr_max i -> match i with
    | Arith a -> max a.dest curr_max
    | Get_Global (dest,_) -> max dest curr_max
    | Concat(dst,_,_) -> max curr_max dst
    | Move (dst,_) -> max dst curr_max
    | Return _ -> curr_max
    | Load_Const (dst,_) -> max dst curr_max
    | Call (func_reg, _, return_reg) -> max curr_max (func_reg + return_reg - 2)
    | Make_Table (t_reg,_,_) -> max t_reg curr_max
    | Load_Table (dest,_,_) -> max dest curr_max
    | Closure (dst,_) -> max dst curr_max
    | Set_Table _ | Set_List _ | Jump _ | Cmp _ | Set_Global _ -> curr_max)
  2 i_list


let write_header channel = 
  let w_byte i = write_byte i channel in
  w_byte 0x1b;
  w_byte 0x4C;
  w_byte 0x75;
  w_byte 0x61;
  w_byte 0x51; (* version number *)
  w_byte 0; (* format *)
  w_byte 1; (* endianess (1 = little endian) *)
  w_byte int_size; (* size of integer *)
  w_byte size_t_size; (* size of size_t *)
  w_byte 0x4; (* size of an instruction *)
  w_byte 0x8; (* size of a lua number *)
  w_byte 0x0 (* whether to use integers or double for lua numbers (0 = floats) *)


let write_constants constant_list channel =
  write_integer (List.length constant_list) channel;
  List.iter (fun const ->
    match const with
      | `L_String s -> 
        write_byte 4 channel;
        write_string s channel;
      | `L_Double f -> 
        write_byte 3 channel;
        write_float f channel;
      | `L_Nill -> write_byte 0 channel
      | `L_Bool b ->
	write_byte 1 channel;
	write_byte (if b then 1 else 0) channel;
  ) constant_list

let rec assemble_function is_top function_unit channel =
  let max_reg = (get_max_reg function_unit.instructions) + 1 in
  let constant_list = build_constants function_unit.instructions in
  let code_size = List.length function_unit.instructions in
  begin
    write_string "expression.lua" channel;
    write_integer 0 channel;
    write_integer 0 channel;
    write_byte 0 channel;
    write_byte (if is_top then 0 else function_unit.num_params) channel;
    write_byte (if is_top then 2 else 0) channel;
    write_byte max_reg channel;
    write_integer code_size channel;
    List.iter (fun instr ->
      write_instruction (compile_instruction constant_list instr) channel
    ) function_unit.instructions;
    write_constants constant_list channel;
    write_integer (List.length function_unit.child_functions) channel;
    List.iter (fun f_unit ->
      assemble_function false f_unit channel) function_unit.child_functions;
    write_integer 0 channel;
    write_integer 0 channel;
    write_integer 0 channel
 end

let assemble function_unit channel =
  write_header channel;
  (* function header *)
  assemble_function true function_unit channel
