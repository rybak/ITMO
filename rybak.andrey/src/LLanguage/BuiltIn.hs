module LLanguage.BuiltIn (
	builtInFunctions,
	builtInConsts
) where

builtInConsts, builtInFunctions :: String
builtInConsts = unlines [
		scanf_d, scanf_s,
		printf_d, printf_s,
		bool_false_str, bool_true_str
	]

llvm_const name size val = "@." ++ name
	++ " = private unnamed_addr constant [" ++ show size ++ " x i8] c"
	++ val ++ ", align 1"
scanf_d = llvm_const "read_int" 3 "\"%d\\00\""
scanf_s = llvm_const "read_str" 6 "\"%255s\\00\""
printf_d = llvm_const "write_int" 3 "\"%d\\00\""
printf_s = llvm_const "write_str" 3 "\"%s\\00\""
bool_false_str = llvm_const "false_str" 6 "\"false\\00\""
bool_true_str = llvm_const "true_str" 5 "\"true\\00\""
--
builtInFunctions = unlines [
		decl_scanf, decl_printf
	]

decl_scanf = "declare i32 @scanf(i8*, ...) nounwind\n"
decl_printf = "declare i32 @printf(i8*, ...) nounwind\n"
