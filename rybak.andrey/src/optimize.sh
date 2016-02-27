

llvm-as-3.5 < "${1}.ll" | opt-3.5 -mem2reg | llvm-dis-3.5 > "${1}-optimized.ll"
