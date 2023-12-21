#ifndef __X86_ASSEMBLER_LIB_PARSER_HH__
#define __X86_ASSEMBLER_LIB_PARSER_HH__

#include <variant>

#include "lib/core.hh"
#include "lib/utils.hh"
#include "lib/lexer.hh"

namespace fiska::syntax {

struct Expr;
struct ProcDecl;
struct X86_Instruction;

struct Module {
    // AST of this module.
    Vec<Expr*> exprs;
    // All instructions within this module.
    Vec<X86_Instruction*> x86_instrs;
    // Module name.
    std::string name;

    ~Module();
};

struct X86_Instruction {
    enum struct Ty {
        Invalid,

        Mov,
    };
    Ty ty = Ty::Invalid;

    X86_Instruction(Ty ty) : ty(ty) {}
    virtual ~X86_Instruction() = default;

    // Create an expression and bind it to its parent module.
    void* operator new(usz sz, Module* mod);
    // Disallow creating expressions with no owner module.
    void* operator new(usz sz) = delete;
};

struct Mov : public X86_Instruction {
    core::Operand dst;
    core::Operand src;

    Mov(core::Operand dst, core::Operand src) 
        : X86_Instruction(X86_Instruction::Ty::Mov),
        dst(dst), src(src)
    {}
};

struct Expr {
    enum struct Ty {
        Invalid,

        Proc,
        Block,
    };

    Ty ty = Ty::Invalid;

    Expr(Ty ty) : ty(ty) {}
    virtual ~Expr() = default;

    // Create an expression and bind it to its parent module.
    void* operator new(usz sz, Module* mod);
    // Disallow creating expressions with no owner module.
    void* operator new(usz sz) = delete;
};

struct BlockExpr : public Expr {
    Vec<X86_Instruction*> instructions;

    BlockExpr(Vec<X86_Instruction*> instructions) 
        : Expr(Expr::Ty::Block), instructions(std::move(instructions)) {}
};

struct ProcExpr : public Expr {
    std::string name;
    BlockExpr* body;

    ProcExpr(std::string name, BlockExpr* body) 
        : Expr(Expr::Ty::Proc), name(name), body(body) {}
};

struct Parser {
    Lexer lxr;
    Module* mod{};

    Parser(fsm::File& file) : lxr(Lexer(file)) {
        mod = new Module();
    }

    // Helper functions.
    auto At(std::same_as<Tok::Ty> auto... tok_tys) -> bool {
        return ((lxr.tok.ty == tok_tys) or ...);
    }

    auto Consume(std::same_as<Tok::Ty> auto... tok_tys) -> bool {
        if (At(tok_tys...)) {
            lxr.NextTok();
            return true;
        }
        return false;
    }

    auto ParseProcExpr() -> ProcExpr*;
    auto ParseBlockExpr() -> BlockExpr*;
    auto ParseX86Instruction() -> X86_Instruction*;
    auto ParseX86Operand() -> core::Operand;
    auto ParseFile() -> Module*;
};

}  // namespace fiska::syntax

#endif  // __X86_ASSEMBLER_LIB_PARSER_HH__
