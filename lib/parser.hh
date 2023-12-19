#ifndef __X86_ASSEMBLER_LIB_PARSER_HH__
#define __X86_ASSEMBLER_LIB_PARSER_HH__

#include <variant>

#include "lib/core.hh"
#include "lib/utils.hh"
#include "lib/lexer.hh"

namespace fiska::syntax {

using core::Operand;
using fsm::SourceLoc;

struct Expr;
struct ProcDecl;

struct Module {
    // AST of this module.
    Vec<Expr*> exprs;
    // Module name.
    std::string name;
};

struct X86_Instruction {
    enum struct Ty {
        Invalid,

        Mov,
    };
    Ty ty = Ty::Invalid;

    X86_Instruction(Ty ty) : ty(ty) {}
    virtual ~X86_Instruction() = default;
};

struct Mov : public X86_Instruction {
    Operand dst;
    Operand src;

    Mov(Operand dst, Operand src) 
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
    Vec<Expr*> exprs;

    BlockExpr(Vec<Expr*> exprs) 
        : Expr(Expr::Ty::Block), exprs(std::move(exprs)) {}
};

struct ProcDecl : public Expr {
    std::string name;
    BlockExpr* body;

    ProcDecl(std::string name, BlockExpr* body) 
        : Expr(Expr::Ty::Proc), name(name), body(body) {}
};

struct Parser {
    Lexer lxr;

    Parser(fsm::File& file) : lxr(Lexer(file)) {}

    auto ParseProcDecl() -> ProcDecl*;
    auto ParseBlockExpr() -> BlockExpr*;
    auto ParseFile() -> Module*;
};

}  // namespace fiska::syntax

#endif  // __X86_ASSEMBLER_LIB_PARSER_HH__
