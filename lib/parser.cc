#include <string_view>

#include "lib/utils.hh"
#include "lib/parser.hh"

namespace fiska::syntax {

namespace {

const utils::StringMap<X86_Instruction::Ty> x86_mnemonics = {
    {"mov", X86_Instruction::Ty::Mov},
};

const utils::StringMap<core::Bit_Width> bit_widths = {
    {"b8", core::Bit_Width::B8},
    {"b16", core::Bit_Width::B16},
    {"b32", core::Bit_Width::B32},
    {"b64", core::Bit_Width::B64},
};

const utils::StringMap<core::Register::Id> x86_registers = {
    {"rax", core::Register::Id::Rax},
    {"rcx", core::Register::Id::Rcx},
    {"rdx", core::Register::Id::Rdx},
    {"rbx", core::Register::Id::Rbx},
    {"rsp", core::Register::Id::Rsp},
    {"rbp", core::Register::Id::Rbp},
    {"rsi", core::Register::Id::Rsi},
    {"rdi", core::Register::Id::Rdi},
    {"rip", core::Register::Id::Rip},
    {"r8", core::Register::Id::R8},
    {"r9", core::Register::Id::R9},
    {"r10", core::Register::Id::R10},
    {"r11", core::Register::Id::R11},
    {"r12", core::Register::Id::R12},
    {"r13", core::Register::Id::R13},
    {"r14", core::Register::Id::R14},
    {"r15", core::Register::Id::R15},
};

auto StringToI64(std::string_view str) -> Opt<i64> {
    dbg::Unreachable();
}

}  // namespace

void* Expr::operator new(usz sz, Module* mod) {
    auto ptr = static_cast<Expr*>(::operator new(sz));
    mod->exprs.push_back(ptr);
    return ptr;
}

auto Parser::ParseProcDecl() -> ProcDecl* {
    dbg::Assert(Consume(Tok::Ty::Fn),
            "Missing 'fn' keyword when parsing a procedure decl");

    dbg::Assert(At(Tok::Ty::Ident),
            "Expected an identifier after the 'fn' keyword");

    std::string name = lxr.tok.str;
    Consume(Tok::Ty::Ident);

    auto block_expr = ParseBlockExpr();

    return new (mod) ProcDecl(std::move(name), block_expr);
}

auto Parser::ParseBlockExpr() -> BlockExpr* {
    dbg::Assert(Consume(Tok::Ty::Lbrace),
            "Expected a '{{' at the begining of a block expression!");

    Vec<X86_Instruction*> x86_instrs;
    while (At(Tok::Ty::Ident)) {
        x86_instrs.push_back(ParseX86Instruction());
    }

    dbg::Assert(Consume(Tok::Ty::Rbrace),
            "Expected a '}}' at the end of a block expression!");

    return new (mod) BlockExpr(x86_instrs);
}

auto Parser::ParseX86Instruction() -> X86_Instruction* {
    using Ty = X86_Instruction::Ty;

    dbg::Assert(x86_mnemonics.contains(lxr.tok.str),
            "Encountered an unknown x86 instruction: '{}'", lxr.tok.str);

    auto mnemonic = x86_mnemonics.find(lxr.tok.str)->second;
    Consume(Tok::Ty::Ident);

    switch (mnemonic) {
    case Ty::Mov: {
        dbg::Assert(Consume(Tok::Ty::Lparen), "Expected a '(' after an x86 instruction");
        auto dst = ParseX86Operand();
        dbg::Assert(Consume(Tok::Ty::Comma), "Expected a ',' after operand");
        auto src = ParseX86Operand();

        dbg::Assert(Consume(Tok::Ty::Rparen),
                "Expected a ')' after the second operand of a mov instruction");
        dbg::Assert(Consume(Tok::Ty::SemiColon), "Expected a ';' after a mov instruction");

        return new (mod) Mov(dst, src);
    }

    case Ty::Invalid:
        dbg::Unreachable();
    } // switch

    dbg::Unreachable();
}

auto Parser::ParseX86Operand() -> core::Operand {
    auto ParseX86Register = [&]() {
        dbg::Assert(At(Tok::Ty::Register),
                "Expected a register operand after an operand size");

        dbg::Assert(x86_registers.contains(lxr.tok.str),
                "Encountered an unkown x86 register: '{}'", lxr.tok.str);

        auto reg_id = x86_registers.find(lxr.tok.str)->second;
        Consume(Tok::Ty::Register);
        return reg_id;
    };

    auto ParseBitWidth = [&]() {
        dbg::Assert(bit_widths.contains(lxr.tok.str),
                "Encountered an unknown register size: '{}'", lxr.tok.str);
        auto bit_width = bit_widths.find(lxr.tok.str)->second;
        Consume(Tok::Ty::Bsize);
        return bit_width;
    };
    // Register:
    // -> b[x] rax
    // 
    // Mem_Ref: 
    // ->Kind::Base_Index_Maybe_Disp: @b[x] [rax][2][rbx] + 0x11223344
    // ->Kind::Base_Maybe_Disp: @b[x] [rax] + 0x11223344
    // ->Kind::Index_Maybe_Disp: @b[x] [][2][rbx] + 0x11223344
    // ->Kind::Disp_Only: @b[x] 0x11223344
    //
    // Imm:
    // -> 0x11223344
    switch (lxr.tok.ty) {
    // Register:
    // -> b[x] rax
    case Tok::Ty::Bsize: {
        auto bit_width = ParseBitWidth();
        auto reg_id = ParseX86Register();
        return core::Operand(core::Register(reg_id, bit_width));
    }
    // Mem_Ref: 
    // ->Kind::Base_Index_Maybe_Disp: @b[x] [rax][2][rbx] + 0x11223344
    // ->Kind::Base_Maybe_Disp: @b[x] [rax] + 0x11223344
    // ->Kind::Index_Maybe_Disp: @b[x] [][2][rbx] + 0x11223344
    // ->Kind::Disp_Only: @b[x] 0x11223344
    case Tok::Ty::At: {
        Consume(Tok::Ty::At);
        auto operand_size = ParseBitWidth();

        Opt<core::Register> base_reg{std::nullopt};
        Opt<core::Register> index_reg{std::nullopt};
        Opt<core::Mem_Ref::Scale> scale{std::nullopt};
        Opt<i64> mem_ref_disp{std::nullopt};

        dbg::Assert(At(Tok::Ty::Lbracket, Tok::Ty::Num), "Expected either a '[' or an absolute displacement");

        if (At(Tok::Ty::Lbracket)) {
            Consume(Tok::Ty::Lbracket);
            dbg::Assert(At(Tok::Ty::Register, Tok::Ty::Rbracket),
                    "Expected a base register field or an empty field");

            if (At(Tok::Ty::Register)) {
                // Always use 64-bit addressing. Support for 32-bit addressing may come 
                // in the future. I believe 32-bit addressing modes only require a certain
                // prefix added to the instruction.
                base_reg.emplace(core::Register(ParseX86Register(), core::Bit_Width::B64));
            }
            dbg::Assert(Consume(Tok::Ty::Rbracket), "Expected ']' after the base register");

            // Optional scale and index fields.
            if (At(Tok::Ty::Lbracket)) {
                Consume(Tok::Ty::Lbracket);
                dbg::Assert(At(Tok::Ty::Num),
                        "Expected a number in the scale field of a memory reference, found: '{}'. "
                        "If you don't expect to use a scaled index, try ommiting the brackets like so:\n"
                        "mov(b64 rax, @b64 [rax] + 0xdeadbeef);",
                        lxr.tok.str);

                // FIXME: check if the parsed number is a valid memory ref scale.
                scale.emplace(core::Mem_Ref::Scale(StringToI64(lxr.tok.str).value()));
                Consume(Tok::Ty::Num);

                dbg::Assert(Consume(Tok::Ty::Rbracket), "Missing ']' after the scale field");

                dbg::Assert(Consume(Tok::Ty::Lbracket),
                        "Expected the index field to be present when "
                        "a scale is already defined");

                index_reg.emplace(core::Register(ParseX86Register(), core::Bit_Width::B64));
                dbg::Assert(Consume(Tok::Ty::Rbracket), "Missing ']' after the index field");
            }

            // Optional displacement.
            if (At(Tok::Ty::Plus, Tok::Ty::Minus)) {
                Opt<i64> disp = [&] {
                    std::string buf;
                    buf += lxr.tok.str;

                    Consume(Tok::Ty::Plus, Tok::Ty::Minus);
                    dbg::Assert(At(Tok::Ty::Num), "Expected a number after a '+' or '-' sign");

                    buf += lxr.tok.str;
                    Consume(Tok::Ty::Num);

                    return StringToI64(buf);
                }();

                dbg::Assert(disp.has_value(), "Invalid number used a displacement");
                mem_ref_disp.emplace(disp.value());
            }

        // ->Kind::Disp_Only: @b[x] 0x11223344
        } else {
            dbg::Assert(At(Tok::Ty::Num), "Expected a displacement after the operand size.");

            Opt<i64> disp = StringToI64(lxr.tok.str);
            dbg::Assert(disp.has_value(), "Invalid number used a displacement");

            mem_ref_disp.emplace(disp.value());
        }

        core::Mem_Ref::Kind mem_ref_kind = [&] {
            using Kind = core::Mem_Ref::Kind;

            if (base_reg.has_value() and index_reg.has_value()) {
                return Kind::Base_Index_Maybe_Disp;
            }

            if (not base_reg.has_value() and index_reg.has_value()) {
                return Kind::Index_Maybe_Disp;
            }

            if (base_reg.has_value() and not index_reg.has_value()) {
                return Kind::Base_Maybe_Disp;
            }
            return Kind::Disp_Only;
        }();

        return core::Operand(
            core::Mem_Ref(
                mem_ref_kind,
                base_reg,
                scale,
                index_reg,
                mem_ref_disp,
                operand_size
            )
        );
    }
    // Imm:
    // -> 0x11223344
    case Tok::Ty::Num: {
        Opt<i64> imm = StringToI64(lxr.tok.str);
        Consume(Tok::Ty::Num);
        dbg::Assert(imm.has_value(), "Invalid number used as an immediate");

        return core::Operand(core::Imm(imm.value()));
    }

    default: 
       dbg::Unreachable("Token '{}' does not start a valid x86 operand", lxr.tok.str);
    } // switch
}

}  // namespace fiska::syntax
