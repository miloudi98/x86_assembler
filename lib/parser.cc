#include <string_view>

#include "lib/utils.hh"
#include "lib/parser.hh"

namespace fiska::syntax {

namespace {

const utils::StringMap<X86_Instruction::Ty> x86_mnemonics = {
    {"mov", X86_Instruction::Ty::Mov},
};

const utils::StringMap<core::Operand_Size> bit_widths = {
    {"b8", core::Operand_Size::B8},
    {"b16", core::Operand_Size::B16},
    {"b32", core::Operand_Size::B32},
    {"b64", core::Operand_Size::B64},
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


// Credit to llvm: https://llvm.org/doxygen/StringRef_8cpp_source.html
auto StringToI64(std::string_view str) -> Opt<i64> {
    bool is_negative = str.starts_with('-');
    str.remove_prefix(1);

    u8 radix = str.starts_with("0x") ? 16 : 10;
    if (radix == 16) { str.remove_prefix(2); }

    auto curr_str = str;

    u64 ret = 0;

    while (not curr_str.empty()) {
        u8 ord = 0;

        if (curr_str[0] >= '0' and curr_str[0] <= '9') {
            ord = u8(curr_str[0] - '0');
        } else if (curr_str[0] >= 'a' and curr_str[0] <= 'z') {
            ord = u8(curr_str[0] - 'a' + 10);
        } else if (curr_str[0] >= 'A' and curr_str[0] <= 'Z') {
            ord = u8(curr_str[0] - 'A' + 10);
        } else {
            return std::nullopt;
        }

        if (ord >= radix) {
            return std::nullopt;
        }

        u64 old_ret = ret;
        ret = ret * radix + ord;

        // overflow detected.
        if ((ret / radix) < old_ret) {
            return std::nullopt;
        }

        curr_str.remove_prefix(1);
    }

    // check if negative number is not too large to 
    // fit in an i64.
    if (is_negative and static_cast<i64>(-ret) > 0) {
        return std::nullopt;
    }

    return static_cast<i64>(ret);
}

constexpr auto IsValidMemRefScale(i64 scale) -> bool {
    return (scale >= 0) and (scale & (scale - 1)) == 0 and scale <= 8;
}

}  // namespace

Module::~Module() {
    for (Expr* e : exprs) { delete e; }
    for (X86_Instruction* instr : x86_instrs) { delete instr; }
}

void* Expr::operator new(usz sz, Module* mod) {
    auto ptr = static_cast<Expr*>(::operator new(sz));
    mod->exprs.push_back(ptr);
    return ptr;
}

void* X86_Instruction::operator new(usz sz, Module* mod) {
    auto ptr = static_cast<X86_Instruction*>(::operator new(sz));
    mod->x86_instrs.push_back(ptr);
    return ptr;
}

auto Parser::ParseProcDecl() -> ProcDecl* {
    dbg::Assert(Consume(Tok::Ty::Fn));
    dbg::Assert(At(Tok::Ty::Ident));

    std::string name = lxr.tok.str;
    Consume(Tok::Ty::Ident);

    auto block_expr = ParseBlockExpr();

    return new (mod) ProcDecl(std::move(name), block_expr);
}

auto Parser::ParseBlockExpr() -> BlockExpr* {
    dbg::Assert(Consume(Tok::Ty::Lbrace));

    Vec<X86_Instruction*> x86_instrs;
    while (At(Tok::Ty::Ident)) {
        x86_instrs.push_back(ParseX86Instruction());
    }

    dbg::Assert(Consume(Tok::Ty::Rbrace));

    return new (mod) BlockExpr(x86_instrs);
}

auto Parser::ParseX86Instruction() -> X86_Instruction* {
    using Ty = X86_Instruction::Ty;

    dbg::Assert(At(Tok::Ty::Ident));
    dbg::Assert(x86_mnemonics.contains(lxr.tok.str));
    auto mnemonic = x86_mnemonics.find(lxr.tok.str)->second;
    Consume(Tok::Ty::Ident);

    switch (mnemonic) {
    case Ty::Mov: {
        dbg::Assert(Consume(Tok::Ty::Lparen));

        auto dst = ParseX86Operand();

        dbg::Assert(Consume(Tok::Ty::Comma));
        auto src = ParseX86Operand();

        dbg::Assert(Consume(Tok::Ty::Rparen));
        dbg::Assert(Consume(Tok::Ty::SemiColon));

        return new (mod) Mov(dst, src);
    }

    case Ty::Invalid:
        dbg::Unreachable();
    } // switch

    dbg::Unreachable();
}

auto Parser::ParseX86Operand() -> core::Operand {
    auto ParseX86Register = [&]() {
        dbg::Assert(At(Tok::Ty::Register));

        dbg::Assert(x86_registers.contains(lxr.tok.str));

        auto reg_id = x86_registers.find(lxr.tok.str)->second;
        Consume(Tok::Ty::Register);
        return reg_id;
    };

    auto ParseBitWidth = [&]() {
        dbg::Assert(bit_widths.contains(lxr.tok.str));
        auto bit_width = bit_widths.find(lxr.tok.str)->second;
        Consume(Tok::Ty::Bsize);
        return bit_width;
    };

    switch (lxr.tok.ty) {
    // Register:
    // -> b[x] rax
    // x here represents the operand size.
    case Tok::Ty::Bsize: {
        auto bit_width = ParseBitWidth();
        auto reg_id = ParseX86Register();
        return core::Operand(core::Register(reg_id, bit_width));
    }

    // Mem_Ref: 
    // ->Kind::Base_Index_Maybe_Disp    @b[x] [rax][2][rbx] + 0x11223344
    // ->Kind::Base_Maybe_Disp          @b[x] [rax] + 0x11223344
    // ->Kind::Index_Maybe_Disp         @b[x] [][2][rbx] + 0x11223344
    // ->Kind::Disp_Only                @b[x] 0x11223344
    case Tok::Ty::At: {
        Consume(Tok::Ty::At);
        auto operand_size = ParseBitWidth();

        Opt<core::Register> base_reg{std::nullopt};
        Opt<core::Register> index_reg{std::nullopt};
        Opt<core::Mem_Ref::Scale> scale{std::nullopt};
        Opt<i64> mem_ref_disp{std::nullopt};

        dbg::Assert(At(Tok::Ty::Lbracket, Tok::Ty::Num));

        if (At(Tok::Ty::Lbracket)) {
            Consume(Tok::Ty::Lbracket);
            dbg::Assert(At(Tok::Ty::Register, Tok::Ty::Rbracket));

            if (At(Tok::Ty::Register)) {
                // Always use 64-bit addressing. Support for 32-bit addressing may come 
                // in the future. I believe 32-bit addressing modes only require a certain
                // prefix added to the instruction.
                base_reg.emplace(core::Register(ParseX86Register(), core::Operand_Size::B64));
            }
            dbg::Assert(Consume(Tok::Ty::Rbracket));

            // Optional scale and index fields.
            if (At(Tok::Ty::Lbracket)) {
                Consume(Tok::Ty::Lbracket);
                dbg::Assert(At(Tok::Ty::Num));

                Opt<i64> parsed_scale = StringToI64(lxr.tok.str);
                dbg::Assert(parsed_scale and IsValidMemRefScale(*parsed_scale));
                scale.emplace(core::Mem_Ref::Scale(parsed_scale.value()));
                Consume(Tok::Ty::Num);

                dbg::Assert(Consume(Tok::Ty::Rbracket));
                dbg::Assert(Consume(Tok::Ty::Lbracket));

                index_reg.emplace(core::Register(ParseX86Register(), core::Operand_Size::B64));
                dbg::Assert(Consume(Tok::Ty::Rbracket));
            }

            // Optional displacement.
            if (At(Tok::Ty::Plus, Tok::Ty::Minus)) {
                Opt<i64> disp = [&] {
                    std::string buf;
                    buf += lxr.tok.str;

                    Consume(Tok::Ty::Plus, Tok::Ty::Minus);
                    dbg::Assert(At(Tok::Ty::Num));

                    buf += lxr.tok.str;
                    Consume(Tok::Ty::Num);

                    return StringToI64(buf);
                }();

                mem_ref_disp.emplace(disp.value());
            }
        // Displacement only. Must be a positive address.
        } else {
            dbg::Assert(At(Tok::Ty::Num));

            Opt<i64> disp = StringToI64(lxr.tok.str);
            mem_ref_disp.emplace(disp.value());
        }

        core::Mem_Ref::Kind mem_ref_kind = [&] {
            using Kind = core::Mem_Ref::Kind;

            if (base_reg and index_reg) {
                return Kind::Base_Index_Maybe_Disp;
            }

            if (not base_reg and index_reg) {
                return Kind::Index_Maybe_Disp;
            }

            if (base_reg and not index_reg) {
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
    case Tok::Ty::Plus:
    case Tok::Ty::Minus:
    case Tok::Ty::Num: {
        std::string buf;
        if (At(Tok::Ty::Plus, Tok::Ty::Minus)) {
            buf += lxr.tok.str;
            Consume(Tok::Ty::Plus, Tok::Ty::Minus);
        }
        dbg::Assert(At(Tok::Ty::Num));
        buf += lxr.tok.str;
        Consume(Tok::Ty::Num);

        Opt<i64> imm = StringToI64(buf);
        dbg::Assert(imm.has_value());
        return core::Operand(core::Imm(imm.value()));
    }

    default: 
       dbg::Unreachable("Token '{}' does not start a valid x86 operand", lxr.tok.str);
    } // switch
}

}  // namespace fiska::syntax
