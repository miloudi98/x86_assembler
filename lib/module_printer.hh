#ifndef __X86_ASSEMBLER_LIB_MODULE_PRINTER_HH__
#define __X86_ASSEMBLER_LIB_MODULE_PRINTER_HH__

#include "lib/utils.hh"
#include "lib/parser.hh"

namespace fiska::syntax::ast_printer {

namespace {

constexpr std::string_view bottom_left_corner = "\u2514\u2500";
constexpr std::string_view vertical_right_with_continuation = "\u251c\u2500";

auto GetTreePart(u32 indent_lvl, bool is_last_level) -> std::string {
    // Check for overflow.
    dbg::Assert(indent_lvl >= 1);
    std::string spaces((indent_lvl - 1) << 1, ' ');

    return fmt::format("{}{}", spaces,
            is_last_level 
            ? bottom_left_corner 
            : vertical_right_with_continuation);
}

}  // namespace

auto PrintOperand(const core::Operand& op) -> std::string {
    auto ColoredString = [&](std::string op, fmt::color color) {
        return fmt::format("{}", fmt::styled(op, fg(color)));
    };

    if (op.Is<core::Register>()) { return ColoredString("Register", fmt::color::green); }
    if (op.Is<core::M_Offs>()) { return ColoredString("M_Offs", fmt::color::green); }
    if (op.Is<core::M_Offs>()) { return ColoredString("Mem_Ref", fmt::color::green); }
    if (op.Is<core::Imm>()) { return ColoredString("Imm", fmt::color::green); }

    if (op.Is<core::Mem_Ref>()) {
        const auto& mem_ref = op.As<core::Mem_Ref>();

        std::string mem_ref_kind = [&] {
            using Kind = core::Mem_Ref::Kind;
            switch (mem_ref.kind) {
            case Kind::Invalid: return "Invalid";
            case Kind::Base_Maybe_Disp: return "Base_Maybe_Disp";
            case Kind::Base_Index_Maybe_Disp: return "Base_Index_Maybe_Disp";
            case Kind::Index_Maybe_Disp: return "Index_Maybe_Disp";
            case Kind::Disp_Only: return "Disp_Only";
            } // switch
            dbg::Unreachable();
        }();

        return fmt::format("{} {}",
                ColoredString("Mem_Ref", fmt::color::green),
                fmt::format(fg(fmt::color::cadet_blue), "<{}>", mem_ref_kind));
    }

    dbg::Unreachable();
}

void Print(X86_Instruction* instruction, u32 indent_lvl, std::string& out, bool is_last_level = true) {
    out += GetTreePart(indent_lvl, is_last_level);

    switch (instruction->ty) {
    case X86_Instruction::Ty::Invalid: {
        dbg::Unreachable("Encountered invalid instruction");
    }
    case X86_Instruction::Ty::Mov: {
        Mov* mov = static_cast<Mov*>(instruction);
        out += fmt::format("{}: {}  {}\n",
                fmt::styled("Mov", fg(fmt::color::blue)),
                PrintOperand(mov->dst),
                PrintOperand(mov->src));
        break;

    }
    } // switch
}

void Print(Expr* e, u32 indent_lvl, std::string& out, bool is_last_level = true) {
    out += GetTreePart(indent_lvl, is_last_level);

    switch (e->ty) {
    case Expr::Ty::Invalid: {
        dbg::Unreachable("Encountered an invalid expression");
    }
    case Expr::Ty::Proc: {
        ProcExpr* proc_expr = static_cast<ProcExpr*>(e);
        out += fmt::format("{} {}\n",
                fmt::styled("ProcDecl", fg(fmt::color::red)),
                proc_expr->name);

        Print(proc_expr->body, indent_lvl + 1, out);
        break;
    }
    case Expr::Ty::Block: {
        out += fmt::format("{}\n", fmt::styled("BlockExpr", fg(fmt::color::red)));

        BlockExpr* block = static_cast<BlockExpr*>(e);

        for (u32 instr_idx = 0; instr_idx < block->instructions.size(); ++instr_idx) {
            X86_Instruction* instr = block->instructions[instr_idx];
            Print(instr, indent_lvl + 1, out,
                    instr_idx == block->instructions.size() - 1);
        }
        break;
    }
    } // switch
}

void Print(Module* mod) {
    std::string out;

    out += fmt::format("{}\n",
            fmt::styled("Module", fg(fmt::color::red)));

    for (Expr* e : mod->exprs) {
        Print(e, 0, out);
    }

    fmt::print("{}\n", out);
}

}  // namespace fiska::syntax::printer

#endif  // __X86_ASSEMBLER_LIB_MODULE_PRINTER_HH__
