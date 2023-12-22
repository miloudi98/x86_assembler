#ifndef __X86_ASSEMBLER_LIB_MODULE_PRINTER_HH__
#define __X86_ASSEMBLER_LIB_MODULE_PRINTER_HH__

#include "lib/utils.hh"
#include "lib/parser.hh"

namespace fiska::syntax::ast_printer {

namespace {

constexpr std::string_view bottom_left_corner = "\u2514\u2500";
constexpr std::string_view bottom_left_corner_with_cont = "\u251c\u2500";
constexpr std::string_view vertical_line = "\u2502";

}  // namespace

struct Module_Printer {
    std::string out{};

    auto GetIndentationFormat(
            u32 indentation_level,
            Vec<bool>& must_continue_vertical_line,
            bool is_last_level) -> std::string
    {
        std::string ret;
        // Check for overflow.
        dbg::Assert(indentation_level >= 1);
        u32 num_spaces = (indentation_level - 1) << 1;

        dbg::Assert(must_continue_vertical_line.size() <= num_spaces);
        for (u32 idx = 0; idx < num_spaces; ++idx) {
            ret += must_continue_vertical_line[idx]
                ? vertical_line
                : " ";
        }
        
        // If it's a bottom corner we don't want to continue drawing the 
        // parent's vertical line. We do if it's a bottom corner with 
        // a continuation.
        must_continue_vertical_line.push_back(not is_last_level);
        // The bottom_left_corner(_cont) contains 2 characters.
        // The second character is a simple horizontal bar. The
        // reason we push_back false is to avoid drawing vertical lines
        // underneath it.
        must_continue_vertical_line.push_back(false);

        return fmt::format("{}{}", ret,
                is_last_level
                ? bottom_left_corner
                : bottom_left_corner_with_cont);
    }

    void Print(
            X86_Instruction* instr,
            u32 indentation_level,
            Vec<bool> must_continue_vertical_line,
            bool is_last_level = true)
    {
        out += GetIndentationFormat(indentation_level,
                must_continue_vertical_line,
                is_last_level);
        switch (instr->ty) {
        case X86_Instruction::Ty::Mov: {
            Mov* mov = static_cast<Mov*>(instr);
            out += fmt::format("{}: {}  {}\n",
                    fmt::styled("Mov", fg(fmt::color::blue)),
                    PrintOperand(mov->dst),
                    PrintOperand(mov->src));
            break;

        }
        case X86_Instruction::Ty::Invalid: {
            dbg::Unreachable("Encountered invalid instruction");
        }
        } // switch
    }

    void Print(
            Expr* e,
            u32 indentation_level,
            Vec<bool> must_continue_vertical_line,
            bool is_last_level = true
            )
    {
        out += GetIndentationFormat(indentation_level,
                must_continue_vertical_line,
                is_last_level);

        switch (e->ty) {
        case Expr::Ty::Proc: {
            ProcExpr* proc_expr = static_cast<ProcExpr*>(e);
            out += fmt::format("{} {}\n",
                    fmt::styled("ProcDecl", fg(fmt::color::red)),
                    fmt::format(fg(fmt::color::cadet_blue), proc_expr->name));

            Print(proc_expr->body, indentation_level + 1, must_continue_vertical_line);

            break;
        }
        case Expr::Ty::Block: {
            out += fmt::format("{}\n", fmt::styled("BlockExpr", fg(fmt::color::red)));

            BlockExpr* block = static_cast<BlockExpr*>(e);

            for (u32 instr_idx = 0; instr_idx < block->instructions.size(); ++instr_idx) {
                X86_Instruction* instr = block->instructions[instr_idx];
                Print(instr,
                        indentation_level + 1,
                        must_continue_vertical_line,
                        instr_idx == block->instructions.size() - 1);

            }
            break;
        }
        case Expr::Ty::Invalid: {
            dbg::Unreachable("Encountered an invalid expression");
        }
        } // switch
    }

    void PrintModuleHelper(Module* mod) {
        out += fmt::format("{}\n", fmt::styled("Module", fg(fmt::color::red)));

        Vec<bool> must_continue_vertical_line;
        auto IsTopLevel = [](const Expr* e) { return e->ty == Expr::Ty::Proc; };


        i64 num_top_level_exprs = rgs::count_if(mod->exprs, IsTopLevel);
        i64 expr_idx = 0;
        for (Expr* expr : mod->exprs | vws::filter(IsTopLevel)) {
            Print(expr,
                    /*indentation_level=*/1,
                    must_continue_vertical_line,
                    expr_idx == num_top_level_exprs - 1);
            ++expr_idx;
        }

        fmt::print("{}\n", out);
    }

    static auto PrintOperand(const core::Operand& op) -> std::string {
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

    static auto PrintModule(Module* mod) {
        Module_Printer printer{};
        printer.PrintModuleHelper(mod);
    }

};

}  // namespace fiska::syntax::printer

#endif  // __X86_ASSEMBLER_LIB_MODULE_PRINTER_HH__
