#ifndef __X86_ASSEMBLER_LIB_CORE_HH__
#define __X86_ASSEMBLER_LIB_CORE_HH__

#include <vector>
#include <variant>

#include "lib/utils.hh"

namespace fiska::core {

enum struct Bit_Width : i32 {
    Invalid = -1,

    B8 = 8,
    B16 = 16,
    B32 = 32,
    B64 = 64
};

enum struct Patch_Opcode : bool {
    No = 0,
    Yes = 1,
};

union Rex {
    struct {
        // Mod_Rm::r/m or Sib::Base extension.
        u8 b: 1;
        // Sib::Index extension.
        u8 x: 1;
        // Mod_Rm::reg extension.
        u8 r: 1;
        // Operand size override.
        u8 w: 1;
        u8 mod: 4 {0b0100}; 
    };
    u8 raw;

    auto IsRequired() const -> bool { return b or x or r or w; }
};

struct Register {
    enum struct Id : i32 {
        Invalid = -1,

        Rax = 0, R8 = 8,
        Rcx = 1, R9 = 9,
        Rdx = 2, R10 = 10,
        Rbx = 3, R11 = 11,
        Rsp = 4, R12 = 12,
        Rbp = 5, R13 = 13,
        Rsi = 6, R14 = 14,
        Rdi = 7, R15 = 15,
        // The value 0x85 is intentionally selected to ensure that Rip.Index() returns 101.
        // This is needed when rip serves as a base in a memory reference.
        // Setting the top bit of the second byte to 1 helps distinguish it from other registers.
        Rip = 0x85,
    };

    Id id = Id::Invalid;
    Bit_Width size = Bit_Width::Invalid;

    auto Index() const -> u8 { return +id & 0x7; }

    auto RequiresExtension() const -> bool { return +id >= +Id::R8 and +id <= +Id::R15; }
};

struct Mem_Ref {
    enum struct Scale : u8 {
        Zero = 0,
        One = 1,
        Two = 2,
        Four = 4,
        Eight = 8
    };
    enum struct Kind {
        Invalid,

        Base_Maybe_Disp,
        Base_Index_Maybe_Disp,
        Index_Maybe_Disp,
        Disp_Only
    };

    Kind kind = Kind::Invalid;
    Opt<Scale> scale = std::nullopt;
    Opt<Register> base = std::nullopt;
    Opt<Register> index = std::nullopt;
    Opt<i64> disp = std::nullopt;
    Bit_Width size = Bit_Width::Invalid;
};

struct M_Offs {
    i64 inner{};

    M_Offs(i64 moffs) : inner(moffs) {}

    auto ToI64() const -> i64 { return inner; }
};
struct Imm {
    i64 inner{};

    Imm(i64 imm) : inner(imm) {}
    auto ToI64() const -> i64 { return inner; }
};

struct Operand {
    using Inner = std::variant<
        std::monostate,
        Register,
        Mem_Ref,
        M_Offs,
        Imm
    >;
    Inner inner{};

    Operand(Inner op) : inner(op) {}

    auto IsRegister() const -> bool { return std::holds_alternative<Register>(inner); }
    auto IsMemRef() const -> bool { return std::holds_alternative<Mem_Ref>(inner); }
    auto IsRegisterOrMemRef() const -> bool { return IsRegister() or IsMemRef(); }
    auto IsMoffs() const -> bool { return std::holds_alternative<M_Offs>(inner); }
    auto IsImm() const -> bool { return std::holds_alternative<Imm>(inner); }


    template <typename T>
    auto As() -> T& { return std::get<T>(inner); }

    template <typename T>
    auto As() const -> const T& { return std::get<T>(inner); }
};


struct Mod_Rm_Builder {
    static constexpr u8 kMod_Mem_Transfer = 0b00;
    static constexpr u8 kMod_Mem_Disp8_Transfer = 0b01;
    static constexpr u8 kMod_Mem_Disp32_Transfer = 0b10;
    static constexpr u8 kMod_Reg_Transfer = 0b11;

    static constexpr u8 kSib_Byte_Following = 0b100;
    static constexpr u8 kNo_Scaled_Index = 0b100;
    static constexpr u8 kNo_Base_Reg = 0b101;

    union Mod_Rm {
        struct {
            u8 rm: 3;
            u8 reg: 3;
            u8 mod: 2;
        };
        u8 raw;
    };

    union Sib {
        struct {
            u8 base: 3;
            u8 index: 3;
            u8 scale: 2;
        };
        u8 raw;
    };

    Mod_Rm mod_rm{};
    Opt<Sib> sib = std::nullopt;

    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    auto SetMod(const Mem_Ref& mem_ref) -> Mod_Rm_Builder& {
        mod_rm.mod = [&] {
            auto ModBasedOnDisp = [&](i64 disp) {
                return utils::FitsInU8(disp) 
                    ? kMod_Mem_Disp8_Transfer
                    : kMod_Mem_Disp32_Transfer;
            };

            switch (mem_ref.kind) {
            case Mem_Ref::Kind::Base_Maybe_Disp: {
                if (mem_ref.base.value().id == Register::Id::Rbp
                        or mem_ref.base.value().id == Register::Id::R13) 
                {
                    return ModBasedOnDisp(mem_ref.disp.value_or(0));
                }

                if (not mem_ref.disp.has_value()
                        or mem_ref.base.value().id == Register::Id::Rip)
                {
                    return kMod_Mem_Transfer;
                }
                return ModBasedOnDisp(mem_ref.disp.value());
            }
            case Mem_Ref::Kind::Base_Index_Maybe_Disp: {
                if (not mem_ref.disp.has_value()) {
                    return kMod_Mem_Transfer;
                }
                return ModBasedOnDisp(mem_ref.disp.value());
            }
            case Mem_Ref::Kind::Disp_Only: 
            case Mem_Ref::Kind::Index_Maybe_Disp: {
                return kMod_Mem_Transfer; 
            }
            case Mem_Ref::Kind::Invalid: {
                dbg::Unreachable();
            }
            } // switch
            dbg::Unreachable();
        }();
        return *this;
    }
    GCC_DIAG_IGNORE_POP();

    auto SetMod(const Operand& dst, const Operand& src) -> Mod_Rm_Builder& {
        dbg::Assert(dst.IsRegisterOrMemRef() and src.IsRegisterOrMemRef(),
                "|dst| or |src| are neither a Register nor a Mem_Ref");

        if (dst.IsRegister() and src.IsRegister()) {
            mod_rm.mod = kMod_Reg_Transfer;
            return *this;
        }

        return SetMod(dst.IsMemRef() ? dst.As<Mem_Ref>() : src.As<Mem_Ref>());
    }

    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    auto SetRmAndSib(const Operand& op) -> Mod_Rm_Builder& {
        dbg::Assert(op.IsRegisterOrMemRef(), 
                "Passing an operand other than a Register or a Mem_Ref to the Mod_Rm_Builder "
                "to set the r/m and sib fields.");

        if (op.IsRegister()) {
            mod_rm.rm = op.As<Register>().Index();

            return *this;
        }

        const Mem_Ref& mem_ref = op.As<Mem_Ref>();
        
        mod_rm.rm = [&] {
            switch (mem_ref.kind) {
            case Mem_Ref::Kind::Base_Maybe_Disp: {
                return mem_ref.base.value().id == Register::Id::Rsp
                    or mem_ref.base.value().id == Register::Id::R12
                    ? kSib_Byte_Following
                    : mem_ref.base.value().Index();
            }
            case Mem_Ref::Kind::Base_Index_Maybe_Disp:
            case Mem_Ref::Kind::Index_Maybe_Disp:
            case Mem_Ref::Kind::Disp_Only:
                return kSib_Byte_Following;
            
            case Mem_Ref::Kind::Invalid:
                dbg::Unreachable();
            } // switch

            dbg::Unreachable();
        }();

        // The trailing return type helps the compiler implicitely cast the
        // `std::nullopt` returned inside the lambda to the right optional type.
        // There is probably a better way of doing this, I'm sure.
        sib = [&] -> Opt<Sib> {
            switch (mem_ref.kind) {
            case Mem_Ref::Kind::Base_Maybe_Disp: {
                if (mem_ref.base.value().id == Register::Id::Rsp
                        or mem_ref.base.value().id == Register::Id::R12)
                {
                    return Sib {
                        .base = mem_ref.base.value().Index(),
                        .index = kNo_Scaled_Index,
                        .scale = +Mem_Ref::Scale::Zero
                    };
                }
                return std::nullopt;
            }
            case Mem_Ref::Kind::Base_Index_Maybe_Disp:
            case Mem_Ref::Kind::Index_Maybe_Disp:
            case Mem_Ref::Kind::Disp_Only: {
                return Sib {
                    .base = mem_ref.base.has_value() ? mem_ref.base->Index() : kNo_Base_Reg,
                    .index = mem_ref.index.has_value() ? mem_ref.index->Index() : kNo_Scaled_Index,
                    .scale = mem_ref.scale.has_value() ? +mem_ref.scale.value() : +Mem_Ref::Scale::Zero
                };
            }

            case Mem_Ref::Kind::Invalid:
                dbg::Unreachable();
            } // switch

            dbg::Unreachable();
        }();

        return *this;
    }
    GCC_DIAG_IGNORE_POP();

    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    auto SetReg(const Operand& op) -> Mod_Rm_Builder& {
        dbg::Assert(op.IsRegister(),
                "Passing an operand that is not a Register to the Mod_Rm_Builder "
                "to set the reg field");

        mod_rm.reg = op.As<Register>().Index();
        return *this;
    }
    GCC_DIAG_IGNORE_POP();

    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    auto SetReg(u8 value) -> Mod_Rm_Builder& {
        dbg::Assert(value <= 0b111,
                "Attempting to assign a number bigger than 0b111 to a 3-bit field in the Mod_Rm.");
        mod_rm.reg = value;
        return *this;
    }
    GCC_DIAG_IGNORE_POP();

    auto AsU8() const -> u8 { return mod_rm.raw; }
    
    // Function specifiying the of casting the Mod_Rm_Builder to a u8.
    operator u8() const { return mod_rm.raw; }
};

struct Assembler {
    Vec<u8> out;

    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    auto Emit8(u8 byte) -> void {
        out.push_back(byte);
    }

    auto Emit16(u16 word) -> void {
        out.push_back((word >> 0) & 0xff);
        out.push_back((word >> 8) & 0xff);
    }

    auto Emit32(u32 dword) -> void {
        out.push_back((dword >> 0) & 0xff);
        out.push_back((dword >> 8) & 0xff);
        out.push_back((dword >> 16) & 0xff);
        out.push_back((dword >> 24) & 0xff);
    }

    auto Emit64(u64 qword) -> void {
        out.push_back((qword >> 0) & 0xff);
        out.push_back((qword >> 8) & 0xff);
        out.push_back((qword >> 16) & 0xff);
        out.push_back((qword >> 24) & 0xff);
        out.push_back((qword >> 32) & 0xff);
        out.push_back((qword >> 40) & 0xff);
        out.push_back((qword >> 48) & 0xff);
        out.push_back((qword >> 56) & 0xff);
    }
    
    auto EmitSized(u64 qword, Bit_Width size) -> void {
        switch (size) {
        case Bit_Width::B8:
            Emit8(static_cast<u8>(qword));
            break;
        case Bit_Width::B16:
            Emit16(static_cast<u16>(qword));
            break;
        case Bit_Width::B32:
            Emit32(static_cast<u32>(qword));
            break;
        case Bit_Width::B64:
            Emit64(static_cast<u64>(qword));
            break;

        case Bit_Width::Invalid:
            dbg::Unreachable();
        } // switch
    }
    GCC_DIAG_IGNORE_POP();

    auto mov(const Operand& dst, const Operand& src) -> void {
        // MOV r/m, r
        if (dst.IsRegisterOrMemRef() and src.IsRegister()) {
            u8 op_code = 0x89 - (src.As<Register>().size == Bit_Width::B8);

            Rex rex {
                .b = dst.IsMemRef() 
                    ? dst.As<Mem_Ref>().base.has_value() and dst.As<Mem_Ref>().base->RequiresExtension()
                    : dst.As<Register>().RequiresExtension(),
                .x = dst.IsMemRef() and dst.As<Mem_Ref>().index.has_value() and dst.As<Mem_Ref>().index->RequiresExtension(),
                .r = src.As<Register>().RequiresExtension(),
                .w = src.As<Register>().size == Bit_Width::B64
            };

            auto mod_rm_builder = Mod_Rm_Builder()
                .SetMod(dst, src)
                .SetRmAndSib(dst)
                .SetReg(src);

            if (rex.IsRequired()) { Emit8(rex.raw); }
            Emit8(op_code);
            Emit8(mod_rm_builder.AsU8());
            if (mod_rm_builder.sib.has_value()) { Emit8(mod_rm_builder.sib.value().raw); }

            // FIXME: This logic is shared with the branch underneath. Try to refactor
            // this into a function.
            switch (u8(mod_rm_builder.mod_rm.mod)) {
            case Mod_Rm_Builder::kMod_Mem_Transfer: {
                const Mem_Ref& mem_ref = dst.As<Mem_Ref>();

                // If Rip is the base of a memory reference then it is required to have
                // a 32-bit displacement following the Mod_Rm and Sib byte.
                if (mem_ref.base.has_value() 
                        and mem_ref.base->id == Register::Id::Rip) {
                    Emit32(u32(mem_ref.disp.value_or(0)));
                }
                break;
            }
            case Mod_Rm_Builder::kMod_Mem_Disp8_Transfer: {
                Emit8(static_cast<u8>(dst.As<Mem_Ref>().disp.value_or(0)));
                break;
            }
            case Mod_Rm_Builder::kMod_Mem_Disp32_Transfer: {
                Emit32(static_cast<u32>(dst.As<Mem_Ref>().disp.value_or(0)));
                break;
            }
            case Mod_Rm_Builder::kMod_Reg_Transfer: {
                // No displacement to emit.
                break;
            }
            default:
                dbg::Unreachable("Encountered an unknown mod in the Mod_Rm byte: '{}'.", 
                        u8(mod_rm_builder.mod_rm.mod));
            } // switch
        }
        // MOV r, r/m
        else if (dst.IsRegister() and src.IsRegisterOrMemRef()) {
            u8 op_code = 0x8b - (dst.As<Register>().size == Bit_Width::B8);

            Rex rex {
                .b = src.IsMemRef() 
                    ? src.As<Mem_Ref>().base.has_value() and src.As<Mem_Ref>().base->RequiresExtension()
                    : src.As<Register>().RequiresExtension(),
                .x = src.IsMemRef() and src.As<Mem_Ref>().index.has_value() and src.As<Mem_Ref>().index->RequiresExtension(),
                .r = dst.As<Register>().RequiresExtension(),
                .w = dst.As<Register>().size == Bit_Width::B64
            };

            auto mod_rm_builder = Mod_Rm_Builder()
                .SetMod(dst, src)
                .SetRmAndSib(src)
                .SetReg(dst);

            if (rex.IsRequired()) { Emit8(rex.raw); }
            Emit8(op_code);
            Emit8(mod_rm_builder.AsU8());
            if (mod_rm_builder.sib.has_value()) { Emit8(mod_rm_builder.sib.value().raw); }

            switch (u8(mod_rm_builder.mod_rm.mod)) {
            case Mod_Rm_Builder::kMod_Mem_Transfer: {
                const Mem_Ref& mem_ref = src.As<Mem_Ref>();

                // If Rip is the base of a memory reference then it is required to have
                // a 32-bit displacement following the Mod_Rm and Sib byte.
                if (mem_ref.base.has_value() 
                        and mem_ref.base->id == Register::Id::Rip) {
                    Emit32(u32(mem_ref.disp.value_or(0)));
                }
                break;
            }
            case Mod_Rm_Builder::kMod_Mem_Disp8_Transfer: {
                Emit8(static_cast<u8>(src.As<Mem_Ref>().disp.value_or(0)));
                break;
            }
            case Mod_Rm_Builder::kMod_Mem_Disp32_Transfer: {
                Emit32(static_cast<u32>(src.As<Mem_Ref>().disp.value_or(0)));
                break;
            }
            case Mod_Rm_Builder::kMod_Reg_Transfer: {
                // No displacement to emit.
                break;
            }
            default:
                dbg::Unreachable("Encountered an unknown mod in the Mod_Rm byte: '{}'.", 
                        u8(mod_rm_builder.mod_rm.mod));
            } // switch

        }

        // MOV rax, moffs
        // MOV moffs, rax
        else if ((dst.IsRegister() and src.IsMoffs()) or (dst.IsMoffs() and src.IsRegister())) {
            const Register& reg = src.IsRegister() ? src.As<Register>() : dst.As<Register>();
            const M_Offs& moffs = src.IsMoffs() ? src.As<M_Offs>() : dst.As<M_Offs>();

            u8 op_code = [&] {
                if (dst.IsRegister()) {
                    return u8(0xa1 - (reg.size == Bit_Width::B8));
                }
                return u8(0xa3 - (reg.size == Bit_Width::B8));
            }();

            Rex rex {
                .b = 0,
                .x = 0,
                .r = 0,
                .w = reg.size == Bit_Width::B64
            };

            if (rex.IsRequired()) { Emit8(rex.raw); }
            Emit8(op_code);
            Emit64(u64(moffs.ToI64()));
        }

        // MOV r, imm
        else if (dst.IsRegister() and src.IsImm()) {
            u8 op_code = dst.As<Register>().size == Bit_Width::B8 ? 0xb0 : 0xb8;

            Rex rex {
                .b = dst.As<Register>().RequiresExtension(),
                .x = 0,
                .r = 0,
                .w = dst.As<Register>().size == Bit_Width::B64
            };

            // Implementation of the +rb part of the opcode.
            op_code |= dst.As<Register>().Index();

            if (rex.IsRequired()) { Emit8(rex.raw); }
            Emit8(op_code);
            EmitSized(u64(src.As<Imm>().ToI64()), dst.As<Register>().size);
        }

        // MOV r/m, imm
        else if (dst.IsMemRef() and src.IsImm()) {
            u8 op_code = 0xc7 - (dst.As<Mem_Ref>().size == Bit_Width::B8);

            const Mem_Ref& mem_ref = dst.As<Mem_Ref>();

            Rex rex {
                .b = mem_ref.base.has_value() and mem_ref.base->RequiresExtension(),
                .x = mem_ref.index.has_value() and mem_ref.index->RequiresExtension(),
                .r = 0,
                .w = mem_ref.size == Bit_Width::B64
            };

            auto mod_rm_builder = Mod_Rm_Builder()
                .SetMod(mem_ref)
                .SetRmAndSib(dst)
                .SetReg(0);

            if (rex.IsRequired()) { Emit8(rex.raw); }
            Emit8(op_code);
            Emit8(mod_rm_builder.AsU8());
            if (mod_rm_builder.sib.has_value()) { Emit8(mod_rm_builder.sib.value().raw); }
            EmitSized(u64(src.As<Imm>().ToI64()), dst.As<Mem_Ref>().size);
        }
        else {
            dbg::Assert(false, "Unreachable!");
        }
    }
};


}  // namespace fiska::core

#endif  // __X86_ASSEMBLER_LIB_CORE_HH__
