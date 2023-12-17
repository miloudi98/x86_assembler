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
        // The value 0x85 was given so that Rip.Index() returns 101.
        // This is needed when rip is the base of a memory reference.
        // the top bit is set to 1 just to differentiate it from other registers.
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

struct Operand {
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
    using Inner = std::variant<
        std::monostate,
        Register,
        Mem_Ref,
        M_Offs,
        Imm
    >;
    Inner inner{};

    auto IsRegister() const -> bool { return std::holds_alternative<Register>(inner); }
    auto IsMemRef() const -> bool { return std::holds_alternative<Mem_Ref>(inner); }
    auto IsRegisterOrMemRef() const -> bool { return IsRegister() or IsMemRef(); }
    auto IsMoffs() const -> bool { return std::holds_alternative<M_Offs>(inner); }
    auto IsImm() const -> bool { return std::holds_alternative<Imm>(inner); }


    template <typename T>
    auto as() -> T& { return std::get<T>(inner); }

    template <typename T>
    auto as() const -> const T& { return std::get<T>(inner); }

};

struct Assembler {
    Vec<u8> out;

    auto EmitU8(u8 byte) -> void {
        out.push_back(byte);
    }

    auto EmitU16(u16 word) -> void {
        // u8(...) to fix Wconversion warning.
        out.push_back(u8((word >> 0) & 0xff));
        out.push_back(u8((word >> 8) & 0xff));
    }

    auto EmitU32(u32 dword) -> void {
        // u8(...) to fix Wconversion warning.
        out.push_back(u8((dword >> 0) & 0xff));
        out.push_back(u8((dword >> 8) & 0xff));
        out.push_back(u8((dword >> 16) & 0xff));
        out.push_back(u8((dword >> 24) & 0xff));
    }

    auto EmitU64(u64 qword) -> void {
        // u8(...) to fix Wconversion warning.
        out.push_back(u8((qword >> 0) & 0xff));
        out.push_back(u8((qword >> 8) & 0xff));
        out.push_back(u8((qword >> 16) & 0xff));
        out.push_back(u8((qword >> 24) & 0xff));
        out.push_back(u8((qword >> 32) & 0xff));
        out.push_back(u8((qword >> 40) & 0xff));
        out.push_back(u8((qword >> 48) & 0xff));
        out.push_back(u8((qword >> 54) & 0xff));
    }
};

union Rex {
    struct {
        u8 b: 1;
        u8 x: 1;
        u8 r: 1;
        u8 w: 1;
        u8 mod: 4 {0b0100}; 
    };
    u8 raw;
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
    auto SetMod(const Operand& dst, const Operand& src) -> Mod_Rm_Builder& {
        dbg::Assert(dst.IsRegisterOrMemRef() and src.IsRegisterOrMemRef(),
                "|dst| or |src| is neither a Register nor a Mem_Ref");

        if (dst.IsRegister() and src.IsRegister()) {
            mod_rm.mod = kMod_Reg_Transfer;
            return *this;
        }

        mod_rm.mod = [&] {
            const Mem_Ref& mem_ref = dst.IsMemRef() ? dst.as<Mem_Ref>() : src.as<Mem_Ref>();

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
                dbg::Assert(false, "Unreachable!");
            }

            } // switch
        }();
        return *this;
    }
    GCC_DIAG_IGNORE_POP();

    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    auto SetRmAndSib(const Operand& op) -> Mod_Rm_Builder& {
        dbg::Assert(op.IsRegisterOrMemRef(), 
                "Passing an operand other than a Register or a Mem_Ref to the Mod_Rm_Builder "
                "to set the r/m and sib fields.");

        if (op.IsRegister()) {
            mod_rm.rm = op.as<Register>().Index();

            return *this;
        }

        const Mem_Ref& mem_ref = op.as<Mem_Ref>();
        
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
                dbg::Assert(false, "Unreachable!");
            } // switch
        }();

        // The trailing return type helps the compiler implicitely cast the
        // `std::nullopt` returned inside the lambda to the right optional type.
        // There is probably a better way of doing this, I'm sure.
        sib = [&] -> Opt<Sib> {
            switch (mem_ref.kind) {
            case Mem_Ref::Kind::Base_Maybe_Disp: {
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
                dbg::Assert(false, "Unreachable!");
            } // switch
        }();

        return *this;
    }
    GCC_DIAG_IGNORE_POP();

    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    auto SetReg(const Operand& op) -> Mod_Rm_Builder& {
        dbg::Assert(op.IsRegister(),
                "Passing an operand that is not a Register to the Mod_Rm_Builder "
                "to set the reg field");

        mod_rm.reg = op.as<Register>().Index();
        return *this;
    }
    GCC_DIAG_IGNORE_POP();

    auto AsU8() const -> u8 { return mod_rm.raw; }
    
    // Function specifiying the of casting the Mod_Rm_Builder to a u8.
    operator u8() const { return mod_rm.raw; }
};

}  // namespace fiska::core

#endif  // __X86_ASSEMBLER_LIB_CORE_HH__
