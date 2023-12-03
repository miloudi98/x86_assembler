#ifndef __X86_ASSEMBLER_LIB_CORE_HH__
#define __X86_ASSEMBLER_LIB_CORE_HH__

#include <vector>
#include <variant>

#include "lib/utils.hh"

namespace fiska {
namespace core {

struct Assembler {
    Vec<u8> out;

    auto EmitU8(u8 byte) -> void {
        out.push_back(byte);
    }

    auto EmitU16(u16 word) -> void {
        out.push_back((word >> 0) & 0xff);
        out.push_back((word >> 8) & 0xff);
    }

    auto EmitU32(u32 dword) -> void {
        out.push_back((dword >> 0) & 0xff);
        out.push_back((dword >> 8) & 0xff);
        out.push_back((dword >> 16) & 0xff);
        out.push_back((dword >> 24) & 0xff);
    }

    auto EmitU64(u64 qword) -> void {
        out.push_back((qword >> 0) & 0xff);
        out.push_back((qword >> 8) & 0xff);
        out.push_back((qword >> 16) & 0xff);
        out.push_back((qword >> 24) & 0xff);
        out.push_back((qword >> 32) & 0xff);
        out.push_back((qword >> 40) & 0xff);
        out.push_back((qword >> 48) & 0xff);
        out.push_back((qword >> 54) & 0xff);
    }
};

enum struct Bit_Width : i32 {
    Invalid = -1,

    B8 = 8,
    B16 = 16,
    B32 = 32,
    B64 = 64
};

struct Rex {
    static constexpr u8 mod = (0b0100 << 4);
    static constexpr u8 w = (1 << 3);
    static constexpr u8 r = (1 << 2);
    static constexpr u8 x = (1 << 1);
    static constexpr u8 b = (1 << 0);

    u8 raw{ mod };

    // Override operand size to 64 bits.
    auto W() -> bool { return (raw & w) != 0; }
    // Extend the ModRM reg field.
    auto R() -> bool { return (raw & r) != 0; }
    // Extend the SIB index field.
    auto X() -> bool { return (raw & x) != 0; }
    // Extend either ModRM r/m field, SIB base field or Opcode reg field.
    auto B() -> bool { return (raw & b) != 0; }

    auto SetWIf(bool condition) -> void { raw |= condition * w; }
    auto SetRIf(bool condition) -> void { raw |= condition * r; }
    auto SetXIf(bool condition) -> void { raw |= condition * x; }
    auto SetBIf(bool condition) -> void { raw |= condition * b; }

    // It's possible that some instructions only require the MOD field 
    // set without the `R`, `W`, `X` or `B`. In that case we want |WasUpdated| to return true.
    // We'll update this logic to account for this edge case if needed.
    auto WasUpdated() -> bool {
        return raw != mod;
    }

    // Implicit conversion to u8.
    operator u8() const { return raw; } 
};

struct Mod_Rm {
    static constexpr u8 reg_shift_idx = 0;
    static constexpr u8 rm_shift_idx = 3;
    static constexpr u8 mod_shift_idx = 6;

    u8 raw{};

    auto SetRm(u8 value) -> void {
        dbg::Assert(value <= 0b111);
        raw |= value << rm_shift_idx;
    }
    auto SetReg(u8 value) -> void {
        dbg::Assert(value <= 0b111);
        raw |= value << reg_shift_idx;
    }

    auto SetMod(u8 value) -> void {
        dbg::Assert(value <= 0b11);
        raw |= value << mod_shift_idx;
    }

    operator u8() const { return raw; }
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
    };

    Id id = Id::Invalid;
    Bit_Width size = Bit_Width::Invalid;

    auto Index() -> u8 { return +id & 0x7; }

    auto RequiresExtension() -> bool {
        return +id >= +Id::R8 and +id <= +Id::R15;
    }

    auto UpdateRex(Rex& rex) -> void {
        rex.SetWIf(size == Bit_Width::B64);
        // First, try to set the R field of the Rex prefix.
        // If that's already set, this means that we have 2 register
        // operands and in that case, we set the B field next.
        if (not rex.R()) {
            rex.SetRIf(RequiresExtension());
        } else {
            rex.SetBIf(RequiresExtension());
        }
    }
};

struct Mem_Ref {
    enum struct Scale : i8 {
        Invalid = -1,
        
        One = 1,
        Two = 2,
        Four = 4,
        Eight = 8
    };
    enum struct Kind : i8 {
        Invalid = -1,

        Base_Only = 0, 
        Disp_Only = 1,
        SS_Index_Only = 2,
        Base_SS_Index = 3,
        Base_SS_Index_Disp = 4,
        Base_Disp = 5,
        SS_Index_Disp = 6
    };

    Kind kind = Kind::Invalid;
    Scale scale = Scale::Invalid;
    Opt<Register> base = std::nullopt;
    Opt<Register> index = std::nullopt;
    i64 dip{};
    Bit_Width size = Bit_Width::Invalid;

    auto UpdateRex(Rex& rex) -> void {
        rex.SetWIf(size == Bit_Width::B64);
        rex.SetBIf(base.has_value() and base->RequiresExtension());
        rex.SetXIf(index.has_value() and index->RequiresExtension());
    }
};

struct Operand {
    using M_Offs_Or_Imm = i64;
    using Inner = std::variant<
        std::monostate,
        Register,
        Mem_Ref,
        M_Offs_Or_Imm
    >;
    enum struct Kind {
        Register,
        Mem_Ref,
        M_Offs_Or_Imm,
    };

    Inner inner;

    auto GetKind() -> Kind {
        if (std::holds_alternative<Register>(inner)) { return Kind::Register; }
        if (std::holds_alternative<Mem_Ref>(inner)) { return Kind::Mem_Ref; }
        if (std::holds_alternative<M_Offs_Or_Imm>(inner)) { return Kind::M_Offs_Or_Imm; }

        dbg::Assert(false, "Unrecognized variant in member variable |inner|.");
    }

    auto IsRegister() -> bool { return GetKind() == Kind::Register; }
    auto IsMemRef() -> bool { return GetKind() == Kind::Mem_Ref; }
    auto IsMoffsOrImm() -> bool { return GetKind() == Kind::M_Offs_Or_Imm; }

    template <typename T>
    auto as() -> T& { return std::get<T>(inner); }

    auto UpdateRex(Rex& rex) -> void {
        switch (GetKind()) {
        case Kind::Register: {
            as<Register>().UpdateRex(rex);
            break;
        }
        case Kind::Mem_Ref: {
            as<Mem_Ref>().UpdateRex(rex);
            break;
        }
        case Kind::M_Offs_Or_Imm: {
            // Do nothing.
            break;
        }
        } // switch
    }
};

}  // namespace core
}  // namespace fiska

#endif  // __X86_ASSEMBLER_LIB_CORE_HH__
