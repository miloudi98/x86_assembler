#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "lib/core.hh"
#include "lib/utils.hh"

namespace fiska::core {
namespace assembler_test {

struct MovAssemblerTest : public ::testing::Test {
    Assembler as;

    auto Mov(const Operand::Inner& dst, const Operand::Inner& src) -> Vec<u8> {
        as.mov(dst, src);
        Vec<u8> bytes = std::move(as.out);
        as.out.clear();
        return bytes;
    }
};

constexpr Register rax {
    .id = Register::Id::Rax,
    .size = Bit_Width::B64
};
constexpr Register eax {
    .id = Register::Id::Rax,
    .size = Bit_Width::B32
};
constexpr Register al {
    .id = Register::Id::Rax,
    .size = Bit_Width::B8
};
constexpr Register rbx {
    .id = Register::Id::Rbx,
    .size = Bit_Width::B64
};
constexpr Register r8 {
    .id = Register::Id::R8,
    .size = Bit_Width::B64
};
constexpr Register r12 {
    .id = Register::Id::R12,
    .size = Bit_Width::B64
};
constexpr Register r8d {
    .id = Register::Id::R8,
    .size = Bit_Width::B32
};

TEST_F(MovAssemblerTest, Mov_Register_To_Register) {
    // The dissasembly is taken from https://defuse.ca/online-x86-assembler.htm.
    // This is simply a smoke test for now. We will write more involved tests once
    // we have a parser ready. The parser is needed so that we can convert our 
    // assembly to Gnu's assembly and then compare the output.
    EXPECT_THAT(Mov(rax, rbx), ::testing::ElementsAreArray({0x48, 0x89, 0xd8}));
    EXPECT_THAT(Mov(r8, r12), ::testing::ElementsAreArray({0x4d, 0x89, 0xe0}));
    EXPECT_THAT(Mov(eax, r8d), ::testing::ElementsAreArray({0x44, 0x89, 0xc0}));
    EXPECT_THAT(Mov(rbx, rax), ::testing::ElementsAreArray({0x48, 0x89, 0xc3}));
    EXPECT_THAT(Mov(r12, r8), ::testing::ElementsAreArray({0x4d, 0x89, 0xc4}));
    EXPECT_THAT(Mov(r8d, eax), ::testing::ElementsAreArray({0x41, 0x89, 0xc0}));
    EXPECT_THAT(Mov(rax, r12), ::testing::ElementsAreArray({0x4c, 0x89, 0xe0}));
    EXPECT_THAT(Mov(rbx, r8), ::testing::ElementsAreArray({0x4c, 0x89, 0xc3}));
}

TEST_F(MovAssemblerTest, Mov_Mem_Ref_To_Register) {
    Mem_Ref mem_ref_rsp {
        .kind = Mem_Ref::Kind::Base_Maybe_Disp,
        .base = static_cast<Opt<Register>>(Register::Id::Rsp),
        .size = Bit_Width::B64
    };
    Mem_Ref mem_ref_rbp {
        .kind = Mem_Ref::Kind::Base_Maybe_Disp,
        .base = static_cast<Opt<Register>>(Register::Id::Rbp),
        .size = Bit_Width::B64
    };
    Mem_Ref mem_ref_rsp_disp {
        .kind = Mem_Ref::Kind::Base_Maybe_Disp,
        .base = static_cast<Opt<Register>>(Register::Id::Rsp),
        .disp = 0x11223344,
        .size = Bit_Width::B64
    };
    Mem_Ref mem_ref_rsp_negative_disp32 {
        .kind = Mem_Ref::Kind::Base_Maybe_Disp,
        .base = static_cast<Opt<Register>>(Register::Id::Rsp),
        .disp = -0x11223344,
        .size = Bit_Width::B64
    };
    Mem_Ref mem_ref_rsp_negative_disp8 {
        .kind = Mem_Ref::Kind::Base_Maybe_Disp,
        .base = static_cast<Opt<Register>>(Register::Id::Rsp),
        .disp = -0x11,
        .size = Bit_Width::B64
    };

    Mem_Ref mem_ref_rip_base {
        .kind = Mem_Ref::Kind::Base_Maybe_Disp,
        .base = static_cast<Opt<Register>>(Register::Id::Rip),
        .disp = -0x11,
        .size = Bit_Width::B64
    };

    // The dissasembly is taken from https://defuse.ca/online-x86-assembler.htm.
    EXPECT_THAT(Mov(rbx, mem_ref_rsp), ::testing::ElementsAreArray({0x48, 0x8b, 0x1c, 0x24}));
    EXPECT_THAT(Mov(rbx, mem_ref_rbp), ::testing::ElementsAreArray({0x48, 0x8b, 0x5d, 0x00}));
    EXPECT_THAT(Mov(rbx, mem_ref_rsp_disp), ::testing::ElementsAreArray({0x48, 0x8b, 0x9c, 0x24, 0x44, 0x33, 0x22, 0x11}));
    EXPECT_THAT(Mov(rbx, mem_ref_rsp_negative_disp32), ::testing::ElementsAreArray({0x48, 0x8b, 0x9c, 0x24, 0xbc, 0xcc, 0xdd, 0xee}));
    EXPECT_THAT(Mov(rbx, mem_ref_rsp_negative_disp8), ::testing::ElementsAreArray({0x48, 0x8b, 0x5c, 0x24, 0xef}));
    EXPECT_THAT(Mov(rbx, mem_ref_rip_base), ::testing::ElementsAreArray({0x48, 0x8b, 0x1d, 0xef, 0xff, 0xff, 0xff}));
}

TEST_F(MovAssemblerTest, Mov_Imm_To_Reg) {
    // The dissasembly is taken from https://defuse.ca/online-x86-assembler.htm.
    EXPECT_THAT(Mov(rax, Imm(0x1122334455667788)), ::testing::ElementsAreArray({0x48, 0xb8, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11}));
    EXPECT_THAT(Mov(rax, Imm(-0x1122334455667788)), ::testing::ElementsAreArray({0x48, 0xb8, 0x78, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee}));
    EXPECT_THAT(Mov(rax, Imm(-0x1122334455667788)), ::testing::ElementsAreArray({0x48, 0xb8, 0x78, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee}));
    EXPECT_THAT(Mov(eax, Imm(0x11223344)), ::testing::ElementsAreArray({0xb8, 0x44, 0x33, 0x22, 0x11}));
}

TEST_F(MovAssemblerTest, Mov_Rax_Moffs) {
    // The dissasembly is taken from https://defuse.ca/online-x86-assembler.htm.
    EXPECT_THAT(Mov(rax, M_Offs(0x1122334455667788)), ::testing::ElementsAreArray({0x48, 0xa1, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11}));
    EXPECT_THAT(Mov(eax, M_Offs(0x1122334455667788)), ::testing::ElementsAreArray({0xa1, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11}));
    EXPECT_THAT(Mov(al, M_Offs(0x1122334455667788)), ::testing::ElementsAreArray({0xa0, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11}));
    EXPECT_THAT(Mov(M_Offs(0x1122334455667788), al), ::testing::ElementsAreArray({0xa2, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11}));

}

}  // namespace mov_testing 
}  // namespace fiska::core
