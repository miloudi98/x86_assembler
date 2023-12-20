#include "fmt/format.h"
#include "lib/utils.hh"
#include "lib/parser.hh"
#include "lib/file_manager.hh"

using fiska::syntax::Parser;
using fiska::fsm::File_Manager;

constexpr char kFile_Name[] = "/home/jawad/fiska/dev/x86_assembler/tests/syntax/mov.fska";

auto main(i32 argc, char* argv[]) -> i32 {
    File_Manager fm{};

    u16 fid = fm.LoadFile(fs::path(kFile_Name));
    fmt::print("File id = {}\n", fid);
    fmt::print("===== File Content ====\n");
    fmt::print("{}\n", fm.ViewFileContent(fid));

    fmt::print("======= Parser output =====\n");
    Parser parser{fm.GetFile(fid)};

    parser.ParseProcExpr();
    delete parser.mod;
    return 0;
}
