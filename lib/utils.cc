#include <string>
#include <execinfo.h>
#include <limits>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "lib/utils.hh"

auto dbg::PrintStackTrace() -> void {
    constexpr u8 max_stack_frames = 64; 
    void *buffer[max_stack_frames];

    int stack_frames = backtrace(buffer, max_stack_frames);
    char **strings = backtrace_symbols(buffer, stack_frames);

    if (strings == NULL) {
        fmt::print("Failed to print the stack trace! Exiting...\n");
        std::exit(1);
    }

    for (i32 idx = 0; idx < stack_frames; ++idx) {
        std::string func_call = strings[idx];

        // Skip non relevant function calls.
        if (func_call.find("libasan.so") != std::string::npos) continue;
        if (func_call.find("PrintStackTrace") != std::string::npos) continue;
        if (func_call.find("Assert") != std::string::npos) continue;

        fmt::print("--> {}\n", func_call);
    }

    free(strings);
}

auto dbg::AssertionHelper(std::string err_msg, std::source_location loc) -> void {
    std::string out;

    out += "\n=============================================\n";
    out += fmt::format("{}: {}\n",
            fmt::styled("Assertion Failed", fmt::emphasis::bold | fg(fmt::color::red)),
            err_msg);

    out += fmt::format("{} @ {}\n",
        fmt::format(fmt::emphasis::bold | fg(fmt::color::cyan)| fmt::emphasis::underline,
                "-->{}:{}:{}", loc.file_name(), loc.line(), loc.column()),
        loc.function_name());
    
    fmt::print("{}\n", out);

    // Print the backtrace
    out += fmt::format(fmt::emphasis::bold | fg(fmt::color::cyan) | fmt::emphasis::underline,
            "Backtrace:\n");

    PrintStackTrace();

    std::exit(1);
}

auto utils::FitsInU8(i64 num) -> bool {
    return num >= -128 and num <= 127;
}

auto utils::FitsInU32(i64 num) -> bool {
    return num >= std::numeric_limits<i32>::min()
        and num <= std::numeric_limits<i32>::max();
}

auto utils::LoadFile(const fs::path& path) -> Vec<char> {
    i32 fd = open(path.c_str(), O_RDONLY);
    dbg::Assert(fd >= 0,
            "Failed to open file '{}'", path.string());

    struct stat file_st{};

    dbg::Assert(fstat(fd, &file_st) >= 0,
            "Failed to query stats of file: '{}'", path.string());

    void* ptr = mmap(nullptr, usz(file_st.st_size), PROT_READ, MAP_PRIVATE, fd, 0);
    dbg::Assert(ptr != MAP_FAILED, "Failed to mmap file: '{}'", path.string());

    // QUESTION: do we really need to copy the contents of the map to a vector?
    // Can't we reuse the same memory? 
    // IOW, return a std::vector<char> {.data = ptr, .size = file_st.st_size }
    // instead of copying the entire mapped memory chunk.
    Vec<char> content(usz(file_st.st_size));
    memcpy(content.data(), ptr, usz(file_st.st_size));

    // TODO: Implement defer macro.
    close(fd);
    munmap(ptr, static_cast<usz>(file_st.st_size));

    return content;
}
