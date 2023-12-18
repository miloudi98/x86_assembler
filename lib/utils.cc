#include <string>
#include <execinfo.h>
#include <limits>

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
