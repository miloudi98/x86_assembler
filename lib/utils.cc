#include <string>
#include <execinfo.h>

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
