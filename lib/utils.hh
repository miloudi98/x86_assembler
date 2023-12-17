#ifndef __X86_ASSEMBLER_LIB_UTILS_HH__
#define __X86_ASSEMBLER_LIB_UTILS_HH__

#include <vector>
#include <memory>
#include <string>
#include <utility>
#include <optional>
#include <source_location>
#include <fmt/format.h>
#include <fmt/color.h>

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using f32 = float;
using f64 = double;

using usz = size_t;
using uptr = uintptr_t;
using isz = ptrdiff_t;
using iptr = intptr_t;

template <typename T>
using Rc = std::shared_ptr<T>;

template <typename T>
using Box = std::unique_ptr<T>;

template<typename T>
using Vec = std::vector<T>;

template <typename T>
using Opt = std::optional<T>;

namespace dbg {

auto PrintStackTrace() -> void;

// FIXME: Replace the Assertion machinery with a macro. It's cleaner
// that way.
// Assertion helpers
template <typename... Args>
struct Assert {
    explicit Assert(std::source_location loc,
            bool assertion,
            fmt::format_string<Args...> fmt,
            Args&&... args)
    {
        // TODO: think about this overload to implement something like
        // dbg::Unreachable();
    }
    explicit Assert(bool assertion,
            fmt::format_string<Args...> fmt,
            Args&&... args,
            std::source_location loc = std::source_location::current())
    {
        // Assertion holds. Do nothing.
        if (assertion) return;

        std::string out;
        std::string err_msg = fmt::format(fmt, std::forward<Args>(args)...);

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
};

// Deduction guide.
// TODO(miloudi): Explain why we need this deduction guide.
template <typename... Args>
Assert(bool assertion, fmt::format_string<Args...> fmt, Args&&... args) -> Assert<Args...>;

}  // namespace dbg

template <typename E> 
requires std::is_enum_v<E>
constexpr auto operator+(E e) -> std::underlying_type_t<E> {
    return std::to_underlying(e);
}

namespace utils {

auto FitsInU8(i64 num) -> bool;

}  // namespace utils

// Helper macro to concatenate strings inside a _Pragma call.
#define FISKA_PRAGMA_HELPER(x) _Pragma (#x)

#define GCC_DIAG_IGNORE_PUSH(warning) \
    _Pragma("GCC diagnostic push") \
    FISKA_PRAGMA_HELPER(GCC diagnostic ignored #warning) \

#define GCC_DIAG_IGNORE_POP() \
    _Pragma("GCC diagnostic pop")

#endif  // __X86_ASSEMBLER_LIB_UTILS_HH__
