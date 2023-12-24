#ifndef __X86_ASSEMBLER_LIB_UTILS_HH__
#define __X86_ASSEMBLER_LIB_UTILS_HH__

#include <vector>
#include <memory>
#include <algorithm>
#include <ranges>
#include <string>
#include <utility>
#include <optional>
#include <span>
#include <source_location>
#include <string_view>
#include <concepts>
#include <filesystem>
#include <unordered_map>
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

namespace fs = std::filesystem;
namespace vws = std::views;
namespace rgs = std::ranges;

// Useful concepts
template <typename T, typename... OtherTs>
concept IsAnyOf = (std::same_as<T, OtherTs> or ...);

namespace dbg {

auto PrintStackTrace() -> void;

[[noreturn]] auto AssertionHelper(
        std::string err_msg,
        std::source_location loc) -> void;


// Assertion helpers
template <typename... Args>
struct Assert {
    explicit Assert(bool assertion,
            std::source_location loc = std::source_location::current())
    {
        Assert(assertion, "", loc);
    }

    // Having an explicit multi-argument constructor disallows 
    // copy-initialization using an initializer list.
    //
    // Assert<Args...> assert = { ... } // won't compile.
    //
    // This will probably never happen in practice because that's not how 
    // this type is intended to be used, but we'll keep the explicit for now.
    explicit Assert(bool assertion,
            fmt::format_string<Args...> fmt,
            Args&&... args,
            std::source_location loc = std::source_location::current())
    {
        // Assertion holds. Do nothing.
        if (assertion) return;
        AssertionHelper(fmt::format(fmt, std::forward<Args>(args)...), loc);
    }
};

template <typename... Args>
struct Unreachable {
    [[noreturn]] explicit Unreachable(
            std::source_location loc = std::source_location::current())
    {
        Unreachable("Encountered an unreachable state!", loc);
    }

    [[noreturn]] explicit Unreachable(
            fmt::format_string<Args...> fmt,
            Args&&... args,
            std::source_location loc = std::source_location::current())
    {
        AssertionHelper(fmt::format(fmt, std::forward<Args>(args)...), loc);
    }
};

// Deduction guide.
// TODO(miloudi): Explain why we need this deduction guide.
template <typename... Args>
Assert(bool assertion, fmt::format_string<Args...> fmt, Args&&... args) -> Assert<Args...>;

template <typename... Args>
Unreachable(fmt::format_string<Args...> fmt, Args&&... args) -> Unreachable<Args...>;

}  // namespace dbg

template <typename E> 
requires std::is_enum_v<E>
constexpr auto operator+(E e) -> std::underlying_type_t<E> {
    return std::to_underlying(e);
}

namespace utils {

// ============================================================================
// StringMap utility type.
//
// This is basically an std::unordered_map<std::string, T> but with the 
// additional property of heterogeneous lookups. Lookups can be made using
// any type that is convertible to a string_view. One important thing to note
// here is that the type with which the lookup is made must be comparable to 
// a string. Something like std::span<const char> will not work!
// A vanilla std::unordered_map doesn't allow this kind of behaviour by default.
// ============================================================================
struct StringHash {
    using is_transparent = void;

    [[nodiscard]] auto operator()(std::string_view data) const { return std::hash<std::string_view>{}(data); }

    [[nodiscard]] auto operator()(const std::string& data) const { return std::hash<std::string>{}(data); }

    template <typename Ty>
    requires requires(const Ty& ty) {
        { ty.size() } -> std::convertible_to<usz>;
        { ty.data() } -> std::convertible_to<const char*>;
    }
    [[nodiscard]] auto operator()(const Ty& ty) const { 
        return std::hash<std::string_view>{}(std::string_view{ty.data(), ty.size()});
    }
};

template <typename T>
using StringMap = std::unordered_map<std::string, T, StringHash, std::equal_to<>>;

auto FitsInU8(i64 num) -> bool;
auto FitsInU32(i64 num) -> bool;

auto LoadFile(const fs::path& path) -> Vec<char>;

}  // namespace utils

// Helper macro to concatenate strings inside a _Pragma call.
#define FISKA_PRAGMA_HELPER(x) _Pragma (#x)

#define GCC_DIAG_IGNORE_PUSH(warning) \
    _Pragma("GCC diagnostic push") \
    FISKA_PRAGMA_HELPER(GCC diagnostic ignored #warning) \

#define GCC_DIAG_IGNORE_POP() \
    _Pragma("GCC diagnostic pop")

#endif  // __X86_ASSEMBLER_LIB_UTILS_HH__
