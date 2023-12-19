#include "lib/utils.hh"
#include "lib/lexer.hh"

namespace fiska::syntax {

// Symbols declared whithin an anonymous namespace are confined to the translation unit
// of that namespace, much like the `static` keyword in C.
// This feature prevents ODR violations. It allows the creation of identically named
// free functions across multiple translation units within anonymous namespaces without
// encountering conflicts during the link process.
// Source: https://stackoverflow.com/questions/357404/why-are-unnamed-namespaces-used-and-what-are-their-benefits
namespace {

// Below are the characters considered to be whitespace.
// \n: New line.
// \t: Tab.
// \r: Carriage return.
// \f: Form feed.
// \v: vertical tab.
constexpr auto IsWhiteSpace(char c) -> bool {
    return c == '\n' or c == '\t'
        or c == '\f' or c == '\a'
        or c == '\r' or c == '\v'
        or c == '\b';
}

constexpr auto IsDigit(char c) -> bool {
    return c >= '0' and c <= '9';
}

constexpr auto IsHexDigit(char c) -> bool {
    return IsDigit(c) or (c >= 'a' and c <= 'f') 
        or (c >= 'A' and c <= 'F');
}

constexpr auto IsIdentStart(char c) -> bool {
    return std::isalpha(c) or c == '_';
}

constexpr auto IsIdentCont(char c) -> bool {
    return std::isalpha(c) or IsDigit(c);
}

const ::utils::StringMap<Tok::Ty> keywords = {
    {"fn", Tok::Ty::Fn},
    {"b8", Tok::Ty::Bsize},
    {"b16", Tok::Ty::Bsize},
    {"b32", Tok::Ty::Bsize},
    {"b64", Tok::Ty::Bsize},
    {"rax", Tok::Ty::Register},
    {"rcx", Tok::Ty::Register},
    {"rdx", Tok::Ty::Register},
    {"rbx", Tok::Ty::Register},
    {"rsp", Tok::Ty::Register},
    {"rbp", Tok::Ty::Register},
    {"rsi", Tok::Ty::Register},
    {"rdi", Tok::Ty::Register},
    {"rip", Tok::Ty::Register},
    {"r8", Tok::Ty::Register},
    {"r9", Tok::Ty::Register},
    {"r10", Tok::Ty::Register},
    {"r11", Tok::Ty::Register},
    {"r12", Tok::Ty::Register},
    {"r13", Tok::Ty::Register},
    {"r14", Tok::Ty::Register},
    {"r15", Tok::Ty::Register},
};

}  // namespace

Lexer::Lexer(fsm::File& file) : chars(file.Content()), fid(file.fid) {
    // Initialize member variable |tok|.
    NextTok();
}

auto Lexer::NextChar(bool ignore_whitespace) -> Opt<char> {
    if (IsEof()) { return std::nullopt; }

    // FIXME: Ignoring whitespace here is probably a mistake that can lead
    // to some hard to find bugs. Have an explicit function that ignores
    // whitespace instead. Once that's done, remove functions |NextCharRaw|
    // and |PeekCharRaw| as they will become useless.
    while (not ignore_whitespace and not IsEof() and IsWhiteSpace(chars[foffset++])) {}

    return NextChar();
}

auto Lexer::PeekChar(u32 n, bool ignore_whitespace) -> Opt<char> {
    if (IsEof()) { return std::nullopt; }

    // Store the current offset.
    u64 old_offset = foffset;

    Opt<char> c = std::nullopt;
    while (n--) { c = NextChar(ignore_whitespace); }

    // Restore the foffset altered by the consecutive 
    // calls to |NextChar|.
    foffset = old_offset;
    return c;
}

auto Lexer::NextTok() -> void {
    tok.loc.offset = foffset;

    Opt<char> c = NextChar();
    if (not c.has_value()) {
        tok.ty = Tok::Ty::Eof;
        return;
    }


    switch (*c) {
    case ' ': {
        // This is probably dangerous. A long sequence of whitespaces 
        // may overflow the stack! Maybe TCO will come to the rescue?
        NextTok();
        break;
    }
    case ',': {
        tok.ty = Tok::Ty::Comma;
        break;
    }
    case ':': {
        tok.ty = Tok::Ty::Colon;
        break;
    }
    case '(': {
        tok.ty = Tok::Ty::Lparen;
        break;
    }
    case ')': {
        tok.ty = Tok::Ty::Rparen;
        break;
    }
    case '{': {
        tok.ty = Tok::Ty::Lbrace;
        break;
    }
    case '}': {
        tok.ty = Tok::Ty::Rbrace;
        break;
    }
    case '[': {
        tok.ty = Tok::Ty::Lbracket;
        break;
    }
    case ']': {
        tok.ty = Tok::Ty::Rbracket;
        break;
    }
    case '@': {
        tok.ty = Tok::Ty::At;
        break;
    }
    case '+': {
        tok.ty = Tok::Ty::Plus;
        break;
    }
    case '-': {
        tok.ty = Tok::Ty::Minus;
        break;
    }
    case '/': {
        dbg::Assert(PeekCharRaw().value() == '/',
                "Foud character '/' that does not start a line comment!");
        LexComment();
        NextTok();
        break;
    }
    case '0': {
        Opt<char> cc = PeekChar();
        // Remove the '0x' prefix before lexing the hex digit.
        if (cc.has_value() and *cc == 'x') {
            // Pop the 'x' from the '0x' prefix.
            NextChar();
            LexHexDigit();
            break;
        }
        [[fallthrough]];
    }
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
        LexDigit();
        break;
    }
    default: {
        dbg::Assert(IsIdentStart(*c),
                "Attempting to create a token from "
                "an unknwon character: '{}'", *c);

        LexIdent();
        break;
    }
    }  // switch

    tok.loc.len = u32(foffset - tok.loc.offset - 1);
    tok.loc.fid = fid;
    tok.str = SpellingView(foffset, tok.loc.len);
}

auto Lexer::SpellingView(u64 offset, u32 len) -> std::string_view {
    dbg::Assert(offset < chars.size() and offset + len <= chars.size(),
            "Attempting to get the spelling of a token using "
            "an out of bounds range!");

    return chars.substr(offset, len);
}

auto Lexer::LexComment() -> void {
    while (PeekCharRaw().has_value() and *PeekCharRaw() != '\n') {
        NextCharRaw();
    }
}

auto Lexer::LexHexDigit() -> void {
    tok.ty = Tok::Ty::Num;
    while (PeekChar().has_value() and IsHexDigit(*PeekChar())) {
        NextChar();
    }
}

auto Lexer::LexDigit() -> void {
    tok.ty = Tok::Ty::Num;
    while (PeekChar().has_value() and IsDigit(*PeekChar())) {
        NextChar();
    }
}

auto Lexer::LexIdent() -> void {
    while (PeekChar().has_value() and IsIdentCont(*PeekChar())) {
        NextChar();
    }
    
    std::string_view tok_str_view = SpellingView(tok.loc.offset, u32(foffset - tok.loc.offset));
    if (auto ty = keywords.find(tok_str_view); ty != keywords.end()) {
        tok.ty = ty->second;
    } else {
        tok.ty = Tok::Ty::Ident;
    }
}

}  // namespace fiska::syntax
