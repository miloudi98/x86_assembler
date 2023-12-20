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
// \a: terminal ring bell.
// \b: backspace.
// ' ': regular space.
constexpr auto IsWhiteSpace(char c) -> bool {
    return c == '\n' or c == '\t'
        or c == '\f' or c == '\a'
        or c == '\r' or c == '\v'
        or c == '\b' or c == ' ';
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
    return std::isalpha(c) or IsDigit(c) or c == '_';
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
    // Initialize first char and first tok.
    NextChar();
    NextTok();
}

auto Lexer::NextChar() -> void {
    if (IsEof()) {
        lastc = 0;
        return;
    }
    lastc = chars[foffset++];
}

auto Lexer::PeekChar(u32 n) -> Opt<char> {
    if (foffset + n >= chars.size()) {
        return std::nullopt;
    }
    return chars[foffset + n];
}

auto Lexer::NextTok() -> void {
    SkipWhiteSpace();

    // Since foffset is unsigned, make sure foffset >= 1 so that 
    // foffset - 1 doesn't overflow.
    dbg::Assert(foffset >= 1);
    tok.loc.offset = foffset - 1;

    if (lastc == 0) {
        tok.ty = Tok::Ty::Eof;
        return;
    }

    switch (lastc) {
    case ',': {
        NextChar();
        tok.ty = Tok::Ty::Comma;
        break;
    }
    case ':': {
        NextChar();
        tok.ty = Tok::Ty::Colon;
        break;
    }
    case ';': {
        NextChar();
        tok.ty = Tok::Ty::SemiColon;
        break;
    }
    case '(': {
        NextChar();
        tok.ty = Tok::Ty::Lparen;
        break;
    }
    case ')': {
        NextChar();
        tok.ty = Tok::Ty::Rparen;
        break;
    }
    case '{': {
        NextChar();
        tok.ty = Tok::Ty::Lbrace;
        break;
    }
    case '}': {
        NextChar();
        tok.ty = Tok::Ty::Rbrace;
        break;
    }
    case '[': {
        NextChar();
        tok.ty = Tok::Ty::Lbracket;
        break;
    }
    case ']': {
        NextChar();
        tok.ty = Tok::Ty::Rbracket;
        break;
    }
    case '@': {
        NextChar();
        tok.ty = Tok::Ty::At;
        break;
    }
    case '+': {
        NextChar();
        tok.ty = Tok::Ty::Plus;
        break;
    }
    case '-': {
        NextChar();
        tok.ty = Tok::Ty::Minus;
        break;
    }
    case '/': {
        // Eat the first '/'
        NextChar();
        dbg::Assert(lastc == '/');
        // Eat the second '/'
        NextChar();

        LexComment();
        return NextTok();
    }
    case '0': {
        Opt<char> cc = PeekChar();
        dbg::Assert(cc and not IsDigit(*cc));
        if (cc and *cc == 'x') {
            // Remove the '0x' prefix before lexing the hex digit.
            NextChar();
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
        dbg::Assert(IsIdentStart(lastc));

        LexIdent();
        break;
    }
    }  // switch

    tok.loc.len = u32(foffset - tok.loc.offset - 1);
    tok.loc.fid = fid;
    tok.str = SpellingView(tok.loc.offset, tok.loc.len);
}

auto Lexer::SpellingView(u64 offset, u32 len) -> std::string_view {
    dbg::Assert(offset < chars.size() and offset + len <= chars.size(),
            "Attempting to get the spelling of a token using "
            "an out of bounds range!");

    return chars.substr(offset, len);
}

auto Lexer::SkipWhiteSpace() -> void {
    while (IsWhiteSpace(lastc)) {
        NextChar();
    }
}

auto Lexer::LexComment() -> void {
    while (lastc != '\n') {
        NextChar();
    }
}

auto Lexer::LexHexDigit() -> void {
    tok.ty = Tok::Ty::Num;
    while (IsHexDigit(lastc)) {
        NextChar();
    }
}

auto Lexer::LexDigit() -> void {
    tok.ty = Tok::Ty::Num;
    while (IsDigit(lastc)) {
        NextChar();
    }
}

auto Lexer::LexIdent() -> void {
    while (IsIdentCont(lastc)) {
        NextChar();
    }
    
    std::string_view tok_str_view = SpellingView(tok.loc.offset, u32(foffset - tok.loc.offset - 1));
    if (auto ty = keywords.find(tok_str_view); ty != keywords.end()) {
        tok.ty = ty->second;
    } else {
        tok.ty = Tok::Ty::Ident;
    }
}

}  // namespace fiska::syntax
