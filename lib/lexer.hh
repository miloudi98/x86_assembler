#ifndef __X86_ASSEMBLER_LIB_LEXER_HH__ 
#define __X86_ASSEMBLER_LIB_LEXER_HH__

#include <span>

#include "lib/utils.hh"
#include "lib/file_manager.hh"

namespace fiska::syntax {

using fsm::SourceLoc;

struct Tok {
    enum struct Ty {
        Invalid,
        Eof,

        // One character tokens.
        // '('
        Lparen,
        // ')'
        Rparen,
        // '{'
        Lbrace,
        // '}'
        Rbrace,
        // '['
        Lbracket,
        // ']'
        Rbracket,
        // '@'
        At,
        // ';'
        SemiColon,
        // ':'
        Colon,
        // ','
        Comma,
        // '+'
        Plus,
        // '-'
        Minus,

        // Multi-character tokens.
        // Identifier
        Ident,
        // Number
        Num,
        // 'b8', 'b16', 'b32' ...
        Bsize,
        // keyword 'fn'
        Fn,
        // x86 Registers.
        Register,
    };

    // Token kind.
    Ty ty = Ty::Invalid;
    // Token text.
    std::string str{};
    // Token location
    SourceLoc loc{};


    template <typename... args>
    static auto Make(Ty ty, args&&... arguments) -> Tok {
        return Tok{ty, std::forward<args>(arguments)...};
    }
};

struct Lexer {
    // The program text we are lexing.
    std::string_view chars;
    // Offset into the file we are lexing.
    u64 foffset{};
    u16 fid{};
    Tok tok{};
    char lastc{};

    Lexer(fsm::File& file);

    auto SpellingView(u64 offset, u32 len) -> std::string_view;
    auto NextChar() -> void;
    auto PeekChar(u32 n = 0) -> Opt<char>;
    auto SkipWhiteSpace() -> void;

    auto IsEof() const -> bool { return foffset >= chars.size(); }
    auto NextTok() -> void;
    auto LexComment() -> void;
    auto LexHexDigit() -> void;
    auto LexDigit() -> void;
    auto LexIdent() -> void;
};


}  // namepsace fiska::syntax


#endif  // __X86_ASSEMBLER_LIB_LEXER_HH__
