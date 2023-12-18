#ifndef __X86_ASSEMBLER_LIB_LEXER_HH__ 
#define __X86_ASSEMBLER_LIB_LEXER_HH__

#include <span>

#include "lib/utils.hh"
#include "lib/file_manager.hh"

namespace fiska::syntax {

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
        // ','
        Comma,

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
    fsm::SourceLoc loc{};


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
    char lastc{'\0'};

    Lexer(fsm::File& file)
        : chars(file.Content()), fid(file.fid) {}

    auto Spelling(u64 offset, u32 len) -> std::string;
    auto SpellingView(u64 offset, u32 len) -> std::string_view;
    auto NextChar() -> Opt<char>;
    auto PeekChar(u32 n = 0) -> Opt<char>;
    auto IsEof() const -> bool { return foffset >= chars.size(); }
    auto NextTok() -> Tok;
    auto LexHexDigit(Tok& tok) -> void;
    auto LexDigit(Tok& tok) -> void;
    auto LexIdent(Tok& tok) -> void;
};


}  // namepsace syntax


#endif  // __X86_ASSEMBLER_LIB_LEXER_HH__
