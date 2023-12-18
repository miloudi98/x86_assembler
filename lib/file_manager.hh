#ifndef __X86_ASSEMBLER_LIB_FILE_MANAGER_HH__
#define __X86_ASSEMBLER_LIB_FILE_MANAGER_HH__

#include <filesystem>
#include <span>

#include "lib/utils.hh"

namespace fiska::fsm {

struct File {
    u16 fid{};
    fs::path path;

    // Keep one instance of the file in memory.
    // Files can't be moved as well.
    File(const File&) = delete;
    File(File&&) = delete;
    File& operator=(const File&) = delete;
    File& operator=(File&&) = delete;

    File(u16 fid, fs::path path, Vec<char> content) 
        : fid(fid), path(path), content(content) {}

    [[nodiscard]] auto Content() -> std::string_view {
        return std::string_view{content.data(), content.size()};
    }

private:
    // Private to make sure we don't accidentally copy it.
    Vec<char> content;
};

struct File_Manager {
    Vec<Box<File>> active_files;

    // Keep one instance of the File_Manager in use.
    // The File_Manager can't be moved as well.
    File_Manager(const File_Manager&) = delete;
    File_Manager(File_Manager&&) = delete;
    File_Manager& operator=(const File_Manager&) = delete;
    File_Manager& operator=(File_Manager&&) = delete;

    // Loads a file from disk and return its fid.
    [[nodiscard]] auto LoadFile(const fs::path& path) -> u16;
    // Returns a view of the contents of a file.
    [[nodiscard]] auto ViewFileContent(u16 fid) -> std::span<const char>;
};

struct SourceLoc {
    u64 offset{};
    u32 len{};
    u16 fid{};
};

}  // namespace fiska::fs

#endif
