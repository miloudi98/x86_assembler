#include "lib/utils.hh"
#include "lib/file_manager.hh"

auto fiska::fsm::File_Manager::LoadFile(const fs::path& path) -> u16 {
    Vec<char> file_content = ::utils::LoadFile(path);
    u16 fid = static_cast<u16>(active_files.size());
    auto file = std::make_unique<File>(fid, path, std::move(file_content));
    active_files.push_back(std::move(file));
    return fid;
}

auto fiska::fsm::File_Manager::ViewFileContent(u16 fid) -> std::span<const char> {
    dbg::Assert(fid < active_files.size(),
            "Attempting to access a file using an invalid fid: '{}'", fid);

    return active_files[fid]->Content();
}
