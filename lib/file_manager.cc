#include "lib/utils.hh"
#include "lib/file_manager.hh"

auto fiska::fsm::File_Manager::LoadFile(const fs::path& path) -> u16 {
    Vec<char> file_content = ::utils::LoadFile(path);
    u16 fid = static_cast<u16>(active_files.size());
    active_files.push_back(
        std::make_unique<File>(fid, path, std::move(file_content))
    );
    return fid;
}

auto fiska::fsm::File_Manager::ViewFileContent(u16 fid) -> std::string_view {
    dbg::Assert(fid < active_files.size(),
            "Attempting to access a file using an invalid fid: '{}'", fid);

    return active_files[fid]->Content();
}

auto fiska::fsm::File_Manager::GetFile(u16 fid) -> File& {
    dbg::Assert(fid < active_files.size(),
            "Attempting to access a file using an invalid fid: '{}'", fid);
    return *active_files[fid];
}
