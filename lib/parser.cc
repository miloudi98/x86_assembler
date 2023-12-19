#include "lib/utils.hh"
#include "lib/parser.hh"

namespace fiska::syntax {

void* Expr::operator new(usz sz, Module* mod) {
    auto ptr = static_cast<Expr*>(::operator new(sz));
    mod->exprs.push_back(ptr);
    return ptr;
}

}  // namespace fiska::syntax
