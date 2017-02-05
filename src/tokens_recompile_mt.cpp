#include <Rcpp.h>
#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;

#if RCPP_PARALLEL_USE_TBB
typedef tbb::concurrent_vector<unsigned int> VecIds;
typedef tbb::concurrent_unordered_map<std::string, unsigned int> MapTypes;
#else
typedef std::vector<unsigned int> VecIds;
typedef std::std_unordered_map<std::string, unsigned int> MapTypes;
#endif

struct compile_mt : public Worker{
    
    Texts &texts;
    VecIds &ids_new;
    
    compile_mt(Texts &texts_, VecIds &ids_new_):
               texts(texts_), ids_new(ids_new_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++) {
            for (std::size_t i = 0; i < texts[h].size(); i++) {
                texts[h][i] = ids_new[texts[h][i]];
            }
        }
    }
};


/* 
* This funciton recompiles tokens object.
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param types_ types in tokens
*/


// [[Rcpp::export]]
List qatd_cpp_recompile(const List &texts_, 
                        const CharacterVector types_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    unsigned int filler = std::numeric_limits<unsigned int>::max(); // use upper limit as a filler
    
    // Get unique types
    VecIds ids_new(types.size());
    ids_new[0] = 0; // reserved for padding
    unsigned int id_new = 0;
    
    // Check if IDs have gaps
    std::vector<bool> flags_used(types.size(), false);
    bool padding = false; // record use of padding
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            unsigned int id = texts[h][i];
            if (id == 0) {
                padding = true;
                continue;
            }
            flags_used[id - 1] = true;
        }
    }
    bool all_used = std::all_of(flags_used.begin(), flags_used.end(), [](bool v) { return v; });
    
    // Check if types are duplicated
    std::vector<bool> flags_unique(types.size(), false);
    MapTypes types_unique;
    for (std::size_t g = 0; g < types.size(); g++) {
        auto it = types_unique.insert(std::pair<std::string, unsigned int>(types[g], id_new));
        ids_new[g] = it.first->second;
        if (it.second) {
            flags_unique[g] = true;
            if (flags_used[g]) {
                id_new++; // increment iff there is no gap
            }
        }
        //Rcout << types[g] << ": " << g << " -> " << ids_new[g] << "\n";
    }
    bool all_unique = std::all_of(flags_unique.begin(), flags_unique.end(), [](bool v) { return v; });
    
    // Check gaps and duplicates
    if (all_used && all_unique) return texts_;

    // Convert old IDs to new IDs
    #if RCPP_PARALLEL_USE_TBB
    compile_mt compile_mt(texts, ids_new);
    parallelFor(0, texts.size(), compile_mt);
    #else
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            texts[h][i] = ids_new[texts[h][i]];
        }
    }
    #endif
    
    std::vector<std::string> types_new;
    types_new.reserve(types.size());
    for (std::size_t j = 0; j < ids_new.size(); j++) {
        if (flags_used[j] && flags_unique[j]) {
            types_new.push_back(types[ids_new[j]]);
        }
    }
    
    // dev::stop_timer("Dictionary lookup", timer);
    ListOf<IntegerVector> texts_list = Rcpp::wrap(texts);
    texts_list.attr("padding") = padding;
    texts_list.attr("types") = types_new;
    return texts_list;
     
}

/***R

toks3 <- list(rep(0:5, 1), rep(10:15, 1))
qatd_cpp_recompile(toks3, letters)
qatd_cpp_recompile(toks3, rep(c('a', 'b', 'c'), 6))



*/
