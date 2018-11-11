#ifndef TALTA_TALTA_HPP
#define TALTA_TALTA_HPP

#include <altacore.hpp>
#include <ceetah.hpp>
#include <vector>
#include <tuple>

namespace Talta {
  std::string escapeName(std::string name);
  std::string mangleType(AltaCore::DET::Type* type);
  std::string mangleName(AltaCore::DET::Module* mod, bool fullName = true);
  std::string mangleName(AltaCore::DET::ScopeItem* item, bool fullName = true);

  class CTranspiler {
    private:
      std::string cTypeNameify(AltaCore::DET::Type* type);
      std::shared_ptr<Ceetah::AST::Expression> transpile(AltaCore::AST::Node* node);
      std::shared_ptr<Ceetah::AST::Type> transpileType(AltaCore::DET::Type* type);
    public:
      std::shared_ptr<Ceetah::AST::RootNode> cRoot = nullptr;
      std::shared_ptr<Ceetah::AST::RootNode> hRoot = nullptr;
      Ceetah::Builder source = Ceetah::Builder(cRoot);
      Ceetah::Builder header = Ceetah::Builder(hRoot);
      
      void transpile(std::shared_ptr<AltaCore::AST::RootNode> altaRoot);
  };
  std::map<std::string, std::tuple<std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<AltaCore::DET::Module>>> recursivelyTranspileToC(std::shared_ptr<AltaCore::AST::RootNode> altaRoot);
};

#endif // TALTA_TALTA_HPP
