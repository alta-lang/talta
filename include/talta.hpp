#ifndef TALTA_TALTA_HPP
#define TALTA_TALTA_HPP

#include <altacore.hpp>
#include <ceetah.hpp>
#include <vector>
#include <tuple>

namespace Talta {
  std::string escapeName(std::string name);
  std::string cTypeNameify(AltaCore::DET::Type* type, bool mangled = false);
  std::string mangleType(AltaCore::DET::Type* type);
  std::string mangleName(AltaCore::DET::Scope* scope, bool fullName = true);
  std::string mangleName(AltaCore::DET::Module* mod, bool fullName = true);
  std::string mangleName(AltaCore::DET::ScopeItem* item, bool fullName = true);
  std::string headerMangle(AltaCore::DET::Module* item, bool fullName = true);
  std::string headerMangle(AltaCore::DET::ScopeItem* item, bool fullName = true);

  extern ALTACORE_MAP<std::string, std::vector<std::string>> moduleIncludes;
  extern ALTACORE_MAP<std::string, bool> varargTable;
  extern ALTACORE_MAP<std::string, std::vector<std::shared_ptr<AltaCore::DET::ScopeItem>>> genericDependencies;

  void registerAttributes(AltaCore::Filesystem::Path modulePath);

  class CTranspiler {
    private:
      std::shared_ptr<Ceetah::AST::Expression> transpile(AltaCore::AST::Node* node, AltaCore::DH::Node* info);
      std::shared_ptr<Ceetah::AST::Type> transpileType(AltaCore::DET::Type* type);
      void headerPredeclaration(std::string def, std::string mangledModuleName, bool includeAll = true);
      std::vector<uint8_t> convertTypeModifiers(std::vector<uint8_t> altaModifiers);
      void defineFunctionalType(std::shared_ptr<AltaCore::DET::Type> type, bool inHeader = false);
      std::vector<std::shared_ptr<Ceetah::AST::Expression>> processArgs(std::vector<ALTACORE_VARIANT<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>, std::vector<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>>>> adjustedArguments, std::vector<std::tuple<std::string, std::shared_ptr<AltaCore::DET::Type>, bool, std::string>> func);
      void stackBookkeepingStart(std::shared_ptr<AltaCore::DET::Scope> scope);
      void stackBookkeepingStop(std::shared_ptr<AltaCore::DET::Scope> scope);
      std::shared_ptr<Ceetah::AST::Expression> doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> transpiled, std::shared_ptr<AltaCore::AST::ExpressionNode> expr, std::shared_ptr<AltaCore::DH::ExpressionNode> info);
      std::shared_ptr<Ceetah::AST::Expression> doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType);
      std::shared_ptr<Ceetah::AST::Expression> doParentRetrieval(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, std::shared_ptr<AltaCore::DET::Type> targetType, bool* didRetrieval = nullptr);
      std::shared_ptr<Ceetah::AST::Expression> doChildRetrieval(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, std::shared_ptr<AltaCore::DET::Type> targetType, bool* didRetrieval = nullptr);
      void includeGeneric(std::shared_ptr<AltaCore::DET::ScopeItem> generic, bool inHeader = false);
    public:
      std::shared_ptr<Ceetah::AST::RootNode> cRoot = nullptr;
      std::shared_ptr<Ceetah::AST::RootNode> hRoot = nullptr;
      std::shared_ptr<AltaCore::DET::Module> currentModule = nullptr;
      Ceetah::Builder source = Ceetah::Builder(cRoot);
      Ceetah::Builder header = Ceetah::Builder(hRoot);
      std::vector<std::pair<std::shared_ptr<AltaCore::DET::ScopeItem>, std::shared_ptr<Ceetah::AST::RootNode>>> generics;
    protected:
      std::shared_ptr<Ceetah::AST::Type> size_tType = source.createType("size_t", { { Ceetah::AST::TypeModifierFlag::Constant } });
    public:
      void transpile(std::shared_ptr<AltaCore::AST::RootNode> altaRoot);
  };
  ALTACORE_MAP<std::string, std::tuple<std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::vector<std::shared_ptr<Ceetah::AST::RootNode>>, std::vector<std::shared_ptr<AltaCore::DET::ScopeItem>>, std::shared_ptr<AltaCore::DET::Module>>> recursivelyTranspileToC(std::shared_ptr<AltaCore::AST::RootNode> altaRoot, CTranspiler* transpiler = nullptr);
};

#endif // TALTA_TALTA_HPP
