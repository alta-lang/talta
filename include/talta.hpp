#ifndef TALTA_TALTA_HPP
#define TALTA_TALTA_HPP

#include <altacore.hpp>
#include <ceetah.hpp>
#include <vector>
#include <tuple>
#include <queue>

namespace Talta {
  namespace Helpers {
    template<class... Args>
    class CoroutineManager;

    template<class... Args>
    class Coroutine {
      friend class CoroutineManager<Args...>;

      public:
        using ManagerType = CoroutineManager<Args...>;
        using FunctionType = std::function<Coroutine&(Coroutine&)>;
      private:
        ManagerType& manager;
        FunctionType target = nullptr;
        size_t _iteration = 0;
        ALTACORE_ANY value = nullptr;
        bool _finalized = false;
        ALTACORE_OPTIONAL<std::tuple<Args...>> _arguments = ALTACORE_NULLOPT;
        std::queue<ALTACORE_ANY> saved;
      public:
        Coroutine(ManagerType& _manager, FunctionType function):
          manager(_manager),
          target(function)
          {};

        inline size_t iteration() const {
          return _iteration;
        };
        inline FunctionType function() const {
          return target;
        };
        inline bool finalized() const {
          return _finalized;
        };
        inline std::tuple<Args...> arguments() const {
          return *_arguments;
        };

        template<class V>
        inline V result() const {
          return ALTACORE_ANY_CAST<V>(value);
        };

        inline ALTACORE_ANY resultAny() const {
          return value;
        };

        template<class V = ALTACORE_ANY>
        inline V* maybeResult() const {
          return ALTACORE_ANY_CAST<V>(&value);
        };

        template<class... Vars>
        inline Coroutine& save(Vars&&... vars) {
          saved.emplace(std::make_tuple<typename std::decay<Vars>::type...>(std::forward<typename std::decay<Vars>::type>(vars)...));
          return *this;
        };

        inline Coroutine& saveAny(ALTACORE_ANY value) {
          saved.emplace(value);
          return *this;
        };

        template<class... Vars>
        inline std::tuple<typename std::decay<Vars>::type...> load() {
          auto result = ALTACORE_ANY_CAST<std::tuple<typename std::decay<Vars>::type...>>(saved.front());
          saved.pop();
          return result;
        };

        inline ALTACORE_ANY loadAny() {
          auto result = saved.front();
          saved.pop();
          return result;
        };

        template<class V>
        inline Coroutine& yield(V _value) {
          if (_finalized) {
            throw std::runtime_error("can't yield after coroutine has been finalized");
          }
          value = _value;
          return *this;
        };

        inline Coroutine& yield() {
          return yield(nullptr);
        };

        template<class V>
        inline Coroutine& finalYield(V _value) {
          value = _value;
          _finalized = true;
          return *this;
        };

        inline Coroutine& finalYield() {
          return finalYield(nullptr);
        };

        Coroutine& await(FunctionType other, Args... args);
    };

    template<class... Args>
    class CoroutineManager {
      friend class Coroutine<Args...>;

      public:
        using CoroutineType = Coroutine<Args...>;
      private:
        std::stack<CoroutineType> coroutines;
        typename CoroutineType::FunctionType awaitingFunction = nullptr;
        ALTACORE_OPTIONAL<std::tuple<Args...>> awaitingArguments = ALTACORE_NULLOPT;
      public:
        ALTACORE_ANY await(typename CoroutineType::FunctionType function, Args... args) {
          if (coroutines.size() > 0) {
            throw std::runtime_error("another call is in progress");
          }

          coroutines.emplace(*this, function);
          ALTACORE_ANY result = nullptr;

          while (coroutines.size() > 0) {
            auto& co = coroutines.top();
            co.value = result;

            if (!co._arguments) {
              co._arguments = coroutines.size() == 1 ? std::make_tuple(args...) : *awaitingArguments;
            }

            awaitingFunction = nullptr;
            awaitingArguments = ALTACORE_NULLOPT;

            co.function()(co);
            ++co._iteration;
            result = co.value;

            if (awaitingFunction) {
              coroutines.emplace(*this, awaitingFunction);
            } else if (co._finalized) {
              coroutines.pop();
            }
          }

          return result;
        };
    };

    template<typename... Args>
    auto Coroutine<Args...>::await(FunctionType other, Args... args) -> Coroutine& {
      if (_finalized) {
        throw std::runtime_error("can't await after coroutine has been finalized");
      }
      manager.awaitingFunction = other;
      manager.awaitingArguments = std::make_tuple(args...);
      return *this;
    };
  };

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
      using Coroutine = Talta::Helpers::Coroutine<std::shared_ptr<AltaCore::AST::Node>, std::shared_ptr<AltaCore::DH::Node>>;
      using CoroutineManager = Talta::Helpers::CoroutineManager<std::shared_ptr<AltaCore::AST::Node>, std::shared_ptr<AltaCore::DH::Node>>;
      using CoroutineMemberFunction = Coroutine&(CTranspiler::*)(Coroutine&);
      using CExpression = std::shared_ptr<Ceetah::AST::Expression>;
      using CopyInfo = std::pair<bool, bool>;

      static const CopyInfo defaultCopyInfo;

      std::shared_ptr<Ceetah::AST::Expression> transpile(std::shared_ptr<AltaCore::AST::Node> node, std::shared_ptr<AltaCore::DH::Node> info);
      //std::shared_ptr<Ceetah::AST::Expression> transpile(AltaCore::AST::Node* node, AltaCore::DH::Node* info);
      std::shared_ptr<Ceetah::AST::Type> transpileType(AltaCore::DET::Type* type);
      void headerPredeclaration(std::string def, std::string mangledModuleName, bool includeAll = true);
      std::vector<uint8_t> convertTypeModifiers(std::vector<uint8_t> altaModifiers);
      void hoist(std::shared_ptr<AltaCore::DET::ScopeItem> item, bool inHeader = false);
      std::vector<std::shared_ptr<Ceetah::AST::Expression>> processArgs(std::vector<ALTACORE_VARIANT<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>, std::vector<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>>>> adjustedArguments, std::vector<std::tuple<std::string, std::shared_ptr<AltaCore::DET::Type>, bool, std::string>> func);
      void stackBookkeepingStart(std::shared_ptr<AltaCore::DET::Scope> scope);
      void stackBookkeepingStop(std::shared_ptr<AltaCore::DET::Scope> scope);
      std::shared_ptr<Ceetah::AST::Expression> doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> transpiled, std::shared_ptr<AltaCore::AST::ExpressionNode> expr, std::shared_ptr<AltaCore::DH::ExpressionNode> info, bool* didCopy = nullptr);
      std::shared_ptr<Ceetah::AST::Expression> doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, CopyInfo additionalCopyInfo, bool* didCopy = nullptr);
      std::shared_ptr<Ceetah::AST::Expression> doParentRetrieval(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, std::shared_ptr<AltaCore::DET::Type> targetType, bool* didRetrieval = nullptr);
      std::shared_ptr<Ceetah::AST::Expression> doChildRetrieval(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, std::shared_ptr<AltaCore::DET::Type> targetType, bool* didRetrieval = nullptr);
      void includeGeneric(std::shared_ptr<AltaCore::DET::ScopeItem> generic, bool inHeader = false);
      void insertExportDefinition(std::string def);
      void saveExportDefinitions(bool inHeader = true);
      void restoreExportDefinitions(bool inHeader = true);
      std::shared_ptr<Ceetah::AST::Expression> cast(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> source, std::shared_ptr<AltaCore::DET::Type> dest, bool copy = false, CopyInfo additionalCopyInfo = defaultCopyInfo);
      inline CopyInfo additionalCopyInfo(AltaCore::AST::NodeType type) const {
        return std::make_pair(
          (
            type != AltaCore::AST::NodeType::ClassInstantiationExpression &&
            type != AltaCore::AST::NodeType::FunctionCallExpression
          ),
          (
            type == AltaCore::AST::NodeType::FunctionCallExpression ||
            type == AltaCore::AST::NodeType::ClassInstantiationExpression ||
            type == AltaCore::AST::NodeType::ConditionalExpression
          )
        );
      };
      void includeClassIfNecessary(std::shared_ptr<AltaCore::DET::Type> type);

      // <coroutine-helpers>
      auto bind(const CoroutineMemberFunction function) -> Coroutine::FunctionType;
      Coroutine& tmpify(Coroutine& co);
      CExpression tmpify(CExpression expr, std::shared_ptr<AltaCore::DET::Type> type);
      // </coroutine-helpers>

      // <transpilation-methods>
      Coroutine& transpile(Coroutine& co);
      Coroutine& transpileExpressionStatement(Coroutine& co);
      Coroutine& transpileType(Coroutine& co);
      Coroutine& transpileBlockNode(Coroutine& co);
      Coroutine& transpileFunctionDefinitionNode(Coroutine& co);
      Coroutine& transpileReturnDirectiveNode(Coroutine& co);
      Coroutine& transpileIntegerLiteralNode(Coroutine& co);
      Coroutine& transpileVariableDefinitionExpression(Coroutine& co);
      Coroutine& transpileAccessor(Coroutine& co);
      Coroutine& transpileFetch(Coroutine& co);
      Coroutine& transpileAssignmentExpression(Coroutine& co);
      Coroutine& transpileBooleanLiteralNode(Coroutine& co);
      Coroutine& transpileBinaryOperation(Coroutine& co);
      Coroutine& transpileImportStatement(Coroutine& co);
      Coroutine& transpileFunctionCallExpression(Coroutine& co);
      Coroutine& transpileStringLiteralNode(Coroutine& co);
      Coroutine& transpileFunctionDeclarationNode(Coroutine& co);
      Coroutine& transpileConditionalStatement(Coroutine& co);
      Coroutine& transpileConditionalExpression(Coroutine& co);
      Coroutine& transpileClassDefinitionNode(Coroutine& co);
      Coroutine& transpileClassMethodDefinitionStatement(Coroutine& co);
      Coroutine& transpileClassSpecialMethodDefinitionStatement(Coroutine& co);
      Coroutine& transpileClassInstantiationExpression(Coroutine& co);
      Coroutine& transpilePointerExpression(Coroutine& co);
      Coroutine& transpileDereferenceExpression(Coroutine& co);
      Coroutine& transpileWhileLoopStatement(Coroutine& co);
      Coroutine& transpileCastExpression(Coroutine& co);
      Coroutine& transpileClassReadAccessorDefinitionStatement(Coroutine& co);
      Coroutine& transpileCharacterLiteralNode(Coroutine& co);
      Coroutine& transpileTypeAliasStatement(Coroutine& co);
      Coroutine& transpileSubscriptExpression(Coroutine& co);
      Coroutine& transpileSuperClassFetch(Coroutine& co);
      Coroutine& transpileInstanceofExpression(Coroutine& co);
      Coroutine& transpileGeneric(Coroutine& co);
      Coroutine& transpileForLoopStatement(Coroutine& co);
      Coroutine& transpileRangedForLoopStatement(Coroutine& co);
      Coroutine& transpileUnaryOperation(Coroutine& co);
      Coroutine& transpileSizeofOperation(Coroutine& co);
      Coroutine& transpileFloatingPointLiteralNode(Coroutine& co);
      Coroutine& transpileStructureDefinitionStatement(Coroutine& co);
      Coroutine& transpileExportStatement(Coroutine& co);
      Coroutine& transpileDeleteStatement(Coroutine& co);
      Coroutine& transpileControlDirective(Coroutine& co);
      Coroutine& transpileTryCatchBlock(Coroutine& co);
      Coroutine& transpileThrowStatement(Coroutine& co);
      Coroutine& transpileNullptrExpression(Coroutine& co);
      Coroutine& transpileCodeLiteralNode(Coroutine& co);
      Coroutine& transpileAttributeStatement(Coroutine& co);
      Coroutine& transpileBitfieldDefinitionNode(Coroutine& co);
      // </transpilation-methods>

      static const ALTACORE_MAP<AltaCore::AST::NodeType, CoroutineMemberFunction> transpilationMethods;
      const Coroutine::FunctionType boundTranspile = bind(&CTranspiler::transpile);
    public:
      std::shared_ptr<Ceetah::AST::RootNode> cRoot = nullptr;
      std::shared_ptr<Ceetah::AST::RootNode> hRoot = nullptr;
      std::shared_ptr<Ceetah::AST::RootNode> dRoot = nullptr;
      std::shared_ptr<AltaCore::DET::Module> currentModule = nullptr;
      std::shared_ptr<AltaCore::DET::Scope> currentScope = nullptr;
      Ceetah::Builder source = Ceetah::Builder(cRoot);
      Ceetah::Builder header = Ceetah::Builder(hRoot);
      Ceetah::Builder definitions = Ceetah::Builder(dRoot);
      Ceetah::Builder* target = &source;
      std::vector<std::pair<std::shared_ptr<AltaCore::DET::ScopeItem>, std::shared_ptr<Ceetah::AST::RootNode>>> generics;
    protected:
      std::shared_ptr<Ceetah::AST::Type> size_tType = source.createType("size_t", { { Ceetah::AST::TypeModifierFlag::Constant } });
    public:
      void transpile(std::shared_ptr<AltaCore::AST::RootNode> altaRoot);
  };
  ALTACORE_MAP<std::string, std::tuple<std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::vector<std::shared_ptr<Ceetah::AST::RootNode>>, std::vector<std::shared_ptr<AltaCore::DET::ScopeItem>>, std::shared_ptr<AltaCore::DET::Module>>> recursivelyTranspileToC(std::shared_ptr<AltaCore::AST::RootNode> altaRoot, CTranspiler* transpiler = nullptr);
};

#endif // TALTA_TALTA_HPP
