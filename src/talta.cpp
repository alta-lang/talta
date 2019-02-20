#include "../include/talta.hpp"
#include "../include/talta/util.hpp"

namespace Talta {
  std::map<std::string, std::vector<std::string>> moduleIncludes;
  std::unordered_map<std::string, bool> varargTable;
};

std::vector<std::shared_ptr<Ceetah::AST::Expression>> Talta::CTranspiler::processArgs(std::vector<ALTACORE_VARIANT<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>, std::vector<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>>>> adjustedArguments, std::vector<std::tuple<std::string, std::shared_ptr<AltaCore::DET::Type>, bool, std::string>> parameters) {
    namespace CAST = Ceetah::AST;
    namespace AAST = AltaCore::AST;
    namespace DH = AltaCore::DH;
    std::vector<std::shared_ptr<Ceetah::AST::Expression>> args;
    for (size_t i = 0; i < adjustedArguments.size(); i++) {
      auto& arg = adjustedArguments[i];
      if (auto solo = ALTACORE_VARIANT_GET_IF<std::pair<std::shared_ptr<AAST::ExpressionNode>, std::shared_ptr<DH::ExpressionNode>>>(&arg)) {
        auto& [arg, info] = *solo;
        args.push_back(transpile(arg.get(), info.get()));
      } else if (auto multi = ALTACORE_VARIANT_GET_IF<std::vector<std::pair<std::shared_ptr<AAST::ExpressionNode>, std::shared_ptr<DH::ExpressionNode>>>>(&arg)) {
        auto [name, targetType, isVariable, id] = parameters[i];
        if (varargTable[id]) {
          for (auto& [arg, info]: *multi) {
            args.push_back(transpile(arg.get(), info.get()));
          }
        } else {
          std::vector<std::shared_ptr<CAST::Expression>> arrItems;
          for (auto& [arg, info]: *multi) {
            arrItems.push_back(transpile(arg.get(), info.get()));
          }
          auto cType = transpileType(targetType.get());
          cType->arraySize = SIZE_MAX;
          args.push_back(source.createArrayLiteral(arrItems, cType));
          args.push_back(source.createIntegerLiteral((*multi).size()));
        }
      }
    }
    return args;
  };

std::string Talta::cTypeNameify(AltaCore::DET::Type* type, bool mangled) {
  using NT = AltaCore::DET::NativeType;
  if (type->isFunction) {
    std::string result = "_Alta_func_ptr_" + mangleType(type->returnType.get());
    for (auto& [name, param, isVariable, id]: type->parameters) {
      result += '_';
      auto target = isVariable ? param->point() : param;
      result += mangleType(target.get());
    }
    return result;
  } else if (type->isNative) {
    switch (type->nativeTypeName) {
      case NT::Integer:
        return "int";
      case NT::Byte:
        return "char";
      case NT::Bool:
        return "_Alta_bool";
      case NT::Void:
        return "void";
      default:
        throw std::runtime_error("ok, wtaf.");
    }
  } else {
    return mangleName(type->klass.get());
  }
};

std::string Talta::mangleType(AltaCore::DET::Type* type) {
  using TypeModifier = AltaCore::Shared::TypeModifierFlag;
  std::string mangled;

  for (auto& mod: type->modifiers) {
    if (mod & (uint8_t)TypeModifier::Constant) {
      mangled += "const_3_";
    }
    if (mod & (uint8_t)TypeModifier::Pointer) {
      mangled += "ptr_3_";
    }
    if (mod & (uint8_t)TypeModifier::Reference) {
      mangled += "ref_3_";
    }
  }

  mangled += cTypeNameify(type, true);

  return mangled;
};

std::string Talta::escapeName(std::string name) {
  std::string escaped;

  /**
   * Escapes 0-30 are reserved for internal use (since they're control characters in ASCII anyways).
   * `_0_` is reserved for scope item name separation
   * `_1_` is reserved for function parameter type separation
   * `_2_` is reserved for generic instantiation argument separation (TBD later)
   * `_3_` is reserved for type modifier separation
   * `_4_` is reserved for scope identifier delimination
   * `_5_` is reserved for version delimitation
   * `_6_` is reserved for version prerelease delimination
   * `_7_` is reserved for version build information delimination
   * `_8_` is reserved for variable function parameter type separation
   */

  for (size_t i = 0; i < name.size(); i++) {
    auto character = name[i];
    if (character == '_') {
      escaped += "__";
    } else if (
      (character > 0x2f && character < 0x3a) || // 0-9
      (character > 0x40 && character < 0x5b) || // A-Z
      (character > 0x60 && character < 0x7b)    // a-z
    ) {
      escaped += character;
    } else {
      escaped += "_" + std::to_string((unsigned short int)character) + "_";
    }
  }

  return escaped;
};

std::string Talta::headerMangle(AltaCore::DET::Module* item, bool fullName) {
  return "_ALTA_MODULE_" + mangleName(item, fullName);
};
std::string Talta::headerMangle(AltaCore::DET::ScopeItem* item, bool fullName) {
  using NodeType = AltaCore::DET::NodeType;
  auto nodeType = item->nodeType();

  if (nodeType == NodeType::Function) {
    return "_ALTA_FUNCTION_" + mangleName(item, fullName);
  } else if (nodeType == NodeType::Variable) {
    return "_ALTA_VARIABLE_" + mangleName(item, fullName);
  } else if (nodeType == NodeType::Class) {
    return "_ALTA_CLASS_" + mangleName(item, fullName);
  } else {
    return "";
  }

  return mangleName(item, fullName);
};
std::string Talta::mangleName(AltaCore::DET::Module* mod, bool fullName) {
  auto version = mod->packageInfo.version;
  // since we use underscores as special escape characters,
  // and dashes aren't allowed in identifiers,
  // we have to use another character to separate version parts
  // 'a' works just fine
  auto versionString = std::to_string(version.major) + 'a' + std::to_string(version.minor) + 'a' + std::to_string(version.patch);
  if (version.prerelease != NULL) {
    versionString += std::string("_6_") + version.prerelease;
  }
  if (version.metadata != NULL) {
    versionString += std::string("_7_") + version.metadata;
  }
  return escapeName(mod->name) + "_5_" + versionString;
};
std::string Talta::mangleName(AltaCore::DET::Scope* scope, bool fullName) {
  std::string mangled = "_4_" + std::to_string(scope->relativeID);
  if (!fullName) return mangled;
  if (auto mod = scope->parentModule.lock()) {
    mangled = mangleName(mod.get(), true) + "_0_" + mangled;
  } else if (auto func = scope->parentFunction.lock()) {
    mangled = mangleName(func.get(), true) + "_0_" + mangled;
  } else if (auto parent = scope->parent.lock()) {
    mangled = mangleName(parent.get(), true) + "_0_" + mangled;
  } else if (auto ns = scope->parentNamespace.lock()) {
    mangled = mangleName(ns.get(), true) + "_0_" + mangled;
  }
  return mangled;
};
std::string Talta::mangleName(AltaCore::DET::ScopeItem* item, bool fullName) {
  using NodeType = AltaCore::DET::NodeType;
  namespace DET = AltaCore::DET;
  auto nodeType = item->nodeType();
  std::string itemName;
  auto isLiteral = false;
  std::string mangled;

  if (nodeType == NodeType::Function) {
    auto func = dynamic_cast<DET::Function*>(item);
    itemName = func->name;
    isLiteral = func->isLiteral;

    if (!isLiteral) {
      itemName = escapeName(itemName);
      for (auto& [name, type, isVariable, id]: func->parameters) {
        itemName += ((isVariable) ? "_8_" : "_1_") + mangleType(type.get());
      }
    }
  } else if (nodeType == NodeType::Variable) {
    auto var = dynamic_cast<DET::Variable*>(item);
    itemName = var->name;
    isLiteral = var->isLiteral;
  } else if (nodeType == NodeType::Namespace) {
    auto ns = dynamic_cast<DET::Namespace*>(item);
    itemName = ns->name;
    isLiteral = false;
  } else if (nodeType == NodeType::Class) {
    auto klass = dynamic_cast<DET::Class*>(item);
    itemName = klass->name;
    isLiteral = false;
  }

  if (!isLiteral && fullName) {
    auto maybeScope = item->parentScope;
    while (!maybeScope.expired()) {
      auto scope = maybeScope.lock();
      if (!scope->parentModule.expired()) {
        mangled = mangleName(scope->parentModule.lock().get()) + "_0_" + mangled;
        maybeScope = std::weak_ptr<DET::Scope>(); // modules are root nodes, stop because we found one
      } else if (!scope->parentFunction.expired()) {
        mangled = mangleName(scope->parentFunction.lock().get()) + "_0_" + mangled;
        maybeScope = scope->parentFunction.lock()->parentScope;
      } else if (!scope->parent.expired()) {
        mangled = "_4_" + std::to_string(scope->relativeID) + "_0_" + mangled;
        maybeScope = scope->parent;
      } else if (!scope->parentNamespace.expired()) {
        mangled = mangleName(scope->parentNamespace.lock().get()) + "_0_" + mangled;
        maybeScope = scope->parentNamespace.lock()->parentScope;
      } else if (!scope->parentClass.expired()) {
        mangled = mangleName(scope->parentClass.lock().get()) + "_0_" + mangled;
        maybeScope = scope->parentClass.lock()->parentScope;
      }
    }
  }

  mangled += itemName;

  return mangled;
};

/*
to include a module:
header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangleName(altaRoot->$module) + "_0_INCLUDE_" + mangleName(dependency), Ceetah::AST::InclusionType::Computed);
*/

std::shared_ptr<Ceetah::AST::Type> Talta::CTranspiler::transpileType(AltaCore::DET::Type* type) {
  return source.createType(cTypeNameify(type), convertTypeModifiers(type->modifiers));
};

std::vector<uint8_t> Talta::CTranspiler::convertTypeModifiers(std::vector<uint8_t> altaModifiers) {
  // translate certain Alta-only modifiers to C-compatible modifiers
  //
  // for example, Alta contains both pointers and references. in our
  // C implementation, references are implemented as pointers, so
  // we can just change references to pointers
  for (auto& mod: altaModifiers) {
    if (mod & (uint8_t)AltaCore::Shared::TypeModifierFlag::Reference) {
      mod &= ~(uint8_t)AltaCore::Shared::TypeModifierFlag::Reference;
      mod |= (uint8_t)Ceetah::AST::TypeModifierFlag::Pointer;
    } else if (mod >= (uint8_t)AltaCore::Shared::TypeModifierFlag::Signed) {
      mod = mod >> 1;
    }
  }

  return altaModifiers;
};

void Talta::CTranspiler::headerPredeclaration(std::string def, std::string mangledModName) {
  header.insertPreprocessorConditional("(defined(" + def + ") || defined(_ALTA_MODULE_ALL_" + mangledModName + ")) && !defined(_DEFINED_" + def + ')');
  header.insertPreprocessorUndefinition(def);
  header.insertPreprocessorDefinition("_DEFINED_" + def);
};

void Talta::CTranspiler::defineFunctionalType(std::shared_ptr<AltaCore::DET::Type> type, bool inHeader) {
  if (!type->isFunction) return;

  auto& target = (inHeader ? header : source);

  auto name = cTypeNameify(type.get());
  auto mods = convertTypeModifiers(type->modifiers);

  auto def = "_ALTA_FUNC_PTR_" + name.substr(15);
  target.insertPreprocessorConditional("!defined(" + def + ")");
  target.insertPreprocessorDefinition(def);
  std::vector<std::shared_ptr<Ceetah::AST::Type>> cParams;
  for (auto& [name, param, isVariable, id]: type->parameters) {
    auto target = isVariable ? param->point() : param;
    cParams.push_back(transpileType(target.get()));
  }
  target.insertTypeDefinition(name, target.createType(transpileType(type->returnType.get()), cParams, mods));
  target.exitInsertionPoint();
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::transpile(AltaCore::AST::Node* node, AltaCore::DH::Node* _info) {
  using AltaNodeType = AltaCore::AST::NodeType;
  namespace AAST = AltaCore::AST;
  namespace CAST = Ceetah::AST;
  namespace DH = AltaCore::DH;
  AltaNodeType nodeType = node->nodeType();

  if (nodeType == AltaNodeType::FunctionDefinitionNode) {
    auto aFunc = dynamic_cast<AAST::FunctionDefinitionNode*>(node);
    auto info = dynamic_cast<DH::FunctionDefinitionNode*>(_info);

    if (!info->function->isExport) {
      for (auto& hoistedType: info->function->publicHoistedFunctionalTypes) {
        defineFunctionalType(hoistedType);
      }
    }

    for (auto& hoistedType: info->function->hoistedFunctionalTypes) {
      defineFunctionalType(hoistedType);
    }

    auto mangledFuncName = mangleName(info->function.get());
    std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> cParams;
    if (info->function->isMethod) {
      cParams.push_back(std::make_tuple("_Alta_self", transpileType(info->function->parentClassType.get())));
    }
    for (size_t i = 0; i < info->function->parameterVariables.size(); i++) {
      auto& var = info->function->parameterVariables[i];
      auto& param = aFunc->parameters[i];
      auto& paramInfo = info->parameters[i];
      auto type = (param->isVariable) ? paramInfo->type->type->point() : paramInfo->type->type;
      auto mangled = mangleName(var.get());
      cParams.push_back({ (param->isVariable ? "_Alta_array_" : "") + mangled, transpileType(type.get()) });
      if (param->isVariable) {
        cParams.push_back({ "_Alta_array_length_" + mangled, size_tType });
      }
    }
    auto returnType = transpileType(info->function->returnType.get());

    source.insertFunctionDefinition(mangledFuncName, cParams, returnType);
    bool isMain = false;
    if (info->function->isLiteral && info->function->name == "main") {
      isMain = true;
    }
    if (isMain) {
      source.insertExpressionStatement(
        source.createFunctionCall(source.createFetch("_Alta_init_global_runtime"), {})
      );
      source.insertExpressionStatement(
        source.createFunctionCall(source.createFetch("atexit"), {
          source.createFetch("_Alta_unwind_global_runtime")
        })
      );
    }
    for (size_t i = 0; i < aFunc->body->statements.size(); i++) {
      auto& stmt = aFunc->body->statements[i];
      auto& stmtInfo = info->body->statements[i];
      transpile(stmt.get(), stmtInfo.get());
    }
    source.exitInsertionPoint();

    if (info->function->isExport || info->function->isMethod) {
      for (auto& hoistedType: info->function->publicHoistedFunctionalTypes) {
        defineFunctionalType(hoistedType, true);
      }
      auto mod = AltaCore::Util::getModule(info->function->parentScope.lock().get()).lock();
      auto mangledModName = mangleName(mod.get());
      headerPredeclaration("_ALTA_FUNCTION_" + mangledFuncName, mangledModName);
      header.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
      header.exitInsertionPoint();
    }
  } else if (nodeType == AltaNodeType::ExpressionStatement) {
    auto exprStmt = dynamic_cast<AAST::ExpressionStatement*>(node);
    auto info = dynamic_cast<DH::ExpressionStatement*>(_info);
    auto expr = transpile(exprStmt->expression.get(), info->expression.get());
    if (expr != nullptr) {
      source.insertExpressionStatement(expr);
    }
  } else if (nodeType == AltaNodeType::ReturnDirectiveNode) {
    auto retDir = dynamic_cast<AAST::ReturnDirectiveNode*>(node);
    auto info = dynamic_cast<DH::ReturnDirectiveNode*>(_info);
    std::shared_ptr<Ceetah::AST::Expression> expr = nullptr;
    if (retDir->expression != nullptr) {
      expr = transpile(retDir->expression.get(), info->expression.get());
      if (info->functionReturnType) {
        for (size_t i = 0; i < info->functionReturnType->referenceLevel(); i++) {
          expr = source.createPointer(expr);
        }
      }
    }
    source.insertReturnDirective(expr);
  } else if (nodeType == AltaNodeType::IntegerLiteralNode) {
    auto intLit = dynamic_cast<AAST::IntegerLiteralNode*>(node);
    return source.createIntegerLiteral(intLit->raw);
  } else if (nodeType == AltaNodeType::VariableDefinitionExpression) {
    auto varDef = dynamic_cast<AAST::VariableDefinitionExpression*>(node);
    auto info = dynamic_cast<DH::VariableDefinitionExpression*>(_info);

    auto mangledVarName = mangleName(info->variable.get());
    auto type = transpileType(info->variable->type.get());

    std::shared_ptr<CAST::Expression> init = nullptr;
    if (varDef->initializationExpression != nullptr) {
      init = transpile(varDef->initializationExpression.get(), info->initializationExpression.get());
      for (size_t i = 0; i < info->variable->type->referenceLevel(); i++) {
        init = source.createPointer(init);
      }
    }

    source.insertVariableDefinition(type, mangledVarName, init);

    // check whether the variable is contained
    // i.e. whether it's in a function (or a class, later once classes are added)
    // if it's not contained (i.e. it's a defined in a module root), then declare
    // it in the header
    if (
      !info->variable->parentScope.expired() &&
      !info->variable->parentScope.lock()->parentModule.expired()
    ) {
      // it's not contained, therefore, declare it in the header, as well
      auto mod = info->variable->parentScope.lock()->parentModule.lock();
      auto mangledModName = mangleName(mod.get());
      headerPredeclaration("_ALTA_VARIABLE_" + mangledVarName, mangledModName);
      header.insertVariableDeclaration(type, mangledVarName);
      header.exitInsertionPoint();
    } else if (AltaCore::Util::isInFunction(info->variable.get())) {
      // if it is contained in a function, we can return a reference to the newly defined variable
      return source.createPointer(source.createFetch(mangledVarName));
    }
  } else if (nodeType == AltaNodeType::Accessor) {
    auto acc = dynamic_cast<AAST::Accessor*>(node);
    auto info = dynamic_cast<DH::Accessor*>(_info);
    std::shared_ptr<CAST::Expression> result = nullptr;
    size_t refLevel = 0;
    if (info->narrowedTo) {
      if (info->accessesNamespace) {
        result = source.createFetch(mangleName(info->narrowedTo.get()));
      } else {
        auto tgt = transpile(acc->target.get(), info->target.get());
        if (info->targetType && info->targetType->pointerLevel() > 0) {
          for (size_t i = 0; i < info->targetType->pointerLevel(); i++) {
            tgt = source.createDereference(tgt);
          }
        }
        if (info->parentClassAccessors.find(info->narrowedToIndex) != info->parentClassAccessors.end()) {
          auto& parents = info->parentClassAccessors[info->narrowedToIndex];
          for (auto& parent: parents) {
            tgt = source.createAccessor(tgt, mangleName(parent.get()));
          }
        }
        result = source.createAccessor(tgt, mangleName(info->narrowedTo.get()));
      }
      auto ut = AltaCore::DET::Type::getUnderlyingType(info->narrowedTo);
      refLevel = ut->referenceLevel();
    } else if (info->readAccessor) {
      auto readAccFetch = source.createFetch(mangleName(info->readAccessor.get()));
      
      std::vector<std::shared_ptr<Ceetah::AST::Expression>> args;
      if (!info->accessesNamespace) {
        auto selfAlta = acc->target.get();
        auto selfInfo = info->target.get();
        auto self = transpile(selfAlta, selfInfo);
        if (info->parentClassAccessors.find(info->readAccessorIndex) != info->parentClassAccessors.end()) {
          auto& parents = info->parentClassAccessors[info->readAccessorIndex];
          for (auto& parent: parents) {
            self = source.createAccessor(self, mangleName(parent.get()));
          }
        }
        /*
        auto selfType = AltaCore::DET::Type::getUnderlyingType(selfAlta);
        for (size_t i = 0; i < selfType->referenceLevel(); i++) {
          self = source.createDereference(self);
        }
        */
        args.push_back(source.createPointer(self));
      }
      
      result = source.createFunctionCall(readAccFetch, args);
      refLevel = info->readAccessor->returnType->referenceLevel();
    }
    if (!result) {
      throw std::runtime_error("AHH, this accessor needs to be narrowed!");
    }
    for (size_t i = 0; i < refLevel; i++) {
      result = source.createDereference(result);
    }
    return result;
  } else if (nodeType == AltaNodeType::Fetch) {
    auto fetch = dynamic_cast<AAST::Fetch*>(node);
    auto info = dynamic_cast<DH::Fetch*>(_info);
    if (!info->narrowedTo) {
      throw std::runtime_error("AHH, this fetch needs to be narrowed!");
    }
    if (info->narrowedTo->nodeType() == AltaCore::DET::NodeType::Variable && info->narrowedTo->name == "this") {
      auto var = std::dynamic_pointer_cast<AltaCore::DET::Variable>(info->narrowedTo);
      if (!var->parentScope.expired() && !var->parentScope.lock()->parentClass.expired()) {
        return source.createDereference(source.createFetch("_Alta_self"));
      }
    }
    std::shared_ptr<CAST::Expression> cFetch = source.createFetch(mangleName(info->narrowedTo.get()));
    size_t refLevel = 0;
    bool findLevel = true;
    if (info->narrowedTo->nodeType() == AltaCore::DET::NodeType::Function) {
      auto func = std::dynamic_pointer_cast<AltaCore::DET::Function>(info->narrowedTo);
      if (func->isAccessor) {
        findLevel = false;
        cFetch = source.createFunctionCall(cFetch, {});
        refLevel = func->returnType->referenceLevel();
      }
    }
    if (findLevel) {
      auto ut = AltaCore::DET::Type::getUnderlyingType(info->narrowedTo);
      refLevel = ut->referenceLevel();
    }
    for (size_t i = 0; i < refLevel; i++) {
      cFetch = source.createDereference(cFetch);
    }
    return cFetch;
  } else if (nodeType == AltaNodeType::AssignmentExpression) {
    auto assign = dynamic_cast<AAST::AssignmentExpression*>(node);
    auto info = dynamic_cast<DH::AssignmentExpression*>(_info);
    return source.createAssignment(transpile(assign->target.get(), info->target.get()), transpile(assign->value.get(), info->value.get()));
  } else if (nodeType == AltaNodeType::BooleanLiteralNode) {
    auto boolLit = dynamic_cast<AAST::BooleanLiteralNode*>(node);
    if (boolLit->value) {
      return source.createFetch("_Alta_bool_true");
    } else {
      return source.createFetch("_Alta_bool_false");
    }
  } else if (nodeType == AltaNodeType::BinaryOperation) {
    auto binOp = dynamic_cast<AAST::BinaryOperation*>(node);
    auto info = dynamic_cast<DH::BinaryOperation*>(_info);
    // for now, we can just cast from one to the other, since they're
    // identical. however, if Alta ever introduces non-C binary operators,
    // or changes up the order of its OperatorType enum, this
    // will need to be changed. please take note of that!
    auto cOpType = (CAST::OperatorType)binOp->type;
    return source.createBinaryOperation(cOpType, transpile(binOp->left.get(), info->left.get()), transpile(binOp->right.get(), info->right.get()));
  } else if (nodeType == AltaNodeType::ImportStatement) {
    auto import = dynamic_cast<AAST::ImportStatement*>(node);
    auto info = dynamic_cast<DH::ImportStatement*>(_info);
    auto mangledParentName = mangleName(info->parentModule.get());
    auto mangleImportName = mangleName(info->importedModule.get());
    if (import->isAliased) {
      header.insertPreprocessorDefinition("_ALTA_MODULE_ALL_" + mangleImportName);
    } else {
      for (auto& item: info->importedItems) {
        auto def = headerMangle(item.get());
        if (def.empty()) continue;
        header.insertPreprocessorDefinition(def);
      }
    }
    header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangleImportName, CAST::InclusionType::Computed);
  } else if (nodeType == AltaNodeType::FunctionCallExpression) {
    auto call = dynamic_cast<AAST::FunctionCallExpression*>(node);
    auto info = dynamic_cast<DH::FunctionCallExpression*>(_info);
    std::vector<std::shared_ptr<CAST::Expression>> args;
    if (info->isMethodCall) {
      auto selfAlta = info->methodClassTarget.get();
      auto selfInfo = info->methodClassTargetInfo.get();
      auto self = transpile(selfAlta, selfInfo);
      auto selfType = AltaCore::DET::Type::getUnderlyingType(selfInfo);
      for (size_t i = 0; i < selfType->pointerLevel(); i++) {
        self = source.createDereference(self);
      }
      args.push_back(source.createPointer(self));
    }
    auto restArgs = processArgs(info->adjustedArguments, info->targetType->parameters);
    args.insert(args.end(), restArgs.begin(), restArgs.end());
    /*
    for (size_t i = 0; i < info->adjustedArguments.size(); i++) {
      auto& arg = info->adjustedArguments[i];
      if (auto solo = ALTACORE_VARIANT_GET_IF<std::pair<std::shared_ptr<AAST::ExpressionNode>, std::shared_ptr<DH::ExpressionNode>>>(&arg)) {
        auto& [soloArg, soloInfo] = *solo;
        auto [name, targetType, isVariable, id] = info->targetType->parameters[i];
        auto argType = AltaCore::DET::Type::getUnderlyingType(soloInfo.get());
        auto target = transpile(soloArg.get(), soloInfo.get());
        for (size_t j = 0; j < targetType->referenceLevel(); j++) {
          target = source.createPointer(target);
        }
        args.push_back(target);
      } else if (auto multi = ALTACORE_VARIANT_GET_IF<std::vector<std::pair<std::shared_ptr<AAST::ExpressionNode>, std::shared_ptr<DH::ExpressionNode>>>>(&arg)) {
        auto [name, targetType, isVariable, id] = info->targetType->parameters[i];
        if (varargTable[id]) {
          for (auto& [arg, info]: *multi) {
            args.push_back(transpile(arg.get(), info.get()));
          }
        } else {
          std::vector<std::shared_ptr<CAST::Expression>> arrItems;
          for (auto& [arg, info]: *multi) {
            arrItems.push_back(transpile(arg.get(), info.get()));
          }
          auto cType = transpileType(targetType.get());
          cType->arraySize = SIZE_MAX;
          args.push_back(source.createArrayLiteral(arrItems, cType));
          args.push_back(source.createIntegerLiteral((*multi).size()));
        }
      }
    }
    */
    std::shared_ptr<CAST::Expression> result = nullptr;
    auto refLevel = info->targetType->returnType->referenceLevel();
    if (info->isMethodCall) {
      auto acc = std::dynamic_pointer_cast<AAST::Accessor>(call->target);
      auto accInfo = std::dynamic_pointer_cast<DH::Accessor>(info->target);
      result = source.createFunctionCall(source.createFetch(mangleName(accInfo->narrowedTo.get())), args);
    } else {
      result = source.createFunctionCall(transpile(call->target.get(), info->target.get()), args);
    }
    for (size_t i = 0; i < refLevel; i++) {
      result = source.createDereference(result);
    }
    return result;
  } else if (nodeType == AltaNodeType::StringLiteralNode) {
    auto lit = dynamic_cast<AAST::StringLiteralNode*>(node);
    return source.createStringLiteral(lit->value);
  } else if (nodeType == AltaNodeType::AttributeStatement) {
    /*
    auto attr = dynamic_cast<AAST::AttributeStatement*>(node);
    auto info = dynamic_cast<DH::AttributeStatement*>(_info);
    attr->attribute->findAttribute(info->attribute);
    attr->attribute->run(info->attribute);
    */
  } else if (nodeType == AltaNodeType::BlockNode) {
    auto block = dynamic_cast<AAST::BlockNode*>(node);
    auto info = dynamic_cast<DH::BlockNode*>(_info);
    source.insertBlock();
    for (size_t i = 0; i < block->statements.size(); i++) {
      auto& stmt = block->statements[i];
      auto& stmtInfo = info->statements[i];
      transpile(stmt.get(), stmtInfo.get());
    }
    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ConditionalStatement) {
    auto cond = dynamic_cast<AAST::ConditionalStatement*>(node);
    auto info = dynamic_cast<DH::ConditionalStatement*>(_info);

    source.insertConditionalStatement(transpile(cond->primaryTest.get(), info->primaryTest.get()));
    transpile(cond->primaryResult.get(), info->primaryResult.get());

    for (size_t i = 0; i < cond->alternatives.size(); i++) {
      auto& [altTest, altResult] = cond->alternatives[i];
      auto& [testInfo, resultInfo] = info->alternatives[i];
      source.enterConditionalAlternative(i);
      source.insert(transpile(altTest.get(), testInfo.get()));
      transpile(altResult.get(), resultInfo.get());
    }

    if (cond->finalResult) {
      source.enterConditionalUltimatum();
      transpile(cond->finalResult.get(), info->finalResult.get());
    }

    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ConditionalExpression) {
    auto cond = dynamic_cast<AAST::ConditionalExpression*>(node);
    auto info = dynamic_cast<DH::ConditionalExpression*>(_info);
    return source.createTernaryOperation(transpile(cond->test.get(), info->test.get()), transpile(cond->primaryResult.get(), info->primaryResult.get()), transpile(cond->secondaryResult.get(), info->secondaryResult.get()));
  } else if (nodeType == AltaNodeType::ClassDefinitionNode) {
    auto aClass = dynamic_cast<AAST::ClassDefinitionNode*>(node);
    auto info = dynamic_cast<DH::ClassDefinitionNode*>(_info);

    for (auto& hoistedType: info->klass->hoistedFunctionalTypes) {
      defineFunctionalType(hoistedType);
    }

    std::vector<std::pair<std::string, std::shared_ptr<CAST::Type>>> members;
    members.emplace_back("_Alta_class_info_struct", source.createType("_Alta_class_info"));
    for (auto& parent: info->klass->parents) {
      auto mangledParent = mangleName(parent.get());
      members.emplace_back(mangledParent, source.createType(mangledParent));
    }
    for (auto item: info->klass->scope->items) {
      if (item->nodeType() == AltaCore::DET::NodeType::Variable && item->name != "this") {
        auto var = std::dynamic_pointer_cast<AltaCore::DET::Variable>(item);
        members.emplace_back(mangleName(var.get()), transpileType(var->type.get()));
      }
    }
    
    auto mod = AltaCore::Util::getModule(info->klass->parentScope.lock().get()).lock();
    auto mangledModName = mangleName(mod.get());
    auto mangledClassName = mangleName(info->klass.get());
    headerPredeclaration("_ALTA_CLASS_" + mangledClassName, mangledModName);
    header.insertStructureDefinition("_s_" + mangledClassName, members);
    header.insertTypeDefinition(mangledClassName, header.createType("_s_" + mangledClassName, {}, true));

    auto self = header.createType(mangledClassName, { { CAST::TypeModifierFlag::Pointer } });
    header.insertFunctionDeclaration("_init_" + mangledClassName, { std::make_tuple("_Alta_self", self) }, self);

    source.insertFunctionDefinition("_init_" + mangledClassName, { std::make_tuple("_Alta_self", self) }, self);

    source.insertExpressionStatement(
      source.createAssignment(
        source.createAccessor(
          source.createAccessor(
            source.createDereference(
              source.createFetch("_Alta_self")
            ),
            "_Alta_class_info_struct"
          ),
          "typeName"
        ),
        source.createStringLiteral(mangledClassName)
      )
    );

    enum class LoopKind {
      Members,
      All,
    };

    auto loop = [&](std::vector<std::shared_ptr<AAST::ClassStatementNode>>& tgt, std::vector<std::shared_ptr<DH::ClassStatementNode>> infos, LoopKind kind = LoopKind::All) -> void {
      for (size_t i = 0; i < tgt.size(); i++) {
        auto& stmt = tgt[i];
        auto& stmtInfo = infos[i];
        if (kind == LoopKind::Members && stmt->nodeType() == AAST::NodeType::ClassMemberDefinitionStatement) {
          auto member = std::dynamic_pointer_cast<AAST::ClassMemberDefinitionStatement>(stmt);
          auto memberInfo = std::dynamic_pointer_cast<DH::ClassMemberDefinitionStatement>(stmtInfo);
          auto mangledMemberName = mangleName(memberInfo->varDef->variable.get());
          if (member->varDef->initializationExpression) {
            source.insertExpressionStatement(source.createAssignment(source.createAccessor(source.createDereference(source.createFetch("_Alta_self")), mangledMemberName), transpile(member->varDef->initializationExpression.get(), memberInfo->varDef->initializationExpression.get())));
          }
        }
        if (kind == LoopKind::All && stmt->nodeType() == AAST::NodeType::ClassSpecialMethodDefinitionStatement) {
          auto special = std::dynamic_pointer_cast<AAST::ClassSpecialMethodDefinitionStatement>(stmt);
          auto specialInfo = std::dynamic_pointer_cast<DH::ClassSpecialMethodDefinitionStatement>(stmtInfo);
          if (special->type == AAST::SpecialClassMethod::Constructor) {
            transpile(special.get(), specialInfo.get());
          } else {
            throw std::runtime_error("destructors aren't supported yet");
          }
        } else if (kind == LoopKind::All) {
          transpile(stmt.get(), stmtInfo.get());
        }
      }
    };
    
    loop(aClass->statements, info->statements, LoopKind::Members);

    source.insertReturnDirective(source.createFetch("_Alta_self"));
    source.exitInsertionPoint();

    loop(aClass->statements, info->statements);

    if (info->createDefaultConstructor) {
      transpile(info->defaultConstructor.get(), info->defaultConstructorDetail.get());
    }

    header.exitInsertionPoint();
  } else if (nodeType == AAST::NodeType::ClassSpecialMethodDefinitionStatement) {
    auto special = dynamic_cast<AAST::ClassSpecialMethodDefinitionStatement*>(node);
    auto info = dynamic_cast<DH::ClassSpecialMethodDefinitionStatement*>(_info);
    auto mangledClassName = mangleName(info->klass.get());
    auto constr = info->method;
    auto mangledName = mangleName(constr.get());
    std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> params;
    for (size_t i = 0; i < constr->parameterVariables.size(); i++) {
      auto& var = constr->parameterVariables[i];
      auto& [name, ptype, isVariable, alias] = constr->parameters[i];
      auto type = (isVariable) ? ptype->point() : ptype;
      auto mangled = mangleName(var.get());
      params.push_back({ (isVariable ? "_Alta_array_" : "") + mangled, transpileType(type.get()) });
      if (isVariable) {
        params.push_back({ "_Alta_array_length_" + mangled, size_tType });
      }
    }
    auto ret = header.createType(mangledClassName);
    auto retPtr = header.createType(mangledClassName, { { CAST::TypeModifierFlag::Pointer } });
    decltype(params) fullParams;
    fullParams.push_back({ "_Alta_self", retPtr });
    fullParams.insert(fullParams.end(), params.begin(), params.end());
    header.insertFunctionDeclaration("_c_" + mangledName, fullParams, retPtr);
    source.insertFunctionDefinition("_c_" + mangledName, fullParams, retPtr);
    for (size_t i = 0; i < special->body->statements.size(); i++) {
      auto& stmt = special->body->statements[i];
      auto& stmtInfo = info->body->statements[i];
      transpile(stmt.get(), stmtInfo.get());
    }
    source.insertReturnDirective(source.createFetch("_Alta_self"));
    source.exitInsertionPoint();

    std::vector<std::shared_ptr<CAST::Expression>> args;
    for (auto param: constr->parameterVariables) {
      args.push_back(source.createFetch(mangleName(param.get())));
    }
    
    header.insertFunctionDeclaration("_cp_" + mangledName, params, retPtr);
    source.insertFunctionDefinition("_cp_" + mangledName, params, retPtr);
    source.insertVariableDefinition(retPtr, "_Alta_self", source.createFunctionCall(source.createFetch("malloc"), {
      source.createSizeof(ret)
    }));
    source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_init_" + mangledClassName), { source.createFetch("_Alta_self") }));
    std::vector<std::shared_ptr<CAST::Expression>> pArgs = { source.createFetch("_Alta_self") };
    pArgs.insert(pArgs.end(), args.begin(), args.end());
    source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_c_" + mangledName), pArgs));
    source.insertReturnDirective(source.createFetch("_Alta_self"));
    source.exitInsertionPoint();

    header.insertFunctionDeclaration("_cn_" + mangledName, params, ret);
    source.insertFunctionDefinition("_cn_" + mangledName, params, ret);
    source.insertVariableDefinition(ret, "_Alta_self", source.createArrayLiteral({ source.createIntegerLiteral(0) }));
    source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_init_" + mangledClassName), { source.createPointer(source.createFetch("_Alta_self")) }));
    std::vector<std::shared_ptr<CAST::Expression>> nArgs = { source.createPointer(source.createFetch("_Alta_self")) };
    nArgs.insert(nArgs.end(), args.begin(), args.end());
    source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_c_" + mangledName), nArgs));
    source.insertReturnDirective(source.createFetch("_Alta_self"));
    source.exitInsertionPoint();
  } else if (nodeType == AAST::NodeType::ClassInstantiationExpression) {
    auto call = dynamic_cast<AAST::ClassInstantiationExpression*>(node);
    auto info = dynamic_cast<DH::ClassInstantiationExpression*>(_info);
    auto args = processArgs(info->adjustedArguments, info->constructor->parameters);
    auto mangledClass = mangleName(info->klass.get());
    auto mangledCtor = mangleName(info->constructor.get());
    if (info->superclass) {
      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createDereference(
              source.createFetch("_Alta_self")
            ),
            mangledClass
          ),
          source.createFunctionCall(
            source.createFetch("_cn_" + mangledCtor),
            args
          )
        )
      );
      return source.createPointer(
        source.createAccessor(
          source.createDereference(
            source.createFetch("_Alta_self")
          ),
          mangledClass
        )
      );
    } else {
      return source.createFunctionCall(source.createFetch("_cn_" + mangledCtor), args);
    }
  } else if (nodeType == AAST::NodeType::ClassMethodDefinitionStatement) {
    auto method = dynamic_cast<AAST::ClassMethodDefinitionStatement*>(node);
    auto info = dynamic_cast<DH::ClassMethodDefinitionStatement*>(_info);
    header.insertPreprocessorDefinition(headerMangle(info->funcDef->function.get()));
    transpile(method->funcDef.get(), info->funcDef.get());
  } else if (nodeType == AAST::NodeType::PointerExpression) {
    auto ptr = dynamic_cast<AAST::PointerExpression*>(node);
    auto info = dynamic_cast<DH::PointerExpression*>(_info);
    return source.createPointer(transpile(ptr->target.get(), info->target.get()));
  } else if (nodeType == AAST::NodeType::DereferenceExpression) {
    auto deref = dynamic_cast<AAST::DereferenceExpression*>(node);
    auto info = dynamic_cast<DH::DereferenceExpression*>(_info);
    return source.createDereference(transpile(deref->target.get(), info->target.get()));
  } else if (nodeType == AAST::NodeType::WhileLoopStatement) {
    auto loop = dynamic_cast<AAST::WhileLoopStatement*>(node);
    auto info = dynamic_cast<DH::WhileLoopStatement*>(_info);
    source.insertWhileLoop(transpile(loop->test.get(), info->test.get()));
    transpile(loop->body.get(), info->body.get());
    source.exitInsertionPoint();
  } else if (nodeType == AAST::NodeType::CastExpression) {
    auto cast = dynamic_cast<AAST::CastExpression*>(node);
    auto info = dynamic_cast<DH::CastExpression*>(_info);
    return source.createCast(transpile(cast->target.get(), info->target.get()), transpileType(info->type->type.get()));
  } else if (nodeType == AAST::NodeType::CharacterLiteralNode) {
    auto lit = dynamic_cast<AAST::CharacterLiteralNode*>(node);
    return source.createCharacterLiteral(lit->value, lit->escaped);
  } else if (nodeType == AAST::NodeType::SubscriptExpression) {
    auto subs = dynamic_cast<AAST::SubscriptExpression*>(node);
    auto info = dynamic_cast<DH::SubscriptExpression*>(_info);
    auto cTarget = transpile(subs->target.get(), info->target.get());
    auto cIndex = transpile(subs->index.get(), info->index.get());
    /*
     * now, why transpile to an add-and-dereference, you ask?
     * i could tell you a bunch of crap about wanting to guarantee
     * compiler behavior in case the C compiler decides that
     * a subscript is not equal to an add-and-dereference, but the
     * real reason is that i just didn't feel like adding subscript
     * expression to Ceetah right now. so...
     */
    return source.createDereference(source.createBinaryOperation(CAST::OperatorType::Addition, cTarget, cIndex));
  } else if (nodeType == AAST::NodeType::SuperClassFetch) {
    auto sc = dynamic_cast<AAST::SuperClassFetch*>(node);
    auto info = dynamic_cast<DH::SuperClassFetch*>(_info);
    // since we return a reference,
    // we would have to dereference a pointer if we were to insert
    // one, so that's completely useless. just insert a regular
    // accessor. later code can create a pointer to it if necessary
    return source.createAccessor(
      source.createDereference(
        source.createFetch("_Alta_self")
      ),
      mangleName(info->superclass.get())
    );
  }
  return nullptr;
};
void Talta::CTranspiler::transpile(std::shared_ptr<AltaCore::AST::RootNode> altaRoot) {
  cRoot = std::make_shared<Ceetah::AST::RootNode>();
  hRoot = std::make_shared<Ceetah::AST::RootNode>();
  source = Ceetah::Builder(cRoot);
  header = Ceetah::Builder(hRoot);

  auto& info = altaRoot->info;

  auto mangledModuleName = mangleName(info->module.get());

  header.insertPreprocessorInclusion("_ALTA_RUNTIME_COMMON_HEADER_" + mangledModuleName, Ceetah::AST::InclusionType::Computed);

  header.insertPreprocessorConditional("!defined(_ALTA_MODULE_HEADER_" + mangledModuleName + ")");
  header.insertPreprocessorDefinition("_ALTA_MODULE_HEADER_" + mangledModuleName);

  for (auto& incl: moduleIncludes[info->module->path.toString()]) {
    header.insertPreprocessorInclusion(incl, Ceetah::AST::InclusionType::System);
  }

  for (size_t i = 0; i < altaRoot->statements.size(); i++) {
    auto& stmt = altaRoot->statements[i];
    auto& stmtInfo = info->statements[i];
    transpile(stmt.get(), stmtInfo.get());
  }

  header.exitInsertionPoint();

  header.insertPreprocessorUndefinition("_ALTA_MODULE_ALL_" + mangledModuleName);
};

std::map<std::string, std::tuple<std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<AltaCore::DET::Module>>> Talta::recursivelyTranspileToC(std::shared_ptr<AltaCore::AST::RootNode> altaRoot, CTranspiler* transpiler) {
  std::map<std::string, std::tuple<std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<AltaCore::DET::Module>>> results;

  bool deleteIt = false;
  if (transpiler == nullptr) {
    deleteIt = true;
    transpiler = new CTranspiler();
  }

  transpiler->transpile(altaRoot);
  results[altaRoot->info->module->name] = { transpiler->cRoot, transpiler->hRoot, altaRoot->info->module };
  for (auto& dep: altaRoot->info->dependencyASTs) {
    auto other = recursivelyTranspileToC(dep, transpiler);
    for (auto& [name, item]: other) {
      results[name] = item;
    }
  }

  if (deleteIt) {
    delete transpiler;
  }

  return results;
};

void Talta::registerAttributes(AltaCore::Filesystem::Path modulePath) {
  AltaCore::Attributes::registerAttribute({ "CTranspiler", "include" }, {}, [=](std::shared_ptr<AltaCore::AST::Node> target, std::shared_ptr<AltaCore::DH::Node> info, std::vector<AltaCore::Attributes::AttributeArgument> args) -> void {
    if (args.size() == 0) return;
    if (!args[0].isString) return;
    moduleIncludes[modulePath.toString()].push_back(args[0].string);
  }, modulePath.toString());
  AltaCore::Attributes::registerAttribute({ "CTranspiler", "vararg" }, {
    AltaCore::AST::NodeType::Parameter,
  }, [=](std::shared_ptr<AltaCore::AST::Node> target, std::shared_ptr<AltaCore::DH::Node> info, std::vector<AltaCore::Attributes::AttributeArgument> args) -> void {
    auto param = std::dynamic_pointer_cast<AltaCore::AST::Parameter>(target);
    if (!param) throw std::runtime_error("target was not of the expected type");
    varargTable[param->id] = true;
  }, modulePath.toString());
};
