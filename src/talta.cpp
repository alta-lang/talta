#include "../include/talta.hpp"
#include "../include/talta/util.hpp"

namespace Talta {
  std::map<std::string, std::vector<std::string>> moduleIncludes;
  std::unordered_map<std::string, bool> varargTable;
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
        return mangled ? "unsigned_int" : "unsigned int";
      case NT::Void:
        return "void";
      default:
        throw std::runtime_error("ok, wtaf.");
    }
  } else {
    throw std::runtime_error("wtf, no.");
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

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::transpile(AltaCore::AST::Node* node) {
  using AltaNodeType = AltaCore::AST::NodeType;
  namespace AAST = AltaCore::AST;
  namespace CAST = Ceetah::AST;
  AltaNodeType nodeType = node->nodeType();

  if (nodeType == AltaNodeType::FunctionDefinitionNode) {
    auto aFunc = dynamic_cast<AAST::FunctionDefinitionNode*>(node);

    if (!aFunc->$function->isExport) {
      for (auto& hoistedType: aFunc->$function->publicHoistedFunctionalTypes) {
        defineFunctionalType(hoistedType);
      }
    }

    for (auto& hoistedType: aFunc->$function->hoistedFunctionalTypes) {
      defineFunctionalType(hoistedType);
    }

    auto mangledFuncName = mangleName(aFunc->$function.get());
    std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> cParams;
    for (size_t i = 0; i < aFunc->$function->parameterVariables.size(); i++) {
      auto& var = aFunc->$function->parameterVariables[i];
      auto& param = aFunc->parameters[i];
      auto type = (param->isVariable) ? param->type->$type->point() : param->type->$type;
      auto mangled = mangleName(var.get());
      cParams.push_back({ (param->isVariable ? "_Alta_array_" : "") + mangled, transpileType(type.get()) });
      if (param->isVariable) {
        cParams.push_back({ "_Alta_array_length_" + mangled, size_tType });
      }
    }
    auto returnType = transpileType(aFunc->$function->returnType.get());

    source.insertFunctionDefinition(mangledFuncName, cParams, returnType);
    for (auto& stmt: aFunc->body->statements) {
      transpile(stmt.get());
    }
    source.exitInsertionPoint();

    if (aFunc->$function->isExport) {
      for (auto& hoistedType: aFunc->$function->publicHoistedFunctionalTypes) {
        defineFunctionalType(hoistedType, true);
      }
      auto mod = AltaCore::Util::getModule(aFunc->$function->parentScope.lock().get()).lock();
      auto mangledModName = mangleName(mod.get());
      headerPredeclaration("_ALTA_FUNCTION_" + mangledFuncName, mangledModName);
      header.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
      header.exitInsertionPoint();
    }
  } else if (nodeType == AltaNodeType::ExpressionStatement) {
    auto exprStmt = dynamic_cast<AAST::ExpressionStatement*>(node);
    auto expr = transpile(exprStmt->expression.get());
    if (expr != nullptr) {
      source.insertExpressionStatement(expr);
    }
  } else if (nodeType == AltaNodeType::ReturnDirectiveNode) {
    auto retDir = dynamic_cast<AAST::ReturnDirectiveNode*>(node);
    std::shared_ptr<Ceetah::AST::Expression> expr = nullptr;
    if (retDir->expression != nullptr) {
      expr = transpile(retDir->expression.get());
    }
    source.insertReturnDirective(expr);
  } else if (nodeType == AltaNodeType::IntegerLiteralNode) {
    auto intLit = dynamic_cast<AAST::IntegerLiteralNode*>(node);
    return source.createIntegerLiteral(intLit->raw);
  } else if (nodeType == AltaNodeType::VariableDefinitionExpression) {
    auto varDef = dynamic_cast<AAST::VariableDefinitionExpression*>(node);

    auto mangledVarName = mangleName(varDef->$variable.get());
    auto type = transpileType(varDef->$variable->type.get());

    std::shared_ptr<CAST::Expression> init = nullptr;
    if (varDef->initializationExpression != nullptr) {
      init = transpile(varDef->initializationExpression.get());
    }

    source.insertVariableDefinition(type, mangledVarName, init);

    // check whether the variable is contained
    // i.e. whether it's in a function (or a class, later once classes are added)
    // if it's not contained (i.e. it's a defined in a module root), then declare
    // it in the header
    if (
      !varDef->$variable->parentScope.expired() &&
      !varDef->$variable->parentScope.lock()->parentModule.expired()
    ) {
      // it's not contained, therefore, declare it in the header, as well
      auto mod = varDef->$variable->parentScope.lock()->parentModule.lock();
      auto mangledModName = mangleName(mod.get());
      headerPredeclaration("_ALTA_VARIABLE_" + mangledVarName, mangledModName);
      header.insertVariableDeclaration(type, mangledVarName);
      header.exitInsertionPoint();
    } else if (AltaCore::Util::isInFunction(varDef->$variable.get())) {
      // if it is contained in a function, we can return a reference to the newly defined variable
      return source.createPointer(source.createFetch(mangledVarName));
    }
  } else if (nodeType == AltaNodeType::Accessor) {
    auto acc = dynamic_cast<AAST::Accessor*>(node);
    if (!acc->$narrowedTo) {
      throw std::runtime_error("AHH, this accessor needs to be narrowed!");
    }
    if (acc->accessesNamespace) {
      return source.createFetch(mangleName(acc->$narrowedTo.get()));
    } else {
      return source.createAccessor(transpile(acc->target.get()), mangleName(acc->$narrowedTo.get()));
    }
  } else if (nodeType == AltaNodeType::Fetch) {
    auto fetch = dynamic_cast<AAST::Fetch*>(node);
    if (!fetch->$narrowedTo) {
      throw std::runtime_error("AHH, this fetch needs to be narrowed!");
    }
    return source.createFetch(mangleName(fetch->$narrowedTo.get()));
  } else if (nodeType == AltaNodeType::AssignmentExpression) {
    auto assign = dynamic_cast<AAST::AssignmentExpression*>(node);
    return source.createAssignment(transpile(assign->target.get()), transpile(assign->value.get()));
  } else if (nodeType == AltaNodeType::BooleanLiteralNode) {
    auto boolLit = dynamic_cast<AAST::BooleanLiteralNode*>(node);
    if (boolLit->value) {
      return source.createIntegerLiteral(1);
    } else {
      return source.createIntegerLiteral(0);
    }
  } else if (nodeType == AltaNodeType::BinaryOperation) {
    auto binOp = dynamic_cast<AAST::BinaryOperation*>(node);
    // for now, we can just cast from one to the other, since they're
    // identical. however, if Alta ever introduces non-C binary operators,
    // this will need to be changed. please take note of that!
    auto cOpType = (CAST::OperatorType)binOp->type;
    return source.createBinaryOperation(cOpType, transpile(binOp->left.get()), transpile(binOp->right.get()));
  } else if (nodeType == AltaNodeType::ImportStatement) {
    auto import = dynamic_cast<AAST::ImportStatement*>(node);
    auto mangledParentName = mangleName(import->$parentModule.get());
    auto mangleImportName = mangleName(import->$importedModule.get());
    if (import->isAliased) {
      header.insertPreprocessorDefinition("_ALTA_MODULE_ALL_" + mangleImportName);
    } else {
      for (auto& item: import->$importedItems) {
        header.insertPreprocessorDefinition(headerMangle(item.get()));
      }
    }
    header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangleImportName, CAST::InclusionType::Computed);
  } else if (nodeType == AltaNodeType::FunctionCallExpression) {
    auto call = dynamic_cast<AAST::FunctionCallExpression*>(node);
    std::vector<std::shared_ptr<CAST::Expression>> args;
    for (size_t i = 0; i < call->$adjustedArguments.size(); i++) {
      auto& arg = call->$adjustedArguments[i];
      if (auto solo = ALTACORE_VARIANT_GET_IF<std::shared_ptr<AAST::ExpressionNode>>(&arg)) {
        args.push_back(transpile((*solo).get()));
      } else if (auto multi = ALTACORE_VARIANT_GET_IF<std::vector<std::shared_ptr<AAST::ExpressionNode>>>(&arg)) {
        auto [name, targetType, isVariable, id] = call->$targetType->parameters[i];
        if (varargTable[id]) {
          for (auto& item: *multi) {
            args.push_back(transpile(item.get()));
          }
        } else {
          std::vector<std::shared_ptr<CAST::Expression>> arrItems;
          for (auto& item: *multi) {
            arrItems.push_back(transpile(item.get()));
          }
          auto cType = transpileType(targetType.get());
          cType->arraySize = SIZE_MAX;
          args.push_back(source.createArrayLiteral(arrItems, cType));
          args.push_back(source.createIntegerLiteral((*multi).size()));
        }
      }
    }
    return source.createFunctionCall(transpile(call->target.get()), args);
  } else if (nodeType == AltaNodeType::StringLiteralNode) {
    auto lit = dynamic_cast<AAST::StringLiteralNode*>(node);
    return source.createStringLiteral(lit->value);
  } else if (nodeType == AltaNodeType::AttributeStatement) {
    auto attr = dynamic_cast<AAST::AttributeStatement*>(node);
    attr->attribute->findAttribute();
    attr->attribute->run();
  } else if (nodeType == AltaNodeType::BlockNode) {
    auto block = dynamic_cast<AAST::BlockNode*>(node);
    source.insertBlock();
    for (auto& stmt: block->statements) {
      transpile(stmt.get());
    }
    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ConditionalStatement) {
    auto cond = dynamic_cast<AAST::ConditionalStatement*>(node);

    source.insertConditionalStatement(transpile(cond->primaryTest.get()));
    transpile(cond->primaryResult.get());

    for (size_t i = 0; i < cond->alternatives.size(); i++) {
      auto& [altTest, altResult] = cond->alternatives[i];
      source.enterConditionalAlternative(i);
      source.insert(transpile(altTest.get()));
      transpile(altResult.get());
    }

    if (cond->finalResult) {
      source.enterConditionalUltimatum();
      transpile(cond->finalResult.get());
    }

    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ConditionalExpression) {
    auto cond = dynamic_cast<AAST::ConditionalExpression*>(node);
    return source.createTernaryOperation(transpile(cond->test.get()), transpile(cond->primaryResult.get()), transpile(cond->secondaryResult.get()));
  }
  return nullptr;
};
void Talta::CTranspiler::transpile(std::shared_ptr<AltaCore::AST::RootNode> altaRoot) {
  cRoot = std::make_shared<Ceetah::AST::RootNode>();
  hRoot = std::make_shared<Ceetah::AST::RootNode>();
  source = Ceetah::Builder(cRoot);
  header = Ceetah::Builder(hRoot);

  auto mangledModuleName = mangleName(altaRoot->$module.get());

  header.insertPreprocessorInclusion("_ALTA_RUNTIME_COMMON_HEADER_" + mangledModuleName, Ceetah::AST::InclusionType::Computed);

  header.insertPreprocessorConditional("!defined(_ALTA_MODULE_HEADER_" + mangledModuleName + ")");
  header.insertPreprocessorDefinition("_ALTA_MODULE_HEADER_" + mangledModuleName);

  for (auto& incl: moduleIncludes[altaRoot->$module->path.toString()]) {
    header.insertPreprocessorInclusion(incl, Ceetah::AST::InclusionType::System);
  }

  for (auto& stmt: altaRoot->statements) {
    transpile(stmt.get());
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
  results[altaRoot->$module->name] = { transpiler->cRoot, transpiler->hRoot, altaRoot->$module };
  for (auto& dep: altaRoot->$dependencyASTs) {
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
  AltaCore::Attributes::registerAttribute({ "CTranspiler", "include" }, {}, [=](std::shared_ptr<AltaCore::AST::Node> target, std::vector<AltaCore::Attributes::AttributeArgument> args) -> void {
    if (args.size() == 0) return;
    if (!args[0].isString) return;
    moduleIncludes[modulePath.toString()].push_back(args[0].string);
  }, modulePath.toString());
  AltaCore::Attributes::registerAttribute({ "CTranspiler", "vararg" }, {
    AltaCore::AST::NodeType::Parameter,
  }, [=](std::shared_ptr<AltaCore::AST::Node> target, std::vector<AltaCore::Attributes::AttributeArgument> args) -> void {
    auto param = std::dynamic_pointer_cast<AltaCore::AST::Parameter>(target);
    if (!param) throw std::runtime_error("target was not of the expected type");
    varargTable[param->id] = true;
  }, modulePath.toString());
};
