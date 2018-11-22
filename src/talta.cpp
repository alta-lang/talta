#include "../include/talta.hpp"
#include "../include/talta/util.hpp"

std::string Talta::cTypeNameify(AltaCore::DET::Type* type) {
  using NT = AltaCore::DET::NativeType;
  if (type->isFunction) {
    std::string result = "_Alta_func_ptr_" + mangleType(type->returnType.get());
    for (auto& param: type->parameters) {
      result += '_';
      result += mangleType(param.get());
    }
    return result;
  } else if (type->isNative) {
    switch (type->nativeTypeName) {
      case NT::Integer:
        return "int";
      case NT::Byte:
        return "char";
      case NT::Bool:
        return "unsigned int";
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

  mangled += cTypeNameify(type);

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
      for (auto& [name, type]: func->parameters) {
        itemName += "_1_" + mangleType(type.get());
      }
    }
  } else if (nodeType == NodeType::Variable) {
    auto var = dynamic_cast<DET::Variable*>(item);
    itemName = var->name;
    isLiteral = var->isLiteral;
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

void Talta::CTranspiler::defineFunctionalType(std::shared_ptr<AltaCore::DET::Type> type) {
  if (!type->isFunction) return;

  auto name = cTypeNameify(type.get());
  auto mods = convertTypeModifiers(type->modifiers);

  auto def = "_ALTA_FUNC_PTR_" + name.substr(15);
  source.insertPreprocessorConditional("!defined(" + def + ")");
  source.insertPreprocessorDefinition(def);
  std::vector<std::shared_ptr<Ceetah::AST::Type>> cParams;
  for (auto& param: type->parameters) {
    cParams.push_back(transpileType(param.get()));
  }
  source.insertTypeDefinition(name, source.createType(transpileType(type->returnType.get()), cParams, mods));
  source.exitInsertionPoint();
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::transpile(AltaCore::AST::Node* node) {
  using AltaNodeType = AltaCore::AST::NodeType;
  namespace AAST = AltaCore::AST;
  namespace CAST = Ceetah::AST;
  AltaNodeType nodeType = node->nodeType();

  if (nodeType == AltaNodeType::FunctionDefinitionNode) {
    auto aFunc = dynamic_cast<AAST::FunctionDefinitionNode*>(node);

    for (auto& hoistedType: aFunc->$function->hoistedFunctionalTypes) {
      defineFunctionalType(hoistedType);
    }

    auto mangledFuncName = mangleName(aFunc->$function.get());
    std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> cParams;
    for (auto& var: aFunc->$function->parameterVariables) {
      cParams.push_back({ mangleName(var.get()), transpileType(var->type.get()) });
    }
    auto returnType = transpileType(aFunc->$function->returnType.get());
    
    source.insertFunctionDefinition(mangledFuncName, cParams, returnType);
    for (auto& stmt: aFunc->body->statements) {
      transpile(stmt.get());
    }
    source.exitInsertionPoint();

    auto mod = AltaCore::Util::getModule(aFunc->$function->parentScope.lock().get()).lock();
    auto mangledModName = mangleName(mod.get());
    headerPredeclaration("_ALTA_FUNCTION_" + mangledFuncName, mangledModName);
    header.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
    header.exitInsertionPoint();
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
    return source.createAccessor(transpile(acc->target.get()), mangleName(acc->$item.get()));
  } else if (nodeType == AltaNodeType::Fetch) {
    auto fetch = dynamic_cast<AAST::Fetch*>(node);
    if (!fetch->$narrowedTo) {
      throw std::runtime_error("AHH, this fetch needs to be narrowed!");
    }
    /*
    if (fetch->$narrowedTo->nodeType() == AltaCore::DET::NodeType::Function) {
      return source.createPointer(source.createFetch(mangleName(fetch->$narrowedTo.get())));
    } else {
      return source.createFetch(mangleName(fetch->$narrowedTo.get()));
    }
    */
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
    for (auto& arg: call->arguments) {
      args.push_back(transpile(arg.get()));
    }
    return source.createFunctionCall(transpile(call->target.get()), args);
  }
  return nullptr;
};
void Talta::CTranspiler::transpile(std::shared_ptr<AltaCore::AST::RootNode> altaRoot) {
  cRoot = std::make_shared<Ceetah::AST::RootNode>();
  hRoot = std::make_shared<Ceetah::AST::RootNode>();
  source = Ceetah::Builder(cRoot);
  header = Ceetah::Builder(hRoot);

  for (auto& stmt: altaRoot->statements) {
    transpile(stmt.get());
  }
  header.insertPreprocessorUndefinition("_ALTA_MODULE_ALL_" + mangleName(altaRoot->$module.get()));
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
