#include "../include/talta.hpp"
#include "../include/talta/util.hpp"

std::string Talta::CTranspiler::cTypeNameify(AltaCore::DET::Type* type) {
  using NT = AltaCore::DET::NativeType;
  if (type->isNative) {
    switch (type->nativeTypeName) {
      case NT::Integer:
        return "int";
      case NT::Byte:
        return "char";
      default:
        throw std::runtime_error("ok, wtaf.");
    }
  } else {
    throw std::runtime_error("wtf, no.");
  }
};

std::string Talta::mangleType(AltaCore::DET::Type* type) {
  using TypeModifier = AltaCore::Shared::TypeModifierFlag;
  using NT = AltaCore::DET::NativeType;
  std::string mangled;

  for (auto& mod: type->modifiers) {
    if (mod & (uint8_t)TypeModifier::Constant) {
      mangled += "const_3_";
    }
    if (mod & (uint8_t)TypeModifier::Pointer || mod & (uint8_t)TypeModifier::Reference) {
      mangled += "ptr_3_";
    }
  }

  if (type->isNative) {
    std::string nativeTypeName;
    switch (type->nativeTypeName) {
      case NT::Integer:
        nativeTypeName = "int";
        break;
      case NT::Byte:
        nativeTypeName = "char";
        break;
      default:
        throw std::runtime_error("ok, wtaf.");
    }
    mangled += nativeTypeName;
  } else {
    throw std::runtime_error("wtf, no.");
  }

  return mangled;
};

std::string Talta::escapeName(std::string name) {
  std::string escaped;
  
  /**
   * Escapes 0-10 are reserved for internal use.
   * `_0_` is reserved for scope item name separation
   * `_1_` is reserved for function parameter type separation
   * `_2_` is reserved for generic instantiation argument separation (TBD later)
   * `_3_` is reserved for type modifier separation
   * `_4_` is reserved for scope identifier delimination
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

std::string Talta::mangleName(AltaCore::DET::Module* mod, bool fullName) {
  return escapeName(mod->name);
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
        itemName += "_1_" + mangleType(type);
      }
    }
  } else if (nodeType == NodeType::Variable) {
    auto var = dynamic_cast<DET::Variable*>(item);
    itemName = var->name;
    isLiteral = var->isLiteral;
  }

  if (!isLiteral && fullName) {
    auto scope = item->parentScope;
    while (scope != nullptr) {
      if (scope->parentModule != nullptr) {
        mangled = mangleName(scope->parentModule) + "_0_" + mangled;
        scope = nullptr; // modules are root nodes, stop because we found one
      } else if (scope->parentFunction != nullptr) {
        mangled = mangleName(scope->parentFunction) + "_0_" + mangled;
        scope = scope->parentFunction->parentScope;
      } else if (scope->parent != nullptr) {
        mangled = "_4_" + std::to_string(scope->relativeID) + "_0_" + mangled;
        scope = scope->parent;
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

Ceetah::AST::Type* Talta::CTranspiler::transpileType(AltaCore::DET::Type* type) {
  auto mods = type->modifiers;

  // translate certain Alta-only modifiers to C-compatible modifiers
  //
  // for example, Alta contains both pointers and references. in our
  // C implementation, references are implemented as pointers, so
  // we can just change references to pointers
  for (auto& mod: mods) {
    if (mod & (uint8_t)AltaCore::Shared::TypeModifierFlag::Reference) {
      mod &= ~(uint8_t)AltaCore::Shared::TypeModifierFlag::Reference;
      mod |= (uint8_t)Ceetah::AST::TypeModifierFlag::Pointer;
    }
  }

  return source.createType(cTypeNameify(type), mods);
};

Ceetah::AST::Expression* Talta::CTranspiler::transpile(AltaCore::AST::Node* node) {
  using AltaNodeType = AltaCore::AST::NodeType;
  namespace AAST = AltaCore::AST;
  namespace CAST = Ceetah::AST;
  AltaNodeType nodeType = node->nodeType();

  if (nodeType == AltaNodeType::FunctionDefinitionNode) {
    auto aFunc = dynamic_cast<AAST::FunctionDefinitionNode*>(node);
    auto mangledFuncName = mangleName(aFunc->$function);
    std::vector<std::tuple<std::string, CAST::Type*>> cParams;
    for (auto& [name, type]: aFunc->$function->parameters) {
      cParams.push_back({ name, transpileType(type) });
    }
    auto returnType = transpileType(aFunc->$function->returnType);
    
    source.insertFunctionDefinition(mangledFuncName, cParams, returnType);
    for (auto& stmt: aFunc->body->statements) {
      transpile(stmt);
    }
    source.exitInsertionPoint();

    auto def = "_ALTA_FUNCTION_" + mangledFuncName;
    header.insertPreprocessorConditional("!defined(" + def + ")");
    header.insertPreprocessorDefinition(def);
    header.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
    header.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ExpressionStatement) {
    auto exprStmt = dynamic_cast<AAST::ExpressionStatement*>(node);
    auto expr = transpile(exprStmt->expression);
    if (expr != nullptr) {
      source.insertExpressionStatement(expr);
    }
  } else if (nodeType == AltaNodeType::ReturnDirectiveNode) {
    auto retDir = dynamic_cast<AAST::ReturnDirectiveNode*>(node);
    Ceetah::AST::Expression* expr = nullptr;
    if (retDir->expression != nullptr) {
      expr = transpile(retDir->expression);
    }
    source.insertReturnDirective(expr);
  } else if (nodeType == AltaNodeType::IntegerLiteralNode) {
    auto intLit = dynamic_cast<AAST::IntegerLiteralNode*>(node);
    return source.createIntegerLiteral(intLit->raw);
  } else if (nodeType == AltaNodeType::VariableDefinitionExpression) {
    auto varDef = dynamic_cast<AAST::VariableDefinitionExpression*>(node);

    auto mangledVarName = mangleName(varDef->$variable);
    auto type = transpileType(varDef->$variable->type);

    CAST::Expression* init = nullptr;
    if (varDef->initializationExpression != nullptr) {
      init = transpile(varDef->initializationExpression);
    }

    source.insertVariableDefinition(type, mangledVarName, init);

    // check whether the variable is contained
    // i.e. whether it's in a function (or a class, later once classes are added)
    // if it's not contained (i.e. it's a defined in a module root), then declare
    // it in the header
    if (
      varDef->$variable->parentScope != nullptr &&
      varDef->$variable->parentScope->parentModule != nullptr
    ) {
      // it's not contained, therefore, declare it in the header, as well
      auto def = "_ALTA_VARIABLE_" + mangledVarName;
      header.insertPreprocessorConditional("!defined(" + def + ")");
      header.insertPreprocessorDefinition(def);
      header.insertVariableDeclaration(type, mangledVarName);
      header.exitInsertionPoint();
    }
  } else if (nodeType == AltaNodeType::Accessor) {
    auto acc = dynamic_cast<AAST::Accessor*>(node);
    return header.createAccessor(transpile(acc->target), mangleName(acc->$item));
  } else if (nodeType == AltaNodeType::Fetch) {
    auto fetch = dynamic_cast<AAST::Fetch*>(node);
    return header.createFetch(mangleName(fetch->$item));
  }
  return nullptr;
};
void Talta::CTranspiler::transpile(AltaCore::AST::RootNode* altaRoot) {
  cRoot = new Ceetah::AST::RootNode();
  hRoot = new Ceetah::AST::RootNode();
  source = Ceetah::Builder(cRoot);
  header = Ceetah::Builder(hRoot);

  for (auto& stmt: altaRoot->statements) {
    transpile(stmt);
  }
};

std::map<std::string, std::tuple<Ceetah::AST::RootNode*, Ceetah::AST::RootNode*, AltaCore::DET::Module*>> Talta::recursivelyTranspileToC(AltaCore::AST::RootNode* altaRoot) {
  std::map<std::string, std::tuple<Ceetah::AST::RootNode*, Ceetah::AST::RootNode*, AltaCore::DET::Module*>> results;

  AltaCore::AST::RootNode* currentRoot = altaRoot;
  CTranspiler transpiler;
  while (currentRoot != nullptr) {
    transpiler.transpile(currentRoot);
    results[currentRoot->$module->name] = { transpiler.cRoot, transpiler.hRoot, currentRoot->$module };
    currentRoot = currentRoot->parent;
  }

  return results;
};
