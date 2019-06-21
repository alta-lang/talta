#include "../include/talta.hpp"
#include "../include/talta/util.hpp"

namespace Talta {
  ALTACORE_MAP<std::string, std::vector<std::string>> moduleIncludes;
  ALTACORE_MAP<std::string, bool> varargTable;
  ALTACORE_MAP<std::string, size_t> tempVarIDs;

  std::shared_ptr<AltaCore::DET::ScopeItem> followAlias(std::shared_ptr<AltaCore::DET::ScopeItem> maybeAlias) {
    while (auto alias = std::dynamic_pointer_cast<AltaCore::DET::Alias>(maybeAlias)) {
      maybeAlias = alias->target;
    }
    return maybeAlias;
  };
  AltaCore::DET::ScopeItem* followAlias(AltaCore::DET::ScopeItem* maybeAlias) {
    while (auto alias = dynamic_cast<AltaCore::DET::Alias*>(maybeAlias)) {
      maybeAlias = alias->target.get();
    }
    return maybeAlias;
  };
};

std::vector<std::shared_ptr<Ceetah::AST::Expression>> Talta::CTranspiler::processArgs(std::vector<ALTACORE_VARIANT<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>, std::vector<std::pair<std::shared_ptr<AltaCore::AST::ExpressionNode>, std::shared_ptr<AltaCore::DH::ExpressionNode>>>>> adjustedArguments, std::vector<std::tuple<std::string, std::shared_ptr<AltaCore::DET::Type>, bool, std::string>> parameters) {
    namespace CAST = Ceetah::AST;
    namespace AAST = AltaCore::AST;
    namespace DH = AltaCore::DH;
    std::vector<std::shared_ptr<Ceetah::AST::Expression>> args;
    for (size_t i = 0; i < adjustedArguments.size(); i++) {
      auto& arg = adjustedArguments[i];
      auto [name, targetType, isVariable, id] = parameters[i];
      if (auto solo = ALTACORE_VARIANT_GET_IF<std::pair<std::shared_ptr<AAST::ExpressionNode>, std::shared_ptr<DH::ExpressionNode>>>(&arg)) {
        auto& [arg, info] = *solo;
        auto transpiled = transpile(arg.get(), info.get());

        bool didRetrieval = false;
        auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
        auto val = doParentRetrieval(transpiled, exprType, targetType, &didRetrieval);
        bool canCopy = targetType->indirectionLevel() < 1;
        if (didRetrieval && canCopy) {
          val = doCopyCtor(val, targetType);
        } else if (canCopy) {
          val = doCopyCtor(val, arg, info);
        }
        for (size_t i = 0; i < targetType->referenceLevel(); i++) {
          val = source.createPointer(val);
        }
        args.push_back(val);
      } else if (auto multi = ALTACORE_VARIANT_GET_IF<std::vector<std::pair<std::shared_ptr<AAST::ExpressionNode>, std::shared_ptr<DH::ExpressionNode>>>>(&arg)) {
        if (varargTable[id]) {
          for (auto& [arg, info]: *multi) {
            auto transpiled = transpile(arg.get(), info.get());
            bool didRetrieval = false;
            auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
            auto val = doParentRetrieval(transpiled, exprType, targetType, &didRetrieval);
            bool canCopy = targetType->indirectionLevel() < 1;
            if (didRetrieval && canCopy) {
              val = doCopyCtor(val, targetType);
            } else if (canCopy) {
              val = doCopyCtor(val, arg, info);
            }
            for (size_t i = 0; i < targetType->referenceLevel(); i++) {
              val = source.createPointer(val);
            }
            args.push_back(val);
          }
        } else {
          std::vector<std::shared_ptr<CAST::Expression>> arrItems;
          for (auto& [arg, info]: *multi) {
            auto transpiled = transpile(arg.get(), info.get());
            bool didRetrieval = false;
            auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
            auto val = doParentRetrieval(transpiled, exprType, targetType, &didRetrieval);
            bool canCopy = targetType->indirectionLevel() < 1;
            if (didRetrieval && canCopy) {
              val = doCopyCtor(val, targetType);
            } else if (canCopy) {
              val = doCopyCtor(val, arg, info);
            }
            for (size_t i = 0; i < targetType->referenceLevel(); i++) {
              val = source.createPointer(val);
            }
            arrItems.push_back(val);
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
      case NT::Double:
        return "double";
      case NT::Float:
        return "float";
      case NT::UserDefined:
        return type->userDefinedName;
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
    if (mod & (uint8_t)TypeModifier::Long) {
      mangled += "long_3_";
    }
    if (mod & (uint8_t)TypeModifier::Short) {
      mangled += "short_3_";
    }
    if (mod & (uint8_t)TypeModifier::Unsigned) {
      mangled += "unsigned_3_";
    }
    if (mod & (uint8_t)TypeModifier::Signed) {
      mangled += "signed_3_";
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
   * `_2_` is reserved for generic instantiation argument separation
   * `_3_` is reserved for type modifier separation
   * `_4_` is reserved for scope identifier delimination
   * `_5_` is reserved for version delimitation
   * `_6_` is reserved for version prerelease delimination
   * `_7_` is reserved for version build information delimination
   * `_8_` is reserved for variable function parameter type separation
   * `_9_` is reserved for parameter name separation
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
  item = followAlias(item);

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
  item = followAlias(item);

  using NodeType = AltaCore::DET::NodeType;
  namespace DET = AltaCore::DET;
  auto nodeType = item->nodeType();
  std::string itemName;
  auto isLiteral = false;
  std::string mangled;

  if (nodeType == NodeType::Function) {
    auto func = dynamic_cast<DET::Function*>(item);
    itemName = func->name;
    for (auto arg: func->genericArguments) {
      itemName += "_2_" + mangleType(arg.get());
    }
    isLiteral = func->isLiteral;

    if (!isLiteral) {
      itemName = escapeName(itemName);
      for (auto& [name, type, isVariable, id]: func->parameters) {
        itemName += "_9_" + escapeName(name) + ((isVariable) ? "_8_" : "_1_") + mangleType(type.get());
      }
    }
  } else if (nodeType == NodeType::Variable) {
    auto var = dynamic_cast<DET::Variable*>(item);
    itemName = var->name;
    if (auto ps = var->parentScope.lock()) {
      if (auto pc = ps->parentClass.lock()) {
        isLiteral = pc->isLiteral;
      }
    }
    isLiteral = isLiteral || var->isLiteral;
    if (var->isVariable) {
      mangled += "_Alta_array_";
    }
  } else if (nodeType == NodeType::Namespace) {
    auto ns = dynamic_cast<DET::Namespace*>(item);
    itemName = ns->name;
    isLiteral = false;
  } else if (nodeType == NodeType::Class) {
    auto klass = dynamic_cast<DET::Class*>(item);
    itemName = klass->name;
    for (auto arg: klass->genericArguments) {
      itemName += "_2_" + mangleType(arg.get());
    }
    isLiteral = klass->isLiteral;
  }

  if (!isLiteral && fullName) {
    auto maybeScope = item->parentScope;
    while (!maybeScope.expired()) {
      auto scope = maybeScope.lock();
      if (!scope->parentModule.expired()) {
        mangled += mangleName(scope->parentModule.lock().get()) + "_0_" + mangled;
        maybeScope = std::weak_ptr<DET::Scope>(); // modules are root nodes, stop because we found one
      } else if (!scope->parentFunction.expired()) {
        mangled += mangleName(scope->parentFunction.lock().get()) + "_0_" + mangled;
        maybeScope = scope->parentFunction.lock()->parentScope;
      } else if (!scope->parent.expired()) {
        mangled += "_4_" + std::to_string(scope->relativeID) + "_0_" + mangled;
        maybeScope = scope->parent;
      } else if (!scope->parentNamespace.expired()) {
        mangled += mangleName(scope->parentNamespace.lock().get()) + "_0_" + mangled;
        maybeScope = scope->parentNamespace.lock()->parentScope;
      } else if (!scope->parentClass.expired()) {
        mangled += mangleName(scope->parentClass.lock().get()) + "_0_" + mangled;
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
  return source.createType(cTypeNameify(type), convertTypeModifiers(type->modifiers), !type->isNative && !type->klass->isTyped);
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

void Talta::CTranspiler::headerPredeclaration(std::string def, std::string mangledModName, bool includeAll) {
  header.insertPreprocessorConditional("(defined(" + def + ")" + (includeAll ? (" || defined(_ALTA_MODULE_ALL_" + mangledModName + ')') : "") + ") && !defined(_DEFINED_" + def + ')');
  header.insertPreprocessorUndefinition(def);
  header.insertPreprocessorDefinition("_DEFINED_" + def);
  insertExportDefinition(def);
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

void Talta::CTranspiler::includeGeneric(std::shared_ptr<AltaCore::DET::ScopeItem> generic, bool inHeader) {
  if (generic->genericParameterCount < 1) return;

  auto& target = inHeader ? header : source;

  auto importModule = AltaCore::Util::getModule(generic->parentScope.lock().get()).lock();
  auto mangledImportName = mangleName(importModule.get());
  auto mangledParentName = mangleName(currentModule.get());

  target.insertPreprocessorDefinition(headerMangle(generic.get()));

  saveExportDefinitions(inHeader);

  target.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangledImportName, Ceetah::AST::InclusionType::Computed);

  restoreExportDefinitions(inHeader);
};

void Talta::CTranspiler::stackBookkeepingStart(std::shared_ptr<AltaCore::DET::Scope> scope) {
  source.insertVariableDefinition(
    source.createType("size_t"),
    mangleName(scope.get()) + "_stack_index",
    source.createAccessor(
      source.createAccessor(
        source.createFetch("_Alta_global_runtime"),
        "local"
      ),
      "nodeCount"
    )
  );
};

void Talta::CTranspiler::stackBookkeepingStop(std::shared_ptr<AltaCore::DET::Scope> scope) {
  source.insertExpressionStatement(
    source.createFunctionCall(source.createFetch("_Alta_object_stack_unwind"), {
      source.createPointer(
        source.createAccessor(
          source.createFetch("_Alta_global_runtime"),
          "local"
        )
      ),
      source.createFetch(mangleName(scope.get()) + "_stack_index"),
      source.createFetch("_Alta_bool_true"),
    })
  );
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> transpiled, std::shared_ptr<AltaCore::AST::ExpressionNode> expr, std::shared_ptr<AltaCore::DH::ExpressionNode> info) {
  using NT = AltaCore::AST::NodeType;
  auto retExpr = transpiled;
  auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
  auto nodeType = expr->nodeType();
  if (
    // native types are copied by value
    !exprType->isNative &&
    // make sure we have a copy constructor,
    // otherwise, there's no point in continuing
    exprType->klass->copyConstructor &&
    // pointers are copied by value
    //
    // note that this does not include references,
    // those do need to be copied
    exprType->pointerLevel() < 1 &&
    // check that the expression isn't a class instantiation
    // or a function call, since:
    //   a) for class insts., we *just* created the class. there's
    //      no need to copy it
    //   b) for function calls, returned expressions are automatically
    //      copied if necessary (via this same method, `doCopyCtor`)
    nodeType != NT::ClassInstantiationExpression &&
    nodeType != NT::FunctionCallExpression
  ) {
    retExpr = source.createFunctionCall(
      source.createFetch("_cn_" + mangleName(exprType->klass->copyConstructor.get())),
      {
        source.createPointer(retExpr),
      }
    );
  }
  return retExpr;
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType) {
  using NT = AltaCore::AST::NodeType;
  auto retExpr = expr;
  auto nodeType = expr->nodeType();
  if (
    // native types are copied by value
    !exprType->isNative &&
    // make sure we have a copy constructor,
    // otherwise, there's no point in continuing
    exprType->klass->copyConstructor &&
    // pointers are copied by value
    //
    // note that this does not include references,
    // those do need to be copied
    exprType->pointerLevel() < 1
  ) {
    retExpr = source.createFunctionCall(
      source.createFetch("_cn_" + mangleName(exprType->klass->copyConstructor.get())),
      {
        source.createPointer(retExpr),
      }
    );
  }
  return retExpr;
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::doParentRetrieval(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, std::shared_ptr<AltaCore::DET::Type> targetType, bool* didRetrieval) {
  auto result = expr;
  if (didRetrieval != nullptr) {
    *didRetrieval = false;
  }
  if (
    !exprType->isNative &&
    !targetType->isNative &&
    exprType->klass->id != targetType->klass->id &&
    exprType->klass->hasParent(targetType->klass)
  ) {
    if (exprType->pointerLevel() > 0) {
      result = source.createDereference(result);
    }
    std::deque<std::shared_ptr<AltaCore::DET::Class>> parentAccessors;
    std::stack<size_t> idxs;
    parentAccessors.push_back(exprType->klass);
    idxs.push(0);
    while (parentAccessors.size() > 0) {
      auto& parents = parentAccessors.back()->parents;
      bool cont = false;
      bool done = false;
      for (size_t i = idxs.top(); i < parents.size(); i++) {
        auto& parent = parents[i];
        parentAccessors.push_back(parent);
        if (parent->id == targetType->klass->id) {
          done = true;
          break;
        } else if (parent->parents.size() > 0) {
          cont = true;
          idxs.push(0);
          break;
        } else {
          parentAccessors.pop_back();
        }
      }
      if (done) break;
      if (cont) continue;
      parentAccessors.pop_back();
      idxs.pop();
      if (idxs.size() > 0) {
        ++idxs.top();
      }
    }
    parentAccessors.pop_front();
    for (auto& parent: parentAccessors) {
      result = source.createAccessor(result, mangleName(parent.get()));
    }
    auto altaType = std::make_shared<AltaCore::DET::Type>(targetType->klass)->reference();
    result = source.createDereference(
      source.createCast(
        source.createFunctionCall(
          source.createFetch("_Alta_get_real_version"),
          {
            source.createCast(
              source.createPointer(result),
              source.createType("_Alta_basic_class", { { Ceetah::AST::TypeModifierFlag::Pointer } })
            ),
          }
        ),
        transpileType(altaType.get())
      )
    );
    if (targetType->pointerLevel() > 0 && targetType->pointerLevel() < 2) {
      result = source.createPointer(result);
    } else if (targetType->pointerLevel() > 0) {
      throw std::runtime_error("can't do that");
    }
    if (didRetrieval != nullptr) {
      *didRetrieval = true;
    }
  }
  return result;
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::doChildRetrieval(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, std::shared_ptr<AltaCore::DET::Type> targetType, bool* didRetrieval) {
  auto result = expr;
  if (didRetrieval != nullptr) {
    *didRetrieval = false;
  }
  if (
    !exprType->isNative &&
    !targetType->isNative &&
    exprType->klass->id != targetType->klass->id &&
    targetType->klass->hasParent(exprType->klass)
  ) {
    if (exprType->pointerLevel() > 0) {
      result = source.createDereference(result);
    }
    std::deque<std::shared_ptr<AltaCore::DET::Class>> parentAccessors;
    std::stack<size_t> idxs;
    parentAccessors.push_back(targetType->klass);
    idxs.push(0);
    while (parentAccessors.size() > 0) {
      auto& parents = parentAccessors.back()->parents;
      bool cont = false;
      bool done = false;
      for (size_t i = idxs.top(); i < parents.size(); i++) {
        auto& parent = parents[i];
        parentAccessors.push_back(parent);
        if (parent->id == exprType->klass->id) {
          done = true;
          break;
        } else if (parent->parents.size() > 0) {
          cont = true;
          idxs.push(0);
          break;
        } else {
          parentAccessors.pop_back();
        }
      }
      if (done) break;
      if (cont) continue;
      parentAccessors.pop_back();
      idxs.pop();
      if (idxs.size() > 0) {
        ++idxs.top();
      }
    }
    std::vector<std::shared_ptr<Ceetah::AST::Expression>> args = {
      source.createCast(
        source.createPointer(result),
        source.createType("_Alta_basic_class", { { Ceetah::AST::TypeModifierFlag::Pointer } })
      ),
      source.createIntegerLiteral(parentAccessors.size() - 1),
    };
    for (size_t i = parentAccessors.size() - 1; i > 0; i--) {
      args.push_back(
        source.createStringLiteral(
          mangleName(parentAccessors[i - 1].get())
        )
      );
    }
    auto altaType = std::make_shared<AltaCore::DET::Type>(targetType->klass)->reference();
    result = source.createDereference(
      source.createCast(
        source.createFunctionCall(
          source.createFetch("_Alta_get_child"),
          args
        ),
        transpileType(altaType.get())
      )
    );
    if (targetType->pointerLevel() > 0 && targetType->pointerLevel() < 2) {
      result = source.createPointer(result);
    } else if (targetType->pointerLevel() > 0) {
      throw std::runtime_error("can't do that");
    }
    if (didRetrieval != nullptr) {
      *didRetrieval = true;
    }
  }
  return result;
};

void Talta::CTranspiler::insertExportDefinition(std::string def) {
  auto modName = mangleName(currentModule.get());

  definitions.insertPreprocessorConditional("defined(_ALTA_SAVE_DEFS_" + modName + ") && defined(" + def + ")");
  definitions.insertPreprocessorDefinition("_ALTA_DEF_" + def);
  definitions.insertPreprocessorUndefinition(def);
  definitions.exitInsertionPoint();

  definitions.insertPreprocessorConditional("!defined(_ALTA_SAVE_DEFS_" + modName + ") && defined(_ALTA_DEF_" + def + ")");
  definitions.insertPreprocessorDefinition(def);
  definitions.insertPreprocessorUndefinition("_ALTA_DEF_" + def);
  definitions.exitInsertionPoint();
};

void Talta::CTranspiler::saveExportDefinitions(bool inHeader) {
  auto modName = mangleName(currentModule.get());
  Ceetah::Builder& target = inHeader ? header : source;

  target.insertPreprocessorDefinition("_ALTA_SAVE_DEFS_" + modName);
  target.insertPreprocessorInclusion("_ALTA_DEF_HEADER_" + modName, Ceetah::AST::InclusionType::Computed);
  target.insertPreprocessorUndefinition("_ALTA_SAVE_DEFS_" + modName);
};

void Talta::CTranspiler::restoreExportDefinitions(bool inHeader) {
  auto modName = mangleName(currentModule.get());
  Ceetah::Builder& target = inHeader ? header : source;

  target.insertPreprocessorInclusion("_ALTA_DEF_HEADER_" + modName, Ceetah::AST::InclusionType::Computed);
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::tmpify(std::shared_ptr<AltaCore::AST::ExpressionNode> expr, std::shared_ptr<AltaCore::DH::ExpressionNode> info) {
  using ANT = AltaCore::AST::NodeType;

  auto type = AltaCore::DET::Type::getUnderlyingType(info.get());
  auto result = transpile(expr.get(), info.get());
  if (
    !type->isNative &&
    type->indirectionLevel() < 1 &&
    (
      expr->nodeType() == ANT::FunctionCallExpression ||
      expr->nodeType() == ANT::ClassInstantiationExpression ||
      expr->nodeType() == ANT::ConditionalExpression
    )
  ) {
    auto id = tempVarIDs[currentScope->id]++;
    auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id);
    source.insertVariableDefinition(transpileType(type.get()), tmpName);
    source.insertExpressionStatement(
      source.createFunctionCall(
        source.createFetch("_Alta_object_stack_push"),
        {
          source.createPointer(source.createFetch("_Alta_global_runtime.local")),
          source.createCast(
            source.createPointer(source.createFetch(tmpName)),
            source.createType("_Alta_basic_class", { { Ceetah::AST::TypeModifierFlag::Pointer } })
          ),
        }
      )
    );
    result = source.createMultiExpression({
      source.createAssignment(
        source.createFetch(tmpName),
        result
      ),
      source.createFetch(tmpName),
    });
  }
  return result;
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::transpile(AltaCore::AST::Node* node, AltaCore::DH::Node* _info) {
  using AltaNodeType = AltaCore::AST::NodeType;
  namespace AAST = AltaCore::AST;
  namespace CAST = Ceetah::AST;
  namespace DH = AltaCore::DH;
  AltaNodeType nodeType = node->nodeType();

  currentScope = _info->inputScope;

  if (nodeType == AltaNodeType::FunctionDefinitionNode) {
    auto aFunc = dynamic_cast<AAST::FunctionDefinitionNode*>(node);
    auto funcInfo = dynamic_cast<DH::FunctionDefinitionNode*>(_info);

    auto detailLoop =  [&](DH::FunctionDefinitionNode* info) {
      Ceetah::Builder sourceBuilderCache(nullptr);
      bool isGeneric = false;

      auto mod = AltaCore::Util::getModule(info->function->parentScope.lock().get()).lock();
      auto mangledModName = mangleName(mod.get());

      if (info->function->genericArguments.size() > 0) {
        auto root = std::make_shared<CAST::RootNode>();
        generics.push_back(std::make_pair(info->function, root));
        sourceBuilderCache = source;
        source = Ceetah::Builder(root);
        isGeneric = true;
      }

      if (!info->function->isExport && !info->function->isMethod) {
        for (auto& hoistedGeneric: info->function->publicHoistedGenerics) {
          includeGeneric(hoistedGeneric);
        }
      }

      for (auto& hoistedGeneric: info->function->privateHoistedGenerics) {
        includeGeneric(hoistedGeneric);
      }

      if (!info->function->isExport && !info->function->isMethod) {
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
        if (param->isVariable) {
          mangled = mangled.substr(12);
        }
        cParams.push_back({ (param->isVariable ? "_Alta_array_" : "") + mangled, transpileType(type.get()) });
        if (param->isVariable) {
          cParams.push_back({ "_Alta_array_length_" + mangled, size_tType });
        }
      }

      auto returnType = transpileType(info->function->returnType.get());

      if (info->function->throws) {
        cParams.push_back(std::make_tuple("_Alta_return_value", transpileType(info->function->returnType->point().get())));
        returnType = source.createType("_Alta_bool");
      }

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
            // the cast is necessary to shut up a useless compiler warning
            //
            // ... because apparently C compilers think that void() != void(void)
            source.createCast(
              source.createFetch("_Alta_unwind_global_runtime"),
              source.createType(source.createType("void"), { source.createType("void") }, std::vector<uint8_t> {})
            )
          })
        );
      }

      stackBookkeepingStart(info->function->scope);

      for (size_t i = 0; i < info->function->parameterVariables.size(); i++) {
        auto& var = info->function->parameterVariables[i];
        if (!var->type->isNative && var->type->indirectionLevel() < 1) {
          source.insertExpressionStatement(source.createFunctionCall(
            source.createFetch("_Alta_object_stack_push"),
            {
              source.createPointer(
                source.createAccessor(
                  source.createFetch("_Alta_global_runtime"),
                  "local"
                )
              ),
              source.createCast(
                source.createPointer(
                  source.createFetch(mangleName(var.get()))
                ),
                source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } })
              ),
            }
          ));
        }
      }

      for (size_t i = 0; i < aFunc->body->statements.size(); i++) {
        auto& stmt = aFunc->body->statements[i];
        auto& stmtInfo = info->body->statements[i];
        transpile(stmt.get(), stmtInfo.get());
      }

      stackBookkeepingStop(info->function->scope);
      source.exitInsertionPoint();


      if (isGeneric && !(info->function->isExport || info->function->isMethod)) {
        auto tmp = source;
        source = sourceBuilderCache;
        for (auto& hoistedGeneric: info->function->publicHoistedGenerics) {
          includeGeneric(hoistedGeneric);
        }
        for (auto& hoistedType: info->function->publicHoistedFunctionalTypes) {
          defineFunctionalType(hoistedType);
        }
        auto mod = AltaCore::Util::getModule(info->function->parentScope.lock().get()).lock();
        auto mangledModName = mangleName(mod.get());
        source.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
        source = tmp;
      }

      if (info->function->isExport || info->function->isMethod) {
        for (auto& hoistedGeneric: info->function->publicHoistedGenerics) {
          includeGeneric(hoistedGeneric, true);
        }
        for (auto& hoistedType: info->function->publicHoistedFunctionalTypes) {
          defineFunctionalType(hoistedType, true);
        }
        auto mod = AltaCore::Util::getModule(info->function->parentScope.lock().get()).lock();
        auto mangledModName = mangleName(mod.get());
        headerPredeclaration("_ALTA_FUNCTION_" + mangledFuncName, mangledModName);
        header.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
      }

      auto target = (info->function->isExport || info->function->isMethod) ? header : source;

      for (auto arg: info->function->genericArguments) {
        if (arg->klass) {
          auto dependency = AltaCore::Util::getModule(arg->klass->parentScope.lock().get()).lock();
          auto mangledImportName = mangleName(dependency.get());
          target.insertPreprocessorDefinition(headerMangle(arg->klass.get()));
          target.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledModName + "_0_INCLUDE_" + mangledImportName, CAST::InclusionType::Computed);
        }
      }

      if (info->function->isExport || info->function->isMethod) {
        header.exitInsertionPoint();
      }

      if (isGeneric) {
        source = sourceBuilderCache;
      }
    };

    if (aFunc->generics.size() > 0) {
      for (auto& inst: funcInfo->genericInstantiations) {
        detailLoop(inst.get());
      }
    } else {
      detailLoop(funcInfo);
    }
  } else if (nodeType == AltaNodeType::ExpressionStatement) {
    auto exprStmt = dynamic_cast<AAST::ExpressionStatement*>(node);
    auto info = dynamic_cast<DH::ExpressionStatement*>(_info);
    auto expr = tmpify(exprStmt->expression, info->expression);
    if (expr != nullptr) {
      source.insertExpressionStatement(expr);
    }
  } else if (nodeType == AltaNodeType::ReturnDirectiveNode) {
    auto retDir = dynamic_cast<AAST::ReturnDirectiveNode*>(node);
    auto info = dynamic_cast<DH::ReturnDirectiveNode*>(_info);

    std::shared_ptr<Ceetah::AST::Expression> expr = nullptr;
    if (retDir->expression != nullptr) {
      auto transpiled = transpile(retDir->expression.get(), info->expression.get());
      auto functionReturnType = info->parentFunction ? info->parentFunction->returnType : nullptr;
      // if we're returing a reference, there's no need to copy anything
      if (functionReturnType && functionReturnType->referenceLevel() > 0) {
        expr = transpiled;
      } else {
        bool didRetrieval = false;
        auto exprType = AltaCore::DET::Type::getUnderlyingType(info->expression.get());
        auto val = doParentRetrieval(transpiled, exprType, functionReturnType, &didRetrieval);
        if (didRetrieval) {
          val = doCopyCtor(val, functionReturnType);
        } else {
          val = doCopyCtor(val, retDir->expression, info->expression);
        }
        expr = val;
      }
      if (functionReturnType) {
        for (size_t i = 0; i < functionReturnType->referenceLevel(); i++) {
          expr = source.createPointer(expr);
        }
      }
    }

    std::string tmpName;

    if (expr) {
      auto id = tempVarIDs[currentScope->id]++;
      tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id);
      source.insertVariableDefinition(transpileType(info->parentFunction->returnType.get()), tmpName, expr);
    }

    std::shared_ptr<AltaCore::DET::Scope> target = info->inputScope;
    while (target) {
      stackBookkeepingStop(target);
      if (target->parentFunction.lock()) {
        break;
      }
      target = target->parent.lock();
    }

    if (info->parentFunction->throws) {
      if (expr) {
        source.insertExpressionStatement(
          source.createAssignment(
            source.createDereference(
              source.createFetch("_Alta_return_value")
            ),
            source.createFetch(tmpName)
          )
        );
      }
      source.insertReturnDirective(source.createFetch("_Alta_bool_true"));
    } else {
      source.insertReturnDirective(expr ? source.createFetch(tmpName) : nullptr);
    }
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
      auto transpiled = transpile(varDef->initializationExpression.get(), info->initializationExpression.get());
      auto exprType = AltaCore::DET::Type::getUnderlyingType(info->initializationExpression.get());

      bool didRetrieval = false;
      auto val = doParentRetrieval(transpiled, exprType, info->variable->type, &didRetrieval);
      bool canCopy = info->variable->type->indirectionLevel() < 1;
      if (didRetrieval && canCopy) {
        val = doCopyCtor(val, info->variable->type);
      } else if (canCopy) {
        val = doCopyCtor(val, varDef->initializationExpression, info->initializationExpression);
      }
      init = val;

      for (size_t i = 0; i < info->variable->type->referenceLevel(); i++) {
        init = source.createPointer(init);
      }
    } else if (info->type->type->pointerLevel() > 0) {
      init = source.createFetch("NULL");
    } else if (info->type->type->isNative) {
      init = source.createArrayLiteral({ source.createIntegerLiteral(0) });
    } else {
      init = source.createFunctionCall(source.createFetch("_cn_" + mangleName(info->type->type->klass->defaultConstructor.get())), {});
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
      // and add it to the stack (if it's not native)
      if (!info->variable->type->isNative && info->variable->type->indirectionLevel() == 0) {
        source.insertExpressionStatement(source.createFunctionCall(
          source.createFetch("_Alta_object_stack_push"),
          {
            source.createPointer(source.createFetch("_Alta_global_runtime.local")),
            source.createCast(
              source.createPointer(source.createFetch(mangledVarName)),
              source.createType("_Alta_basic_class", { { Ceetah::AST::TypeModifierFlag::Pointer } })
            ),
          }
        ));
      }
      return source.createPointer(source.createFetch(mangledVarName));
    }
  } else if (nodeType == AltaNodeType::Accessor) {
    auto acc = dynamic_cast<AAST::Accessor*>(node);
    auto info = dynamic_cast<DH::Accessor*>(_info);
    if (info->getsVariableLength) {
      auto tgt = std::dynamic_pointer_cast<AAST::Fetch>(acc->target);
      auto tgtInfo = std::dynamic_pointer_cast<DH::Fetch>(info->target);
      auto mangled = mangleName(tgtInfo->narrowedTo.get());
      // ("_Alta_array_").length == 12
      // substr(length + 1) to skip substring
      return source.createFetch("_Alta_array_length_" + mangled.substr(12));
    }
    std::shared_ptr<CAST::Expression> result = nullptr;
    size_t refLevel = 0;
    if (info->narrowedTo) {
      if (info->accessesNamespace) {
        result = source.createFetch(mangleName(info->narrowedTo.get()));
      } else {
        auto tgt = tmpify(acc->target, info->target);
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
          auto altaType = std::make_shared<AltaCore::DET::Type>(parents.back())->reference();
          tgt = source.createDereference(
            source.createCast(
              source.createFunctionCall(
                source.createFetch("_Alta_get_real_version"),
                {
                  source.createCast(
                    source.createPointer(tgt),
                    source.createType("_Alta_basic_class", { { Ceetah::AST::TypeModifierFlag::Pointer } })
                  ),
                }
              ),
              transpileType(altaType.get())
            )
          );
        }
        result = source.createAccessor(tgt, mangleName(info->narrowedTo.get()));
      }
      auto ut = AltaCore::DET::Type::getUnderlyingType(info->narrowedTo);
      refLevel = ut->referenceLevel();
    } else if (info->readAccessor) {
      auto readAccFetch = source.createFetch(mangleName(info->readAccessor.get()));

      std::vector<std::shared_ptr<Ceetah::AST::Expression>> args;
      if (!info->accessesNamespace) {
        auto selfAlta = acc->target;
        auto selfInfo = info->target;
        auto self = tmpify(selfAlta, selfInfo);
        auto selfType = AltaCore::DET::Type::getUnderlyingType(selfInfo.get());
        if (selfType->pointerLevel() > 0) {
          for (size_t i = 0; i < selfType->pointerLevel(); i++) {
            self = source.createDereference(self);
          }
        }
        if (info->parentClassAccessors.find(info->readAccessorIndex) != info->parentClassAccessors.end()) {
          auto& parents = info->parentClassAccessors[info->readAccessorIndex];
          for (auto& parent: parents) {
            self = source.createAccessor(self, mangleName(parent.get()));
          }
          auto altaType = std::make_shared<AltaCore::DET::Type>(parents.back())->reference();
          self = source.createDereference(
            source.createCast(
              source.createFunctionCall(
                source.createFetch("_Alta_get_real_version"),
                {
                  source.createCast(
                    source.createPointer(self),
                    source.createType("_Alta_basic_class", { { Ceetah::AST::TypeModifierFlag::Pointer } })
                  ),
                }
              ),
              transpileType(altaType.get())
            )
          );
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
    auto tgt = transpile(assign->target.get(), info->target.get());
    auto tgtType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    bool canCopy = !tgtType->isNative && tgtType->pointerLevel() < 1 && (!info->strict || tgtType->indirectionLevel() < 1);
    bool canDestroy = !info->strict && !tgtType->isNative && tgtType->pointerLevel() < 1 && tgtType->klass->destructor;

    std::vector<std::shared_ptr<Ceetah::AST::Expression>> exprs;

    if (canDestroy) {
      auto id = tempVarIDs[info->inputScope->id]++;
      auto tmpName = mangleName(info->inputScope.get()) + "_temp_var_" + std::to_string(id);
      tgtType = tgtType->destroyReferences()->deconstify()->reference();
      source.insertVariableDefinition(transpileType(tgtType.get()), tmpName);
      exprs.push_back(
        source.createAssignment(
          source.createFetch(tmpName),
          source.createPointer(tgt)
        )
      );
      tgt = source.createDereference(source.createFetch(tmpName));
    }

    bool didRetrieval = false;
    auto expr = transpile(assign->value.get(), info->value.get());
    auto exprType = AltaCore::DET::Type::getUnderlyingType(info->value.get());
    auto val = doParentRetrieval(expr, exprType, tgtType, &didRetrieval);

    if (canCopy) {
      auto id = tempVarIDs[info->inputScope->id]++;
      auto tmpName = mangleName(info->inputScope.get()) + "_temp_var_" + std::to_string(id);
      if (didRetrieval) {
        val = doCopyCtor(val, tgtType);
      } else {
        val = doCopyCtor(expr, assign->value, info->value);
      }
      source.insertVariableDefinition(transpileType(exprType->deconstify().get()), tmpName);
      exprs.push_back(
        source.createAssignment(
          source.createFetch(tmpName),
          val
        )
      );
      val = source.createFetch(tmpName);
    }

    if (canDestroy) {
      exprs.push_back(
        source.createFunctionCall(
          source.createFetch("_d_" + mangleName(tgtType->klass->destructor.get())),
          {
            source.createCast(
              source.createPointer(tgt),
              source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } })
            ),
            source.createFetch("_Alta_bool_false"),
          }
        )
      );
    }

    if (info->strict) {
      for (size_t i = 0; i < tgtType->referenceLevel(); i++) {
        tgt = source.createPointer(tgt);
      }
      for (size_t i = 0; i < tgtType->referenceLevel(); i++) {
        val = source.createPointer(val);
      }
    }

    exprs.push_back(
      source.createAssignment(
        tgt,
        val,
        (CAST::AssignmentType)info->type
      )
    );

    return source.createMultiExpression(exprs);
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
      auto selfAlta = info->methodClassTarget;
      auto selfInfo = info->methodClassTargetInfo;
      auto selfType = AltaCore::DET::Type::getUnderlyingType(selfInfo.get());
      auto self = tmpify(selfAlta, selfInfo);
      for (size_t i = 0; i < selfType->pointerLevel(); i++) {
        self = source.createDereference(self);
      }
      bool didRetrieval = false;
      auto exprType = std::make_shared<AltaCore::DET::Type>(selfType->klass);
      auto targetType = std::make_shared<AltaCore::DET::Type>(info->targetType->methodParent);
      self = doParentRetrieval(self, exprType, targetType, &didRetrieval);
      self = source.createPointer(self);
      args.push_back(self);
    }
    auto restArgs = processArgs(info->adjustedArguments, info->targetType->parameters);
    args.insert(args.end(), restArgs.begin(), restArgs.end());
    std::string tmpName;
    if (info->targetType->throws) {
      auto id = tempVarIDs[currentScope->id]++;
      tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id);
      source.insertVariableDefinition(
        transpileType(info->targetType->returnType.get()),
        tmpName
      );
      args.push_back(source.createPointer(source.createFetch(tmpName)));
    }
    std::shared_ptr<CAST::Expression> result = nullptr;
    auto refLevel = info->targetType->returnType->referenceLevel();
    if (info->isMethodCall) {
      auto acc = std::dynamic_pointer_cast<AAST::Accessor>(call->target);
      auto accInfo = std::dynamic_pointer_cast<DH::Accessor>(info->target);
      result = source.createFunctionCall(source.createFetch(mangleName(accInfo->narrowedTo.get())), args);
    } else {
      result = source.createFunctionCall(transpile(call->target.get(), info->target.get()), args);
    }
    if (info->targetType->throws) {
      source.insertConditionalStatement(source.createUnaryOperation(CAST::UOperatorType::Not, result));
      auto scope = info->inputScope->isTry ? info->inputScope : info->inputScope->findTry().lock();
      if (auto func = scope->parentFunction.lock()) {
        if (func->throws) {
          source.insertReturnDirective(source.createFetch("_Alta_bool_false"));
        }
      } else {
        source.insertExpressionStatement(
          source.createIntegerLiteral("break")
        );
      }
      source.exitInsertionPoint();
      result = source.createFetch(tmpName);
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
    stackBookkeepingStart(info->inputScope);
    for (size_t i = 0; i < block->statements.size(); i++) {
      auto& stmt = block->statements[i];
      auto& stmtInfo = info->statements[i];
      transpile(stmt.get(), stmtInfo.get());
    }
    stackBookkeepingStop(info->inputScope);
    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ConditionalStatement) {
    auto cond = dynamic_cast<AAST::ConditionalStatement*>(node);
    auto info = dynamic_cast<DH::ConditionalStatement*>(_info);

    source.insertConditionalStatement(transpile(cond->primaryTest.get(), info->primaryTest.get()));
    bool doBlock = cond->primaryResult->nodeType() != AltaNodeType::BlockNode;
    if (doBlock) {
      source.insertBlock();
      stackBookkeepingStart(info->primaryScope);
    }
    transpile(cond->primaryResult.get(), info->primaryResult.get());
    if (doBlock) {
      stackBookkeepingStop(info->primaryScope);
      source.exitInsertionPoint();
    }

    for (size_t i = 0; i < cond->alternatives.size(); i++) {
      auto& [altTest, altResult] = cond->alternatives[i];
      auto& [testInfo, resultInfo] = info->alternatives[i];
      source.enterConditionalUltimatum();
      source.insertBlock();
      source.insertConditionalStatement(transpile(altTest.get(), testInfo.get()));
      bool doBlock = altResult->nodeType() != AltaNodeType::BlockNode;
      if (doBlock) {
        source.insertBlock();
        stackBookkeepingStart(info->alternativeScopes[i]);
      }
      transpile(altResult.get(), resultInfo.get());
      if (doBlock) {
        stackBookkeepingStop(info->alternativeScopes[i]);
        source.exitInsertionPoint();
      }
    }

    if (cond->finalResult) {
      source.enterConditionalUltimatum();
      bool doBlock = cond->finalResult->nodeType() != AltaNodeType::BlockNode;
      if (doBlock) {
        source.insertBlock();
        stackBookkeepingStart(info->finalScope);
      }
      transpile(cond->finalResult.get(), info->finalResult.get());
      if (doBlock) {
        stackBookkeepingStop(info->finalScope);
        source.exitInsertionPoint();
      }
    }

    for (size_t i = 0; i < cond->alternatives.size(); i++) {
      source.exitInsertionPoint();
      source.exitInsertionPoint();
    }

    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ConditionalExpression) {
    auto cond = dynamic_cast<AAST::ConditionalExpression*>(node);
    auto info = dynamic_cast<DH::ConditionalExpression*>(_info);
    return source.createTernaryOperation(tmpify(cond->test, info->test), transpile(cond->primaryResult.get(), info->primaryResult.get()), transpile(cond->secondaryResult.get(), info->secondaryResult.get()));
  } else if (nodeType == AltaNodeType::ClassDefinitionNode) {
    auto aClass = dynamic_cast<AAST::ClassDefinitionNode*>(node);
    auto klassInfo = dynamic_cast<DH::ClassDefinitionNode*>(_info);

    auto detailLoop = [&](DH::ClassDefinitionNode* info) {
      Ceetah::Builder sourceBuilderCache(nullptr);
      bool isGeneric = false;

      auto mod = AltaCore::Util::getModule(info->klass->parentScope.lock().get()).lock();
      auto mangledModName = mangleName(mod.get());

      if (info->klass->genericArguments.size() > 0) {
        auto root = std::make_shared<CAST::RootNode>();
        generics.push_back(std::make_pair(info->klass, root));
        sourceBuilderCache = source;
        source = Ceetah::Builder(root);
        isGeneric = true;
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

      auto mangledClassName = mangleName(info->klass.get());
      headerPredeclaration("_ALTA_CLASS_" + mangledClassName, mangledModName, !isGeneric);

      for (auto& hoistedType: info->klass->hoistedFunctionalTypes) {
        defineFunctionalType(hoistedType, true);
      }

      for (auto& hoistedGeneric: info->klass->privateHoistedGenerics) {
        includeGeneric(hoistedGeneric, true);
      }

      for (auto arg: info->klass->genericArguments) {
        if (arg->klass) {
          auto dependency = AltaCore::Util::getModule(arg->klass->parentScope.lock().get()).lock();
          auto mangledImportName = mangleName(dependency.get());
          header.insertPreprocessorDefinition(headerMangle(arg->klass.get()));
          header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledModName + "_0_INCLUDE_" + mangledImportName, CAST::InclusionType::Computed);
        }
      }

      header.insertStructureDefinition("_s_" + mangledClassName, members);
      header.insertTypeDefinition(mangledClassName, header.createType("_s_" + mangledClassName, {}, true));

      auto self = header.createType(mangledClassName, { { CAST::TypeModifierFlag::Pointer } });
      auto basicClassType = header.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } });
      auto rawstringType = header.createType("char", { { CAST::TypeModifierFlag::Pointer } });

      header.insertFunctionDeclaration("_Alta_getParentClass_" + mangledClassName, {
        std::make_tuple("_Alta_self", self),
        std::make_tuple("target", rawstringType),
      }, basicClassType);
      source.insertFunctionDefinition("_Alta_getParentClass_" + mangledClassName, {
        std::make_tuple("_Alta_self", self),
        std::make_tuple("target", rawstringType),
      }, basicClassType);

      // parent check
      for (auto& parent: info->klass->parents) {
        auto mangled = mangleName(parent.get());
        source.insertConditionalStatement(
          source.createBinaryOperation(
            CAST::OperatorType::EqualTo,
            source.createFunctionCall(
              source.createFetch("strcmp"),
              {
                source.createFetch("target"),
                source.createStringLiteral(mangled),
              }
            ),
            source.createIntegerLiteral(0)
          )
        );
        source.insertReturnDirective(
          source.createPointer(
            source.createAccessor(
              source.createDereference(
                source.createFetch("_Alta_self")
              ),
              mangled
            )
          )
        );
        source.exitInsertionPoint();
      }

      source.insertVariableDefinition(basicClassType, "result", source.createFetch("NULL"));

      for (auto& parent: info->klass->parents) {
        auto mangled = mangleName(parent.get());
        source.insertConditionalStatement(
          source.createAssignment(
            source.createFetch("result"),
            source.createFunctionCall(
              source.createFetch("_Alta_getParentClass_" + mangled),
              {
                source.createPointer(
                  source.createAccessor(
                    source.createDereference(
                      source.createFetch("_Alta_self")
                    ),
                    mangled
                  )
                ),
                source.createFetch("target"),
              }
            )
          )
        );
        source.insertReturnDirective(
          source.createFetch("result")
        );
        source.exitInsertionPoint();
      }

      source.insertReturnDirective(source.createFetch("NULL"));

      source.exitInsertionPoint();

      header.insertFunctionDeclaration("_init_" + mangledClassName, {
        std::make_tuple("_Alta_self", self),
        std::make_tuple("_isSuper", source.createType("_Alta_bool", { { CAST::TypeModifierFlag::Constant } })),
      }, self);
      source.insertFunctionDefinition("_init_" + mangledClassName, {
        std::make_tuple("_Alta_self", self),
        std::make_tuple("_isSuper", source.createType("_Alta_bool", { { CAST::TypeModifierFlag::Constant } })),
      }, self);

      auto infoStruct = source.createAccessor(source.createDereference(source.createFetch("_Alta_self")), "_Alta_class_info_struct");

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "typeName"
          ),
          source.createStringLiteral(mangledClassName)
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "destroyed"
          ),
          source.createFetch("_Alta_bool_false")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "persistent"
          ),
          source.createFetch("_Alta_bool_false")
        )
      );

      source.insertExpressionStatement(source.createAssignment(
        source.createAccessor(
          infoStruct,
          "isBaseStruct"
        ),
        source.createFetch("_Alta_bool_true")
      ));

      source.insertExpressionStatement(source.createAssignment(
        source.createAccessor(
          infoStruct,
          "parentTypeName"
        ),
        source.createStringLiteral("")
      ));

      std::shared_ptr<CAST::Expression> dtor = nullptr;
      if (info->klass->destructor) {
        dtor = source.createFetch("_d_" + mangleName(info->klass->destructor.get()));
      } else {
        dtor = source.createFetch("NULL");
      }

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "destructor"
          ),
          dtor
        )
      );

      source.insertConditionalStatement(source.createUnaryOperation(CAST::UOperatorType::Not, source.createFetch("_isSuper")));
      source.insertBlock();

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "realOffset"
          ),
          source.createFetch("PTRDIFF_MAX")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "nextOffset"
          ),
          source.createFetch("PTRDIFF_MAX")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "baseOffset"
          ),
          source.createIntegerLiteral("0")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            infoStruct,
            "parentOffset"
          ),
          source.createIntegerLiteral("0")
        )
      );

      ALTACORE_MAP<std::string, std::shared_ptr<Ceetah::AST::Expression>> parentOffsets;
      ALTACORE_MAP<std::string, std::pair<std::shared_ptr<Ceetah::AST::Expression>, std::shared_ptr<Ceetah::AST::Expression>>> recents;
      std::stack<size_t> indexes;
      std::stack<std::shared_ptr<AltaCore::DET::Class>> children;
      std::stack<std::shared_ptr<Ceetah::AST::Expression>> offsets;
      std::stack<std::shared_ptr<Ceetah::AST::Expression>> accessors;
      indexes.push(0);
      children.push(info->klass);
      offsets.push(source.createIntegerLiteral(0));
      accessors.push(source.createDereference(source.createFetch("_Alta_self")));
      while (indexes.size() > 0 && children.size() > 0 && offsets.size() > 0 && accessors.size() > 0) {
        auto& i = indexes.top();
        auto& child = children.top();

        if (i >= child->parents.size()) {
          indexes.pop();
          children.pop();
          offsets.pop();
          accessors.pop();
          continue;
        }

        auto& parent = child->parents[i];
        auto& childOffset = offsets.top();
        auto& childAccessor = accessors.top();
        auto type = std::make_shared<AltaCore::DET::Type>(child);
        auto mangledParentName = mangleName(parent.get());

        std::shared_ptr<CAST::Expression> tgt;
        std::shared_ptr<CAST::Expression> tgt2;
        std::shared_ptr<CAST::Expression> offsetToPush = source.createFunctionCall(source.createFetch("offsetof"), {
          // not really an integer; just used for literal output
          source.createIntegerLiteral(
            transpileType(
              type.get()
            )->toString()
          ),
          source.createFetch(mangledParentName)
        }, true);

        if (parentOffsets.find(parent->id) != parentOffsets.end()) {
          tgt = source.createBinaryOperation(
            CAST::OperatorType::Subtraction,
            source.createBinaryOperation(
              CAST::OperatorType::Addition,
              childOffset,
              offsetToPush
            ),
            parentOffsets[parent->id]
          );
        } else {
          parentOffsets[parent->id] = source.createBinaryOperation(
            CAST::OperatorType::Addition,
            childOffset,
            offsetToPush
          );
          tgt = source.createFetch("PTRDIFF_MAX");
        }

        source.insertExpressionStatement(
          source.createAssignment(
            source.createAccessor(
              source.createAccessor(
                source.createAccessor(
                  childAccessor,
                  mangledParentName
                ),
                "_Alta_class_info_struct"
              ),
              "baseOffset"
            ),
            offsetToPush
          )
        );

        source.insertExpressionStatement(
          source.createAssignment(
            source.createAccessor(
              source.createAccessor(
                source.createAccessor(
                  childAccessor,
                  mangledParentName
                ),
                "_Alta_class_info_struct"
              ),
              "realOffset"
            ),
            tgt
          )
        );

        source.insertExpressionStatement(
          source.createAssignment(
            source.createAccessor(
              source.createAccessor(
                source.createAccessor(
                  childAccessor,
                  mangledParentName
                ),
                "_Alta_class_info_struct"
              ),
              "parentOffset"
            ),
            offsetToPush
          )
        );

        if (recents.find(parent->id) != recents.end()) {
          auto& [acc, off] = recents[parent->id];
          source.insertExpressionStatement(
            source.createAssignment(
              source.createAccessor(
                source.createAccessor(
                  acc,
                  "_Alta_class_info_struct"
                ),
                "nextOffset"
              ),
              source.createBinaryOperation(
                CAST::OperatorType::Subtraction,
                source.createBinaryOperation(
                  CAST::OperatorType::Addition,
                  childOffset,
                  offsetToPush
                ),
                off
              )
            )
          );
        }

        recents[parent->id] = std::make_pair(
          std::dynamic_pointer_cast<CAST::Expression>(
            source.createAccessor(
              childAccessor,
              mangledParentName
            )
          ),
          std::dynamic_pointer_cast<CAST::Expression>(
            source.createBinaryOperation(
              CAST::OperatorType::Addition,
              childOffset,
              offsetToPush
            )
          )
        );

        source.insertExpressionStatement(
          source.createAssignment(
            source.createAccessor(
              source.createAccessor(
                source.createAccessor(
                  childAccessor,
                  mangledParentName
                ),
                "_Alta_class_info_struct"
              ),
              "nextOffset"
            ),
            source.createFetch("PTRDIFF_MAX")
          )
        );

        i++;
        indexes.push(0);
        children.push(parent);
        offsets.push(offsetToPush);
        accessors.push(source.createAccessor(childAccessor, mangledParentName));
      }

      source.exitInsertionPoint();
      source.exitInsertionPoint();

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
          if (kind == LoopKind::All) {
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

      if (info->createDefaultDestructor) {
        transpile(info->defaultDestructor.get(), info->defaultDestructorDetail.get());
      }

      if (info->createDefaultCopyConstructor) {
        transpile(info->defaultCopyConstructor.get(), info->defaultCopyConstructorDetail.get());
      }

      header.exitInsertionPoint();

      if (isGeneric) {
        source = sourceBuilderCache;
      }
    };

    if (aClass->generics.size() > 0) {
      for (auto inst: klassInfo->genericInstantiations) {
        detailLoop(inst.get());
      }
    } else {
      detailLoop(klassInfo);
    }
  } else if (nodeType == AAST::NodeType::ClassSpecialMethodDefinitionStatement) {
    auto special = dynamic_cast<AAST::ClassSpecialMethodDefinitionStatement*>(node);
    auto info = dynamic_cast<DH::ClassSpecialMethodDefinitionStatement*>(_info);
    auto mangledClassName = mangleName(info->klass.get());
    auto constr = info->method;
    auto mangledName = mangleName(constr.get());

    auto ret = header.createType(mangledClassName);
    auto retPtr = header.createType(mangledClassName, { { CAST::TypeModifierFlag::Pointer } });
    auto voidType = header.createType("void");

    bool isCtor = special->type == AltaCore::AST::SpecialClassMethod::Constructor;
    
    std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> params;
    decltype(params) fullParams;

    std::string prefix;
    decltype(ret) retType = nullptr;

    if (isCtor) {
      prefix = "_c_";
      retType = retPtr;

      fullParams.push_back({ "_Alta_self", retPtr });

      for (size_t i = 0; i < constr->parameterVariables.size(); i++) {
        auto& var = constr->parameterVariables[i];
        auto& [name, ptype, isVariable, alias] = constr->parameters[i];
        auto type = (isVariable) ? ptype->point() : ptype;
        auto mangled = mangleName(var.get());
        if (isVariable) {
          mangled = mangled.substr(12);
        }
        params.push_back({ (isVariable ? "_Alta_array_" : "") + mangled, transpileType(type.get()) });
        if (isVariable) {
          params.push_back({ "_Alta_array_length_" + mangled, size_tType });
        }
      }

      fullParams.insert(fullParams.end(), params.begin(), params.end());
    } else {
      prefix = "_d_";
      retType = voidType;

      fullParams.push_back({ "__Alta_self", header.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } }) });
      fullParams.push_back({ "_Alta_isPersistent", header.createType("_Alta_bool", { { CAST::TypeModifierFlag::Constant } }) });
    }

    header.insertFunctionDeclaration(prefix + mangledName, fullParams, retType);
    source.insertFunctionDefinition(prefix + mangledName, fullParams, retType);

    stackBookkeepingStart(constr->scope);

    if (!isCtor) {
      source.insertVariableDefinition(retPtr, "_Alta_self", source.createCast(source.createFetch("__Alta_self"), retPtr));
      source.insertConditionalStatement(
        source.createAccessor(
          source.createAccessor(
            source.createDereference(
              source.createFetch("_Alta_self")
            ),
            "_Alta_class_info_struct"
          ),
          "destroyed"
        )
      );
      source.insertReturnDirective();
      source.exitInsertionPoint();
    } else {
      for (size_t i = 0; i < constr->parameterVariables.size(); i++) {
        auto& var = constr->parameterVariables[i];
        if (!var->type->isNative && var->type->indirectionLevel() < 1) {
          source.insertExpressionStatement(source.createFunctionCall(
            source.createFetch("_Alta_object_stack_push"),
            {
              source.createPointer(
                source.createAccessor(
                  source.createFetch("_Alta_global_runtime"),
                  "local"
                )
              ),
              source.createCast(
                source.createPointer(
                  source.createFetch(mangleName(var.get()))
                ),
                source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } })
              ),
            }
          ));
        }
      }
    }

    if (info->isDefaultCopyConstructor) {
      auto id = tempVarIDs[info->method->scope->id]++;
      auto tmpName = mangleName(info->method->scope.get()) + "_temp_var_" + std::to_string(id);
      auto self = source.createDereference(source.createFetch("_Alta_self"));
      auto selfInfo = source.createAccessor(self, "_Alta_class_info_struct");
      source.insertVariableDefinition(
        source.createType("_Alta_class_info"),
        tmpName,
        selfInfo
      );
      auto other = source.createDereference(source.createFetch(mangleName(constr->parameterVariables[0].get())));
      source.insertExpressionStatement(
        source.createAssignment(
          self,
          other
        )
      );
      source.insertExpressionStatement(
        source.createAssignment(
          selfInfo,
          source.createFetch(tmpName)
        )
      );
      for (auto& parent: info->klass->parents) {
        if (parent->copyConstructor) {
          auto name = mangleName(parent.get());
          std::shared_ptr<CAST::Expression> myParent = source.createAccessor(self, name);
          std::shared_ptr<CAST::Expression> theirParent = source.createAccessor(other, name);
          auto id = tempVarIDs[info->method->scope->id]++;
          auto tmpName = mangleName(info->method->scope.get()) + "_temp_var_" + std::to_string(id);
          auto altaType = std::make_shared<AltaCore::DET::Type>(parent)->reference();
          source.insertVariableDefinition(
            transpileType(altaType.get()),
            tmpName,
            source.createPointer(theirParent)
          );
          theirParent = source.createDereference(
            source.createTernaryOperation(
              source.createBinaryOperation(
                Ceetah::AST::OperatorType::LessThan,
                source.createAccessor(
                  source.createAccessor(
                    source.createDereference(
                      source.createFetch(tmpName)
                    ),
                    "_Alta_class_info_struct"
                  ),
                  "realOffset"
                ),
                source.createFetch("PTRDIFF_MAX")
              ),
              source.createCast(
                source.createBinaryOperation(
                  Ceetah::AST::OperatorType::Subtraction,
                  source.createCast(
                    source.createFetch(tmpName),
                    source.createType("char", std::vector<uint8_t> { (uint8_t)Ceetah::AST::TypeModifierFlag::Pointer })
                  ),
                  source.createAccessor(
                    source.createAccessor(
                      source.createDereference(
                        source.createFetch(tmpName)
                      ),
                      "_Alta_class_info_struct"
                    ),
                    "realOffset"
                  )
                ),
                transpileType(altaType.get())
              ),
              source.createFetch(tmpName)
            )
          );
          auto parentInfo = source.createAccessor(myParent, "_Alta_class_info_struct");
          source.insertExpressionStatement(source.createAssignment(
            source.createAccessor(self, name),
            source.createFunctionCall(
              source.createFetch("_cn_" + mangleName(parent->copyConstructor.get())),
              {
                source.createPointer(theirParent),
              }
            )
          ));
          source.insertExpressionStatement(source.createAssignment(
            source.createAccessor(
              parentInfo,
              "isBaseStruct"
            ),
            source.createFetch("_Alta_bool_false")
          ));
          source.insertExpressionStatement(source.createAssignment(
            source.createAccessor(
              parentInfo,
              "parentTypeName"
            ),
            source.createStringLiteral(mangledClassName)
          ));
        }
      }
      for (auto& var: info->klass->itemsToCopy) {
        auto name = mangleName(var.get());
        source.insertExpressionStatement(source.createAssignment(
          source.createAccessor(
            self,
            name
          ),
          source.createFunctionCall(
            source.createFetch("_cn_" + mangleName(var->type->klass->copyConstructor.get())),
            {
              source.createPointer(source.createAccessor(other, name)),
            }
          )
        ));
      }
    }

    for (size_t i = 0; i < special->body->statements.size(); i++) {
      auto& stmt = special->body->statements[i];
      auto& stmtInfo = info->body->statements[i];
      transpile(stmt.get(), stmtInfo.get());
    }
    stackBookkeepingStop(constr->scope);

    if (isCtor) {
      source.insertReturnDirective(source.createFetch("_Alta_self"));
    } else {
      for (auto& var: info->klass->itemsToDestroy) {
        source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_d_" + mangleName(var->type->klass->destructor.get())), {
          source.createCast(
            source.createPointer(
              source.createAccessor(
                source.createDereference(source.createFetch("_Alta_self")),
                mangleName(var.get())
              )
            ),
            source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } })
          ),
          source.createFetch("_Alta_bool_false"),
        }));
      }
      for (auto& parent: info->klass->parents) {
        if (!parent->destructor) continue;
        source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_d_" + mangleName(parent->destructor.get())), {
          source.createCast(
            source.createPointer(
              source.createAccessor(
                source.createDereference(source.createFetch("_Alta_self")),
                mangleName(parent.get())
              )
            ),
            source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } })
          ),
          source.createFetch("_Alta_bool_false"),
        }));
      }
      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              source.createDereference(
                source.createFetch("_Alta_self")
              ),
              "_Alta_class_info_struct"
            ),
            "destroyed"
          ),
          source.createFetch("_Alta_bool_true")
        )
      );

      source.insertConditionalStatement(
        source.createBinaryOperation(
          CAST::OperatorType::Or,
          source.createFetch("_Alta_isPersistent"),
          source.createAccessor(
            source.createAccessor(
              source.createDereference(
                source.createFetch("_Alta_self")
              ),
              "_Alta_class_info_struct"
            ),
            "persistent"
          )
        )
      );
      source.insertExpressionStatement(source.createFunctionCall(source.createFetch("free"), { source.createFetch("_Alta_self") }));
      source.exitInsertionPoint();
    }

    source.exitInsertionPoint();

    if (isCtor) {
      std::vector<std::shared_ptr<CAST::Expression>> args;
      for (auto param: constr->parameterVariables) {
        args.push_back(source.createFetch(mangleName(param.get())));
      }
      
      header.insertFunctionDeclaration("_cp_" + mangledName, params, retPtr);
      source.insertFunctionDefinition("_cp_" + mangledName, params, retPtr);
      source.insertVariableDefinition(retPtr, "_Alta_self", source.createFunctionCall(source.createFetch("malloc"), {
        source.createSizeof(ret)
      }));
      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              source.createDereference(
                source.createFetch("_Alta_self")
              ),
              "_Alta_class_info_struct"
            ),
            "persistent"
          ),
          source.createFetch("_Alta_bool_true")
        )
      );
      //if (!info->isDefaultCopyConstructor) {
        source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_init_" + mangledClassName), { source.createFetch("_Alta_self"), source.createFetch("_Alta_bool_false") }));
      //}
      std::vector<std::shared_ptr<CAST::Expression>> pArgs = { source.createFetch("_Alta_self") };
      pArgs.insert(pArgs.end(), args.begin(), args.end());
      source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_c_" + mangledName), pArgs));
      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_Alta_object_stack_push"),
          {
            source.createPointer(
              source.createAccessor(
                source.createFetch("_Alta_global_runtime"),
                "persistent"
              )
            ),
            source.createCast(
              source.createFetch("_Alta_self"),
              source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } })
            ),
          }
        )
      );
      source.insertReturnDirective(source.createFetch("_Alta_self"));
      source.exitInsertionPoint();

      header.insertFunctionDeclaration("_cn_" + mangledName, params, ret);
      source.insertFunctionDefinition("_cn_" + mangledName, params, ret);
      source.insertVariableDefinition(ret, "_Alta_self", source.createArrayLiteral({ source.createIntegerLiteral(0) }));
      //if (!info->isDefaultCopyConstructor) {
        source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_init_" + mangledClassName), { source.createPointer(source.createFetch("_Alta_self")), source.createFetch("_Alta_bool_false") }));
      //}
      std::vector<std::shared_ptr<CAST::Expression>> nArgs = { source.createPointer(source.createFetch("_Alta_self")) };
      nArgs.insert(nArgs.end(), args.begin(), args.end());
      source.insertExpressionStatement(source.createFunctionCall(source.createFetch("_c_" + mangledName), nArgs));
      source.insertReturnDirective(source.createFetch("_Alta_self"));
      source.exitInsertionPoint();
    }
  } else if (nodeType == AAST::NodeType::ClassInstantiationExpression) {
    auto call = dynamic_cast<AAST::ClassInstantiationExpression*>(node);
    auto info = dynamic_cast<DH::ClassInstantiationExpression*>(_info);
    auto args = processArgs(info->adjustedArguments, info->constructor->parameters);
    auto mangledClass = mangleName(info->klass.get());
    auto mangledCtor = mangleName(info->constructor.get());
    if (info->superclass) {
      auto childClass = AltaCore::Util::getClass(info->inputScope).lock();
      auto mangledChildClassName = mangleName(childClass.get());
      auto myParent = source.createAccessor(
        source.createDereference(
          source.createFetch("_Alta_self")
        ),
        mangledClass
      );
      auto parentInfo = source.createAccessor(myParent, "_Alta_class_info_struct");
      auto mangledParentName = mangleName(info->klass.get());

      source.insertConditionalStatement(
        source.createBinaryOperation(
          CAST::OperatorType::EqualTo,
          source.createAccessor(
            parentInfo,
            "realOffset"
          ),
          source.createFetch("PTRDIFF_MAX")
        )
      );
      source.insertBlock();

      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_init_" + mangledParentName),
          {
            source.createPointer(myParent),
            source.createFetch("_Alta_bool_true"),
          }
        )
      );

      args.insert(args.begin(), source.createPointer(myParent));

      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_c_" + mangledCtor),
          args
        )
      );

      source.exitInsertionPoint();
      source.exitInsertionPoint();

      source.insertExpressionStatement(source.createAssignment(
        source.createAccessor(
          parentInfo,
          "isBaseStruct"
        ),
        source.createFetch("_Alta_bool_false")
      ));
      source.insertExpressionStatement(source.createAssignment(
        source.createAccessor(
          parentInfo,
          "parentTypeName"
        ),
        source.createStringLiteral(mangledChildClassName)
      ));
      return source.createPointer(
        source.createAccessor(
          source.createDereference(
            source.createFetch("_Alta_self")
          ),
          mangledClass
        )
      );
    } else if (info->klass->isStructure) {
      if (args.size() < 1) {
        args.push_back(source.createIntegerLiteral(0));
      }
      return source.createArrayLiteral(args, source.createType(mangleName(info->klass.get()), {}, !info->klass->isTyped));
    } else {
      std::shared_ptr<CAST::Expression> call = source.createFunctionCall(source.createFetch((info->persistent ? "_cp_" : "_cn_") + mangledCtor), args);
      if (info->persistent) {
        call = source.createDereference(call);
      }
      return call;
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
    source.insertWhileLoop(source.createFetch("_Alta_bool_true"));
    source.insertBlock();
    source.insertConditionalStatement(
      source.createUnaryOperation(
        CAST::UOperatorType::Not,
        transpile(loop->test.get(), info->test.get())
      )
    );
    source.insertExpressionStatement(source.createIntegerLiteral("break"));
    source.exitInsertionPoint();
    transpile(loop->body.get(), info->body.get());
    source.exitInsertionPoint();
    source.exitInsertionPoint();
  } else if (nodeType == AAST::NodeType::CastExpression) {
    auto cast = dynamic_cast<AAST::CastExpression*>(node);
    auto info = dynamic_cast<DH::CastExpression*>(_info);
    auto tgt = transpile(cast->target.get(), info->target.get());
    bool didRetrieval = false;
    bool didChildRetrieval = false;
    auto exprType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    tgt = doParentRetrieval(tgt, exprType, info->type->type, &didRetrieval);
    if (!didRetrieval) {
      tgt = doChildRetrieval(tgt, exprType, info->type->type, &didChildRetrieval);
    }
    for (size_t i = 0; i < info->type->type->referenceLevel(); i++) {
      tgt = source.createPointer(tgt);
    }
    if (info->type->type->isExactlyCompatibleWith(*exprType)) {
      // both types are exactly the same, just do nothing
    } else if (!didRetrieval && info->type->type->indirectionLevel() < 1 && !info->type->type->isNative) {
      throw std::runtime_error("can't cast a plain class to another class");
    } else if ((didRetrieval && info->type->type->indirectionLevel() < 1) || (didChildRetrieval && info->type->type->indirectionLevel() < 1)) {
      // do nothing
    } else if (!didRetrieval && !didChildRetrieval) {
      tgt = source.createCast(tgt, transpileType(info->type->type.get()));
    }
    for (size_t i = info->type->type->referenceLevel(); i > 0; i--) {
      tgt = source.createDereference(tgt);
    }
    return tgt;
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
  } else if (nodeType == AAST::NodeType::InstanceofExpression) {
    auto instOf = dynamic_cast<AAST::InstanceofExpression*>(node);
    auto info = dynamic_cast<DH::InstanceofExpression*>(_info);
    auto targetType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    auto& testType = info->type->type;

    if (targetType->isNative != testType->isNative) {
      return source.createFetch("_Alta_bool_false");
    } else if (targetType->isNative) {
      if (testType->isExactlyCompatibleWith(*targetType)) {
        return source.createFetch("_Alta_bool_false");
      } else {
        return source.createFetch("_Alta_bool_true");
      }
    } else if (targetType->klass->id == testType->klass->id) {
      return source.createFetch("_Alta_bool_true");
    } else if (targetType->klass->hasParent(testType->klass)) {
      return source.createFetch("_Alta_bool_true");
    } else if (testType->klass->hasParent(targetType->klass)) {
      auto tgt = transpile(instOf->target.get(), info->target.get());
      auto tgtType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
      bool didRetrieval = false;
      auto result = doChildRetrieval(tgt, tgtType, info->type->type, &didRetrieval);
      return source.createBinaryOperation(
        CAST::OperatorType::NotEqualTo,
        source.createPointer(result),
        source.createFetch("NULL")
      );
    } else {
      return source.createFetch("_Alta_bool_false");
    }
  } else if (nodeType == AltaNodeType::ForLoopStatement) {
    auto loop = dynamic_cast<AAST::ForLoopStatement*>(node);
    auto info = dynamic_cast<DH::ForLoopStatement*>(_info);
    source.insertBlock();
    source.insertExpressionStatement(transpile(loop->initializer.get(), info->initializer.get()));
    source.insertWhileLoop(source.createFetch("_Alta_bool_true"));
    source.insertBlock();
    source.insertConditionalStatement(
      source.createUnaryOperation(
        CAST::UOperatorType::Not,
        transpile(loop->condition.get(), info->condition.get())
      )
    );
    source.insertExpressionStatement(source.createIntegerLiteral("break"));
    source.exitInsertionPoint();
    transpile(loop->body.get(), info->body.get());
    source.insertExpressionStatement(transpile(loop->increment.get(), info->increment.get()));
    source.exitInsertionPoint();
    source.exitInsertionPoint();
    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::RangedForLoopStatement) {
    auto loop = dynamic_cast<AAST::RangedForLoopStatement*>(node);
    auto info = dynamic_cast<DH::RangedForLoopStatement*>(_info);

    auto mangledCounter = mangleName(info->counter.get());

    source.insertBlock();
    source.insertVariableDefinition(
      transpileType(info->counterType->type.get()),
      mangledCounter,
      transpile(loop->start.get(), info->start.get())
    );

    CAST::OperatorType comparator = CAST::OperatorType::LessThan;
    if (loop->decrement) {
      if (loop->inclusive) {
        comparator = CAST::OperatorType::GreaterThanOrEqualTo;
      } else {
        comparator = CAST::OperatorType::GreaterThan;
      }
    } else {
      if (loop->inclusive) {
        comparator = CAST::OperatorType::LessThanOrEqualTo;
      } else {
        comparator = CAST::OperatorType::LessThan;
      }
    }

    source.insertWhileLoop(
      source.createBinaryOperation(
        comparator,
        source.createFetch(mangledCounter),
        transpile(loop->end.get(), info->end.get())
      )
    );
    source.insertBlock();
    transpile(loop->body.get(), info->body.get());
    source.insertExpressionStatement(
      source.createAssignment(
        source.createFetch(mangledCounter),
        source.createBinaryOperation(
          (loop->decrement) ? CAST::OperatorType::Subtraction : CAST::OperatorType::Addition,
          source.createFetch(mangledCounter),
          source.createIntegerLiteral(1)
        )
      )
    );
    source.exitInsertionPoint();
    source.exitInsertionPoint();
    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::UnaryOperation) {
    auto op = dynamic_cast<AAST::UnaryOperation*>(node);
    auto info = dynamic_cast<DH::UnaryOperation*>(_info);

    return source.createUnaryOperation(
      (CAST::UOperatorType)op->type,
      transpile(op->target.get(), info->target.get()),
      op->type == AAST::UOperatorType::PostIncrement || op->type == AAST::UOperatorType::PostDecrement
    );
  } else if (nodeType == AltaNodeType::SizeofOperation) {
    auto op = dynamic_cast<AAST::SizeofOperation*>(node);
    auto info = dynamic_cast<DH::SizeofOperation*>(_info);

    return source.createSizeof(transpileType(info->target->type.get()));
  } else if (nodeType == AltaNodeType::FloatingPointLiteralNode) {
    auto deci = dynamic_cast<AAST::FloatingPointLiteralNode*>(node);
    auto info = dynamic_cast<DH::FloatingPointLiteralNode*>(_info);

    return source.createIntegerLiteral(deci->raw);
  } else if (nodeType == AltaNodeType::StructureDefinitionStatement) {
    auto structure = dynamic_cast<AAST::StructureDefinitionStatement*>(node);
    auto info = dynamic_cast<DH::StructureDefinitionStatement*>(_info);

    if (info->isExternal) return nullptr;

    auto mod = info->structure->parentScope.lock()->parentModule.lock();
    auto mangledModName = mangleName(mod.get());
    auto mangledClassName = mangleName(info->structure.get());
    headerPredeclaration("_ALTA_CLASS_" + mangledClassName, mangledModName, true);

    for (auto& hoistedType: info->structure->hoistedFunctionalTypes) {
      defineFunctionalType(hoistedType, info->isExport);
    }

    for (auto& hoistedGeneric: info->structure->privateHoistedGenerics) {
      includeGeneric(hoistedGeneric, info->isExport);
    }

    auto& target = info->isExport ? header : source;

    std::vector<std::pair<std::string, std::shared_ptr<CAST::Type>>> members;

    for (auto& item: info->structure->scope->items) {
      if (auto var = std::dynamic_pointer_cast<AltaCore::DET::Variable>(item)) {
        members.push_back(std::make_pair(mangleName(var.get()), transpileType(var->type.get())));
      }
    }

    auto name = info->isLiteral ? structure->name : mangledClassName;
    target.insertStructureDefinition((info->isTyped ? "_struct_" : "") + name, members);
    
    if (info->isTyped) {
      target.insertTypeDefinition(name, target.createType("_struct_" + name, {}, true));
    }
  } else if (nodeType == AltaNodeType::ExportStatement) {
    auto state = dynamic_cast<AAST::ExportStatement*>(node);
    auto info = dynamic_cast<DH::ExportStatement*>(_info);

    if (state->externalTarget) {
      transpile(state->externalTarget.get(), info->externalTarget.get());
    }
  } else if (nodeType == AltaNodeType::FunctionDeclarationNode) {
    auto func = dynamic_cast<AAST::FunctionDeclarationNode*>(node);
    auto info = dynamic_cast<DH::FunctionDeclarationNode*>(_info);

    if (!info->function->isExport && !info->function->isMethod) {
      for (auto& hoistedGeneric: info->function->publicHoistedGenerics) {
        includeGeneric(hoistedGeneric);
      }
    }

    for (auto& hoistedGeneric: info->function->privateHoistedGenerics) {
      includeGeneric(hoistedGeneric);
    }

    if (!info->function->isExport && !info->function->isMethod) {
      for (auto& hoistedType: info->function->publicHoistedFunctionalTypes) {
        defineFunctionalType(hoistedType);
      }
    }

    for (auto& hoistedType: info->function->hoistedFunctionalTypes) {
      defineFunctionalType(hoistedType);
    }

    if (info->function->isExport || info->function->isMethod) {
      for (auto& hoistedGeneric: info->function->publicHoistedGenerics) {
        includeGeneric(hoistedGeneric, true);
      }
      for (auto& hoistedType: info->function->publicHoistedFunctionalTypes) {
        defineFunctionalType(hoistedType, true);
      }
    }
  } else if (nodeType == AltaNodeType::DeleteStatement) {
    auto del = dynamic_cast<AAST::DeleteStatement*>(node);
    auto info = dynamic_cast<DH::DeleteStatement*>(_info);

    auto type = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    auto tgt = transpile(del->target.get(), info->target.get());
    bool canDestroy = !type->isNative && (info->persistent || type->pointerLevel() < 1);

    if (info->persistent) {
      for (size_t i = 0; i < type->pointerLevel(); i++) {
        tgt = source.createDereference(tgt);
      }
    }

    if (canDestroy) {
      auto id = tempVarIDs[currentScope->id]++;
      auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id);
      auto tmpType = std::make_shared<AltaCore::DET::Type>(type->klass, std::vector<uint8_t> { (uint8_t)AltaCore::Shared::TypeModifierFlag::Reference });
      source.insertVariableDefinition(
        transpileType(tmpType.get()),
        tmpName,
        source.createPointer(tgt)
      );

      tgt = source.createFetch(tmpName);

      auto id2 = tempVarIDs[currentScope->id]++;
      auto tmpName2 = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id2);
      auto basicClassType = source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } });
      source.insertVariableDefinition(
        basicClassType,
        tmpName2,
        source.createCast(
          source.createBinaryOperation(
            CAST::OperatorType::Subtraction,
            source.createCast(
              tgt,
              source.createType("char", { { CAST::TypeModifierFlag::Pointer } })
            ),
            source.createAccessor(
              source.createAccessor(
                source.createDereference(tgt),
                "_Alta_class_info_struct"
              ),
              "baseOffset"
            )
          ),
          basicClassType
        )
      );

      tgt = source.createFetch(tmpName2);
    }

    if (canDestroy) {
      if (info->persistent) {
        source.insertConditionalStatement(
          source.createUnaryOperation(
            CAST::UOperatorType::Not,
            source.createFunctionCall(
              source.createFetch("_Alta_object_stack_cherry_pick"),
              {
                source.createPointer(
                  source.createAccessor(
                    source.createFetch("_Alta_global_runtime"),
                    "persistent"
                  )
                ),
                tgt,
              }
            )
          )
        );
        source.insertBlock();
      }
      if (type->klass->destructor) {
        source.insertExpressionStatement(
          source.createFunctionCall(
            source.createAccessor(
              source.createAccessor(
                source.createDereference(tgt),
                "_Alta_class_info_struct"
              ),
              "destructor"
            ),
            {
              tgt,
              source.createFetch(info->persistent ? "_Alta_bool_true" : "_Alta_bool_false"),
            }
          )
        );
      } else if (info->persistent) {
        source.insertExpressionStatement(
          source.createFunctionCall(
            source.createFetch("free"),
            {
              tgt,
            }
          )
        );
      }
      if (info->persistent) {
        source.exitInsertionPoint();
        source.exitInsertionPoint();
      }
    } else if (info->persistent) {
      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("free"),
          {
            tgt,
          }
        )
      );
    }
  } else if (nodeType == AltaNodeType::ControlDirective) {
    auto ctrl = dynamic_cast<AAST::ControlDirective*>(node);

    source.insertExpressionStatement(source.createIntegerLiteral(ctrl->isBreak ? "break" : "continue"));
  } else if (nodeType == AltaNodeType::TryCatchBlock) {
    auto blk = dynamic_cast<AAST::TryCatchBlock*>(node);
    auto info = dynamic_cast<DH::TryCatchBlock*>(_info);

    auto okName = mangleName(info->tryScope.get()) + "_ok";
    auto idxName = mangleName(info->tryScope.get()) + "_index";
    source.insertVariableDefinition(
      source.createType("_Alta_bool"),
      okName,
      source.createFetch("_Alta_bool_false")
    );
    source.insertVariableDefinition(
      source.createType("size_t"),
      idxName,
      source.createAccessor(
        source.createAccessor(
          source.createFetch("_Alta_global_runtime"),
          "lastError"
        ),
        "handlerStackSize"
      )
    );

    
    for (size_t j = blk->catchBlocks.size(); j > 0; j--) {
      auto i = j - 1;
      auto& [type, stmt] = blk->catchBlocks[i];
      auto& id = blk->catchIDs[i];
      auto& [typeDet, stmtDet] = info->catchBlocks[i];
      auto& var = info->errorVariables[i];
      auto& scope = info->catchScopes[i];

      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_Alta_push_error_handler"),
          {
            source.createStringLiteral(mangleType(typeDet->type.get())),
          }
        )
      );
    }

    source.insertWhileLoop(source.createFetch("_Alta_bool_true"));
    source.insertBlock();
    transpile(blk->tryBlock.get(), info->tryBlock.get());
    source.insertExpressionStatement(
      source.createAssignment(
        source.createFetch(okName),
        source.createFetch("_Alta_bool_true")
      )
    );
    source.insertExpressionStatement(
      source.createIntegerLiteral("break")
    );
    source.exitInsertionPoint();
    source.exitInsertionPoint();

    source.insertConditionalStatement(
      source.createUnaryOperation(
        CAST::UOperatorType::Not,
        source.createFetch(okName)
      )
    );
    source.insertBlock();

    bool isFirst = true;

    for (size_t i = 0; i < blk->catchBlocks.size(); i++) {
      auto& [type, stmt] = blk->catchBlocks[i];
      auto& id = blk->catchIDs[i];
      auto& [typeDet, stmtDet] = info->catchBlocks[i];
      auto& var = info->errorVariables[i];
      auto& scope = info->catchScopes[i];

      auto comparison = source.createBinaryOperation(
        CAST::OperatorType::EqualTo,
        source.createFunctionCall(
          source.createFetch("strcmp"),
          {
            source.createAccessor(
              source.createAccessor(
                source.createFetch("_Alta_global_runtime"),
                "lastError"
              ),
              "typeName"
            ),
            source.createStringLiteral(mangleType(typeDet->type.get())),
          }
        ),
        source.createIntegerLiteral(0)
      );

      if (isFirst) {
        isFirst = false;
        source.insertConditionalStatement(comparison);
      } else {
        source.enterConditionalAlternative(i - 1);
        source.insert(comparison);
      }
      source.insertBlock();

      source.insertVariableDefinition(
        transpileType(var->type.get()),
        mangleName(var.get()),
        source.createDereference(
          source.createCast(
            source.createAccessor(
              source.createAccessor(
                source.createFetch("_Alta_global_runtime"),
                "lastError"
              ),
              "value"
            ),
            transpileType(var->type->point().get())
          )
        )
      );

      transpile(stmt.get(), stmtDet.get());

      source.insertExpressionStatement(
        source.createFunctionCall(source.createFetch("_Alta_reset_error"), {
          source.createFetch(idxName),
        })
      );

      source.exitInsertionPoint();
    }

    if (!isFirst) {
      source.enterConditionalUltimatum();
    }

    source.insertBlock();

    source.insertExpressionStatement(
      source.createFunctionCall(source.createFetch("_Alta_reset_error"), {
        source.createFetch(idxName),
      })
    );

    if (blk->catchAllBlock) {
      transpile(blk->catchAllBlock.get(), info->catchAllBlock.get());
    } else {
      source.insertReturnDirective(source.createFetch("_Alta_bool_false"));
    }

    source.exitInsertionPoint();

    if (!isFirst) {
      source.exitInsertionPoint();
    }

    source.exitInsertionPoint();
    source.exitInsertionPoint();
  } else if (nodeType == AltaNodeType::ThrowStatement) {
    auto stmt = dynamic_cast<AAST::ThrowStatement*>(node);
    auto info = dynamic_cast<DH::ThrowStatement*>(_info);

    auto expr = transpile(stmt->expression.get(), info->expression.get());
    expr = doCopyCtor(expr, stmt->expression, info->expression);

    auto type = AltaCore::DET::Type::getUnderlyingType(info->expression.get())->deconstify();

    source.insertExpressionStatement(
      source.createAssignment(
        source.createAccessor(
          source.createAccessor(
            source.createFetch("_Alta_global_runtime"),
            "lastError"
          ),
          "isNative"
        ),
        source.createFetch(type->isNative ? "_Alta_bool_true" : "_Alta_bool_false")
      )
    );

    if (type->isNative) {
      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              source.createFetch("_Alta_global_runtime"),
              "lastError"
            ),
            "typeName"
          ),
          source.createStringLiteral(mangleType(type.get()))
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              source.createFetch("_Alta_global_runtime"),
              "lastError"
            ),
            "value"
          ),
          source.createFunctionCall(
            source.createFetch("malloc"),
            {
              source.createSizeof(transpileType(type.get())),
            }
          )
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createDereference(
            source.createCast(
              source.createAccessor(
                source.createAccessor(
                  source.createFetch("_Alta_global_runtime"),
                  "lastError"
                ),
                "value"
              ),
              transpileType(type->point().get())
            )
          ),
          expr
        )
      );
    } else {
      auto id = tempVarIDs[currentScope->id]++;
      auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id);
      source.insertVariableDefinition(
        source.createType("_Alta_error_handler_node", { { CAST::TypeModifierFlag::Pointer } }),
        tmpName,
        source.createAccessor(
          source.createAccessor(
            source.createFetch("_Alta_global_runtime"),
            "lastError"
          ),
          "handlerStack"
        )
      );

      auto id3 = tempVarIDs[currentScope->id]++;
      auto tmpName3 = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id3);
      source.insertVariableDefinition(
        transpileType(type.get()),
        tmpName3,
        expr
      );

      source.insertWhileLoop(source.createFetch(tmpName));
      source.insertBlock();

      auto id2 = tempVarIDs[currentScope->id]++;
      auto tmpName2 = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id2);
      source.insertVariableDefinition(
        source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } }),
        tmpName2,
        source.createFetch("NULL")
      );

      source.insertConditionalStatement(
        source.createBinaryOperation(
          CAST::OperatorType::EqualTo,
          source.createFunctionCall(
            source.createFetch("strcmp"),
            {
              source.createAccessor(
                source.createDereference(
                  source.createFetch(tmpName)
                ),
                "typeName"
              ),
              source.createStringLiteral(mangleType(type.get()))
            }
          ),
          source.createIntegerLiteral(0)
        )
      );
      source.insertBlock();

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              source.createFetch("_Alta_global_runtime"),
              "lastError"
            ),
            "typeName"
          ),
          source.createStringLiteral(mangleType(type.get()))
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              source.createFetch("_Alta_global_runtime"),
              "lastError"
            ),
            "value"
          ),
          source.createFunctionCall(
            source.createFetch("malloc"),
            {
              source.createSizeof(transpileType(type.get())),
            }
          )
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createDereference(
            source.createCast(
              source.createAccessor(
                source.createAccessor(
                  source.createFetch("_Alta_global_runtime"),
                  "lastError"
                ),
                "value"
              ),
              transpileType(type->point().get())
            )
          ),
          source.createFetch(tmpName3)
        )
      );

      source.insertExpressionStatement(source.createFetch("break"));

      source.exitInsertionPoint();

      if (!type->isNative) {
        source.enterConditionalAlternative(0);
        source.insert(
          source.createAssignment(
            source.createFetch(tmpName2),
            source.createFunctionCall(
              source.createFetch("_Alta_getParentClass_" + mangleName(type->klass.get())),
              {
                source.createPointer(source.createFetch(tmpName3)),
                source.createAccessor(
                  source.createDereference(
                    source.createFetch(tmpName)
                  ),
                  "typeName"
                )
              }
            )
          )
        );
        source.insertBlock();

        source.insertExpressionStatement(
          source.createAssignment(
            source.createAccessor(
              source.createAccessor(
                source.createFetch("_Alta_global_runtime"),
                "lastError"
              ),
              "typeName"
            ),
            source.createAccessor(
              source.createDereference(
                source.createFetch(tmpName)
              ),
              "typeName"
            )
          )
        );

        auto id4 = tempVarIDs[currentScope->id]++;
        auto tmpName4 = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id4);
        source.insertVariableDefinition(
          transpileType(type->point().get()),
          tmpName4,
          source.createCast(
            source.createFunctionCall(
              source.createFetch("malloc"),
              {
                source.createSizeof(transpileType(type.get())),
              }
            ),
            transpileType(type->point().get())
          )
        );
        source.insertExpressionStatement(
          source.createAssignment(
            source.createDereference(
              source.createFetch(tmpName4)
            ),
            source.createFetch(tmpName3)
          )
        );

        source.insertExpressionStatement(
          source.createAssignment(
            source.createAccessor(
              source.createAccessor(
                source.createFetch("_Alta_global_runtime"),
                "lastError"
              ),
              "value"
            ),
            source.createFunctionCall(
              source.createFetch("_Alta_getParentClass_" + mangleName(type->klass.get())),
              {
                source.createFetch(tmpName4),
                source.createAccessor(
                  source.createDereference(
                    source.createFetch(tmpName)
                  ),
                  "typeName"
                )
              }
            )
          )
        );

        source.insertExpressionStatement(source.createFetch("break"));
      }
      
      source.exitInsertionPoint();
      source.exitInsertionPoint();

      source.insertExpressionStatement(
        source.createAssignment(
          source.createFetch(tmpName),
          source.createAccessor(
            source.createDereference(
              source.createFetch(tmpName)
            ),
            "next"
          )
        )
      );

      source.exitInsertionPoint();
      source.exitInsertionPoint();
    }

    auto scope = info->inputScope->isTry ? info->inputScope : info->inputScope->findTry().lock();
    if (auto func = scope->parentFunction.lock()) {
      if (func->throws) {
        source.insertReturnDirective(source.createFetch("_Alta_bool_false"));
      }
    } else {
      source.insertExpressionStatement(
        source.createIntegerLiteral("break")
      );
    }
  }
  return nullptr;
};
void Talta::CTranspiler::transpile(std::shared_ptr<AltaCore::AST::RootNode> altaRoot) {
  cRoot = std::make_shared<Ceetah::AST::RootNode>();
  hRoot = std::make_shared<Ceetah::AST::RootNode>();
  dRoot = std::make_shared<Ceetah::AST::RootNode>();
  source = Ceetah::Builder(cRoot);
  header = Ceetah::Builder(hRoot);
  definitions = Ceetah::Builder(dRoot);
  generics.clear();
  currentModule = altaRoot->info->module;

  auto mangledModuleName = mangleName(currentModule.get());

  definitions.insertPreprocessorConditional("defined(_ALTA_SAVE_DEFS_" + mangledModuleName + ") && defined(_ALTA_MODULE_ALL_" + mangledModuleName + ")");
  definitions.insertPreprocessorDefinition("_AMA_WAS_DEFINED_" + mangledModuleName);
  definitions.insertPreprocessorUndefinition("_ALTA_MODULE_ALL_" + mangledModuleName);
  definitions.exitInsertionPoint();


  definitions.insertPreprocessorConditional("!defined(_ALTA_SAVE_DEFS_" + mangledModuleName + ") && defined(_AMA_WAS_DEFINED_" + mangledModuleName + ")");
  definitions.insertPreprocessorDefinition("_ALTA_MODULE_ALL_" + mangledModuleName);
  definitions.insertPreprocessorUndefinition("_ALTA_SAVE_DEFS_" + mangledModuleName);
  definitions.exitInsertionPoint();

  header.insertPreprocessorInclusion("_ALTA_RUNTIME_COMMON_HEADER_" + mangledModuleName, Ceetah::AST::InclusionType::Computed);

  //header.insertPreprocessorConditional("!defined(_ALTA_MODULE_HEADER_" + mangledModuleName + ")");
  //header.insertPreprocessorDefinition("_ALTA_MODULE_HEADER_" + mangledModuleName);

  for (auto& incl: moduleIncludes[currentModule->path.toString()]) {
    header.insertPreprocessorInclusion(incl, Ceetah::AST::InclusionType::System);
  }

  for (size_t i = 0; i < altaRoot->statements.size(); i++) {
    auto& stmt = altaRoot->statements[i];
    auto& stmtInfo = altaRoot->info->statements[i];
    transpile(stmt.get(), stmtInfo.get());
  }

  //header.exitInsertionPoint();

  for (auto& type: currentModule->hoistedFunctionalTypes) {
    defineFunctionalType(type);
  }

  header.insertPreprocessorUndefinition("_ALTA_MODULE_ALL_" + mangledModuleName);

  definitions.insertPreprocessorUndefinition("_ALTA_SAVE_DEFS_" + mangledModuleName);
};

ALTACORE_MAP<std::string, std::tuple<std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::vector<std::shared_ptr<Ceetah::AST::RootNode>>, std::vector<std::shared_ptr<AltaCore::DET::ScopeItem>>, std::shared_ptr<AltaCore::DET::Module>>> Talta::recursivelyTranspileToC(std::shared_ptr<AltaCore::AST::RootNode> altaRoot, CTranspiler* transpiler) {
  ALTACORE_MAP<std::string, std::tuple<std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::shared_ptr<Ceetah::AST::RootNode>, std::vector<std::shared_ptr<Ceetah::AST::RootNode>>, std::vector<std::shared_ptr<AltaCore::DET::ScopeItem>>, std::shared_ptr<AltaCore::DET::Module>>> results;

  bool deleteIt = false;
  if (transpiler == nullptr) {
    deleteIt = true;
    transpiler = new CTranspiler();
  }

  transpiler->transpile(altaRoot);
  std::vector<std::shared_ptr<Ceetah::AST::RootNode>> gRoots;
  std::vector<std::shared_ptr<AltaCore::DET::ScopeItem>> gItems;
  for (auto& generic: transpiler->generics) {
    gRoots.push_back(generic.second);
    gItems.push_back(generic.first);
  }
  results[altaRoot->info->module->name] = { transpiler->cRoot, transpiler->hRoot, transpiler->dRoot, gRoots, gItems, altaRoot->info->module };
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
