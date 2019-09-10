#include "../include/talta.hpp"
#include "../include/talta/util.hpp"
#include "picosha2.h"

namespace Talta {
  namespace {
    using AltaNodeType = AltaCore::AST::NodeType;
    using ANT = AltaNodeType;
    namespace AAST = AltaCore::AST;
    namespace CAST = Ceetah::AST;
    namespace DH = AltaCore::DH;
    namespace DET = AltaCore::DET;
  };

  ALTACORE_MAP<std::string, std::vector<std::string>> moduleIncludes;
  ALTACORE_MAP<std::string, bool> varargTable;
  ALTACORE_MAP<std::string, size_t> tempVarIDs;
  ALTACORE_MAP<std::string, bool> inHeaderTable;
  ALTACORE_MAP<std::string, bool> alwaysImportTable;
  ALTACORE_MAP<std::string, bool> packedTable;

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

  const ALTACORE_MAP<ANT, CTranspiler::CoroutineMemberFunction> CTranspiler::transpilationMethods = {
    { ANT::ExpressionStatement, &CTranspiler::transpileExpressionStatement },
    { ANT::Type, &CTranspiler::transpileType },
    { ANT::BlockNode, &CTranspiler::transpileBlockNode },
    { ANT::FunctionDefinitionNode, &CTranspiler::transpileFunctionDefinitionNode },
    { ANT::ReturnDirectiveNode, &CTranspiler::transpileReturnDirectiveNode },
    { ANT::IntegerLiteralNode, &CTranspiler::transpileIntegerLiteralNode },
    { ANT::VariableDefinitionExpression, &CTranspiler::transpileVariableDefinitionExpression },
    { ANT::Accessor, &CTranspiler::transpileAccessor },
    { ANT::Fetch, &CTranspiler::transpileFetch },
    { ANT::AssignmentExpression, &CTranspiler::transpileAssignmentExpression },
    { ANT::BooleanLiteralNode, &CTranspiler::transpileBooleanLiteralNode },
    { ANT::BinaryOperation, &CTranspiler::transpileBinaryOperation },
    { ANT::ImportStatement, &CTranspiler::transpileImportStatement },
    { ANT::FunctionCallExpression, &CTranspiler::transpileFunctionCallExpression },
    { ANT::StringLiteralNode, &CTranspiler::transpileStringLiteralNode },
    { ANT::FunctionDeclarationNode, &CTranspiler::transpileFunctionDeclarationNode },
    { ANT::ConditionalStatement, &CTranspiler::transpileConditionalStatement },
    { ANT::ConditionalExpression, &CTranspiler::transpileConditionalExpression },
    { ANT::ClassDefinitionNode, &CTranspiler::transpileClassDefinitionNode },
    { ANT::ClassMethodDefinitionStatement, &CTranspiler::transpileClassMethodDefinitionStatement },
    { ANT::ClassSpecialMethodDefinitionStatement, &CTranspiler::transpileClassSpecialMethodDefinitionStatement },
    { ANT::ClassInstantiationExpression, &CTranspiler::transpileClassInstantiationExpression },
    { ANT::PointerExpression, &CTranspiler::transpilePointerExpression },
    { ANT::DereferenceExpression, &CTranspiler::transpileDereferenceExpression },
    { ANT::WhileLoopStatement, &CTranspiler::transpileWhileLoopStatement },
    { ANT::CastExpression, &CTranspiler::transpileCastExpression },
    { ANT::ClassReadAccessorDefinitionStatement, &CTranspiler::transpileClassReadAccessorDefinitionStatement },
    { ANT::CharacterLiteralNode, &CTranspiler::transpileCharacterLiteralNode },
    { ANT::TypeAliasStatement, &CTranspiler::transpileTypeAliasStatement },
    { ANT::SubscriptExpression, &CTranspiler::transpileSubscriptExpression },
    { ANT::SuperClassFetch, &CTranspiler::transpileSuperClassFetch },
    { ANT::InstanceofExpression, &CTranspiler::transpileInstanceofExpression },
    { ANT::Generic, &CTranspiler::transpileGeneric },
    { ANT::ForLoopStatement, &CTranspiler::transpileForLoopStatement },
    { ANT::RangedForLoopStatement, &CTranspiler::transpileRangedForLoopStatement },
    { ANT::UnaryOperation, &CTranspiler::transpileUnaryOperation },
    { ANT::SizeofOperation, &CTranspiler::transpileSizeofOperation },
    { ANT::FloatingPointLiteralNode, &CTranspiler::transpileFloatingPointLiteralNode },
    { ANT::StructureDefinitionStatement, &CTranspiler::transpileStructureDefinitionStatement },
    { ANT::ExportStatement, &CTranspiler::transpileExportStatement },
    { ANT::DeleteStatement, &CTranspiler::transpileDeleteStatement },
    { ANT::ControlDirective, &CTranspiler::transpileControlDirective },
    { ANT::TryCatchBlock, &CTranspiler::transpileTryCatchBlock },
    { ANT::ThrowStatement, &CTranspiler::transpileThrowStatement },
    { ANT::NullptrExpression, &CTranspiler::transpileNullptrExpression },
    { ANT::CodeLiteralNode, &CTranspiler::transpileCodeLiteralNode },
    { ANT::AttributeStatement, &CTranspiler::transpileAttributeStatement },
    { ANT::BitfieldDefinitionNode, &CTranspiler::transpileBitfieldDefinitionNode },
    { ANT::LambdaExpression, &CTranspiler::transpileLambdaExpression },
    { ANT::SpecialFetchExpression, &CTranspiler::transpileSpecialFetchExpression },
    { ANT::ClassOperatorDefinitionStatement, &CTranspiler::transpileClassOperatorDefinitionStatement },
  };

  const CTranspiler::CopyInfo CTranspiler::defaultCopyInfo = std::make_pair(false, false);
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
      auto [arg, info] = *solo;
      auto transpiled = transpile(arg, info);
      auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
      auto result = cast(transpiled, exprType, targetType, true, additionalCopyInfo(arg, info));
      for (size_t i = 0; i < targetType->referenceLevel(); i++) {
        result = source.createPointer(result);
      }
      args.push_back(result);
    } else if (auto multi = ALTACORE_VARIANT_GET_IF<std::vector<std::pair<std::shared_ptr<AAST::ExpressionNode>, std::shared_ptr<DH::ExpressionNode>>>>(&arg)) {
      if (varargTable[id]) {
        for (auto [arg, info]: *multi) {
          auto transpiled = transpile(arg, info);
          auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
          auto result = cast(transpiled, exprType, targetType, true, additionalCopyInfo(arg, info));
          for (size_t i = 0; i < targetType->referenceLevel(); i++) {
            result = source.createPointer(result);
          }
          args.push_back(result);
        }
      } else {
        std::vector<std::shared_ptr<CAST::Expression>> arrItems;
        for (auto [arg, info]: *multi) {
          auto transpiled = transpile(arg, info);
          auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
          auto result = cast(transpiled, exprType, targetType, true, additionalCopyInfo(arg, info));
          for (size_t i = 0; i < targetType->referenceLevel(); i++) {
            result = source.createPointer(result);
          }
          arrItems.push_back(result);
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
  if (type->isOptional) {
    return "_Alta_optional_" + mangleType(type->optionalTarget.get());
  } if (type->isFunction) {
    if (!type->isRawFunction) {
      return "_Alta_basic_function";
    }
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
  } else if (type->isUnion()) {
    std::string result = "_Alta_union";
    for (auto& item: type->unionOf) {
      result += '_';
      result += mangleType(item.get());
    }
    return result;
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

  if (!type->isRawFunction) {
    auto rawCopy = type->copy();
    rawCopy->isRawFunction = true;
    mangled += cTypeNameify(rawCopy.get(), true);
  }

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
   * `_4_` is reserved for scope identifier delineation
   * `_5_` is reserved for version delimitation
   * `_6_` is reserved for version prerelease delineation
   * `_7_` is reserved for version build information delineation
   * `_8_` is reserved for variable function parameter type separation
   * `_9_` is reserved for parameter name separation
   * `_10_` is reserved for lambda ID delineation
   * `_11_` is reserved for function return type delineation
   */

  for (size_t i = 0; i < name.size(); i++) {
    auto character = name[i];
    if (character == '_') {
      escaped += "__";
    } else if (
      (character >= '0' && character <= '9') || // 0-9
      (character >= 'A' && character <= 'Z') || // A-Z
      (character >= 'a' && character <= 'z')    // a-z
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
    isLiteral = func->isLiteral;
    itemName = isLiteral ? func->name : escapeName(func->name);
    for (auto arg: func->genericArguments) {
      itemName += "_2_" + mangleType(arg.get());
    }

    if (!isLiteral) {
      itemName = escapeName(itemName);
      for (auto& [name, type, isVariable, id]: func->parameters) {
        itemName += "_9_" + escapeName(name) + ((isVariable) ? "_8_" : "_1_") + mangleType(type.get());
      }
      if (func->returnType) {
        itemName += "_11_" + mangleType(func->returnType.get());
      }
    }
  } else if (nodeType == NodeType::Variable) {
    auto var = dynamic_cast<DET::Variable*>(item);
    isLiteral = isLiteral || var->isLiteral;
    itemName = isLiteral ? var->name : escapeName(var->name);
    if (auto ps = var->parentScope.lock()) {
      if (auto pc = ps->parentClass.lock()) {
        isLiteral = pc->isLiteral;
      }
    }
    if (var->isVariable) {
      var->isVariable = false;
      auto tmp = "_Alta_array_" + mangleName(var);
      var->isVariable = true;
      return tmp;
    }
  } else if (nodeType == NodeType::Namespace) {
    auto ns = dynamic_cast<DET::Namespace*>(item);
    isLiteral = false;
    itemName = isLiteral ? ns->name : escapeName(ns->name);
  } else if (nodeType == NodeType::Class) {
    auto klass = dynamic_cast<DET::Class*>(item);
    isLiteral = klass->isLiteral;
    itemName = isLiteral ? klass->name : escapeName(klass->name);
    for (auto arg: klass->genericArguments) {
      itemName += "_2_" + mangleType(arg.get());
    }
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

  // new final step for non-literal names: hashing
  // this is necessary to establish a maximum length for identifiers that
  // is nearly impossible to have the same output for different inputs
  if (!isLiteral) {
    mangled = "Alta_" + picosha2::hash256_hex_string(mangled);
  }

  return mangled;
};

/*
to include a module:
header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangleName(altaRoot->$module) + "_0_INCLUDE_" + mangleName(dependency), Ceetah::AST::InclusionType::Computed);
*/

std::shared_ptr<Ceetah::AST::Type> Talta::CTranspiler::transpileType(AltaCore::DET::Type* type) {
  return source.createType(cTypeNameify(type), convertTypeModifiers(type->modifiers), !type->isNative && type->klass && !type->klass->isTyped);
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
  std::string defStr;
  if (mangledModName.empty()) {
    defStr = "!defined(_DEFINED_" + def + ')';
  } else {
    defStr = "(defined(" + def + ")) && !defined(_DEFINED_" + def + ')';
  }
  header.insertPreprocessorConditional(defStr);
  header.insertPreprocessorUndefinition(def);
  header.insertPreprocessorDefinition("_DEFINED_" + def);
  insertExportDefinition(def);
};

void Talta::CTranspiler::hoist(std::shared_ptr<AltaCore::DET::ScopeItem> item, bool inHeader, bool includeVariables) {
  auto& target = (inHeader ? header : source);

  if (auto type = std::dynamic_pointer_cast<DET::Type>(item)) {
    auto name = cTypeNameify(type.get());
    auto mods = convertTypeModifiers(type->modifiers);

    if (type->isOptional) {
      auto other = type->optionalTarget;
      hoist(other, inHeader);

      auto def = "_ALTA_OPTIONAL_" + name.substr(15);
      target.insertPreprocessorConditional("!defined(" + def + ")");
      target.insertPreprocessorDefinition(def);
      std::vector<std::pair<std::string, std::shared_ptr<CAST::Type>>> members = {
        {"objectType", target.createType("_Alta_object_type")},
        {"present", target.createType("_Alta_bool")},
        {"destructor", target.createType("_Alta_optional_destructor")},
      };
      if (!(*other == DET::Type(DET::NativeType::Void))) {
        members.push_back({"target", transpileType(other.get())});
      }
      target.insertStructureDefinition("_struct_" + name, members);
      target.insertTypeDefinition(name, target.createType("_struct_" + name, {}, true));

      target.insertFunctionDefinition("_Alta_copy_" + name, {
        {"self", transpileType(type->point().get())},
      }, transpileType(type.get()), true);
      target.insertVariableDefinition(transpileType(type.get()), "result", target.createDereference(target.createFetch("self")));
      bool didCopy = false;
      auto copied = doCopyCtor(target.createAccessor(target.createDereference(target.createFetch("self")), "target"), other, defaultCopyInfo, &didCopy);
      if (didCopy) {
        target.insertConditionalStatement(
          target.createAccessor(target.createDereference(target.createFetch("self")), "present")
        );
        target.insertBlock();
        
        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(target.createFetch("result"), "target"),
            copied
          )
        );

        target.exitInsertionPoint();
        target.exitInsertionPoint();
      }
      target.insertReturnDirective(target.createFetch("result"));
      target.exitInsertionPoint();

      target.insertFunctionDefinition("_Alta_destroy_" + name, {
        {"_self", target.createType("_Alta_basic_optional", { { CAST::TypeModifierFlag::Pointer } })},
      }, target.createType("void"), true);
      target.insertVariableDefinition(
        transpileType(type->point().get()),
        "self",
        target.createCast(
          target.createFetch("_self"),
          transpileType(type->point().get())
        )
      );
      if (canDestroy(other)) {
        target.insertConditionalStatement(
          target.createAccessor(target.createDereference(target.createFetch("self")), "present")
        );
        target.insertBlock();

        auto tgt = target.createAccessor(target.createDereference(target.createFetch("self")), "target");
        target.insertExpressionStatement(
          doDtor(tgt, other)
        );

        target.exitInsertionPoint();
        target.exitInsertionPoint();
      }
      target.insertExpressionStatement(
        target.createAssignment(
          target.createAccessor(target.createDereference(target.createFetch("self")), "present"),
          target.createFetch("_Alta_bool_false")
        )
      );
      target.exitInsertionPoint();

      target.insertFunctionDefinition("_Alta_make_" + name, {
        {"source", transpileType(other.get())},
      }, transpileType(type.get()), true);
      target.insertVariableDefinition(transpileType(type.get()), "result", target.createArrayLiteral({ target.createIntegerLiteral(0) }));
      target.insertExpressionStatement(
        target.createAssignment(
          target.createAccessor("result", "present"),
          target.createFetch("_Alta_bool_true")
        )
      );
      target.insertExpressionStatement(
        target.createAssignment(
          target.createAccessor("result", "target"),
          target.createFetch("source")
        )
      );
      target.insertExpressionStatement(
        target.createAssignment(
          target.createAccessor("result", "objectType"),
          target.createFetch("_Alta_object_type_optional")
        )
      );
      target.insertExpressionStatement(
        target.createAssignment(
          target.createAccessor("result", "destructor"),
          target.createFetch("_Alta_destroy_" + name)
        )
      );
      target.insertReturnDirective(target.createFetch("result"));
      target.exitInsertionPoint();

      target.insertFunctionDefinition("_Alta_make_empty_" + name, {}, transpileType(type.get()), true);
      target.insertVariableDefinition(transpileType(type.get()), "result", target.createArrayLiteral({ target.createIntegerLiteral(0) }));
      target.insertExpressionStatement(
        target.createAssignment(
          target.createAccessor("result", "objectType"),
          target.createFetch("_Alta_object_type_optional")
        )
      );
      target.insertExpressionStatement(
        target.createAssignment(
          target.createAccessor("result", "destructor"),
          target.createFetch("_Alta_destroy_" + name)
        )
      );
      target.insertReturnDirective(target.createFetch("result"));
      target.exitInsertionPoint();

      target.exitInsertionPoint();
    } if (type->isFunction) {
      auto def = "_ALTA_FUNC_PTR_" + name.substr(15);
      auto rawCopy = type->copy();
      rawCopy->isRawFunction = true;
      if (!type->isRawFunction) {
        auto root = cTypeNameify(rawCopy.get()).substr(15);
        def = "_ALTA_FUNC_" + root;
        name = "_Alta_function_" + root;
      }
      target.insertPreprocessorConditional("!defined(" + def + ")");
      target.insertPreprocessorDefinition(def);
      std::vector<std::shared_ptr<Ceetah::AST::Type>> cParams;
      if (!type->isRawFunction) {
        hoist(rawCopy, inHeader);
        cParams.push_back(target.createType("_Alta_lambda_state"));
      }
      for (auto& [name, param, isVariable, id]: type->parameters) {
        auto target = isVariable ? param->point() : param;
        hoist(param, inHeader);
        cParams.push_back(transpileType(target.get()));
      }
      hoist(type->returnType, inHeader);
      target.insertTypeDefinition(name, target.createType(transpileType(type->returnType.get()), cParams, mods));
      if (!type->isRawFunction) {
        std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> params = {
          {"func", target.createType("_Alta_basic_function")},
        };
        std::vector<CExpression> noStateArgs;
        std::vector<CExpression> args = { target.createAccessor("func", "state") };
        for (size_t i = 0; i < type->parameters.size(); i++) {
          auto paramName = "_param" + std::to_string(i);
          auto paramFetch = target.createFetch(paramName);
          params.push_back({paramName, transpileType(std::get<1>(type->parameters[i]).get())});
          noStateArgs.push_back(paramFetch);
          args.push_back(paramFetch);
        }
        target.insertFunctionDefinition("_Alta_call_" + name, params, transpileType(type->returnType.get()), true);
        auto callExpr = target.createTernaryOperation(
          target.createBinaryOperation(
            CAST::OperatorType::NotEqualTo,
            target.createAccessor("func", "lambda"),
            target.createFetch("NULL")
          ),
          target.createFunctionCall(
            target.createCast(
              target.createAccessor("func", "lambda"),
              target.createType(name)
            ),
            args
          ),
          target.createFunctionCall(
            target.createCast(
              target.createAccessor("func", "plain"),
              target.createType(cTypeNameify(rawCopy.get()))
            ),
            noStateArgs
          )
        );
        if (*type->returnType == DET::Type(DET::NativeType::Void)) {
          target.insertExpressionStatement(callExpr);
        } else {
          target.insertReturnDirective(callExpr);
        }
        target.exitInsertionPoint();

        target.insertFunctionDefinition("_Alta_make_from_plain_" + name, {
          {"plain", transpileType(rawCopy.get())},
        }, target.createType("_Alta_basic_function"), true);
        target.insertVariableDefinition(
          target.createType("_Alta_basic_function"),
          "result",
          target.createArrayLiteral({ target.createIntegerLiteral(0) })
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              "result",
              "objectType"
            ),
            target.createFetch("_Alta_object_type_function")
          )
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              target.createAccessor(
                "result",
                "state"
              ),
              "referenceCount"
            ),
            target.createFetch("NULL")
          )
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              target.createAccessor(
                "result",
                "state"
              ),
              "copyCount"
            ),
            target.createIntegerLiteral(0)
          )
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              target.createAccessor(
                "result",
                "state"
              ),
              "referenceBlockCount"
            ),
            target.createIntegerLiteral(0)
          )
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              target.createAccessor(
                "result",
                "state"
              ),
              "copies"
            ),
            target.createFetch("NULL")
          )
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              target.createAccessor(
                "result",
                "state"
              ),
              "references"
            ),
            target.createFetch("NULL")
          )
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              "result",
              "plain"
            ),
            target.createFetch("plain")
          )
        );

        target.insertExpressionStatement(
          target.createAssignment(
            target.createAccessor(
              "result",
              "lambda"
            ),
            target.createFetch("NULL")
          )
        );

        target.insertReturnDirective(target.createFetch("result"));
        target.exitInsertionPoint();

        target.insertFunctionDefinition("_Alta_copy_" + name, {
          {"func", target.createType("_Alta_basic_function")},
        }, target.createType("_Alta_basic_function"), true);
        target.insertExpressionStatement(
          target.createUnaryOperation(
            CAST::UOperatorType::PreIncrement,
            target.createDereference(target.createAccessor(target.createAccessor("func", "state"), "referenceCount"))
          )
        );
        target.insertReturnDirective(target.createFetch("func"));
        target.exitInsertionPoint();

        target.insertFunctionDefinition("_Alta_destroy_" + name, {
          {"func", target.createType("_Alta_basic_function", { { CAST::TypeModifierFlag::Pointer } })},
        }, target.createType("void"), true);
        target.insertExpressionStatement(
          target.createFunctionCall(target.createFetch("_Alta_object_destroy"), {
            target.createCast(
              target.createFetch("func"),
              target.createType("_Alta_object", { { CAST::TypeModifierFlag::Pointer } })
            ),
          })
        );
        target.exitInsertionPoint();
      }
      target.exitInsertionPoint();
    } else if (type->klass) {
      auto thatMod = AltaCore::Util::getModule(type->klass->parentScope.lock().get()).lock();
      auto mangledModName = mangleName(thatMod.get());
      auto mangledParentName = mangleName(currentModule.get());
      saveExportDefinitions(inHeader);
      target.insertPreprocessorDefinition(headerMangle(type->klass.get()));
      target.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangledModName, Ceetah::AST::InclusionType::Computed);
      restoreExportDefinitions(inHeader);
    } else if (type->isUnion()) {
      auto mangled = mangleType(type.get());
      auto def = "_ALTA_UNION_" + name.substr(11);
      target.insertPreprocessorConditional("!defined(" + def + ")");
      target.insertPreprocessorDefinition(def);
      std::string uni = "union _u_" + name + " {\n";
      for (auto& item: type->unionOf) {
        hoist(item, inHeader);
        uni += transpileType(item.get())->toString() + " _m_" + mangleType(item.get()) + ";\n";
      }
      uni += "}";
      target.insertStructureDefinition("_s_" + name, {
        {"objectType", target.createType("_Alta_object_type")},
        {"typeName", target.createType("char", { { Ceetah::AST::TypeModifierFlag::Pointer } })},
        {"destructor", target.createType("_Alta_union_destructor")},
        {"members", target.createType(uni)},
      });
      target.insertTypeDefinition(name, target.createType("_s_" + name, mods, true));
      target.insertFunctionDefinition("_copy_" + name, {
        std::make_tuple("other", target.createType(name, { { Ceetah::AST::TypeModifierFlag::Pointer } }))
      }, target.createType(name), true);
      target.insertVariableDefinition(
        target.createType(name),
        "result",
        target.createDereference(
          target.createFetch("other")
        )
      );
      size_t i = 0;
      for (auto& item: type->unionOf) {
        if (!item->isNative) {
          auto mangledItem = mangleType(item.get());
          auto cmp = target.createBinaryOperation(
            Ceetah::AST::OperatorType::EqualTo,
            target.createFunctionCall(
              target.createFetch("strcmp"),
              {
                target.createAccessor(
                  target.createDereference(
                    target.createFetch("other")
                  ),
                  "typeName"
                ),
                target.createStringLiteral(mangledItem),
              }
            ),
            target.createIntegerLiteral(0)
          );
          if (i == 0) {
            target.insertConditionalStatement(cmp);
          } else {
            target.enterConditionalAlternative(i - 1);
            target.insert(cmp);
          }
          ++i;
          target.insertExpressionStatement(
            target.createAssignment(
              target.createAccessor(
                target.createAccessor(
                  target.createFetch("result"),
                  "members"
                ),
                "_m_" + mangledItem
              ),
              doCopyCtor(
                target.createAccessor(
                  target.createAccessor(
                    target.createDereference(
                      target.createFetch("other")
                    ),
                    "members"
                  ),
                  "_m_" + mangledItem
                ),
                item,
                defaultCopyInfo
              )
            )
          );
        }
      }
      if (i > 0) {
        target.exitInsertionPoint();
      }
      target.insertReturnDirective(target.createFetch("result"));
      target.exitInsertionPoint();

      target.insertFunctionDefinition("_destroy_" + name, {
        std::make_tuple("self", target.createType(name, { { Ceetah::AST::TypeModifierFlag::Pointer } }))
      }, target.createType("void"), true);
      i = 0;
      for (auto& item: type->unionOf) {
        if (canDestroy(item)) {
          auto mangledItem = mangleType(item.get());
          auto cmp = target.createBinaryOperation(
            Ceetah::AST::OperatorType::EqualTo,
            target.createFunctionCall(
              target.createFetch("strcmp"),
              {
                target.createAccessor(
                  target.createDereference(
                    target.createFetch("self")
                  ),
                  "typeName"
                ),
                target.createStringLiteral(mangledItem),
              }
            ),
            target.createIntegerLiteral(0)
          );
          if (i == 0) {
            target.insertConditionalStatement(cmp);
          } else {
            target.enterConditionalAlternative(i - 1);
            target.insert(cmp);
          }
          ++i;
          auto tgt = target.createAccessor(
            target.createAccessor(
              target.createDereference(
                target.createFetch("self")
              ),
              "members"
            ),
            "_m_" + mangledItem
          );
          target.insertExpressionStatement(doDtor(tgt, item));
        }
      }
      if (i > 0) {
        target.exitInsertionPoint();
      }
      target.exitInsertionPoint();

      target.exitInsertionPoint();
    }
  } else if (item->nodeType() != DET::NodeType::Variable || includeVariables) {
    auto importModule = AltaCore::Util::getModule(item->parentScope.lock().get()).lock();
    auto mangledImportName = mangleName(importModule.get());
    auto mangledParentName = mangleName(currentModule.get());

    importModule->dependents.push_back(currentModule);

    saveExportDefinitions(inHeader);
    target.insertPreprocessorDefinition(headerMangle(item.get()));
    target.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangledImportName, Ceetah::AST::InclusionType::Computed);
    restoreExportDefinitions(inHeader);
  }
};

void Talta::CTranspiler::includeGeneric(std::shared_ptr<AltaCore::DET::ScopeItem> generic, bool inHeader) {
  if (generic->genericParameterCount < 1) return;

  auto& target = inHeader ? header : source;

  auto importModule = AltaCore::Util::getModule(generic->parentScope.lock().get()).lock();
  auto mangledImportName = mangleName(importModule.get());
  auto mangledParentName = mangleName(currentModule.get());

  saveExportDefinitions(inHeader);

  target.insertPreprocessorDefinition(headerMangle(generic.get()));

  target.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangledImportName, Ceetah::AST::InclusionType::Computed);

  restoreExportDefinitions(inHeader);
};


void Talta::CTranspiler::includeClassIfNecessary(std::shared_ptr<AltaCore::DET::Type> type) {
  if (!type->klass) return;
  auto thatMod = AltaCore::Util::getModule(type->klass->scope.get()).lock();
  if (currentModule->id != thatMod->id) return;
  auto mangledModName = mangleName(thatMod.get()); // thatMod and currentMod are the same, so you could use either one
  saveExportDefinitions(true);
  header.insertPreprocessorDefinition(headerMangle(type->klass.get()));
  header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledModName + "_0_INCLUDE_" + mangledModName, Ceetah::AST::InclusionType::Computed);
  restoreExportDefinitions(true);
};

void Talta::CTranspiler::stackBookkeepingStart(std::shared_ptr<AltaCore::DET::Scope> scope) {
  if (!scope->noRuntime) {
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
  }
};

void Talta::CTranspiler::stackBookkeepingStop(std::shared_ptr<AltaCore::DET::Scope> scope) {
  if (!scope->noRuntime) {
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
  }
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> transpiled, std::shared_ptr<AltaCore::AST::ExpressionNode> expr, std::shared_ptr<AltaCore::DH::ExpressionNode> info, bool* didCopy) {
  auto retExpr = transpiled;
  auto exprType = AltaCore::DET::Type::getUnderlyingType(info.get());
  auto nodeType = expr->nodeType();
  if (didCopy) {
    *didCopy = false;
  }
  if (
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
    nodeType != ANT::ClassInstantiationExpression &&
    nodeType != ANT::FunctionCallExpression
  ) {
    if (exprType->isNative) {
      if (exprType->isFunction && !exprType->isRawFunction) {
        auto rawCopy = exprType->copy();
        rawCopy->isRawFunction = true;
        auto name = "_Alta_function_" + cTypeNameify(rawCopy.get()).substr(15);
        retExpr = source.createFunctionCall(source.createFetch("_Alta_copy_" + name), {
          retExpr,
        });
        if (didCopy) {
          *didCopy = true;
        }
      }
    } else {
      if (additionalCopyInfo(expr, info).second && exprType->indirectionLevel() < 1) {
        retExpr = tmpify(retExpr, exprType);
      }
      if (
        exprType->isUnion()
      ) {
        retExpr = source.createFunctionCall(
          source.createFetch("_copy_" + cTypeNameify(exprType.get())),
          {
            source.createPointer(retExpr),
          }
        );
      } else if (
        exprType->isOptional
      ) {
        retExpr = source.createFunctionCall(
          source.createFetch("_Alta_copy_" + cTypeNameify(exprType.get())),
          {
            source.createPointer(retExpr),
          }
        );
      } else if (
        // make sure we have a copy constructor,
        // otherwise, there's no point in continuing
        exprType->klass->copyConstructor
      ) {
        retExpr = source.createFunctionCall(
          source.createFetch("_cn_" + mangleName(exprType->klass->copyConstructor.get())),
          {
            source.createPointer(retExpr),
          }
        );
        if (didCopy) {
          *didCopy = true;
        }
      }
    }
  }
  return retExpr;
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::doCopyCtor(std::shared_ptr<Ceetah::AST::Expression> retExpr, std::shared_ptr<AltaCore::DET::Type> exprType, CopyInfo additionalCopyInfo, bool* didCopy) {
  if (didCopy) {
    *didCopy = false;
  }
  if (
    // pointers are copied by value
    //
    // note that this does not include references,
    // those do need to be copied
    exprType->pointerLevel() < 1
  ) {
    if (exprType->isNative) {
      if (exprType->isFunction && !exprType->isRawFunction) {
        auto rawCopy = exprType->copy();
        rawCopy->isRawFunction = true;
        auto name = "_Alta_function_" + cTypeNameify(rawCopy.get()).substr(15);
        retExpr = source.createFunctionCall(source.createFetch("_Alta_copy_" + name), {
          retExpr,
        });
        if (didCopy) {
          *didCopy = true;
        }
      }
    } else {
      if (additionalCopyInfo.second && exprType->indirectionLevel() < 1) {
        retExpr = tmpify(retExpr, exprType);
      }
      if (
        exprType->isUnion()
      ) {
        retExpr = source.createFunctionCall(
          source.createFetch("_copy_" + cTypeNameify(exprType.get())),
          {
            source.createPointer(retExpr),
          }
        );
        if (didCopy) {
          *didCopy = true;
        }
      } else if (
        exprType->isOptional
      ) {
        retExpr = source.createFunctionCall(
          source.createFetch("_Alta_copy_" + cTypeNameify(exprType.get())),
          {
            source.createPointer(retExpr),
          }
        );
        if (didCopy) {
          *didCopy = true;
        }
      } else if (
        // make sure we have a copy constructor,
        // otherwise, there's no point in continuing
        exprType->klass->copyConstructor
      ) {
        retExpr = source.createFunctionCall(
          source.createFetch("_cn_" + mangleName(exprType->klass->copyConstructor.get())),
          {
            source.createPointer(retExpr),
          }
        );
        if (didCopy) {
          *didCopy = true;
        }
      }
    }
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
    !exprType->isUnion() &&
    !targetType->isNative &&
    !targetType->isUnion() &&
    exprType->klass &&
    targetType->klass &&
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
    !exprType->isUnion() &&
    !targetType->isNative &&
    !targetType->isUnion() &&
    exprType->klass &&
    targetType->klass &&
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
  /*
  auto modName = mangleName(currentModule.get());
  Ceetah::Builder& target = inHeader ? header : source;

  target.insertPreprocessorDefinition("_ALTA_SAVE_DEFS_" + modName);
  target.insertPreprocessorInclusion("_ALTA_DEF_HEADER_" + modName, Ceetah::AST::InclusionType::Computed);
  target.insertPreprocessorUndefinition("_ALTA_SAVE_DEFS_" + modName);
  */
};

void Talta::CTranspiler::restoreExportDefinitions(bool inHeader) {
  /*
  auto modName = mangleName(currentModule.get());
  Ceetah::Builder& target = inHeader ? header : source;

  target.insertPreprocessorInclusion("_ALTA_DEF_HEADER_" + modName, Ceetah::AST::InclusionType::Computed);
  */
};

auto Talta::CTranspiler::tmpify(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto expr = std::dynamic_pointer_cast<AAST::ExpressionNode>(node);
  auto info = std::dynamic_pointer_cast<DH::ExpressionNode>(_info);
  if (co.iteration() == 0) {
    return co.await(boundTranspile, expr, info);
  } else {
    auto type = AltaCore::DET::Type::getUnderlyingType(info.get());
    auto result = co.result<CExpression>();
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
      if (!currentScope->noRuntime && canPush(type)) {
        source.insertExpressionStatement(
          source.createFunctionCall(
            source.createFetch("_Alta_object_stack_push"),
            {
              source.createPointer(source.createFetch("_Alta_global_runtime.local")),
              source.createCast(
                source.createPointer(source.createFetch(tmpName)),
                source.createType(
                  "_Alta_object",
                  { { Ceetah::AST::TypeModifierFlag::Pointer } }
                )
              ),
            }
          )
        );
      }
      result = source.createMultiExpression({
        source.createAssignment(
          source.createFetch(tmpName),
          result
        ),
        source.createFetch(tmpName),
      });
    }
    return co.finalYield(result);
  }
};

auto Talta::CTranspiler::tmpify(CExpression expr, std::shared_ptr<AltaCore::DET::Type> type, bool withStack) -> CExpression {
  auto id = tempVarIDs[currentScope->id]++;
  auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(id);
  source.insertVariableDefinition(transpileType(type->deconstify().get()), tmpName);
  if (withStack && !currentScope->noRuntime && canPush(type)) {
    source.insertExpressionStatement(
      source.createFunctionCall(
        source.createFetch("_Alta_object_stack_push"),
        {
          source.createPointer(source.createFetch("_Alta_global_runtime.local")),
          source.createCast(
            source.createPointer(source.createFetch(tmpName)),
            source.createType(
              "_Alta_object",
              { { Ceetah::AST::TypeModifierFlag::Pointer } }
            )
          ),
        }
      )
    );
  }
  for (auto i = 0; i < type->referenceLevel(); ++i) {
    expr = source.createPointer(expr);
  }
  CExpression tmpFetch = source.createFetch(tmpName);
  for (auto i = 0; i < type->referenceLevel(); ++i) {
    tmpFetch = source.createDereference(tmpFetch);
  }
  return source.createMultiExpression({
    source.createAssignment(
      source.createFetch(tmpName),
      expr
    ),
    tmpFetch,
  });
};

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::cast(std::shared_ptr<Ceetah::AST::Expression> expr, std::shared_ptr<AltaCore::DET::Type> exprType, std::shared_ptr<AltaCore::DET::Type> dest, bool copy, CopyInfo additionalCopyInfo, bool manual) {
  if (dest->isExactlyCompatibleWith(*exprType)) {
    if (
      copy &&
      additionalCopyInfo.first &&
      (
        dest->indirectionLevel() < 1 ||
        (!dest->isUnion() && exprType->isUnion())
      )
    ) {
      expr = doCopyCtor(expr, exprType, additionalCopyInfo);
    }
    return expr;
  }
  auto path = AltaCore::DET::Type::findCast(exprType, dest, manual);
  if (path.size() == 0) {
    throw std::runtime_error("no way to cast from source type to destination type");
  }
  CExpression result = expr;
  auto currentType = exprType;
  auto ref = [&]() {
    for (size_t i = 0; i < currentType->referenceLevel(); ++i) {
      result = source.createPointer(result);
    }
  };
  auto unref = [&]() {
    for (size_t i = 0; i < currentType->referenceLevel(); ++i) {
      result = source.createDereference(result);
    }
  };
  auto canCopy = [&](std::shared_ptr<DET::Type> type = nullptr) {
    if (!type) type = currentType;
    return !type->isNative && type->indirectionLevel() < 1;
  };
  for (size_t i = 0; i < path.size(); i++) {
    using CCT = AltaCore::DET::CastComponentType;
    using SCT = AltaCore::DET::SpecialCastType;
    auto& component = path[i];
    if (component.type == CCT::Destination) {
      if (component.special == SCT::OptionalPresent) {
        result = source.createAccessor(expr, "present");
        currentType = std::make_shared<DET::Type>(DET::NativeType::Bool);
        copy = false;
      } else if (component.special == SCT::EmptyOptional) {
        result = source.createFunctionCall(source.createFetch("_Alta_make_empty_" + cTypeNameify(dest.get())), {});
        currentType = dest;
        copy = false;
      } else if (component.special == SCT::WrapFunction) {
        auto wrappedCopy = exprType->copy();
        wrappedCopy->isRawFunction = false;
        auto name = "_Alta_function_" + cTypeNameify(exprType.get()).substr(15);
        result = source.createFunctionCall(
          source.createFetch("_Alta_make_from_plain_" + name),
          {
            expr,
          }
        );
        currentType = dest;
        copy = false;
      }
      bool didCopy = false;
      if (copy && additionalCopyInfo.first && canCopy()) {
        result = doCopyCtor(result, currentType, additionalCopyInfo, &didCopy);
      }
    } else if (component.type == CCT::SimpleCoercion) {
      ref();
      result = source.createCast(result, transpileType(component.target.get()));
      currentType = component.target;
      copy = false;
      additionalCopyInfo = std::make_pair(false, true);
      unref();
    } else if (component.type == CCT::Upcast) {
      bool didRetrieval = false;
      auto nextType = std::make_shared<DET::Type>(component.klass);
      if (currentType->pointerLevel() > 0) {
        nextType = nextType->point();
      } else if (currentType->referenceLevel() > 0) {
        nextType = nextType->reference();
      }
      if (currentType->pointerLevel() > 1) {
        throw std::runtime_error("too much indirection for upcast");
      }
      result = doParentRetrieval(result, currentType, nextType, &didRetrieval);
      if (!didRetrieval) {
        throw std::runtime_error("suppossed to be able to do parent retrieval");
      }
      currentType = nextType;
      additionalCopyInfo = std::make_pair(true, false);
    } else if (component.type == CCT::Downcast) {
      bool didRetrieval = false;
      auto nextType = std::make_shared<DET::Type>(component.klass);
      if (currentType->pointerLevel() > 0) {
        nextType = nextType->point();
      } else if (currentType->referenceLevel() > 0) {
        nextType = nextType->reference();
      }
      if (currentType->pointerLevel() > 1) {
        throw std::runtime_error("too much indirection for downcast");
      }
      result = doChildRetrieval(result, currentType, nextType, &didRetrieval);
      if (!didRetrieval) {
        throw std::runtime_error("suppossed to be able to do parent retrieval");
      }
      currentType = nextType;
      additionalCopyInfo = std::make_pair(false, false);
    } else if (component.type == CCT::Reference) {
      if (additionalCopyInfo.second) {
        result = tmpify(result, currentType, false);
      }
      currentType = currentType->reference();
      copy = false;
      additionalCopyInfo = std::make_pair(false, true);
    } else if (component.type == CCT::Dereference) {
      currentType = currentType->dereference();
      additionalCopyInfo = std::make_pair(true, false);
    } else if (component.type == CCT::Wrap) {
      bool didCopy = false;
      if (copy && additionalCopyInfo.first && canCopy()) {
        result = doCopyCtor(result, currentType, additionalCopyInfo, &didCopy);
        copy = false;
      }
      ref();
      result = source.createFunctionCall(source.createFetch("_Alta_make_" + cTypeNameify(dest.get())), {
        result,
      });
      currentType = std::make_shared<DET::Type>(true, currentType);
      copy = false;
      additionalCopyInfo = std::make_pair(false, true);
    } else if (component.type == CCT::Unwrap) {
      result = source.createAccessor(result, "target");
      currentType = currentType->optionalTarget;
      additionalCopyInfo = std::make_pair(true, false);
      unref();
    } else if (component.type == CCT::Widen) {
      auto nextType = component.target;
      auto memberType = component.via;
      auto destTmp = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tempVarIDs[currentScope->id]++);
      auto sourceTmp = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tempVarIDs[currentScope->id]++);
      target->insertVariableDefinition(
        transpileType(nextType->deconstify().get()),
        destTmp
      );
      target->insertVariableDefinition(
        transpileType(currentType->deconstify().get()),
        sourceTmp
      );
      auto sourceSizeof = currentType->isUnion()
        ? static_cast<std::shared_ptr<Ceetah::AST::Expression>>(target->createFetch("sizeof(union _u_" + cTypeNameify(currentType.get()) + ")"))
        : static_cast<std::shared_ptr<Ceetah::AST::Expression>>(target->createSizeof(transpileType(currentType.get())))
        ;
      auto destSizeof = nextType->isUnion()
        ? static_cast<std::shared_ptr<Ceetah::AST::Expression>>(target->createFetch("sizeof(union _u_" + cTypeNameify(nextType.get()) + ")"))
        : static_cast<std::shared_ptr<Ceetah::AST::Expression>>(target->createSizeof(transpileType(nextType.get())))
        ;
      bool didCopy = false;
      if (copy && additionalCopyInfo.first && canCopy()) {
        result = doCopyCtor(result, currentType, additionalCopyInfo, &didCopy);
        copy = false;
      }
      ref();
      result = target->createMultiExpression({
        target->createAssignment(
          target->createFetch(sourceTmp),
          result
        ),
        target->createFunctionCall(
          target->createFetch("memcpy"),
          {
            target->createPointer(
              target->createAccessor(
                target->createFetch(destTmp),
                "members"
              )
            ),
            target->createPointer(
              currentType->isUnion()
                ? static_cast<std::shared_ptr<Ceetah::AST::Expression>>(target->createAccessor(
                    target->createFetch(sourceTmp),
                    "members"
                  ))
                : static_cast<std::shared_ptr<Ceetah::AST::Expression>>(target->createFetch(sourceTmp))
            ),
            target->createTernaryOperation(
              target->createBinaryOperation(
                Ceetah::AST::OperatorType::GreaterThan,
                destSizeof,
                sourceSizeof
              ),
              sourceSizeof,
              destSizeof
            ),
          }
        ),
        target->createAssignment(
          target->createAccessor(
            target->createFetch(destTmp),
            "typeName"
          ),
          (currentType->isUnion())
            ? std::dynamic_pointer_cast<Ceetah::AST::Expression>(target->createAccessor(
                target->createFetch(sourceTmp),
                "typeName"
              ))
            : std::dynamic_pointer_cast<Ceetah::AST::Expression>(target->createStringLiteral(mangleType(memberType.get())))
        ),
        target->createAssignment(
          target->createAccessor(
            target->createFetch(destTmp),
            "destructor"
          ),
          target->createCast(
            target->createFetch("_destroy_" + mangleType(nextType.get())),
            target->createType("_Alta_union_destructor")
          )
        ),
        target->createAssignment(
          target->createAccessor(
            target->createFetch(destTmp),
            "objectType"
          ),
          target->createFetch("_Alta_object_type_union")
        ),
        target->createFetch(destTmp),
      });
      currentType = nextType;
      copy = false;
      additionalCopyInfo = std::make_pair(false, false);
    } else if (component.type == CCT::Narrow) {
      auto nextType = component.target;
      size_t mostCompatible = 0;
      size_t mostCompatibleIndex = 0;
      for (size_t i = 0; i < currentType->unionOf.size(); i++) {
        auto& item = currentType->unionOf[i];
        auto compat = item->compatiblity(*nextType);
        if (compat > mostCompatible) {
          mostCompatible = compat;
          mostCompatibleIndex = i;
        }
      }
      result = source.createAccessor(
        source.createAccessor(
          result,
          "members"
        ),
        "_m_" + mangleType(currentType->unionOf[mostCompatibleIndex].get())
      );
      additionalCopyInfo = std::make_pair(true, false);
      currentType = nextType;
      bool didCopy = false;
      unref();
      if (copy && additionalCopyInfo.first && canCopy()) {
        result = doCopyCtor(result, currentType, additionalCopyInfo, &didCopy);
        copy = false;
      }
    } else if (component.type == CCT::From) {
      auto state = popToGlobal();
      hoist(component.method, false);
      pushFromGlobal(state);

      ref();
      result = source.createFunctionCall(source.createFetch(mangleName(component.method.get())), {
        result,
      });
      copy = false;
      additionalCopyInfo = std::make_pair(false, true);
      currentType = std::make_shared<DET::Type>(component.method->parentScope.lock()->parentClass.lock());
    } else if (component.type == CCT::To) {
      auto state = popToGlobal();
      hoist(component.method, false);
      pushFromGlobal(state);

      auto to = component.method;
      auto classType = std::make_shared<DET::Type>(component.method->parentScope.lock()->parentClass.lock());
      if (additionalCopyInfo.second) {
        result = tmpify(result, currentType, false);
      }
      result = source.createFunctionCall(source.createFetch(mangleName(component.method.get())), {
        source.createPointer(result),
      });
      unref();
      copy = false;
      additionalCopyInfo = std::make_pair(false, true);
      currentType = component.method->returnType;
    } else if (component.type == CCT::Multicast) {
      auto& nextType = component.target;
      bool didCopy = false;
      if (copy && additionalCopyInfo.first && canCopy()) {
        result = doCopyCtor(result, currentType, additionalCopyInfo, &didCopy);
        copy = false;
      }
      auto initialVar = std::dynamic_pointer_cast<CAST::MultiExpression>(tmpify(result, currentType, false));
      auto initialInit = initialVar->expressions.front();
      auto initialFetch = initialVar->expressions.back();
      auto destTmp = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tempVarIDs[currentScope->id]++);
      target->insertVariableDefinition(
        transpileType(nextType->deconstify().get()),
        destTmp,
        target->createArrayLiteral({ target->createIntegerLiteral(0) })
      );
      CExpression initExpr = target->createMultiExpression({
        target->createFunctionCall(target->createFetch("_Alta_bad_cast"), {
          target->createStringLiteral(mangleType(currentType.get())),
          target->createStringLiteral(mangleType(nextType.get())),
        }),
        target->createArrayLiteral({
          target->createIntegerLiteral(0),
        }, transpileType(nextType.get())),
      });
      for (size_t i = 0; i < currentType->unionOf.size(); ++i) {
        auto& member = currentType->unionOf[i];
        initExpr = target->createTernaryOperation(
          target->createBinaryOperation(
            Ceetah::AST::OperatorType::EqualTo,
            target->createFunctionCall(
              target->createFetch("strcmp"),
              {
                target->createAccessor(
                  initialFetch,
                  "typeName"
                ),
                target->createStringLiteral(mangleType(member.get())),
              }
            ),
            target->createIntegerLiteral(0)
          ),
          (component.multicasts[i].second.size() == 0)
            ? target->createMultiExpression({
                target->createFunctionCall(target->createFetch("_Alta_bad_cast"), {
                  target->createStringLiteral(mangleType(currentType.get())),
                  target->createStringLiteral(mangleType(nextType.get())),
                }),
                target->createArrayLiteral({
                  target->createIntegerLiteral(0),
                }, transpileType(nextType.get())),
              })
            : cast(
                target->createAccessor(
                  target->createAccessor(
                    initialFetch,
                    "members"
                  ),
                  "_m_" + mangleType(member.get())
                ),
                member,
                nextType,
                false,
                std::make_pair(false, false),
                false
              )
            ,
          initExpr
        );
      }
      result = target->createMultiExpression({
        initialInit,
        target->createAssignment(
          target->createFetch(destTmp),
          initExpr
        ),
        target->createFetch(destTmp),
      });
      currentType = nextType;
      copy = false;
      additionalCopyInfo = std::make_pair(false, true);
    }
  }
  return result;
};

auto Talta::CTranspiler::bind(const CoroutineMemberFunction function) -> Coroutine::FunctionType {
  return std::bind(function, this, std::placeholders::_1);
};

auto Talta::CTranspiler::doDtor(CExpression expr, std::shared_ptr<AltaCore::DET::Type> exprType, bool* didDtor) -> CExpression {
  if (didDtor) {
    *didDtor = false;
  }
  CExpression result = expr;
  if (canDestroy(exprType)) {
    if (!exprType->isRawFunction) {
      result = source.createFunctionCall(source.createFetch("_Alta_object_destroy"), {
        source.createCast(
          source.createPointer(expr),
          source.createType("_Alta_object", { { CAST::TypeModifierFlag::Pointer } })
        ),
      });
    } else if (exprType->isUnion()) {
      result = source.createFunctionCall(
        source.createFetch("_destroy_" + cTypeNameify(exprType.get())),
        {
          source.createPointer(expr),
        }
      );
    } else if (exprType->isOptional) {
      result = source.createFunctionCall(
        source.createFetch("_Alta_destroy_" + cTypeNameify(exprType.get())),
        {
          source.createPointer(expr),
        }
      );
    } else {
      result = source.createFunctionCall(
        source.createFetch("_d_" + mangleName(exprType->klass->destructor.get())),
        {
          source.createCast(
            source.createPointer(expr),
            source.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } })
          ),
          source.createFetch("_Alta_bool_false"),
        }
      );
    }
    if (didDtor) {
      *didDtor = true;
    }
  }
  return result;
};

// <transpilation-coroutines>
auto Talta::CTranspiler::transpile(Coroutine& co) -> Coroutine& {
  auto [node, info] = co.arguments();
  auto nt = node->nodeType();

  if (co.iteration() == 0) {
    auto previousCurrentScope = currentScope;
    currentScope = info->inputScope;
    co.save(previousCurrentScope);

    if (transpilationMethods.find(nt) != transpilationMethods.end()) {
      return co.await(bind(transpilationMethods.at(nt)), node, info);
    } else {
      return co.yield();
    }
  } else {
    auto [previousCurrentScope] = co.load<std::shared_ptr<DET::Scope>>();
    currentScope = previousCurrentScope;
    return co.finalYield(co.resultAny());
  }
};

auto Talta::CTranspiler::transpileFunctionDefinitionNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto func = std::dynamic_pointer_cast<AAST::FunctionDefinitionNode>(node);
  auto funcInfo = std::dynamic_pointer_cast<DH::FunctionDefinitionNode>(_info);
  if (co.iteration() == 0) {
    bool isGeneric = func->generics.size() > 0;
    co.save(isGeneric, (size_t)0, (size_t)0);
    return co.yield();
  } else {
    auto [isGeneric, genericIdx, loopIteration] = co.load<bool, size_t, size_t>();
    if (
      (!isGeneric && genericIdx >= 1) ||
      (isGeneric && genericIdx >= funcInfo->genericInstantiations.size())
    ) {
      return co.finalYield();
    }

    auto info = isGeneric ? funcInfo->genericInstantiations[genericIdx] : funcInfo;

    if (loopIteration == 0) {
      Ceetah::Builder sourceBuilderCache(nullptr);
      bool targetIsHeader = info->function->isExport || info->function->isMethod;

      auto mod = AltaCore::Util::getModule(info->function->parentScope.lock().get()).lock();
      auto mangledModName = mangleName(mod.get());

      if (isGeneric) {
        auto root = std::make_shared<CAST::RootNode>();
        generics.push_back(std::make_pair(info->function, root));
        sourceBuilderCache = source;
        source = Ceetah::Builder(root);
      }

      if (!targetIsHeader) {
        for (auto& hoistedType: info->function->publicHoistedItems) {
          hoist(hoistedType, false);
        }
      }
      for (auto& hoistedType: info->function->privateHoistedItems) {
        hoist(hoistedType, false);
      }

      if (!targetIsHeader) {
        for (auto arg: info->function->genericArguments) {
          if (arg->klass) {
            auto dependency = AltaCore::Util::getModule(arg->klass->parentScope.lock().get()).lock();
            auto mangledImportName = mangleName(dependency.get());
            source.insertPreprocessorDefinition(headerMangle(arg->klass.get()));
            source.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledModName + "_0_INCLUDE_" + mangledImportName, CAST::InclusionType::Computed);
          } else {
            hoist(arg, false);
          }
        }
      }

      auto mangledFuncName = mangleName(info->function.get());
      std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> cParams;

      if (info->function->isMethod) {
        cParams.push_back(std::make_tuple("_Alta_self", transpileType(info->function->parentClassType.get())));
      }

      for (size_t i = 0; i < info->function->parameterVariables.size(); i++) {
        auto& var = info->function->parameterVariables[i];
        auto& param = func->parameters[i];
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

      hoist(info->function, false);

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
        source.insertExpressionStatement(
          source.createFunctionCall(
            source.createFetch("_Alta_module_init_" + mangledModName),
            {}
          )
        );
      }

      stackBookkeepingStart(info->function->scope);

      for (size_t i = 0; i < info->function->parameterVariables.size(); i++) {
        auto& var = info->function->parameterVariables[i];
        if (
          !currentScope->noRuntime &&
          canPush(var->isVariable ? var->type->follow() : var->type)
        ) {
          if (var->isVariable) {
            source.insertBlock();
            source.insertVariableDefinition(source.createType("size_t"), "_Alta_variable_array_index", source.createIntegerLiteral(0));
            source.insertWhileLoop(
              source.createBinaryOperation(
                CAST::OperatorType::LessThan,
                source.createFetch("_Alta_variable_array_index"),
                source.createFetch("_Alta_array_length_" + mangleName(var.get()).substr(12))
              )
            );
            source.insertBlock();
          }
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
                var->isVariable
                  ? source.createBinaryOperation(
                      CAST::OperatorType::Addition,
                      source.createFetch("_Alta_array_" + mangleName(var.get()).substr(12)),
                      source.createFetch("_Alta_variable_array_index")
                    )
                  : source.createPointer(
                      source.createFetch(mangleName(var.get()))
                    ),
                source.createType(
                  "_Alta_object",
                  { { CAST::TypeModifierFlag::Pointer } }
                )
              ),
            }
          ));
          if (var->isVariable) {
            source.insertExpressionStatement(
              source.createUnaryOperation(
                CAST::UOperatorType::PreIncrement,
                source.createFetch("_Alta_variable_array_index")
              )
            );
            source.exitInsertionPoint();
            source.exitInsertionPoint();
            source.exitInsertionPoint();
          }
        }
      }

      co.save(isGeneric, genericIdx, (size_t)1);
      co.save(targetIsHeader, sourceBuilderCache, mangledFuncName, mangledModName, cParams, returnType);
      return co.yield();
    } else if (loopIteration - 1 < func->body->statements.size()) {
      auto i = loopIteration - 1;
      co.save(isGeneric, genericIdx, loopIteration + 1);
      co.saveAny(co.loadAny());
      return co.await(boundTranspile, func->body->statements[i], info->body->statements[i]);
    } else {
      auto [
        targetIsHeader,
        sourceBuilderCache,
        mangledFuncName,
        mangledModName,
        cParams,
        returnType
      ] = co.load<
        bool,
        Ceetah::Builder,
        std::string,
        std::string,
        std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>>,
        std::shared_ptr<CAST::Type>
      >();

      stackBookkeepingStop(info->function->scope);
      source.exitInsertionPoint();

      if (isGeneric && !targetIsHeader) {
        auto tmp = source;
        source = sourceBuilderCache;
        for (auto arg: info->function->genericArguments) {
          hoist(arg, false);
        }
        for (auto& hoistedType: info->function->publicHoistedItems) {
          hoist(hoistedType, false);
        }
        auto mod = AltaCore::Util::getModule(info->function->parentScope.lock().get()).lock();
        auto mangledModName = mangleName(mod.get());
        source.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
        source = tmp;
      }

      if (targetIsHeader) {
        auto mod = AltaCore::Util::getModule(info->function->parentScope.lock().get()).lock();
        auto mangledModName = mangleName(mod.get());
        auto alwaysImport = alwaysImportTable.find(func->id) != alwaysImportTable.end();
        headerPredeclaration("_ALTA_FUNCTION_" + mangledFuncName, alwaysImport ? "" : mangledModName, !isGeneric);
        if (info->function->isMethod) {
          hoist(info->function->parentClassType->klass, true);
        }
        for (auto arg: info->function->genericArguments) {
          hoist(arg, true);
        }
        for (auto& hoistedType: info->function->publicHoistedItems) {
          hoist(hoistedType, true);
        }
        hoist(info->function->returnType, true);
        header.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
        header.exitInsertionPoint();
      }

      if (isGeneric) {
        source = sourceBuilderCache;
      }

      co.save(isGeneric, genericIdx + 1, (size_t)0);
      return co.yield();
    }
  }
};

auto Talta::CTranspiler::transpileExpressionStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto exprStmt = std::dynamic_pointer_cast<AAST::ExpressionStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ExpressionStatement>(_info);

  if (co.iteration() == 0) {
    if (info->inputScope->parentModule.expired()) {
      // not in module root
      return co.await(bind(&CTranspiler::tmpify), exprStmt->expression, info->expression);
    } else {
      // in module root
      return co.await(boundTranspile, exprStmt->expression, info->expression);
    }
  } else {
    if (info->inputScope->parentModule.expired()) {
      // not in module root
      if (exprStmt->expression->nodeType() != ANT::VariableDefinitionExpression) {
        // we don't need to insert an expression statement for variable definitions,
        // since the expression the return is just a pointer to the new variable
        source.insertExpressionStatement(co.result<CExpression>());
      }
    }
    return co.finalYield();
  }
};

auto Talta::CTranspiler::transpileReturnDirectiveNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto retDir = std::dynamic_pointer_cast<AAST::ReturnDirectiveNode>(node);
  auto info = std::dynamic_pointer_cast<DH::ReturnDirectiveNode>(_info);

  if (co.iteration() == 0) {
    if (retDir->expression != nullptr) {
      return co.await(boundTranspile, retDir->expression, info->expression);
    } else {
      return co.yield();
    }
  } else {
    std::shared_ptr<Ceetah::AST::Expression> transpiled = (retDir->expression != nullptr) ? co.result<CExpression>() : nullptr;
    std::shared_ptr<Ceetah::AST::Expression> expr = nullptr;
    bool isVoid = false;
    if (retDir->expression != nullptr) {
      auto functionReturnType = info->parentFunction ? info->parentFunction->returnType : nullptr;

      if (functionReturnType && functionReturnType->isNative && functionReturnType->nativeTypeName == AltaCore::DET::NativeType::Void && functionReturnType->modifiers.size() == 0) {
        isVoid = true;
      } else {
        // if we're returing a reference, there's no need to copy anything
        if (functionReturnType && functionReturnType->referenceLevel() > 0) {
          expr = transpiled;
        } else {
          auto exprType = AltaCore::DET::Type::getUnderlyingType(info->expression.get());
          expr = cast(transpiled, exprType, functionReturnType, true, additionalCopyInfo(retDir->expression, info->expression));
        }
        if (functionReturnType) {
          for (size_t i = 0; i < functionReturnType->referenceLevel(); i++) {
            expr = source.createPointer(expr);
          }
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

    if (isVoid) {
      source.insertReturnDirective(transpiled);
    } else {
      source.insertReturnDirective(expr ? source.createFetch(tmpName) : nullptr);
    }
    return co.finalYield();
  }
};

auto Talta::CTranspiler::transpileIntegerLiteralNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto intLit = std::dynamic_pointer_cast<AAST::IntegerLiteralNode>(node);
  CExpression expr = source.createIntegerLiteral(intLit->integer);
  return co.finalYield(expr);
};

auto Talta::CTranspiler::transpileType(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  return co.finalYield();
};
auto Talta::CTranspiler::transpileBlockNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto block = std::dynamic_pointer_cast<AAST::BlockNode>(node);
  auto info = std::dynamic_pointer_cast<DH::BlockNode>(_info);
  if (co.iteration() == 0) {
    source.insertBlock();
    stackBookkeepingStart(info->inputScope);
    return co.yield();
  } else if (co.iteration() <= block->statements.size()) {
    auto i = co.iteration() - 1;
    return co.await(boundTranspile, block->statements[i], info->statements[i]);
  } else {
    stackBookkeepingStop(info->inputScope);
    source.exitInsertionPoint();
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileVariableDefinitionExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto varDef = std::dynamic_pointer_cast<AAST::VariableDefinitionExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::VariableDefinitionExpression>(_info);

  if (co.iteration() == 0) {
    bool inModuleRoot = !info->variable->parentScope.expired() && !info->variable->parentScope.lock()->parentModule.expired();
    if (varDef->initializationExpression != nullptr && (!inModuleRoot || info->variable->type->isNative || info->variable->type->indirectionLevel() > 0)) {
      return co.await(boundTranspile, varDef->initializationExpression, info->initializationExpression);
    } else {
      return co.yield();
    }
  } else {
    bool inModuleRoot = !info->variable->parentScope.expired() && !info->variable->parentScope.lock()->parentModule.expired();
    bool exprPresent = varDef->initializationExpression != nullptr && (!inModuleRoot || info->variable->type->isNative || info->variable->type->indirectionLevel() > 0);
    CExpression transpiled = (exprPresent) ? co.result<CExpression>() : nullptr;
    auto mangledVarName = mangleName(info->variable.get());
    auto type = transpileType(info->variable->type.get());

    std::shared_ptr<CAST::Expression> init = nullptr;
    if (exprPresent) {
      auto exprType = AltaCore::DET::Type::getUnderlyingType(info->initializationExpression.get());
      init = cast(transpiled, exprType, info->variable->type, true, additionalCopyInfo(varDef->initializationExpression, info->initializationExpression));
    } else if (info->type->type->pointerLevel() > 0) {
      init = source.createFetch("NULL");
    } else if (info->type->type->isNative) {
      init = source.createArrayLiteral({ source.createIntegerLiteral(0) });
    } else if (!inModuleRoot && !info->type->type->isUnion()) {
      init = source.createFunctionCall(source.createFetch("_cn_" + mangleName(info->type->type->klass->defaultConstructor.get())), {});
    }

    for (size_t i = 0; i < info->type->type->referenceLevel(); i++) {
      init = source.createPointer(init);
    }

    if (inModuleRoot) {
      hoist(info->variable, false);
    }

    source.insertVariableDefinition(type, mangledVarName, init);

    // check whether the variable is contained
    // i.e. whether it's in a function (or a class, later once classes are added)
    // if it's not contained (i.e. it's a defined in a module root), then declare
    // it in the header
    if (inModuleRoot) {
      // it's not contained, therefore, declare it in the header, as well
      auto mod = info->variable->parentScope.lock()->parentModule.lock();
      auto mangledModName = mangleName(mod.get());
      auto alwaysImport = alwaysImportTable.find(varDef->id) != alwaysImportTable.end();
      headerPredeclaration("_ALTA_VARIABLE_" + mangledVarName, alwaysImport ? "" : mangledModName);
      hoist(info->variable->type, true);
      header.insertVariableDeclaration(type, mangledVarName);
      header.exitInsertionPoint();
    } else if (AltaCore::Util::isInFunction(info->variable.get())) {
      // if it is contained in a function, we can return a reference to the newly defined variable
      // and add it to the stack (if it's not native)
      if (!currentScope->noRuntime && canPush(info->variable->type)) {
        source.insertExpressionStatement(source.createFunctionCall(
          source.createFetch("_Alta_object_stack_push"),
          {
            source.createPointer(source.createFetch("_Alta_global_runtime.local")),
            source.createCast(
              source.createPointer(source.createFetch(mangledVarName)),
              source.createType(
                "_Alta_object",
                { { Ceetah::AST::TypeModifierFlag::Pointer } }
              )
            ),
          }
        ));
      }
      CExpression expr = source.createPointer(source.createFetch(mangledVarName));
      return co.finalYield(expr);
    }
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileAccessor(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto acc = std::dynamic_pointer_cast<AAST::Accessor>(node);
  auto info = std::dynamic_pointer_cast<DH::Accessor>(_info);

  if (co.iteration() == 0) {
    if (info->getsVariableLength) {
      auto tgt = std::dynamic_pointer_cast<AAST::Fetch>(acc->target);
      auto tgtInfo = std::dynamic_pointer_cast<DH::Fetch>(info->target);
      auto mangled = mangleName(tgtInfo->narrowedTo.get());
      // ("_Alta_array_").length == 12
      // substr(length + 1) to skip substring
      CExpression expr = source.createFetch("_Alta_array_length_" + mangled.substr(12));
      return co.finalYield(expr);
    }
    CExpression result = nullptr;
    if (info->narrowedTo) {
      if (info->accessesNamespace) {
        result = source.createFetch(mangleName(info->narrowedTo.get()));
        return co.finalYield(result);
      } else {
        return co.await(bind(&CTranspiler::tmpify), acc->target, info->target);
      }
    } else if (info->readAccessor) {
      if (!info->accessesNamespace) {
        return co.await(bind(&CTranspiler::tmpify), acc->target, info->target);
      } else {
        return co.yield();
      }
    } else {
      throw std::runtime_error("AHH, this accessor needs to be narrowed!");
    }
  } else {
    CExpression result = nullptr;
    size_t refLevel = 0;
    if (info->narrowedTo) {
      auto tgt = co.result<CExpression>();
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
      if (info->targetType && info->targetType->bitfield) {
        auto bitfieldField = std::dynamic_pointer_cast<DET::Variable>(info->narrowedTo);
        auto [start, end] = bitfieldField->bitfieldBits;
        std::string bits;
        for (size_t i = start; i <= end; i++) {
          bits.push_back('1');
        }
        for (size_t i = 0; i < start; i++) {
          bits.push_back('0');
        }
        auto mask = std::stoull(bits, nullptr, 2);
        result = source.createBinaryOperation(
          CAST::OperatorType::RightShift,
          source.createBinaryOperation(
            CAST::OperatorType::BitwiseAnd,
            tgt,
            source.createIntegerLiteral(mask)
          ),
          source.createIntegerLiteral(start)
        );
        refLevel = 0;
      } else {
        result = source.createAccessor(tgt, mangleName(info->narrowedTo.get()));
        auto ut = AltaCore::DET::Type::getUnderlyingType(info->narrowedTo);
        refLevel = ut->referenceLevel();
      }
    } else if (info->readAccessor) {
      auto readAccFetch = source.createFetch(mangleName(info->readAccessor.get()));

      auto tmpName = mangleName(info->inputScope.get()) + "_temp_var_" + std::to_string(tempVarIDs[info->inputScope->id]++);

      source.insertVariableDefinition(transpileType(info->readAccessor->returnType.get()), tmpName);

      if (!currentScope->noRuntime && canPush(info->readAccessor->returnType)) {
        source.insertExpressionStatement(
          source.createFunctionCall(
            source.createFetch("_Alta_object_stack_push"),
            {
              source.createPointer(source.createFetch("_Alta_global_runtime.local")),
              source.createCast(
                source.createPointer(source.createFetch(tmpName)),
                source.createType(
                  "_Alta_object",
                  { { Ceetah::AST::TypeModifierFlag::Pointer } }
                )
              ),
            }
          )
        );
      }

      std::vector<std::shared_ptr<Ceetah::AST::Expression>> args;
      if (!info->accessesNamespace) {
        auto selfAlta = acc->target;
        auto selfInfo = info->target;
        auto self = co.result<CExpression>();
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
        args.push_back(source.createPointer(self));
      }

      result = source.createMultiExpression({
        source.createAssignment(
          source.createFetch(tmpName),
          source.createFunctionCall(readAccFetch, args)
        ),
        source.createFetch(tmpName),
      });
      refLevel = info->readAccessor->returnType->referenceLevel();
    }
    if (!result) {
      throw std::runtime_error("AHH, this accessor needs to be narrowed!");
    }
    for (size_t i = 0; i < refLevel; i++) {
      result = source.createDereference(result);
    }
    return co.finalYield(result);
  }
};
auto Talta::CTranspiler::transpileFetch(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto fetch = std::dynamic_pointer_cast<AAST::Fetch>(node);
  auto info = std::dynamic_pointer_cast<DH::Fetch>(_info);

  if (!info->narrowedTo) {
    if (info->readAccessor) {
      CExpression cFetch = source.createFunctionCall(source.createFetch(mangleName(info->readAccessor.get())), {});
      return co.finalYield(cFetch);
    }
    throw std::runtime_error("AHH, this fetch needs to be narrowed!");
  }
  if (info->narrowedTo->nodeType() == AltaCore::DET::NodeType::Variable && info->narrowedTo->name == "this") {
    auto var = std::dynamic_pointer_cast<AltaCore::DET::Variable>(info->narrowedTo);
    if (!var->parentScope.expired() && !var->parentScope.lock()->parentClass.expired()) {
      CExpression expr = source.createDereference(source.createFetch("_Alta_self"));
      return co.finalYield(expr);
    }
  }
  CExpression cFetch = source.createFetch(mangleName(info->narrowedTo.get()));
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

  if (info->referencesOutsideLambda) {
    cFetch = source.createDereference(cFetch);
  }

  return co.finalYield(cFetch);
};
auto Talta::CTranspiler::transpileAssignmentExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto assign = std::dynamic_pointer_cast<AAST::AssignmentExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::AssignmentExpression>(_info);

  if (co.iteration() == 0) {
    return co.await(boundTranspile, assign->target, info->target);
  } else if (co.iteration() == 1) {
    auto tgt = co.result<CExpression>();
    auto tgtType = info->targetType;
    auto origTgtType = tgtType;
    bool isNullopt = tgtType->isOptional && tgtType->pointerLevel() < 1 && assign->value->nodeType() == ANT::NullptrExpression;
    bool canCopy = !isNullopt && !tgtType->isNative && tgtType->pointerLevel() < 1 && (!info->strict || tgtType->indirectionLevel() < 1);
    bool canDestroyVar = !info->operatorMethod && !info->strict && canDestroy(tgtType);

    std::vector<CExpression> exprs;

    if (canDestroyVar) {
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

    co.save(tgt, tgtType, origTgtType, exprs, canCopy, canDestroyVar, isNullopt);
    return co.await(boundTranspile, assign->value, info->value);
  } else {
    auto [
      tgt,
      tgtType,
      origTgtType,
      exprs,
      canCopy,
      canDestroyVar,
      isNullopt
    ] = co.load<
      CExpression,
      std::shared_ptr<DET::Type>,
      std::shared_ptr<DET::Type>,
      std::vector<CExpression>,
      bool,
      bool,
      bool
    >();
    auto expr = co.result<CExpression>();

    if (auto acc = std::dynamic_pointer_cast<DH::Accessor>(info->target)) {
      if (auto var = std::dynamic_pointer_cast<DET::Variable>(acc->narrowedTo)) {
        if (var->isBitfieldEntry) {
          auto tgtExpr = std::dynamic_pointer_cast<CAST::BinaryOperation>(tgt);
          tgtExpr = std::dynamic_pointer_cast<CAST::BinaryOperation>(tgtExpr->left);
          CExpression result = source.createDereference(
            source.createFunctionCall(
              source.createFetch("_Alta_bitfield_set_" + mangleName(var.get())),
              {
                source.createPointer(tgtExpr->left),
                expr
              }
            )
          );
          return co.finalYield(result);
        }
      }
    }

    bool didRetrieval = false;
    auto exprType = AltaCore::DET::Type::getUnderlyingType(info->value.get());
    auto val = tgtType->isUnion() ? expr : doParentRetrieval(expr, exprType, tgtType, &didRetrieval);
    auto valTargetType = info->operatorMethod ? info->operatorMethod->parameterVariables.front()->type : origTgtType->destroyReferences();

    if (canCopy) {
      auto id = tempVarIDs[info->inputScope->id]++;
      auto tmpName = mangleName(info->inputScope.get()) + "_temp_var_" + std::to_string(id);
      val = cast(val, exprType, valTargetType, true, additionalCopyInfo(assign->value, info->value));
      source.insertVariableDefinition(
        transpileType((info->operatorMethod ? valTargetType : origTgtType)->deconstify().get()),
        tmpName
      );
      exprs.push_back(
        source.createAssignment(
          source.createFetch(tmpName),
          val
        )
      );
      val = source.createFetch(tmpName);
    } else if (isNullopt) {
      val = source.createFunctionCall(source.createFetch("_Alta_make_empty_" + cTypeNameify(tgtType.get())), {});
    }

    if (canDestroyVar) {
      exprs.push_back(doDtor(tgt, tgtType));
    }

    if (info->strict) {
      for (size_t i = 0; i < tgtType->referenceLevel(); i++) {
        tgt = source.createPointer(tgt);
      }
      for (size_t i = 0; i < tgtType->referenceLevel(); i++) {
        val = source.createPointer(val);
      }
    }

    if (info->operatorMethod) {
      CExpression opCall = source.createFunctionCall(
        source.createFetch(mangleName(info->operatorMethod.get())),
        {
          source.createPointer(
            cast(tgt, tgtType, info->operatorMethod->parentClassType, false, additionalCopyInfo(assign->target, info->target), false)
          ),
          val,
        }
      );
      for (size_t i = 0; i < info->operatorMethod->returnType->referenceLevel(); i++) {
        opCall = source.createDereference(opCall);
      }
      exprs.push_back(opCall);
    } else {
      exprs.push_back(
        source.createAssignment(
          tgt,
          val,
          (CAST::AssignmentType)info->type
        )
      );
    }

    CExpression finalExpr = source.createMultiExpression(exprs);
    return co.finalYield(finalExpr);
  }
};
auto Talta::CTranspiler::transpileBooleanLiteralNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto boolLit = std::dynamic_pointer_cast<AAST::BooleanLiteralNode>(node);
  CExpression expr = nullptr;
  if (boolLit->value) {
    expr = source.createFetch("_Alta_bool_true");
  } else {
    expr = source.createFetch("_Alta_bool_false");
  }
  return co.finalYield(expr);
};
auto Talta::CTranspiler::transpileBinaryOperation(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto binOp = std::dynamic_pointer_cast<AAST::BinaryOperation>(node);
  auto info = std::dynamic_pointer_cast<DH::BinaryOperation>(_info);

  if (co.iteration() == 0) {
    return co.await(boundTranspile, binOp->left, info->left);
  } else if (co.iteration() == 1) {
    co.save(co.result<CExpression>());
    return co.await(boundTranspile, binOp->right, info->right);
  } else {
    auto [left] = co.load<CExpression>();
    auto right = co.result<CExpression>();

    CExpression expr = nullptr;

    if (info->operatorMethod) {
      bool isLeft = info->operatorMethod->orientation == AltaCore::Shared::ClassOperatorOrientation::Left;
      auto instance = isLeft ? left : right;
      auto instanceAlta = isLeft ? binOp->left : binOp->right;
      auto instanceInfo = isLeft ? info->left : info->right;
      auto instanceType = isLeft ? info->leftType : info->rightType;
      if (!additionalCopyInfo(instanceAlta, instanceInfo).first) {
        instance = tmpify(instance, instanceType);
      }
      auto argument = cast(isLeft ? right : left, isLeft ? info->rightType : info->leftType, info->operatorMethod->parameterVariables.front()->type, false, additionalCopyInfo(isLeft ? binOp->right : binOp->left, isLeft ? info->right : info->left), false);
      expr = source.createFunctionCall(source.createFetch(mangleName(info->operatorMethod.get())), {
        source.createPointer(
          cast(instance, instanceType, info->operatorMethod->parentClassType, false, additionalCopyInfo(instanceAlta, instanceInfo), false)
        ),
        argument,
      });
      for (size_t i = 0; i < info->operatorMethod->returnType->referenceLevel(); i++) {
        expr = source.createDereference(expr);
      }
    } else {
      // for now, we can just cast from one to the other, since they're
      // identical. however, if Alta ever introduces non-C binary operators,
      // or changes up the order of its OperatorType enum, this
      // will need to be changed. please take note of that!
      expr = source.createBinaryOperation(
        (CAST::OperatorType)binOp->type,
        cast(left, info->leftType, info->commonOperandType, false, additionalCopyInfo(binOp->left, info->left), false),
        cast(right, info->rightType, info->commonOperandType, false, additionalCopyInfo(binOp->right, info->right), false)
      );
    }

    return co.finalYield(expr);
  }
};
auto Talta::CTranspiler::transpileImportStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto import = std::dynamic_pointer_cast<AAST::ImportStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ImportStatement>(_info);
  auto mangledParentName = mangleName(info->parentModule.get());
  auto mangleImportName = mangleName(info->importedModule.get());
  if (import->isAliased) {
    //header.insertPreprocessorDefinition("_ALTA_MODULE_ALL_" + mangleImportName);
    header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangleImportName, CAST::InclusionType::Computed);
  } else {
    bool imp = false;
    for (auto& item: info->importedItems) {
      auto def = headerMangle(item.get());
      if (def.empty()) continue;
      imp = true;
      header.insertPreprocessorDefinition(def);
      header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangleImportName, CAST::InclusionType::Computed);
    }
    if (!imp) {
      header.insertPreprocessorInclusion("_ALTA_MODULE_" + mangledParentName + "_0_INCLUDE_" + mangleImportName, CAST::InclusionType::Computed);
    }
  }
  return co.finalYield();
};
auto Talta::CTranspiler::transpileFunctionCallExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto call = std::dynamic_pointer_cast<AAST::FunctionCallExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::FunctionCallExpression>(_info);

  if (co.iteration() == 0) {
    if (info->isMethodCall) {
      return co.await(bind(&CTranspiler::tmpify), info->methodClassTarget, info->methodClassTargetInfo);
    } else {
      return co.await(boundTranspile, call->target, info->target);
    }
  } else {
    std::vector<std::shared_ptr<CAST::Expression>> args;
    if (info->isMethodCall) {
      auto selfAlta = info->methodClassTarget;
      auto selfInfo = info->methodClassTargetInfo;
      auto selfType = AltaCore::DET::Type::getUnderlyingType(selfInfo.get());
      auto self = co.result<CExpression>();
      for (size_t i = 0; i < selfType->pointerLevel(); i++) {
        self = source.createDereference(self);
      }
      bool didRetrieval = false;
      auto exprType = std::make_shared<AltaCore::DET::Type>(selfType->klass);
      auto targetType = std::make_shared<AltaCore::DET::Type>(info->targetType->methodParent);
      self = doParentRetrieval(self, exprType, targetType, &didRetrieval);
      self = source.createPointer(self);
      args.push_back(self);
    } else if (!info->targetType->isRawFunction) {
      args.push_back(co.result<CExpression>());
    }
    auto restArgs = processArgs(info->adjustedArguments, info->targetType->parameters);
    args.insert(args.end(), restArgs.begin(), restArgs.end());
    std::string tmpName;
    CExpression result = nullptr;
    auto refLevel = info->targetType->returnType->referenceLevel();
    if (info->isMethodCall) {
      auto acc = std::dynamic_pointer_cast<AAST::Accessor>(call->target);
      auto accInfo = std::dynamic_pointer_cast<DH::Accessor>(info->target);
      result = source.createFunctionCall(source.createFetch(mangleName(accInfo->narrowedTo.get())), args);
    } else if (!info->targetType->isRawFunction) {
      auto rawCopy = info->targetType->copy();
      rawCopy->isRawFunction = true;
      auto funcName = "_Alta_function_" + cTypeNameify(rawCopy.get()).substr(15);
      result = source.createFunctionCall(source.createFetch("_Alta_call_" + funcName), args);
    } else {
      result = source.createFunctionCall(co.result<CExpression>(), args);
    }
    if (call->maybe) {
      auto optionalType = info->targetType->returnType->makeOptional();
      auto idxName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tempVarIDs[currentScope->id]++);
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
      auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tempVarIDs[currentScope->id]++);
      source.insertVariableDefinition(
        transpileType(optionalType.get()),
        tmpName
      );
      auto bufName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tempVarIDs[currentScope->id]++);
      source.insertVariableDefinition(
        source.createType("jmp_buf", { { CAST::TypeModifierFlag::Pointer } }),
        bufName,
        source.createFunctionCall(
          source.createFetch("_Alta_push_error_handler"),
          {
            source.createStringLiteral(""),
          }
        )
      );
      source.insertConditionalStatement(
        source.createFunctionCall(
          source.createFetch("setjmp"),
          {
            source.createDereference(
              source.createFetch(bufName)
            ),
          }
        )
      );
      source.insertBlock();
      source.insertExpressionStatement(
        source.createAssignment(
          source.createFetch(tmpName),
          source.createFunctionCall(
            source.createFetch("_Alta_make_empty_" + cTypeNameify(optionalType.get())),
            {}
          )
        )
      );
      source.exitInsertionPoint();
      source.enterConditionalUltimatum();
      source.insertBlock();
      source.insertExpressionStatement(
        source.createAssignment(
          source.createFetch(tmpName),
          source.createFunctionCall(
            source.createFetch("_Alta_make_" + cTypeNameify(optionalType.get())),
            {
              result,
            }
          )
        )
      );
      source.exitInsertionPoint();
      source.exitInsertionPoint();
      source.insertExpressionStatement(
        source.createFunctionCall(source.createFetch("_Alta_reset_error"), {
          source.createFetch(idxName),
        })
      );
      result = source.createFetch(tmpName);
    } else {
      for (size_t i = 0; i < refLevel; i++) {
        result = source.createDereference(result);
      }
    }
    return co.finalYield(result);
  }
};
auto Talta::CTranspiler::transpileStringLiteralNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto lit = std::dynamic_pointer_cast<AAST::StringLiteralNode>(node);
  CExpression expr = source.createStringLiteral(lit->value);
  return co.finalYield(expr);
};
auto Talta::CTranspiler::transpileFunctionDeclarationNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto func = std::dynamic_pointer_cast<AAST::FunctionDeclarationNode>(node);
  auto info = std::dynamic_pointer_cast<DH::FunctionDeclarationNode>(_info);
  bool targetIsHeader = info->function->isExport || info->function->isMethod;

  if (targetIsHeader) {
    auto alwaysImport = alwaysImportTable.find(func->id) != alwaysImportTable.end();
    headerPredeclaration(headerMangle(info->function.get()), alwaysImport ? "" : mangleName(currentModule.get()));
  }

  for (auto& hoistedType: info->function->publicHoistedItems) {
    hoist(hoistedType, targetIsHeader);
  }

  if (targetIsHeader) {
    header.exitInsertionPoint();
  }

  return co.finalYield();
};
auto Talta::CTranspiler::transpileConditionalStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto cond = std::dynamic_pointer_cast<AAST::ConditionalStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ConditionalStatement>(_info);
  auto boolType = std::make_shared<DET::Type>(DET::NativeType::Bool);
  if (co.iteration() == 0) {
    return co.await(boundTranspile, cond->primaryTest, info->primaryTest);
  } else if (co.iteration() == 1) {
    source.insertConditionalStatement(
      cast(co.result<CExpression>(), DET::Type::getUnderlyingType(info->primaryTest.get()), boolType, false, additionalCopyInfo(cond->primaryTest, info->primaryTest))
    );
    bool doBlock = cond->primaryResult->nodeType() != AltaNodeType::BlockNode;
    if (doBlock) {
      source.insertBlock();
      stackBookkeepingStart(info->primaryScope);
    }
    return co.await(boundTranspile, cond->primaryResult, info->primaryResult);
  } else if (co.iteration() == 2) {
    bool doBlock = cond->primaryResult->nodeType() != AltaNodeType::BlockNode;
    if (doBlock) {
      stackBookkeepingStop(info->primaryScope);
      source.exitInsertionPoint();
    }
    co.save((size_t)0, false, false);
    return co.yield();
  } else {
    auto [i, transpiledCondition, transpiledBlock] = co.load<size_t, bool, bool>();
    if (i >= cond->alternatives.size()) {
      if (i == cond->alternatives.size()) {
        co.save(i + 1, false, false);
        if (cond->finalResult) {
          source.enterConditionalUltimatum();
          bool doBlock = cond->finalResult->nodeType() != AltaNodeType::BlockNode;
          if (doBlock) {
            source.insertBlock();
            stackBookkeepingStart(info->finalScope);
          }
          return co.await(boundTranspile, cond->finalResult, info->finalResult);
        } else {
          return co.yield();
        }
      } else {
        if (cond->finalResult) {
          bool doBlock = cond->finalResult->nodeType() != AltaNodeType::BlockNode;
          if (doBlock) {
            stackBookkeepingStop(info->finalScope);
            source.exitInsertionPoint();
          }
        }

        for (size_t j = 0; j < cond->alternatives.size(); j++) {
          source.exitInsertionPoint();
          source.exitInsertionPoint();
        }

        source.exitInsertionPoint();
        return co.finalYield();
      }
    } else {
      auto [altTest, altResult] = cond->alternatives[i];
      auto [testInfo, resultInfo] = info->alternatives[i];
      if (!transpiledCondition) {
        source.enterConditionalUltimatum();
        source.insertBlock();
        co.save(i, true, false);
        return co.await(boundTranspile, altTest, testInfo);
      } else if (!transpiledBlock) {
        source.insertConditionalStatement(
          cast(co.result<CExpression>(), DET::Type::getUnderlyingType(testInfo.get()), boolType, false, additionalCopyInfo(altTest, testInfo))
        );
        bool doBlock = altResult->nodeType() != AltaNodeType::BlockNode;
        if (doBlock) {
          source.insertBlock();
          stackBookkeepingStart(info->alternativeScopes[i]);
        }
        co.save(i, true, true);
        co.save(doBlock);
        return co.await(boundTranspile, altResult, resultInfo);
      } else {
        auto [doBlock] = co.load<bool>();
        if (doBlock) {
          stackBookkeepingStop(info->alternativeScopes[i]);
          source.exitInsertionPoint();
        }
        co.save(i + 1, false, false);
        return co.yield();
      }
    }
  }
};
auto Talta::CTranspiler::transpileConditionalExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto cond = std::dynamic_pointer_cast<AAST::ConditionalExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::ConditionalExpression>(_info);
  if (co.iteration() == 0) {
    return co.await(bind(&CTranspiler::tmpify), cond->test, info->test);
  } else if (co.iteration() == 1) {
    co.save(co.result<CExpression>());
    return co.await(boundTranspile, cond->primaryResult, info->primaryResult);
  } else if (co.iteration() == 2) {
    co.save(std::get<0>(co.load<CExpression>()), co.result<CExpression>());
    return co.await(boundTranspile, cond->secondaryResult, info->secondaryResult);
  } else {
    auto [condition, primary] = co.load<CExpression, CExpression>();
    auto secondary = co.result<CExpression>();
    CExpression result = source.createTernaryOperation(
      cast(condition, DET::Type::getUnderlyingType(info->test.get()), std::make_shared<DET::Type>(DET::NativeType::Bool), false, additionalCopyInfo(cond->test, info->test)),
      primary,
      secondary
    );
    return co.finalYield(result);
  }
};
auto Talta::CTranspiler::transpileClassDefinitionNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto aClass = std::dynamic_pointer_cast<AAST::ClassDefinitionNode>(node);
  auto klassInfo = std::dynamic_pointer_cast<DH::ClassDefinitionNode>(_info);
  if (co.iteration() == 0) {
    bool isGeneric = aClass->generics.size() > 0;
    co.save(isGeneric, (size_t)0, (size_t)0);
    return co.yield();
  } else {
    auto [isGeneric, genericIndex, loopIteration] = co.load<bool, size_t, size_t>();
    if (
      (!isGeneric && genericIndex == 1) ||
      (isGeneric && genericIndex >= klassInfo->genericInstantiations.size())
    ) {
      return co.finalYield();
    }
    auto info = isGeneric ? klassInfo->genericInstantiations[genericIndex] : klassInfo;

    if (loopIteration == 0) {
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

      auto mangledClassName = mangleName(info->klass.get());
      auto alwaysImport = alwaysImportTable.find(aClass->id) != alwaysImportTable.end();
      headerPredeclaration("_ALTA_CLASS_" + mangledClassName, alwaysImport ? "" : mangledModName, !isGeneric);

      std::vector<std::pair<std::string, std::shared_ptr<CAST::Type>>> members;
      members.emplace_back("objectType", source.createType("_Alta_object_type"));
      members.emplace_back("_Alta_class_info_struct", source.createType("_Alta_class_info"));
      for (auto& parent: info->klass->parents) {
        auto mangledParent = mangleName(parent.get());
        members.emplace_back(mangledParent, source.createType(mangledParent));
      }
      for (auto item: info->klass->scope->items) {
        if (item->nodeType() == AltaCore::DET::NodeType::Variable && item->name != "this") {
          auto var = std::dynamic_pointer_cast<AltaCore::DET::Variable>(item);
          hoist(var->type, true);
          members.emplace_back(mangleName(var.get()), transpileType(var->type.get()));
        }
      }

      for (auto arg: info->klass->genericArguments) {
        hoist(arg, true);
      }
      for (auto& hoistedType: info->klass->publicHoistedItems) {
        hoist(hoistedType, true);
      }
      for (auto& hoistedType: info->klass->privateHoistedItems) {
        hoist(hoistedType, true);
      }

      header.insertStructureDefinition("_s_" + mangledClassName, members);
      header.insertTypeDefinition(mangledClassName, header.createType("_s_" + mangledClassName, {}, true));

      auto self = header.createType(mangledClassName, { { CAST::TypeModifierFlag::Pointer } });
      auto basicClassType = header.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } });
      auto rawstringType = header.createType("char", { { CAST::TypeModifierFlag::Pointer } });
      auto rawconststringType = header.createType("char", { { CAST::TypeModifierFlag::Pointer }, { CAST::TypeModifierFlag::Constant } });

      hoist(info->klass, false);

      if (!klassInfo->klass->scope->noRuntime) {
        header.insertFunctionDeclaration("_Alta_getParentClass_" + mangledClassName, {
          std::make_tuple("_Alta_self", self),
          std::make_tuple("target", rawconststringType),
        }, basicClassType);
        source.insertFunctionDefinition("_Alta_getParentClass_" + mangledClassName, {
          std::make_tuple("_Alta_self", self),
          std::make_tuple("target", rawconststringType),
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
            source.createCast(
              source.createPointer(
                source.createAccessor(
                  source.createDereference(
                    source.createFetch("_Alta_self")
                  ),
                  mangled
                )
              ),
              basicClassType
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
      }

      header.insertFunctionDeclaration("_init_" + mangledClassName, {
        std::make_tuple("_Alta_self", self),
        std::make_tuple("_isSuper", source.createType("_Alta_bool", { { CAST::TypeModifierFlag::Constant } })),
      }, self);
      source.insertFunctionDefinition("_init_" + mangledClassName, {
        std::make_tuple("_Alta_self", self),
        std::make_tuple("_isSuper", source.createType("_Alta_bool", { { CAST::TypeModifierFlag::Constant } })),
      }, self);

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createDereference(
              source.createFetch("_Alta_self")
            ),
            "objectType"
          ),
          source.createFetch("_Alta_object_type_class")
        )
      );

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

      if (info->klass->copyConstructor) {
        hoist(info->klass->copyConstructor, true);
      }

      if (info->klass->destructor) {
        hoist(info->klass->destructor, true);
      }

      header.exitInsertionPoint();

      co.save(isGeneric, genericIndex, (size_t)3);
      co.saveAny(sourceBuilderCache);
      co.save(&aClass->statements, &info->statements, false, (size_t)0, false);
      return co.yield();
    } else if (loopIteration == 1) {
      auto sbc = co.loadAny();
      source.insertReturnDirective(source.createFetch("_Alta_self"));
      source.exitInsertionPoint();

      co.save(isGeneric, genericIndex, (size_t)3);
      co.saveAny(sbc);
      co.save(&aClass->statements, &info->statements, true, (size_t)0, false);
      return co.yield();
    } else if (loopIteration == 2) {
      auto sbc = co.loadAny();
      auto [j] = co.load<size_t>();

      if (j == 0) {
        co.save(isGeneric, genericIndex, (size_t)2);
        co.saveAny(sbc);
        co.save((size_t)1);
        if (info->createDefaultConstructor) {
          return co.await(boundTranspile, info->defaultConstructor, info->defaultConstructorDetail);
        } else {
          return co.yield();
        }
      } else if (j == 1) {
        co.save(isGeneric, genericIndex, (size_t)2);
        co.saveAny(sbc);
        co.save((size_t)2);
        if (info->createDefaultDestructor) {
          return co.await(boundTranspile, info->defaultDestructor, info->defaultDestructorDetail);
        } else {
          return co.yield();
        }
      } else if (j == 2) {
        co.save(isGeneric, genericIndex, (size_t)2);
        co.saveAny(sbc);
        co.save((size_t)3);
        if (info->createDefaultCopyConstructor) {
          return co.await(boundTranspile, info->defaultCopyConstructor, info->defaultCopyConstructorDetail);
        } else {
          return co.yield();
        }
      } else {
        if (isGeneric) {
          source = ALTACORE_ANY_CAST<Ceetah::Builder>(sbc);
        }

        co.save(isGeneric, genericIndex + 1, (size_t)0);
        return co.yield();
      }
    } else {
      auto sbc = co.loadAny();
      auto [
        tgt,
        infos,
        isAll,
        index,
        waiting
      ] = co.load<
        std::vector<std::shared_ptr<AAST::ClassStatementNode>>*,
        std::vector<std::shared_ptr<DH::ClassStatementNode>>*,
        bool,
        size_t,
        bool
      >();

      if (index >= tgt->size()) {
        co.save(isGeneric, genericIndex, (size_t)(isAll ? 2 : 1));
        co.saveAny(sbc);
        if (isAll) {
          co.save((size_t)0);
        }
        return co.yield();
      }

      auto stmt = (*tgt)[index];
      auto stmtInfo = (*infos)[index];

      if (isAll) {
        co.save(isGeneric, genericIndex, loopIteration);
        co.saveAny(sbc);
        co.save(tgt, infos, isAll, index + 1, waiting);
        return co.await(boundTranspile, stmt, stmtInfo);
      } else if (stmt->nodeType() == AAST::NodeType::ClassMemberDefinitionStatement) {
        auto member = std::dynamic_pointer_cast<AAST::ClassMemberDefinitionStatement>(stmt);
        auto memberInfo = std::dynamic_pointer_cast<DH::ClassMemberDefinitionStatement>(stmtInfo);
        auto mangledMemberName = mangleName(memberInfo->varDef->variable.get());
        if (waiting) {
          if (member->varDef->initializationExpression) {
            source.insertExpressionStatement(source.createAssignment(source.createAccessor(source.createDereference(source.createFetch("_Alta_self")), mangledMemberName), co.result<CExpression>()));
          }
          co.save(isGeneric, genericIndex, loopIteration);
          co.saveAny(sbc);
          co.save(tgt, infos, isAll, index + 1, false);
          return co.yield();
        } else {
          co.save(isGeneric, genericIndex, loopIteration);
          co.saveAny(sbc);
          co.save(tgt, infos, isAll, index, true);
          if (member->varDef->initializationExpression) {
            return co.await(boundTranspile, member->varDef->initializationExpression, memberInfo->varDef->initializationExpression);
          } else {
            return co.yield();
          }
        }
      } else {
        co.save(isGeneric, genericIndex, loopIteration);
        co.saveAny(sbc);
        co.save(tgt, infos, isAll, index + 1, false);
        return co.yield();
      }
    }
  }
};
auto Talta::CTranspiler::transpileClassMethodDefinitionStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto method = std::dynamic_pointer_cast<AAST::ClassMethodDefinitionStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ClassMethodDefinitionStatement>(_info);
  if (co.iteration() == 0) {
    //header.insertPreprocessorDefinition(headerMangle(info->funcDef->function.get()));
    return co.await(boundTranspile, method->funcDef, info->funcDef);
  } else {
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileClassSpecialMethodDefinitionStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto special = std::dynamic_pointer_cast<AAST::ClassSpecialMethodDefinitionStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ClassSpecialMethodDefinitionStatement>(_info);
  if (co.iteration() == 0) {
    auto mangledClassName = mangleName(info->klass.get());
    auto constr = info->method;
    auto mangledName = mangleName(constr.get());

    auto ret = header.createType(mangledClassName);
    auto retPtr = header.createType(mangledClassName, { { CAST::TypeModifierFlag::Pointer } });
    auto voidType = header.createType("void");

    bool isCtor = special->type == AAST::SpecialClassMethod::Constructor;
    bool isDtor = special->type == AAST::SpecialClassMethod::Destructor;
    bool isFrom = special->type == AAST::SpecialClassMethod::From;
    bool isTo = special->type == AAST::SpecialClassMethod::To;

    std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> params;
    decltype(params) fullParams;

    std::string prefix;
    decltype(ret) retType = nullptr;

    auto headerName = headerMangle(constr.get());
    auto mod = AltaCore::Util::getModule(constr->parentScope.lock().get()).lock();
    auto mangledModName = mangleName(mod.get());

    headerPredeclaration(headerName, mangledModName);

    for (auto& parent: info->klass->parents) {
      if (parent->defaultConstructor) {
        hoist(parent->defaultConstructor, false);
      }
    }

    for (auto& hoistedType: info->method->publicHoistedItems) {
      hoist(hoistedType, true);
    }
    for (auto& hoistedType: info->method->privateHoistedItems) {
      hoist(hoistedType, false);
    }

    hoist(constr, false);

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
    } else if (isDtor) {
      prefix = "_d_";
      retType = voidType;

      fullParams.push_back({ "__Alta_self", header.createType("_Alta_basic_class", { { CAST::TypeModifierFlag::Pointer } }) });
      fullParams.push_back({ "_Alta_isPersistent", header.createType("_Alta_bool", { { CAST::TypeModifierFlag::Constant } }) });
    } else if (isFrom) {
      prefix = "";
      retType = ret;

      fullParams.push_back({ mangleName(constr->parameterVariables[0].get()), transpileType(info->specialType->type.get()) });
    } else if (isTo) {
      prefix = "";
      retType = transpileType(info->specialType->type.get());

      fullParams.push_back({ "_Alta_self", retPtr });
    } else {
      throw std::runtime_error("impossible error: unrecognized special method type");
    }

    header.insertFunctionDeclaration(prefix + mangledName, fullParams, retType);

    source.insertFunctionDefinition(prefix + mangledName, fullParams, retType);

    stackBookkeepingStart(constr->scope);

    if (isDtor) {
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
    } else if (isCtor) {
      for (size_t i = 0; i < constr->parameterVariables.size(); i++) {
        auto& var = constr->parameterVariables[i];
        if (
          !currentScope->noRuntime &&
          canPush(var->isVariable ? var->type->follow() : var->type)
        ) {
          if (var->isVariable) {
            source.insertBlock();
            source.insertVariableDefinition(source.createType("size_t"), "_Alta_variable_array_index", source.createIntegerLiteral(0));
            source.insertWhileLoop(
              source.createBinaryOperation(
                CAST::OperatorType::LessThan,
                source.createFetch("_Alta_variable_array_index"),
                source.createFetch("_Alta_array_length_" + mangleName(var.get()).substr(12))
              )
            );
            source.insertBlock();
          }
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
                var->isVariable
                  ? source.createBinaryOperation(
                      CAST::OperatorType::Addition,
                      source.createFetch("_Alta_array_" + mangleName(var.get()).substr(12)),
                      source.createFetch("_Alta_variable_array_index")
                    )
                  : source.createPointer(
                      source.createFetch(mangleName(var.get()))
                    ),
                source.createType(
                  "_Alta_object",
                  { { CAST::TypeModifierFlag::Pointer } }
                )
              ),
            }
          ));
          if (var->isVariable) {
            source.insertExpressionStatement(
              source.createUnaryOperation(
                CAST::UOperatorType::PreIncrement,
                source.createFetch("_Alta_variable_array_index")
              )
            );
            source.exitInsertionPoint();
            source.exitInsertionPoint();
            source.exitInsertionPoint();
          }
        }
      }
    } else if (isFrom) {
      source.insertVariableDefinition(ret, "__Alta_self", source.createArrayLiteral({ source.createIntegerLiteral(0) }));
      source.insertVariableDefinition(retPtr, "_Alta_self", source.createPointer(source.createFetch("__Alta_self")));
      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_init_" + mangledClassName),
          {
            source.createFetch("_Alta_self"),
            source.createFetch("_Alta_bool_false")
          }
        )
      );

      auto var = constr->parameterVariables[0];
      if (currentScope->noRuntime && canPush(var->type)) {
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
              source.createType(
                "_Alta_object",
                { { CAST::TypeModifierFlag::Pointer } }
              )
            ),
          }
        ));
      }
    } else if (isTo) {
      // do nothing
    } else {
      throw std::runtime_error("impossible error: unrecognized special method type");
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

    co.save(constr, isCtor, mangledClassName, mangledName, ret, retPtr, params, isDtor, isFrom, isTo);
    return co.yield();
  } else if (co.iteration() - 1 < special->body->statements.size()) {
    auto i = co.iteration() - 1;
    co.saveAny(co.loadAny());
    return co.await(boundTranspile, special->body->statements[i], info->body->statements[i]);
  } else {
    auto [
      constr,
      isCtor,
      mangledClassName,
      mangledName,
      ret,
      retPtr,
      params,
      isDtor,
      isFrom,
      isTo
    ] = co.load<
      std::shared_ptr<DET::Function>,
      bool,
      std::string,
      std::string,
      std::shared_ptr<CAST::Type>,
      std::shared_ptr<CAST::Type>,
      std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>>,
      bool,
      bool,
      bool
    >();

    stackBookkeepingStop(constr->scope);

    if (isCtor) {
      source.insertReturnDirective(source.createFetch("_Alta_self"));
    } else if (isDtor) {
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
    } else if (isFrom) {
      source.insertReturnDirective(source.createFetch("__Alta_self"));
    } else if (isTo) {
      // do nothing
    } else {
      throw std::runtime_error("impossible error: unrecognized special method type");
    }

    source.exitInsertionPoint();

    if (isCtor) {
      std::vector<std::shared_ptr<CAST::Expression>> args;
      for (auto param: constr->parameterVariables) {
        args.push_back(source.createFetch(mangleName(param.get())));
      }

      if (!currentScope->noRuntime) {
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
                source.createType("_Alta_object", { { CAST::TypeModifierFlag::Pointer } })
              ),
            }
          )
        );
        source.insertReturnDirective(source.createFetch("_Alta_self"));
        source.exitInsertionPoint();
      }

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

      if (info->isCastConstructor) {
        hoist(info->correspondingMethod, true);
        header.exitInsertionPoint();

        auto mod = AltaCore::Util::getModule(info->correspondingMethod->parentScope.lock().get()).lock();
        headerPredeclaration(headerMangle(info->correspondingMethod.get()), mangleName(mod.get()));

        auto mangledCast = mangleName(info->correspondingMethod.get());
        auto specialParam = info->correspondingMethod->parameterVariables[0];
        header.insertFunctionDeclaration(mangledCast, {
          { mangleName(specialParam.get()), transpileType(specialParam->type.get()) },
        }, ret);
        source.insertFunctionDefinition(mangledCast, {
          { mangleName(specialParam.get()), transpileType(specialParam->type.get()) },
        }, ret);
        source.insertReturnDirective(source.createFunctionCall(source.createFetch("_cn_" + mangledName), {
          source.createFetch(mangleName(specialParam.get())),
        }));
        source.exitInsertionPoint();
      }
    }

    header.exitInsertionPoint();

    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileClassInstantiationExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto call = std::dynamic_pointer_cast<AAST::ClassInstantiationExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::ClassInstantiationExpression>(_info);

  auto args = processArgs(info->adjustedArguments, info->constructor->parameters);
  auto mangledClass = mangleName(info->klass.get());
  auto mangledCtor = mangleName(info->constructor.get());
  CExpression result = nullptr;
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
    result = source.createPointer(
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
    result = source.createArrayLiteral(args, source.createType(mangleName(info->klass.get()), {}, !info->klass->isTyped));
  } else {
    std::shared_ptr<CAST::Expression> call = source.createFunctionCall(source.createFetch((info->persistent ? "_cp_" : "_cn_") + mangledCtor), args);
    if (info->persistent) {
      call = source.createDereference(call);
    }
    result = call;
  }
  return co.finalYield(result);
};
auto Talta::CTranspiler::transpilePointerExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto ptr = std::dynamic_pointer_cast<AAST::PointerExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::PointerExpression>(_info);
  if (co.iteration() == 0) {
    return co.await(boundTranspile, ptr->target, info->target);
  } else {
    CExpression result = source.createPointer(co.result<CExpression>());
    return co.finalYield(result);
  }
};
auto Talta::CTranspiler::transpileDereferenceExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto deref = std::dynamic_pointer_cast<AAST::DereferenceExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::DereferenceExpression>(_info);
  if (co.iteration() == 0) {
    return co.await(boundTranspile, deref->target, info->target);
  } else {
    auto expr = co.result<CExpression>();
    auto tgtType = DET::Type::getUnderlyingType(info->target.get());
    CExpression result = nullptr;
    if (tgtType->isOptional) {
      result = source.createAccessor(expr, "target");
    } else {
      result = source.createDereference(expr);
    }
    return co.finalYield(result);
  }
};
auto Talta::CTranspiler::transpileWhileLoopStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto loop = std::dynamic_pointer_cast<AAST::WhileLoopStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::WhileLoopStatement>(_info);
  if (co.iteration() == 0) {
    source.insertWhileLoop(source.createFetch("_Alta_bool_true"));
    source.insertBlock();
    return co.await(boundTranspile, loop->test, info->test);
  } else if (co.iteration() == 1) {
    source.insertConditionalStatement(
      source.createUnaryOperation(
        CAST::UOperatorType::Not,
        cast(co.result<CExpression>(), DET::Type::getUnderlyingType(info->test.get()), std::make_shared<DET::Type>(DET::NativeType::Bool), false, additionalCopyInfo(loop->test, info->test))
      )
    );
    source.insertExpressionStatement(source.createIntegerLiteral("break"));
    source.exitInsertionPoint();
    return co.await(boundTranspile, loop->body, info->body);
  } else {
    source.exitInsertionPoint();
    source.exitInsertionPoint();
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileCastExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto castExpr = std::dynamic_pointer_cast<AAST::CastExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::CastExpression>(_info);
  if (co.iteration() == 0) {
    return co.await(boundTranspile, castExpr->target, info->target);
  } else {
    auto tgt = co.result<CExpression>();
    auto exprType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    return co.finalYield(cast(tgt, exprType, info->type->type, false, additionalCopyInfo(castExpr->target, info->target), true));
  }
};
auto Talta::CTranspiler::transpileClassReadAccessorDefinitionStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  return co.finalYield();
};
auto Talta::CTranspiler::transpileCharacterLiteralNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto lit = std::dynamic_pointer_cast<AAST::CharacterLiteralNode>(node);
  CExpression result = source.createCharacterLiteral(lit->value, lit->escaped);
  return co.finalYield(result);
};
auto Talta::CTranspiler::transpileTypeAliasStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  return co.finalYield();
};
auto Talta::CTranspiler::transpileSubscriptExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto subs = std::dynamic_pointer_cast<AAST::SubscriptExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::SubscriptExpression>(_info);
  if (co.iteration() == 0) {
    return co.await(boundTranspile, subs->target, info->target);
  } else if (co.iteration() == 1) {
    co.save(co.result<CExpression>());
    return co.await(boundTranspile, subs->index, info->index);
  } else {
    auto [cTarget] = co.load<CExpression>();
    auto cIndex = co.result<CExpression>();
    /*
     * now, why transpile to an add-and-dereference, you ask?
     * i could tell you a bunch of crap about wanting to guarantee
     * compiler behavior in case the C compiler decides that
     * a subscript is not equal to an add-and-dereference, but the
     * real reason is that i just didn't feel like adding subscript
     * expression to Ceetah right now. so...
     */
    CExpression result = nullptr;
    if (info->operatorMethod) {
      result = source.createFunctionCall(
        source.createFetch(mangleName(info->operatorMethod.get())),
        {
          source.createPointer(
            cast(cTarget, info->targetType, info->operatorMethod->parentClassType, false, additionalCopyInfo(subs->target, info->target), false)
          ),
          cast(cIndex, info->indexType, info->operatorMethod->parameterVariables.front()->type, true, additionalCopyInfo(subs->index, info->index), false),
        }
      );
      for (size_t i = 0; i < info->operatorMethod->returnType->referenceLevel(); i++) {
        result = source.createDereference(result);
      }
    } else {
      result = source.createDereference(source.createBinaryOperation(CAST::OperatorType::Addition, cTarget, cIndex));
    }
    return co.finalYield(result);
  }
};
auto Talta::CTranspiler::transpileSuperClassFetch(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto sc = std::dynamic_pointer_cast<AAST::SuperClassFetch>(node);
  auto info = std::dynamic_pointer_cast<DH::SuperClassFetch>(_info);
  // since we return a reference,
  // we would have to dereference a pointer if we were to insert
  // one, so that's completely useless. just insert a regular
  // accessor. later code can create a pointer to it if necessary
  CExpression result = source.createAccessor(
    source.createDereference(
      source.createFetch("_Alta_self")
    ),
    mangleName(info->superclass.get())
  );
  return co.finalYield(result);
};
auto Talta::CTranspiler::transpileInstanceofExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto instOf = std::dynamic_pointer_cast<AAST::InstanceofExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::InstanceofExpression>(_info);
  if (co.iteration() == 0) {
    auto targetType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    auto& testType = info->type->type;

    CExpression result = nullptr;
    if (testType->isUnion()) {
      return co.await(boundTranspile, instOf->target, info->target);
    } else if (targetType->isUnion()) {
      return co.await(boundTranspile, instOf->target, info->target);
    } else if (targetType->isNative != testType->isNative) {
      result = source.createFetch("_Alta_bool_false");
    } else if (targetType->isNative) {
      if (testType->isExactlyCompatibleWith(*targetType)) {
        result = source.createFetch("_Alta_bool_false");
      } else {
        result = source.createFetch("_Alta_bool_true");
      }
    } else if (targetType->klass->id == testType->klass->id) {
      result = source.createFetch("_Alta_bool_true");
    } else if (targetType->klass->hasParent(testType->klass)) {
      result = source.createFetch("_Alta_bool_true");
    } else if (testType->klass->hasParent(targetType->klass)) {
      return co.await(boundTranspile, instOf->target, info->target);
    } else {
      result = source.createFetch("_Alta_bool_false");
    }
    return co.finalYield(result);
  } else {
    auto targetType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    auto& testType = info->type->type;
    auto tgt = co.result<CExpression>();
    if (testType->isUnion()) {
      if (targetType->isUnion()) {
        CExpression result = nullptr;
        for (auto& item: testType->unionOf) {
          auto test = source.createBinaryOperation(
            Ceetah::AST::OperatorType::EqualTo,
            source.createFunctionCall(
              source.createFetch("strcmp"),
              {
                source.createAccessor(
                  tgt,
                  "typeName"
                ),
                source.createStringLiteral(mangleType(item.get())),
              }
            ),
            source.createIntegerLiteral(0)
          );
          result = result
            ? source.createBinaryOperation(
                Ceetah::AST::OperatorType::Or,
                result,
                test
              )
            : test
            ;
        }
        return co.finalYield(result);
      } else {
        CExpression result = nullptr;
        for (auto& item: testType->unionOf) {
          if (item->isCompatibleWith(*targetType)) {
            result = source.createFetch("_Alta_bool_true");
            return co.finalYield(result);
          }
        }
        result = source.createFetch("_Alta_bool_false");
        return co.finalYield(result);
      }
    } else if (targetType->isUnion()) {
      CExpression result = source.createBinaryOperation(
        Ceetah::AST::OperatorType::EqualTo,
        source.createFunctionCall(
          source.createFetch("strcmp"),
          {
            source.createAccessor(
              tgt,
              "typeName"
            ),
            source.createStringLiteral(mangleType(testType.get())),
          }
        ),
        source.createIntegerLiteral(0)
      );
      return co.finalYield(result);
    } else if (testType->klass->hasParent(targetType->klass)) {
      auto tgtType = AltaCore::DET::Type::getUnderlyingType(info->target.get());
      bool didRetrieval = false;
      auto result = doChildRetrieval(tgt, tgtType, info->type->type, &didRetrieval);
      result = source.createBinaryOperation(
        CAST::OperatorType::NotEqualTo,
        source.createPointer(result),
        source.createFetch("NULL")
      );
      return co.finalYield(result);
    } else {
      throw std::runtime_error("somehow got recalled after finalYield in transpileInstanceofExpression (this error should never happen)");
    }
  }
};
auto Talta::CTranspiler::transpileGeneric(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  return co.finalYield();
};
auto Talta::CTranspiler::transpileForLoopStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto loop = std::dynamic_pointer_cast<AAST::ForLoopStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ForLoopStatement>(_info);
  if (co.iteration() == 0) {
    source.insertBlock();
    return co.await(boundTranspile, loop->increment, info->increment);
  } else if (co.iteration() == 1) {
    source.insertPreprocessorDefinition("_ALTA_" + mangleName(info->scope.get()) + "_NEXT_ITERATION", co.result<CExpression>()->toString());
    return co.await(boundTranspile, loop->initializer, info->initializer);
  } else if (co.iteration() == 2) {
    source.insertExpressionStatement(co.result<CExpression>());
    source.insertWhileLoop(source.createFetch("_Alta_bool_true"));
    source.insertBlock();
    return co.await(boundTranspile, loop->condition, info->condition);
  } else if (co.iteration() == 3) {
    source.insertConditionalStatement(
      source.createUnaryOperation(
        CAST::UOperatorType::Not,
        cast(co.result<CExpression>(), DET::Type::getUnderlyingType(info->condition.get()), std::make_shared<DET::Type>(DET::NativeType::Bool), false, additionalCopyInfo(loop->condition, info->condition))
      )
    );
    source.insertExpressionStatement(source.createIntegerLiteral("break"));
    source.exitInsertionPoint();
    return co.await(boundTranspile, loop->body, info->body);
  } else {
    source.insertExpressionStatement(source.createFetch("_ALTA_" + mangleName(info->scope.get()) + "_NEXT_ITERATION"));
    source.exitInsertionPoint();
    source.exitInsertionPoint();
    source.exitInsertionPoint();
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileRangedForLoopStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto loop = std::dynamic_pointer_cast<AAST::RangedForLoopStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::RangedForLoopStatement>(_info);
  if (co.iteration() == 0) {
    source.insertBlock();
    return co.await(boundTranspile, loop->start, info->start);
  } else if (co.iteration() == 1) {
    auto mangledCounter = mangleName(info->counter.get());

    source.insertVariableDefinition(
      transpileType(info->counterType->type.get()),
      mangledCounter,
      co.result<CExpression>()
    );

    auto inc = source.createAssignment(
      source.createFetch(mangledCounter),
      source.createBinaryOperation(
        (loop->decrement) ? CAST::OperatorType::Subtraction : CAST::OperatorType::Addition,
        source.createFetch(mangledCounter),
        source.createIntegerLiteral(1)
      )
    );

    source.insertPreprocessorDefinition("_ALTA_" + mangleName(info->scope.get()) + "_NEXT_ITERATION", inc->toString());

    co.save(mangledCounter);
    return co.await(boundTranspile, loop->end, info->end);
  } else if (co.iteration() == 2) {
    auto [mangledCounter] = co.load<std::string>();

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
        co.result<CExpression>()
      )
    );
    source.insertBlock();
    return co.await(boundTranspile, loop->body, info->body);
  } else {
    source.insertExpressionStatement(source.createFetch("_ALTA_" + mangleName(info->scope.get()) + "_NEXT_ITERATION"));
    source.exitInsertionPoint();
    source.exitInsertionPoint();
    source.exitInsertionPoint();
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileUnaryOperation(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto op = std::dynamic_pointer_cast<AAST::UnaryOperation>(node);
  auto info = std::dynamic_pointer_cast<DH::UnaryOperation>(_info);

  if (co.iteration() == 0) {
    return co.await(boundTranspile, op->target, info->target);
  } else {
    auto expr = co.result<CExpression>();
    CExpression result = nullptr;

    if (info->operatorMethod) {
      if (!additionalCopyInfo(op->target, info->target).first) {
        expr = tmpify(expr, info->targetType);
      }
      result = source.createFunctionCall(source.createFetch(mangleName(info->operatorMethod.get())), {
        source.createPointer(
          cast(expr, info->targetType, info->operatorMethod->parentClassType, false, additionalCopyInfo(op->target, info->target), false)
        ),
      });
      for (size_t i = 0; i < info->operatorMethod->returnType->referenceLevel(); i++) {
        result = source.createDereference(result);
      }
    } else {
      if (op->type == AAST::UOperatorType::Not) {
        expr = cast(expr, DET::Type::getUnderlyingType(info->target.get()), std::make_shared<DET::Type>(DET::NativeType::Bool), false, additionalCopyInfo(op->target, info->target));
      }

      result = source.createUnaryOperation(
        (CAST::UOperatorType)op->type,
        expr,
        op->type == AAST::UOperatorType::PostIncrement || op->type == AAST::UOperatorType::PostDecrement
      );
    }

    return co.finalYield(result);
  }
};
auto Talta::CTranspiler::transpileSizeofOperation(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto op = std::dynamic_pointer_cast<AAST::SizeofOperation>(node);
  auto info = std::dynamic_pointer_cast<DH::SizeofOperation>(_info);

  CExpression result = source.createSizeof(transpileType(info->target->type.get()));
  return co.finalYield(result);
};
auto Talta::CTranspiler::transpileFloatingPointLiteralNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto deci = std::dynamic_pointer_cast<AAST::FloatingPointLiteralNode>(node);
  auto info = std::dynamic_pointer_cast<DH::FloatingPointLiteralNode>(_info);

  CExpression result = source.createIntegerLiteral(deci->raw);
  return co.finalYield(result);
};
auto Talta::CTranspiler::transpileStructureDefinitionStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto structure = std::dynamic_pointer_cast<AAST::StructureDefinitionStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::StructureDefinitionStatement>(_info);
  if (info->isExternal) return co.finalYield();

  auto mod = info->structure->parentScope.lock()->parentModule.lock();
  auto mangledModName = mangleName(mod.get());
  auto mangledClassName = mangleName(info->structure.get());
  auto alwaysImport = alwaysImportTable.find(structure->id) != alwaysImportTable.end();
  headerPredeclaration("_ALTA_CLASS_" + mangledClassName, alwaysImport ? "" : mangledModName, true);

  for (auto& hoistedType: info->structure->publicHoistedItems) {
    hoist(hoistedType, info->isExport);
  }
  for (auto& hoistedType: info->structure->privateHoistedItems) {
    hoist(hoistedType, false);
  }

  auto& target = info->isExport ? header : source;

  std::vector<std::pair<std::string, std::shared_ptr<CAST::Type>>> members;

  for (auto& item: info->structure->scope->items) {
    if (auto var = std::dynamic_pointer_cast<AltaCore::DET::Variable>(item)) {
      members.push_back(std::make_pair(mangleName(var.get()), transpileType(var->type.get())));
    }
  }

  auto name = info->isLiteral ? structure->name : mangledClassName;
  target.insertStructureDefinition((info->isTyped ? "_struct_" : "") + name, members, packedTable.find(structure->id) != packedTable.end());
  
  if (info->isTyped) {
    target.insertTypeDefinition(name, target.createType("_struct_" + name, {}, true));
  }
  return co.finalYield();
};
auto Talta::CTranspiler::transpileExportStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto state = std::dynamic_pointer_cast<AAST::ExportStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ExportStatement>(_info);

  if (co.iteration() == 0) {
    if (state->externalTarget) {
      return co.await(boundTranspile, state->externalTarget, info->externalTarget);
    } else {
      return co.finalYield();
    }
  } else {
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileDeleteStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto del = std::dynamic_pointer_cast<AAST::DeleteStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::DeleteStatement>(_info);

  if (co.iteration() == 0) {
    auto type = AltaCore::DET::Type::getUnderlyingType(info->target.get());
    bool canDestroy = !type->isNative && (info->persistent || type->pointerLevel() < 1);

    if (!canDestroy) {
      return co.finalYield();
    }

    co.save(type, canDestroy);
    return co.await(boundTranspile, del->target, info->target);
  } else {
    auto [type, canDestroy] = co.load<std::shared_ptr<DET::Type>, bool>();
    auto tgt = co.result<CExpression>();

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
    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileControlDirective(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto ctrl = std::dynamic_pointer_cast<AAST::ControlDirective>(node);
  auto loopScope = AltaCore::Util::findLoopScope(_info->inputScope);


  if (!loopScope) {
    throw std::runtime_error("no loop found for loop control directive");
  }

  if (!ctrl->isBreak) {
    source.insertBlock();
    source.insertExpressionStatement(source.createFetch("_ALTA_" + mangleName(loopScope.get()) + "_NEXT_ITERATION"));
  }

  source.insertExpressionStatement(source.createIntegerLiteral(ctrl->isBreak ? "break" : "continue"));

  if (!ctrl->isBreak) {
    source.exitInsertionPoint();
  }

  return co.finalYield();
};
auto Talta::CTranspiler::transpileTryCatchBlock(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto blk = std::dynamic_pointer_cast<AAST::TryCatchBlock>(node);
  auto info = std::dynamic_pointer_cast<DH::TryCatchBlock>(_info);

  if (co.iteration() == 0) {
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

    auto startID = tempVarIDs[currentScope->id];
    //auto baseName = mangleName(currentScope.get()) + "_temp_var_";
    tempVarIDs[currentScope->id] += blk->catchBlocks.size();

    for (size_t j = blk->catchBlocks.size(); j > 0; j--) {
      auto i = j - 1;
      auto& typeDet = info->catchBlocks[i].first;

      auto tmpID = startID + i;
      auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tmpID);
      source.insertVariableDefinition(
        source.createType("jmp_buf", { { CAST::TypeModifierFlag::Pointer } }),
        tmpName,
        source.createFunctionCall(
          source.createFetch("_Alta_push_error_handler"),
          {
            source.createStringLiteral(mangleType(typeDet->type.get())),
          }
        )
      );
    }

    if (blk->catchAllBlock) {
      auto tmpID = tempVarIDs[currentScope->id]++;
      auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(tmpID);
      source.insertVariableDefinition(
        source.createType("jmp_buf", { { CAST::TypeModifierFlag::Pointer } }),
        tmpName,
        source.createFunctionCall(
          source.createFetch("_Alta_push_error_handler"),
          {
            source.createStringLiteral(""),
          }
        )
      );
    }

    co.save(true, (size_t)0, false, startID);
    return co.yield();
  } else {
    auto [inLoop, i, transpiledBlock, startID] = co.load<bool, size_t, bool, size_t>();

    if (inLoop) {
      if (i >= blk->catchBlocks.size()) {
        co.save(false, blk->catchBlocks.size(), false, startID);
        return co.yield();
      }
      if (!transpiledBlock) {
        auto [type, stmt] = blk->catchBlocks[i];
        auto& id = blk->catchIDs[i];
        auto [typeDet, stmtDet] = info->catchBlocks[i];
        auto& var = info->errorVariables[i];
        auto& scope = info->catchScopes[i];
        auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(startID + i);

        auto comparison = source.createFunctionCall(
          source.createFetch("setjmp"),
          {
            source.createDereference(
              source.createFetch(tmpName)
            ),
          }
        );

        if (i == 0) {
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

        co.save(true, i, true, startID);
        return co.await(boundTranspile, stmt, stmtDet);
      } else {
        auto idxName = mangleName(info->tryScope.get()) + "_index";
        source.insertExpressionStatement(
          source.createFunctionCall(source.createFetch("_Alta_reset_error"), {
            source.createFetch(idxName),
          })
        );

        source.exitInsertionPoint();
        co.save(true, i + 1, false, startID);
        return co.yield();
      }
    } else {
      if (i == blk->catchBlocks.size()) {
        if (!transpiledBlock) {
          co.save(false, i, true, startID);
          if (blk->catchAllBlock) {
            auto tmpName = mangleName(currentScope.get()) + "_temp_var_" + std::to_string(startID + i);
            auto comparison = source.createFunctionCall(
              source.createFetch("setjmp"),
              {
                source.createDereference(
                  source.createFetch(tmpName)
                ),
              }
            );
            if (i == 0) {
              source.insertConditionalStatement(comparison);
            } else {
              source.enterConditionalAlternative(i - 1);
              source.insert(comparison);
            }
            source.insertBlock();
            return co.await(boundTranspile, blk->catchAllBlock, info->catchAllBlock);
          } else {
            return co.yield();
          }
        } else {
          co.save(false, i + 1, false, startID);
          if (blk->catchAllBlock) {
            source.exitInsertionPoint();
          }
          return co.yield();
        }
      } else {
        if (!transpiledBlock) {
          source.enterConditionalUltimatum();
          source.insertBlock();
          co.save(false, i, true, startID);
          return co.await(boundTranspile, blk->tryBlock, info->tryBlock);
        } else {
          source.exitInsertionPoint();
          source.exitInsertionPoint();
          return co.finalYield();
        }
      }
    }
  }
};
auto Talta::CTranspiler::transpileThrowStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto stmt = std::dynamic_pointer_cast<AAST::ThrowStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ThrowStatement>(_info);
  if (co.iteration() == 0) {
    return co.await(boundTranspile, stmt->expression, info->expression);
  } else {
    auto expr = co.result<CExpression>();
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

    source.insertConditionalStatement(
      source.createBinaryOperation(
        CAST::OperatorType::EqualTo,
        source.createFetch(tmpName),
        source.createFetch("NULL")
      )
    );
    source.insertBlock();
    source.insertExpressionStatement(
      source.createFunctionCall(
        source.createFetch("_Alta_uncaught_error"),
        {
          source.createStringLiteral(mangleType(type.get())),
        }
      )
    );
    source.exitInsertionPoint();
    source.exitInsertionPoint();

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
        CAST::OperatorType::Or,
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
        ),
        source.createBinaryOperation(
          CAST::OperatorType::EqualTo,
          source.createFunctionCall(
            source.createFetch("strlen"),
            {
              source.createAccessor(
                source.createDereference(
                  source.createFetch(tmpName)
                ),
                "typeName"
              ),
            }
          ),
          source.createIntegerLiteral(0)
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

    source.insertExpressionStatement(
      source.createFunctionCall(
        source.createFetch("_Alta_restore_state"),
        {
          source.createAccessor(
            source.createDereference(
              source.createFetch(tmpName)
            ),
            "state"
          )
        }
      )
    );

    source.insertExpressionStatement(
      source.createFunctionCall(
        source.createFetch("longjmp"),
        {
          source.createAccessor(
            source.createDereference(
              source.createFetch(tmpName)
            ),
            "jumpPoint"
          ),
          source.createIntegerLiteral(1),
        }
      )
    );

    source.exitInsertionPoint();

    if (!type->isNative && !type->isUnion()) {
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

      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_Alta_restore_state"),
          {
            source.createAccessor(
              source.createDereference(
                source.createFetch(tmpName)
              ),
              "state"
            )
          }
        )
      );

      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("longjmp"),
          {
            source.createAccessor(
              source.createDereference(
                source.createFetch(tmpName)
              ),
              "jumpPoint"
            ),
            source.createIntegerLiteral(1),
          }
        )
      );

      source.exitInsertionPoint();
    }

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

    // if it gets here, the error wasn't caught
    source.insertExpressionStatement(
      source.createFunctionCall(
        source.createFetch("_Alta_uncaught_error"),
        {
          source.createStringLiteral(mangleType(type.get())),
        }
      )
    );

    return co.finalYield();
  }
};
auto Talta::CTranspiler::transpileNullptrExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  CExpression result = source.createFetch("NULL");
  return co.finalYield(result);
};
auto Talta::CTranspiler::transpileCodeLiteralNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto code = std::dynamic_pointer_cast<AAST::CodeLiteralNode>(node);
  auto& target = (inHeaderTable.find(code->id) != inHeaderTable.end()) ? header : source;
  target.insertExpressionStatement(source.createFetch(code->raw));
  return co.finalYield();
};
auto Talta::CTranspiler::transpileAttributeStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto attrStmt = std::dynamic_pointer_cast<AAST::AttributeStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::AttributeStatement>(_info);
  if (info->attribute->attribute) {
    auto id = info->attribute->attribute->id;
    if (id == "initializeModule") {
      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_Alta_module_init_" + mangleName(currentModule.get())),
          {}
        )
      );
    } else if (id == "initializeRuntime") {
      source.insertExpressionStatement(
        source.createFunctionCall(
          source.createFetch("_Alta_init_global_runtime"),
          {}
        )
      );
    }
  }
  return co.finalYield();
};
auto Talta::CTranspiler::transpileBitfieldDefinitionNode(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto bits = std::dynamic_pointer_cast<AAST::BitfieldDefinitionNode>(node);
  auto info = std::dynamic_pointer_cast<DH::BitfieldDefinitionNode>(_info);
  auto mangledName = mangleName(info->bitfield.get());
  auto& target = (info->isExport) ? header : source;
  target.insertPreprocessorConditional("!defined(_ALTA_BITFIELD_" + mangledName + ")");
  target.insertPreprocessorDefinition("_ALTA_BITFIELD_" + mangledName);
  auto bitfieldType = info->bitfield->underlyingBitfieldType.lock()->destroyReferences();
  auto underylingTypePtr = transpileType(bitfieldType->reference().get());
  for (size_t i = 0; i < bits->members.size(); i++) {
    auto [type, name, start, end] = bits->members[i];
    auto fieldType = transpileType(info->memberTypes[i]->type->destroyReferences().get());
    target.insertFunctionDefinition(
      "_Alta_bitfield_set_" + mangleName(info->memberVariables[i].get()),
      {
        { "destination", underylingTypePtr },
        { "source", fieldType },
      },
      underylingTypePtr,
      true
    );
    for (size_t n = start; n <= end; n++) {
      target.insertExpressionStatement(
        target.createAssignment(
          target.createDereference(
            target.createFetch("destination")
          ),
          target.createBinaryOperation(
            CAST::OperatorType::BitwiseOr,
            target.createBinaryOperation(
              CAST::OperatorType::BitwiseAnd,
              target.createDereference(
                target.createFetch("destination")
              ),
              target.createUnaryOperation(
                CAST::UOperatorType::BitwiseNot,
                target.createBinaryOperation(
                  CAST::OperatorType::LeftShift,
                  target.createIntegerLiteral("1UL"),
                  target.createIntegerLiteral(n)
                )
              )
            ),
            target.createBinaryOperation(
              CAST::OperatorType::LeftShift,
              target.createBinaryOperation(
                CAST::OperatorType::BitwiseAnd,
                target.createFetch("source"),
                target.createBinaryOperation(
                  CAST::OperatorType::LeftShift,
                  target.createIntegerLiteral("1UL"),
                  target.createIntegerLiteral(n - start)
                )
              ),
              target.createIntegerLiteral(start)
            )
          )
        )
      );
    }
    target.insertReturnDirective(target.createFetch("destination"));
    target.exitInsertionPoint();
  }
  target.exitInsertionPoint();
  return co.finalYield();
};

auto Talta::CTranspiler::transpileLambdaExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto lambda = std::dynamic_pointer_cast<AAST::LambdaExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::LambdaExpression>(_info);

  if (co.iteration() == 0) {
    CExpression result = source.createFetch("NULL");
    auto savedIP = source.insertionPoint;

    while (source.insertionPoint->parent) {
      source.exitInsertionPoint();
    }

    source.insertionPoint->moveBackward();

    co.save((size_t)0, savedIP);
    return co.yield();
  } else {
    auto [loopIteration, savedIP] = co.load<size_t, decltype(Ceetah::Builder::insertionPoint)>();

    if (loopIteration == 0) {
      auto mod = AltaCore::Util::getModule(info->inputScope.get()).lock();
      auto mangledModName = mangleName(mod.get());

      for (auto& hoistedType: info->function->publicHoistedItems) {
        hoist(hoistedType, false, false);
      }
      for (auto& hoistedType: info->function->privateHoistedItems) {
        hoist(hoistedType, false, false);
      }

      auto mangledFuncName = "_Alta_lambda_" + mangleName(info->inputScope.get()) + "_10_" + std::to_string(info->function->itemID);
      std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> cParams;

      cParams.push_back(std::make_tuple("__Alta_state", source.createType("_Alta_lambda_state")));

      for (size_t i = 0; i < info->function->parameterVariables.size(); i++) {
        auto& var = info->function->parameterVariables[i];
        auto& param = lambda->parameters[i];
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

      hoist(info->function, false);

      source.insertFunctionDefinition(mangledFuncName, cParams, returnType);

      for (size_t i = 0; i < info->toReference.size(); i++) {
        auto pointed = info->toReference[i]->type->point();
        source.insertVariableDefinition(
          transpileType(pointed.get()),
          mangleName(info->toReference[i].get()),
          source.createCast(
            source.createDereference(
              source.createBinaryOperation(
                CAST::OperatorType::Addition,
                source.createAccessor("__Alta_state", "references"),
                source.createIntegerLiteral(i)
              )
            ),
            transpileType(pointed.get())
          )
        );
      }

      for (size_t i = 0; i < info->toCopy.size(); i++) {
        auto pointed = info->toCopy[i]->type->point();
        CExpression expr = source.createCast(
          source.createDereference(
            source.createBinaryOperation(
              CAST::OperatorType::Addition,
              source.createAccessor("__Alta_state", "copies"),
              source.createIntegerLiteral(i)
            )
          ),
          pointed->isNative ? source.createType("_Alta_wrapper", { { CAST::TypeModifierFlag::Pointer } }) : transpileType(pointed.get())
        );
        if (pointed->isNative) {
          expr = source.createCast(
            source.createAccessor(
              source.createDereference(expr),
              "value"
            ),
            transpileType(pointed.get())
          );
        }
        source.insertVariableDefinition(
          transpileType(pointed.get()),
          mangleName(info->toCopy[i].get()),
          expr
        );
      }

      stackBookkeepingStart(info->function->scope);

      for (size_t i = 0; i < info->function->parameterVariables.size(); i++) {
        auto& var = info->function->parameterVariables[i];
        if (
          !currentScope->noRuntime &&
          canPush(var->isVariable ? var->type->follow() : var->type)
        ) {
          if (var->isVariable) {
            source.insertBlock();
            source.insertVariableDefinition(source.createType("size_t"), "_Alta_variable_array_index", source.createIntegerLiteral(0));
            source.insertWhileLoop(
              source.createBinaryOperation(
                CAST::OperatorType::LessThan,
                source.createFetch("_Alta_variable_array_index"),
                source.createFetch("_Alta_array_length_" + mangleName(var.get()).substr(12))
              )
            );
            source.insertBlock();
          }
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
                var->isVariable
                  ? source.createBinaryOperation(
                      CAST::OperatorType::Addition,
                      source.createFetch("_Alta_array_" + mangleName(var.get()).substr(12)),
                      source.createFetch("_Alta_variable_array_index")
                    )
                  : source.createPointer(
                      source.createFetch(mangleName(var.get()))
                    ),
                source.createType(
                  "_Alta_object",
                  { { CAST::TypeModifierFlag::Pointer } }
                )
              ),
            }
          ));
          if (var->isVariable) {
            source.insertExpressionStatement(
              source.createUnaryOperation(
                CAST::UOperatorType::PreIncrement,
                source.createFetch("_Alta_variable_array_index")
              )
            );
            source.exitInsertionPoint();
            source.exitInsertionPoint();
            source.exitInsertionPoint();
          }
        }
      }

      co.save((size_t)1, savedIP);
      co.save(mangledFuncName, mangledModName, cParams, returnType);
      return co.yield();
    } else if (loopIteration - 1 < lambda->body->statements.size()) {
      auto i = loopIteration - 1;
      co.save(loopIteration + 1, savedIP);
      co.saveAny(co.loadAny());
      return co.await(boundTranspile, lambda->body->statements[i], info->body->statements[i]);
    } else {
      auto [
        mangledFuncName,
        mangledModName,
        cParams,
        returnType
      ] = co.load<
        std::string,
        std::string,
        std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>>,
        std::shared_ptr<CAST::Type>
      >();

      stackBookkeepingStop(info->function->scope);
      source.exitInsertionPoint();

      source.insertFunctionDefinition("_Alta_make_" + mangledFuncName, {
        {"copiesInput", source.createType("void", { { CAST::TypeModifierFlag::Pointer }, { CAST::TypeModifierFlag::Pointer } })},
        {"referencesInput", source.createType("void", { { CAST::TypeModifierFlag::Pointer }, { CAST::TypeModifierFlag::Pointer } })},
      }, source.createType("_Alta_basic_function"), true);

      source.insertVariableDefinition(
        source.createType("size_t", { { CAST::TypeModifierFlag::Pointer } }),
        "counterMalloc",
        source.createFunctionCall(source.createFetch("malloc"), {
          source.createSizeof(source.createType("size_t")),
        })
      );
      source.insertVariableDefinition(
        source.createType("_Alta_object", { { CAST::TypeModifierFlag::Pointer }, { CAST::TypeModifierFlag::Pointer } }),
        "copiesMalloc",
        source.createFunctionCall(source.createFetch("malloc"), {
          source.createBinaryOperation(
            CAST::OperatorType::Multiplication,
            source.createIntegerLiteral(info->toCopy.size()),
            source.createSizeof(source.createType("_Alta_object", { { CAST::TypeModifierFlag::Pointer } }))
          ),
        })
      );
      source.insertVariableDefinition(
        source.createType("void", { { CAST::TypeModifierFlag::Pointer }, { CAST::TypeModifierFlag::Pointer } }),
        "referencesMalloc",
        source.createFunctionCall(source.createFetch("malloc"), {
          source.createBinaryOperation(
            CAST::OperatorType::Multiplication,
            source.createIntegerLiteral(info->toReference.size()),
            source.createSizeof(source.createType("void", { { CAST::TypeModifierFlag::Pointer } }))
          ),
        })
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createDereference(source.createFetch("counterMalloc")),
          source.createIntegerLiteral(0)
        )
      );

      source.insertExpressionStatement(
        source.createUnaryOperation(
          CAST::UOperatorType::PreIncrement,
          source.createDereference(source.createFetch("counterMalloc"))
        )
      );

      for (size_t i = 0; i < info->toCopy.size(); i++) {
        auto& curr = info->toCopy[i];
        auto elmPtr = source.createBinaryOperation(
          CAST::OperatorType::Addition,
          source.createFetch("copiesMalloc"),
          source.createIntegerLiteral(i)
        );
        auto elm = source.createDereference(elmPtr);
        source.insertExpressionStatement(
          source.createAssignment(
            elm,
            source.createFunctionCall(source.createFetch("malloc"), {
              source.createSizeof(curr->type->isNative ? source.createType("_Alta_wrapper") : transpileType(curr->type.get())),
            })
          )
        );
        if (curr->type->isNative) {
          auto wrapper = source.createDereference(
            source.createDereference(
              source.createCast(
                elmPtr,
                source.createType("_Alta_wrapper", { { CAST::TypeModifierFlag::Pointer }, { CAST::TypeModifierFlag::Pointer } })
              )
            )
          );
          source.insertExpressionStatement(
            source.createAssignment(
              source.createAccessor(
                wrapper,
                "objectType"
              ),
              source.createFetch("_Alta_object_type_wrapper")
            )
          );
          source.insertExpressionStatement(
            source.createAssignment(
              source.createAccessor(
                wrapper,
                "value"
              ),
              source.createFunctionCall(source.createFetch("malloc"), {
                source.createSizeof(transpileType(curr->type.get())),
              })
            )
          );
          source.insertExpressionStatement(
            source.createAssignment(
              source.createDereference(
                source.createCast(
                  source.createAccessor(
                    wrapper,
                    "value"
                  ),
                  transpileType(curr->type->point().get())
                )
              ),
              source.createDereference(
                source.createCast(
                  source.createDereference(
                    source.createBinaryOperation(
                      CAST::OperatorType::Addition,
                      source.createFetch("copiesInput"),
                      source.createIntegerLiteral(i)
                    )
                  ),
                  transpileType(curr->type->point().get())
                )
              )
            )
          );
          source.insertExpressionStatement(
            source.createAssignment(
              source.createAccessor(
                wrapper,
                "destructor"
              ),
              source.createFetch("NULL")
            )
          );
        } else {
          source.insertExpressionStatement(
            source.createAssignment(
              source.createDereference(
                source.createCast(
                  source.createDereference(elmPtr),
                  transpileType(curr->type->point().get())
                )
              ),
              doCopyCtor(
                source.createDereference(
                  source.createCast(
                    source.createDereference(
                      source.createBinaryOperation(
                        CAST::OperatorType::Addition,
                        source.createFetch("copiesInput"),
                        source.createIntegerLiteral(i)
                      )
                    ),
                    transpileType(curr->type->point().get())
                  )
                ),
                curr->type,
                defaultCopyInfo
              )
            )
          );
        }
      }

      source.insertExpressionStatement(
        source.createFunctionCall(source.createFetch("memcpy"), {
          source.createFetch("referencesMalloc"),
          source.createFetch("referencesInput"),
          source.createBinaryOperation(
            CAST::OperatorType::Multiplication,
            source.createIntegerLiteral(info->toReference.size()),
            source.createSizeof(source.createType("void", { { CAST::TypeModifierFlag::Pointer } }))
          )
        })
      );

      source.insertVariableDefinition(
        source.createType("_Alta_basic_function"),
        "result",
        source.createArrayLiteral({ source.createIntegerLiteral(0) })
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            "result",
            "objectType"
          ),
          source.createFetch("_Alta_object_type_function")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              "result",
              "state"
            ),
            "referenceCount"
          ),
          source.createFetch("counterMalloc")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              "result",
              "state"
            ),
            "copyCount"
          ),
          source.createIntegerLiteral(info->toCopy.size())
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              "result",
              "state"
            ),
            "referenceBlockCount"
          ),
          source.createIntegerLiteral(info->toReference.size())
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              "result",
              "state"
            ),
            "copies"
          ),
          source.createFetch("copiesMalloc")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            source.createAccessor(
              "result",
              "state"
            ),
            "references"
          ),
          source.createFetch("referencesMalloc")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            "result",
            "plain"
          ),
          source.createFetch("NULL")
        )
      );

      source.insertExpressionStatement(
        source.createAssignment(
          source.createAccessor(
            "result",
            "lambda"
          ),
          source.createFetch(mangledFuncName)
        )
      );

      source.insertReturnDirective(source.createFetch("result"));

      source.insertionPoint = savedIP;

      std::vector<CExpression> copyItems;
      std::vector<CExpression> referenceItems;

      for (auto& item: info->toCopy) {
        copyItems.push_back(
          source.createCast(
            source.createPointer(
              source.createFetch(mangleName(item.get()))
            ),
            source.createType("void", { { CAST::TypeModifierFlag::Pointer } })
          )
        );
      }

      for (auto& item: info->toReference) {
        referenceItems.push_back(
          source.createCast(
            source.createPointer(
              source.createFetch(mangleName(item.get()))
            ),
            source.createType("void", { { CAST::TypeModifierFlag::Pointer } })
          )
        );
      }

      if (copyItems.size() == 0) {
        copyItems.push_back(source.createFetch("NULL"));
      }

      if (referenceItems.size() == 0) {
        referenceItems.push_back(source.createFetch("NULL"));
      }

      auto voidArrayType = source.createType("void", { { CAST::TypeModifierFlag::Pointer } });
      voidArrayType->arraySize = SIZE_MAX;

      CExpression result = source.createFunctionCall(source.createFetch("_Alta_make_" + mangledFuncName), {
        source.createArrayLiteral(copyItems, voidArrayType),
        source.createArrayLiteral(referenceItems, voidArrayType),
      });
      return co.finalYield(result);
    }
  }
};

auto Talta::CTranspiler::transpileSpecialFetchExpression(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto fetch = std::dynamic_pointer_cast<AAST::SpecialFetchExpression>(node);
  auto info = std::dynamic_pointer_cast<DH::SpecialFetchExpression>(_info);

  CExpression expr = source.createFetch(mangleName(info->items[0].get()));
  return co.finalYield(expr);
};
auto Talta::CTranspiler::transpileClassOperatorDefinitionStatement(Coroutine& co) -> Coroutine& {
  auto [node, _info] = co.arguments();
  auto op = std::dynamic_pointer_cast<AAST::ClassOperatorDefinitionStatement>(node);
  auto info = std::dynamic_pointer_cast<DH::ClassOperatorDefinitionStatement>(_info);

  if (co.iteration() == 0) {
    auto mangledFuncName = mangleName(info->method.get());

    std::vector<std::tuple<std::string, std::shared_ptr<CAST::Type>>> cParams;

    cParams.push_back(std::make_tuple("_Alta_self", transpileType(info->method->parentClassType.get())));

    if (info->method->operatorType == AltaCore::Shared::ClassOperatorType::Index || info->method->orientation != AltaCore::Shared::ClassOperatorOrientation::Unary) {
      auto& param = info->method->parameterVariables.front();
      cParams.push_back(std::make_tuple(mangleName(param.get()), transpileType(param->type.get())));
    }

    auto returnType = transpileType(info->method->returnType.get());

    bool targetIsHeader = true;

    if (!targetIsHeader) {
      for (auto& hoistedType: info->method->publicHoistedItems) {
        hoist(hoistedType, false);
      }
    }
    for (auto& hoistedType: info->method->privateHoistedItems) {
      hoist(hoistedType, false);
    }

    if (targetIsHeader) {
      auto mod = AltaCore::Util::getModule(info->method->parentScope.lock().get()).lock();
      auto mangledModName = mangleName(mod.get());
      headerPredeclaration("_ALTA_FUNCTION_" + mangledFuncName, mangledModName, true);
      for (auto arg: info->method->genericArguments) {
        hoist(arg, true);
      }
      for (auto& hoistedType: info->method->publicHoistedItems) {
        hoist(hoistedType, true);
      }
      hoist(info->method->returnType, true);
      header.insertFunctionDeclaration(mangledFuncName, cParams, returnType);
      header.exitInsertionPoint();
    }

    source.insertFunctionDefinition(mangledFuncName, cParams, returnType);

    stackBookkeepingStart(info->method->scope);

    for (size_t i = 0; i < info->method->parameterVariables.size(); i++) {
      auto& var = info->method->parameterVariables[i];
      if (!currentScope->noRuntime && canPush(var->type)) {
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
              source.createType(
                "_Alta_object",
                { { CAST::TypeModifierFlag::Pointer } }
              )
            ),
          }
        ));
      }
    }

    return co.yield();
  } else if (co.iteration() - 1 < op->block->statements.size()) {
    auto i = co.iteration() - 1;
    return co.await(boundTranspile, op->block->statements[i], info->block->statements[i]);
  } else {
    stackBookkeepingStop(info->method->scope);
    source.exitInsertionPoint();
    return co.finalYield();
  }
};
// </transpilation-coroutines>

std::shared_ptr<Ceetah::AST::Expression> Talta::CTranspiler::transpile(std::shared_ptr<AltaCore::AST::Node> _node, std::shared_ptr<AltaCore::DH::Node> __info) {
  CoroutineManager coman;
  auto node = _node.get();
  auto _info = __info.get();

  auto result = coman.await(bind(&CTranspiler::transpile), _node, __info);
  auto asPtr = ALTACORE_ANY_CAST<std::shared_ptr<Ceetah::AST::Expression>>(&result);
  if (asPtr) {
    return *asPtr;
  } else {
    return nullptr;
  }
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
  definitions.insertPreprocessorUndefinition("_AMA_WAS_DEFINED_" + mangledModuleName);
  definitions.exitInsertionPoint();

  auto runtimeHeader = currentModule->noRuntimeInclude ? "_ALTA_RUNTIME_DEFINITIONS_HEADER_" : "_ALTA_RUNTIME_COMMON_HEADER_";
  header.insertPreprocessorInclusion(runtimeHeader + mangledModuleName, Ceetah::AST::InclusionType::Computed);

  //header.insertPreprocessorConditional("!defined(_ALTA_MODULE_HEADER_" + mangledModuleName + ")");
  //header.insertPreprocessorDefinition("_ALTA_MODULE_HEADER_" + mangledModuleName);

  for (auto& incl: moduleIncludes[currentModule->path.toString()]) {
    header.insertPreprocessorInclusion(incl, Ceetah::AST::InclusionType::System);
  }

  for (size_t i = 0; i < altaRoot->statements.size(); i++) {
    auto& stmt = altaRoot->statements[i];
    auto& stmtInfo = altaRoot->info->statements[i];
    transpile(stmt, stmtInfo);
  }

  //header.exitInsertionPoint();

  for (auto& type: currentModule->hoistedItems) {
    hoist(type);
  }

  header.insertPreprocessorUndefinition("_ALTA_MODULE_ALL_" + mangledModuleName);

  definitions.insertPreprocessorUndefinition("_ALTA_SAVE_DEFS_" + mangledModuleName);

  auto voidType = header.createType("void");

  header.insertPreprocessorConditional("!defined(_ALTA_INIT_" + mangledModuleName + ')');
  header.insertPreprocessorDefinition("_ALTA_INIT_" + mangledModuleName);
  header.insertFunctionDeclaration("_Alta_module_init_" + mangledModuleName, {}, voidType);
  header.exitInsertionPoint();

  source.insertFunctionDefinition("_Alta_module_init_" + mangledModuleName, {}, voidType);
  source.insertExpressionStatement(source.createFetch("static _Alta_bool alreadyInited = _Alta_bool_false"));
  source.insertConditionalStatement(source.createFetch("alreadyInited"));
  source.insertReturnDirective();
  source.exitInsertionPoint();
  source.insertExpressionStatement(
    source.createAssignment(
      source.createFetch("alreadyInited"),
      source.createFetch("_Alta_bool_true")
    )
  );

  for (auto& mod: currentModule->dependencies) {
    source.insertExpressionStatement(
      source.createFunctionCall(
        source.createFetch("_Alta_module_init_" + mangleName(mod.get())),
        {}
      )
    );
  }

  for (size_t i = 0; i < altaRoot->statements.size(); i++) {
    auto& stmt = altaRoot->statements[i];
    auto& stmtInfo = altaRoot->info->statements[i];
    if (stmt->nodeType() != AAST::NodeType::ExpressionStatement) continue;
    auto exprStmt = std::dynamic_pointer_cast<AAST::ExpressionStatement>(stmt);
    auto exprStmtInfo = std::dynamic_pointer_cast<DH::ExpressionStatement>(stmtInfo);
    if (exprStmt->expression->nodeType() != AAST::NodeType::VariableDefinitionExpression) continue;
    auto var = std::dynamic_pointer_cast<AAST::VariableDefinitionExpression>(exprStmt->expression);
    auto det = std::dynamic_pointer_cast<DH::VariableDefinitionExpression>(exprStmtInfo->expression);
    if (!var->initializationExpression || det->variable->type->isNative || det->variable->type->indirectionLevel() > 0) continue;

    source.insertExpressionStatement(
      source.createAssignment(
        source.createFetch(
          mangleName(det->variable.get())
        ),
        transpile(var->initializationExpression, det->initializationExpression)
      )
    );
  }

  source.exitInsertionPoint();
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

#define AC_ATTRIBUTE_FUNC [=](std::shared_ptr<AltaCore::AST::Node> _target, std::shared_ptr<AltaCore::DH::Node> _info, std::vector<AltaCore::Attributes::AttributeArgument> args) -> void
#define AC_ATTRIBUTE_CAST(x) auto target = std::dynamic_pointer_cast<AltaCore::AST::x>(_target);\
  auto info = std::dynamic_pointer_cast<AltaCore::DH::x>(_info);\
  if (!target || !info) throw std::runtime_error("this isn't supposed to happen");
#define AC_ATTRIBUTE(x, ...) AltaCore::Attributes::registerAttribute({ __VA_ARGS__ }, { AltaCore::AST::NodeType::x }, AC_ATTRIBUTE_FUNC {\
  AC_ATTRIBUTE_CAST(x);
#define AC_GENERAL_ATTRIBUTE(...) AltaCore::Attributes::registerAttribute({ __VA_ARGS__ }, {}, AC_ATTRIBUTE_FUNC {
#define AC_END_ATTRIBUTE }, modulePath.toString())

void Talta::registerAttributes(AltaCore::Filesystem::Path modulePath) {
  AC_GENERAL_ATTRIBUTE("CTranspiler", "include");
    if (args.size() == 0) return;
    if (!args[0].isString) return;
    moduleIncludes[modulePath.toString()].push_back(args[0].string);
  AC_END_ATTRIBUTE;
  AC_ATTRIBUTE(Parameter, "CTranspiler", "vararg");
    varargTable[target->id] = true;
  AC_END_ATTRIBUTE;
  AC_ATTRIBUTE(CodeLiteralNode, "CTranspiler", "inHeader");
    inHeaderTable[target->id] = true;
  AC_END_ATTRIBUTE;
  AC_GENERAL_ATTRIBUTE("initializeModule");
  AC_END_ATTRIBUTE;
  AC_GENERAL_ATTRIBUTE("CTranspiler", "alwaysImport")
    if (_target->nodeType() == ANT::ExpressionStatement) {
      auto exprStmt = std::dynamic_pointer_cast<AAST::ExpressionStatement>(_target);
      alwaysImportTable[exprStmt->expression->id] = true;
    } else {
      alwaysImportTable[_target->id] = true;
    }
  AC_END_ATTRIBUTE;
  AC_GENERAL_ATTRIBUTE("initializeRuntime");
  AC_END_ATTRIBUTE;
  AC_ATTRIBUTE(StructureDefinitionStatement, "CTranspiler", "packed");
    packedTable[target->id] = true;
  AC_END_ATTRIBUTE;
};
