//////////////////////////////////////////////////////////////////////
//
//    TypeCheckVisitor - Walk the parser tree to do the semantic
//                       typecheck for the Asl programming language
//
//    Copyright (C) 2020-2030  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "TypeCheckVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/SemErrors.h"

#include <iostream>
#include <string>

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
TypeCheckVisitor::TypeCheckVisitor(TypesMgr       & Types,
                                   SymTable       & Symbols,
                                   TreeDecoration & Decorations,
                                   SemErrors      & Errors) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations},
  Errors{Errors} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId TypeCheckVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void TypeCheckVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
antlrcpp::Any TypeCheckVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) { 
    visit(ctxFunc);
  }
  if (Symbols.noMainProperlyDeclared())
    Errors.noMainProperlyDeclared(ctx);
  Symbols.popScope();
  Errors.print();
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  if (ctx->basic_type()) {
    TypesMgr::TypeId t = getTypeDecor(ctx->basic_type());
    setCurrentFunctionTy(t);
  } else {
    setCurrentFunctionTy(Types.createVoidTy());
  }
  visit(ctx->statements());
  Symbols.popScope();
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitFunction_call(AslParser::Function_callContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  std::string ident = ctx->ID()->getText();
  if (!Symbols.isFunctionClass(ident)) {
    if (Symbols.findInStack(ident) == -1) {
      Errors.undeclaredIdent(ctx->ID());
      putTypeDecor(ctx, Types.createErrorTy());
      DEBUG_EXIT();
      return 0;
    }
    else {
      Errors.isNotCallable(ctx);
      putTypeDecor(ctx, Types.createErrorTy());
      DEBUG_EXIT();
      return 0;
    }
  }
  TypesMgr::TypeId type = Symbols.getGlobalFunctionType(ident);
  std::vector<TypesMgr::TypeId>  param_types = Types.getFuncParamsTypes(type);
  TypesMgr::TypeId returnType = Types.getFuncReturnType(type);
  putTypeDecor(ctx, returnType);
  if (ctx->expr().size() != param_types.size()) {
    Errors.numberOfParameters(ctx);
    DEBUG_EXIT();
    return 0;
  }
  for (uint i = 0; i < param_types.size(); i++) {
    TypesMgr::TypeId t = getTypeDecor(ctx->expr(i));
    if (not Types.isErrorTy(t) and not Types.copyableTypes(t, param_types[i])) {
      Errors.incompatibleParameter(ctx->expr(i), i+1, ctx);
    }
  }
  DEBUG_EXIT();
  return 0;
}

// antlrcpp::Any TypeCheckVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any TypeCheckVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any TypeCheckVisitor::visitType(AslParser::TypeContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

antlrcpp::Any TypeCheckVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and
      (not Types.copyableTypes(t1, t2)))
    Errors.incompatibleAssignment(ctx->ASSIGN());
  if ((not Types.isErrorTy(t1)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableLeftExpr(ctx->left_expr());
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1)))
    Errors.booleanRequired(ctx);
  visit(ctx->statements(0));
  if (ctx->statements(1))
    visit(ctx->statements(1));
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1)))
    Errors.booleanRequired(ctx);
  visit(ctx->statements());
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitFuncCallExpr(AslParser::FuncCallExprContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->function_call());
  putIsLValueDecor(ctx, false);
  TypesMgr::TypeId type = getTypeDecor(ctx->function_call());
  if (Types.isVoidTy(type)) {
    Errors.isNotFunction(ctx);
    putTypeDecor(ctx, Types.createErrorTy());
    DEBUG_EXIT();
    return 0;
  }
  putTypeDecor(ctx, type);
  DEBUG_EXIT();
  return 0;
}

/*
antlrcpp::Any TypeCheckVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  if (Types.isErrorTy(t1)) {
    ;
  } else if (not Types.isFunctionTy(t1)) {
    Errors.isNotCallable(ctx);
  }
  DEBUG_EXIT();
  return 0;
}*/

antlrcpp::Any TypeCheckVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)) and
      (not Types.isFunctionTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  if ((not Types.isErrorTy(t1)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableExpression(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  TypesMgr::TypeId return_type = getCurrentFunctionTy();
  if (Types.isVoidTy(return_type)) {
    if (ctx->expr()) {
      Errors.incompatibleReturn(ctx->RETURN());
      DEBUG_EXIT();
      return 0;
    }
  } else if (!ctx->expr()) {
    Errors.incompatibleReturn(ctx->RETURN());
    DEBUG_EXIT();
    return 0;
  } else {
    TypesMgr::TypeId expr_type = getTypeDecor(ctx->expr());
    if ((not Types.isErrorTy(expr_type)) and
        (not Types.copyableTypes(return_type, expr_type))) {
          Errors.incompatibleReturn(ctx->RETURN());
        }
  }
  DEBUG_EXIT();
  return 0;
}

// antlrcpp::Any TypeCheckVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

antlrcpp::Any TypeCheckVisitor::visitLeftExprIdent(AslParser::LeftExprIdentContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  putTypeDecor(ctx, t1);
  bool b = getIsLValueDecor(ctx->ident());
  putIsLValueDecor(ctx, b);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitLeftExprArray(AslParser::LeftExprArrayContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  TypesMgr::TypeId expression_type = getTypeDecor(ctx->expr());
  if (not Types.isErrorTy(expression_type) and not Types.isIntegerTy(expression_type)) {
    Errors.nonIntegerIndexInArrayAccess(ctx->expr());
  }
  TypesMgr::TypeId array_type = getTypeDecor(ctx->ident());
  if(Types.isErrorTy(array_type)) {
    putTypeDecor(ctx, Types.createErrorTy());
    putIsLValueDecor(ctx, true);
    DEBUG_EXIT();
    return 0;
  }
  if (not Types.isArrayTy(array_type)) {
    Errors.nonArrayInArrayAccess(ctx->ident());
    putTypeDecor(ctx, Types.createErrorTy());
    putIsLValueDecor(ctx, true);
    DEBUG_EXIT();
    return 0;
  }
  putTypeDecor(ctx, Types.getArrayElemType(array_type));
  putIsLValueDecor(ctx, true);
  DEBUG_EXIT();
  return 0;
}


antlrcpp::Any TypeCheckVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t = getTypeDecor(ctx->expr());
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitBinaryOperation(AslParser::BinaryOperationContext *ctx) {
  DEBUG_ENTER();

  visit(ctx->expr(0));
  visit(ctx->expr(1));

  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
	TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));

  if ((not Types.isErrorTy(t1) and not Types.isBooleanTy(t1)) or
      (not Types.isErrorTy(t2) and not Types.isBooleanTy(t2)))
    Errors.incompatibleOperator(ctx->op);

	putTypeDecor(ctx, Types.createBooleanTy());
	putIsLValueDecor(ctx, false);
	
	DEBUG_EXIT();
	return 0;
}

antlrcpp::Any TypeCheckVisitor::visitBinaryOperationUnary(AslParser::BinaryOperationUnaryContext *ctx) {
  DEBUG_ENTER();
	visit(ctx->expr());
	TypesMgr::TypeId t = getTypeDecor(ctx->expr());
	
	if (not Types.isErrorTy(t) and not Types.isBooleanTy(t))
		Errors.incompatibleOperator(ctx->op);
	
	putTypeDecor(ctx, Types.createBooleanTy());
	putIsLValueDecor(ctx, false);
	
	DEBUG_EXIT();
	return 0;
}

antlrcpp::Any TypeCheckVisitor::visitArithmeticUnary(AslParser::ArithmeticUnaryContext *ctx) {
  DEBUG_ENTER();
	visit(ctx->expr());
	TypesMgr::TypeId t = getTypeDecor(ctx->expr());
	
	if (not Types.isErrorTy(t) and not Types.isNumericTy(t))
		Errors.incompatibleOperator(ctx->op);

  t = Types.createIntegerTy();
	putTypeDecor(ctx, t);
	putIsLValueDecor(ctx, false);

	DEBUG_EXIT();
	return 0;
}

// '%' only allows integer values & errors. '*' and '/' allow numerics and errors. If there is a float then ret type is float.
antlrcpp::Any TypeCheckVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  putIsLValueDecor(ctx, false);
  if (ctx->MOD()) {
    if ((not Types.isErrorTy(t1) and not Types.isIntegerTy(t1)) or
        (not Types.isErrorTy(t2) and not Types.isIntegerTy(t2)))
      Errors.incompatibleOperator(ctx->op);
    putTypeDecor(ctx, Types.createIntegerTy());
  } else {
    if ((not Types.isErrorTy(t1) and not Types.isNumericTy(t1)) or
        (not Types.isErrorTy(t2) and not Types.isNumericTy(t2)))
      Errors.incompatibleOperator(ctx->op);
    if (Types.isFloatTy(t1) or Types.isFloatTy(t2)) {
      putTypeDecor(ctx, Types.createFloatTy());
    } else {
      putTypeDecor(ctx, Types.createIntegerTy());
    }
  }
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  std::string oper = ctx->op->getText();
  if ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and
      (not Types.comparableTypes(t1, t2, oper)))
    Errors.incompatibleOperator(ctx->op);
  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t;

  if (ctx->INTVAL()) t = Types.createIntegerTy();
  else if (ctx->FLOATVAL()) t = Types.createFloatTy();
  else if (ctx->CHARVAL()) t = Types.createCharacterTy();
  else if (ctx->BOOLVAL()) t = Types.createBooleanTy();

  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitLeftExprValue(AslParser::LeftExprValueContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  TypesMgr::TypeId t = getTypeDecor(ctx->left_expr());
  bool b = getIsLValueDecor(ctx->left_expr());

  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, b);

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  std::string ident = ctx->getText();
  if (Symbols.findInStack(ident) == -1) {
    Errors.undeclaredIdent(ctx->ID());
    TypesMgr::TypeId te = Types.createErrorTy();
    putTypeDecor(ctx, te);
    putIsLValueDecor(ctx, true);
  }
  else {
    TypesMgr::TypeId t1 = Symbols.getType(ident);
    putTypeDecor(ctx, t1);
    if (Symbols.isFunctionClass(ident))
      putIsLValueDecor(ctx, false);
    else
      putIsLValueDecor(ctx, true);
  }
  DEBUG_EXIT();
  return 0;
}


// Getters for the necessary tree node atributes:
//   Scope, Type ans IsLValue
SymTable::ScopeId TypeCheckVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId TypeCheckVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getType(ctx);
}
bool TypeCheckVisitor::getIsLValueDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getIsLValue(ctx);
}

// Setters for the necessary tree node attributes:
//   Scope, Type ans IsLValue
void TypeCheckVisitor::putScopeDecor(antlr4::ParserRuleContext *ctx, SymTable::ScopeId s) {
  Decorations.putScope(ctx, s);
}
void TypeCheckVisitor::putTypeDecor(antlr4::ParserRuleContext *ctx, TypesMgr::TypeId t) {
  Decorations.putType(ctx, t);
}
void TypeCheckVisitor::putIsLValueDecor(antlr4::ParserRuleContext *ctx, bool b) {
  Decorations.putIsLValue(ctx, b);
}
