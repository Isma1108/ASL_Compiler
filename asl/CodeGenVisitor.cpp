//////////////////////////////////////////////////////////////////////
//
//    CodeGenVisitor - Walk the parser tree to do
//                     the generation of code
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

#include "CodeGenVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/code.h"

#include <string>
#include <cstddef>    // std::size_t

// uncomment the following line to enable debugging messages with DEBUG*
//  #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
CodeGenVisitor::CodeGenVisitor(TypesMgr       & Types,
                               SymTable       & Symbols,
                               TreeDecoration & Decorations) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId CodeGenVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void CodeGenVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

//Useful coercion function
std::string CodeGenVisitor::doCoercionIntFloat(
    instructionList & code,
    TypesMgr::TypeId t1,
		TypesMgr::TypeId t2,
		const std::string & sourceAddr) {
	if (Types.isIntegerTy(t1) and Types.isFloatTy(t2)) {
		const std::string addr = "%" + codeCounters.newTEMP();
		code = code || instruction::FLOAT(addr, sourceAddr);
		return addr;
	} 
  else return sourceAddr;
}


// Methods to visit each kind of node:
//
antlrcpp::Any CodeGenVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  code my_code;
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) { 
    subroutine subr = visit(ctxFunc);
    my_code.add_subroutine(subr);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return my_code;
}

antlrcpp::Any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();

  const std::string& funcName = ctx->ID()->getText();
  TypesMgr::TypeId funcType = Symbols.getType(funcName);
  TypesMgr::TypeId returnType = Types.getFuncReturnType(funcType);

  std::vector<var> && params = visit(ctx->parameters());
  if (Types.isPrimitiveNonVoidTy(returnType)) subr.add_param("_result", Types.to_string(returnType));
  for (const auto & p : params) {
    subr.add_param(p.name, p.type); //isArray=false by default, we don't have arrays in parameters
  }

  std::vector<var> && lvars = visit(ctx->declarations());
  for (const auto & onevar : lvars) {
    subr.add_var(onevar);
  }
  instructionList && code = visit(ctx->statements());
  //We already have a RETURN statement
  if (Types.isVoidFunction(funcType)) code = code || instruction(instruction::RETURN());
  subr.set_instructions(code);
  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

antlrcpp::Any CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> lvars;
  for (const auto & varDeclCtx : ctx->variable_decl()) {
    const std::vector<var>& onelineVars = visit(varDeclCtx);
    for (const auto & v : onelineVars) lvars.push_back(v);
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  std::size_t      size = Types.getSizeOfType(t1);

  std::vector<var> onelineVars;
  for (const auto& id : ctx->ID()) {
    onelineVars.push_back(var{id->getText(), Types.to_string(t1), size});
  }
  DEBUG_EXIT();
  return onelineVars;
}
 
antlrcpp::Any CodeGenVisitor::visitParameters(AslParser::ParametersContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> params;
  for (const auto & p : ctx->parameter_decl()) params.push_back(visit(p));
  DEBUG_EXIT();
  return params;
}

antlrcpp::Any CodeGenVisitor::visitParameter_decl(AslParser::Parameter_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t = getTypeDecor(ctx->type());
  DEBUG_EXIT();
  return var{ctx->ID()->getText(), Types.to_string(t)}; //by default size = 1
}

antlrcpp::Any CodeGenVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  for (auto stCtx : ctx->statement()) {
    instructionList && codeS = visit(stCtx);
    code = code || codeS;
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  
  
  CodeAttribs     && codAtsE1 = visit(ctx->left_expr());
  std::string           addr1 = codAtsE1.addr;
  std::string           offs1 = codAtsE1.offs;
  instructionList &     code1 = codAtsE1.code;
  //TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  
  CodeAttribs     && codAtsE2 = visit(ctx->expr());
  std::string           addr2 = codAtsE2.addr;
  std::string           offs2 = codAtsE2.offs;
  instructionList &     code2 = codAtsE2.code;
  
  //TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());

  code = code1 || code2;
  
  if (offs1 != "") code = code || instruction::XLOAD(addr1, offs1, addr2);
  else code = code || instruction::LOAD(addr1, addr2);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = visit(ctx->expr());
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  instructionList &&   code2 = visit(ctx->statements(0));

  std::string label = codeCounters.newLabelIF();
  std::string labelEndIf = "endif"+label;

  if (ctx->ELSE()) {
    std::string labelElse = "else" + label;
    instructionList && code3 = visit(ctx->statements(1));
    code = code1 || instruction::FJUMP(addr1, labelElse) ||
           code2 || instruction::UJUMP(labelEndIf) ||
           instruction::LABEL(labelElse) || code3 ||
           instruction::LABEL(labelEndIf);

  }
  else {
    code = code1 || instruction::FJUMP(addr1, labelEndIf) ||
          code2 || instruction::LABEL(labelEndIf);
  }

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs && codAtsE = visit(ctx->expr());
  std::string addrE = codAtsE.addr;
  instructionList & codeE = codAtsE.code;
  instructionList && codeS = visit(ctx->statements());

  std::string label = codeCounters.newLabelWHILE();
  std::string labelWhile = "while" + label;
  std::string labelEndWhile = "endwhile" + label;

  code = instruction::LABEL(labelWhile) || codeE ||
         instruction::FJUMP(addrE, labelEndWhile) ||
         codeS || instruction::UJUMP(labelWhile) ||
         instruction::LABEL(labelEndWhile);

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs && codAt = visit(ctx->function_call());
  code = codAt.code;
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAtsE = visit(ctx->left_expr());
  std::string          addr1 = codAtsE.addr;
  // std::string          offs1 = codAtsE.offs;
  instructionList &    code1 = codAtsE.code;
  instructionList &     code = code1;
  // TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  code = code1 || instruction::READI(addr1);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr());
  std::string         addr1 = codAt1.addr;
  // std::string         offs1 = codAt1.offs;
  instructionList &   code1 = codAt1.code;
  instructionList &    code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());
  if (Types.isFloatTy(tid1)) code = code || instruction::WRITEF(addr1); 
  else if (Types.isCharacterTy(tid1)) code = code || instruction::WRITEC(addr1);
  else code = code1 || instruction::WRITEI(addr1);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  code = code || instruction::WRITES(s);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  if (ctx->expr()) {
    CodeAttribs && codAt = visit(ctx->expr());
    code = code || codAt.code || instruction::LOAD("_result", codAt.addr);
  }
  code = code || instruction::RETURN();
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitLeftExprIdent(AslParser::LeftExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLeftExprArray(AslParser::LeftExprArrayContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codIdent = visit(ctx->ident());
  CodeAttribs && codExp = visit(ctx->expr());

  CodeAttribs codAts(codIdent.addr, codExp.addr, codExp.code);

  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->expr());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitBinaryOperation(AslParser::BinaryOperationContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  // TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  // TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  // TypesMgr::TypeId  t = getTypeDecor(ctx);
  std::string temp = "%"+codeCounters.newTEMP();
  if (ctx->AND())
    code = code || instruction::AND(temp, addr1, addr2);
  else // (ctx->OR())
    code = code || instruction::OR(temp, addr1, addr2);
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitArithmeticUnary(AslParser::ArithmeticUnaryContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAt1 = visit(ctx->expr());
  std::string addr1 = codAt1.addr;
  instructionList& code = codAt1.code;
  
  std::string temp = "%"+codeCounters.newTEMP();
  if (ctx->MINUS()) code = code || instruction::NEG(temp, addr1);
  CodeAttribs codAts(temp, "", code);

  DEBUG_EXIT();
  return codAts;
}



antlrcpp::Any CodeGenVisitor::visitBinaryOperationUnary(AslParser::BinaryOperationUnaryContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAt1 = visit(ctx->expr());
  std::string addr1 = codAt1.addr;
  instructionList& code = codAt1.code;
  
  std::string temp = "%"+codeCounters.newTEMP();
  code = code || instruction::NOT(temp, addr1); 
  CodeAttribs codAts(temp, "", code);

  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  //TypesMgr::TypeId  t = getTypeDecor(ctx);
  
  std::string temp = "%"+codeCounters.newTEMP();
  if (Types.isFloatTy(t1) or Types.isFloatTy(t2)) {
    //Type coercion
    addr1 = doCoercionIntFloat(code, t1, t2, addr1);
    addr2 = doCoercionIntFloat(code, t2, t1, addr2);
    
    if (ctx->MUL()) code = code || instruction::FMUL(temp, addr1, addr2);
    else if (ctx->DIV()) code = code || instruction::FDIV(temp, addr1, addr2);
    else if (ctx->PLUS()) code = code || instruction::FADD(temp, addr1, addr2);
    else code = code || instruction::FSUB(temp, addr1, addr2); //ctx->MINUS()
  }
  else { 
    if (ctx->MUL()) code = code || instruction::MUL(temp, addr1, addr2);
    else if (ctx->DIV()) code = code || instruction::DIV(temp, addr1, addr2);
    else if (ctx->PLUS()) code = code || instruction::ADD(temp, addr1, addr2);
    else code = code || instruction::SUB(temp, addr1, addr2); //ctx->MINUS()
  }

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  //TypesMgr::TypeId  t = getTypeDecor(ctx);
  
  std::string temp = "%"+codeCounters.newTEMP();

  //If we have float types we need to use float versions?
  if (Types.isFloatTy(t1) or Types.isFloatTy(t2)) {
    //Type coercion
    addr1 = doCoercionIntFloat(code, t1, t2, addr1);
    addr2 = doCoercionIntFloat(code, t2, t1, addr2);
    if (ctx->EQUAL()) code = code || instruction::FEQ(temp, addr1, addr2);
    else if (ctx->NEQUAL()) {
      std::string temp_aux = "%"+codeCounters.newTEMP();
      code = code || instruction::FEQ(temp_aux, addr1, addr2);
      code = code || instruction::NOT(temp, temp_aux);
    }
    else if (ctx->GT()) code = code || instruction::FLT(temp, addr2, addr1);
    else if (ctx->GE()) code = code || instruction::FLE(temp, addr2, addr1);
    else if (ctx->LT()) code = code || instruction::FLT(temp, addr1, addr2);
    else code = code || instruction::FLE(temp, addr1, addr2); //ctx->LE()
  }
  else {
    if (ctx->EQUAL()) code = code || instruction::EQ(temp, addr1, addr2);
    else if (ctx->NEQUAL()) {
      std::string temp_aux = "%"+codeCounters.newTEMP();
      code = code || instruction::EQ(temp_aux, addr1, addr2);
      code = code || instruction::NOT(temp, temp_aux);
    }
    else if (ctx->GT()) code = code || instruction::LT(temp, addr2, addr1);
    else if (ctx->GE()) code = code || instruction::LE(temp, addr2, addr1);
    else if (ctx->LT()) code = code || instruction::LT(temp, addr1, addr2);
    else code = code || instruction::LE(temp, addr1, addr2); //ctx->LE()
  }

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLeftExprValue(AslParser::LeftExprValueContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->left_expr());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string temp = "%"+codeCounters.newTEMP();
  TypesMgr::TypeId t = getTypeDecor(ctx);

  if (Types.isIntegerTy(t)) code = instruction::ILOAD(temp, ctx->getText());
  else if (Types.isFloatTy(t)) code = instruction::FLOAD(temp, ctx->getText());
  else if (Types.isCharacterTy(t)) code = instruction::CLOAD(temp, ctx->getText());
  else code = instruction::ILOAD(temp, ctx->getText() == "true" ? "1" : "0"); // boolean value

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitFunctionValue(AslParser::FunctionValueContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->function_call());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitFunction_call(AslParser::Function_callContext *ctx) {
  DEBUG_ENTER();

  const std::string & functionName = ctx->ident()->ID()->getText();
	std::vector <TypesMgr::TypeId> argumentTypes;
	std::vector <std::string> argumentAddresses;
	instructionList code;

  std::string address_return;
  const std::string& funcName = ctx->ident()->ID()->getText();
  TypesMgr::TypeId funcType = Symbols.getType(funcName);

  //If is a non void function we push an empty parameter for the result
  if (Types.isPrimitiveNonVoidTy(Types.getFuncReturnType(funcType))) {
    address_return = "%" + codeCounters.newTEMP();
		code = code || instruction::PUSH();
  }

  //We push the parameters with possible int - float coercion
  uint32_t i = 0;

  if (ctx->exprs_call()) {
    for (const auto & ex : ctx->exprs_call()->expr()) {
      CodeAttribs && codE = visit(ex);
      TypesMgr::TypeId argType = getTypeDecor(ex);
      TypesMgr::TypeId paramType = Types.getParameterType(Symbols.getType(funcName), i++);
      std::string address = codE.addr;
      address = doCoercionIntFloat(code, argType, paramType, address);
      code = code || codE.code || instruction::PUSH(address);
    }
  }

  code = code || instruction::CALL(funcName);

  const std::size_t numParams = Types.getNumOfParameters(funcType);
  for (uint32_t i = 0; i < numParams; ++i) code = code || instruction::POP();

  if (Types.isPrimitiveNonVoidTy(Types.getFuncReturnType(funcType))) code = code || instruction::POP(address_return);
  CodeAttribs codAt(address_return, "", code);

	DEBUG_EXIT();
	return codAt;

}

antlrcpp::Any CodeGenVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs codAts(ctx->ID()->getText(), "", instructionList());
  DEBUG_EXIT();
  return codAts;
}


// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId CodeGenVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId CodeGenVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getType(ctx);
}


// Constructors of the class CodeAttribs:
//
CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList & code) :
  addr{addr}, offs{offs}, code{code} {
}

CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList && code) :
  addr{addr}, offs{offs}, code{code} {
}
