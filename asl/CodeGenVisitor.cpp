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
// #define DEBUG_BUILD
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

class param {
  public:
    std::string name;
    std::string type;
    bool isArray;
};

antlrcpp::Any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();
  std::vector<param> && params = visit(ctx->parameters());
  for (auto & p : params) {
    subr.add_param(p.name, p.type, p.isArray);
  }
  std::vector<var> && lvars = visit(ctx->declarations());
  for (auto & onevar : lvars) {
    subr.add_var(onevar);
  }
  instructionList && code = visit(ctx->statements());
  code = code || instruction(instruction::RETURN());
  subr.set_instructions(code);
  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

antlrcpp::Any CodeGenVisitor::visitParameters(AslParser::ParametersContext *ctx) {
  DEBUG_ENTER();
  std::vector<param> params;
  for(auto & param_decl : ctx->parameter_decl()) {
    param param_info = visit(param_decl);
    params.push_back(param_info);
  }
  DEBUG_EXIT();
  return params;
}

antlrcpp::Any CodeGenVisitor::visitParameter_decl(AslParser::Parameter_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  param p{ctx->ID()->getText(), Types.to_string(t1), Types.isArrayTy(t1)};
  DEBUG_EXIT();
  return p;
}

antlrcpp::Any CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> lvars;
  for (auto & varDeclCtx : ctx->variable_decl()) {
    std::vector<var> vars = visit(varDeclCtx);
    for (auto var : vars) {
      lvars.push_back(var);
    }
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  std::size_t      size = Types.getSizeOfType(t1);
  std::vector<var> vars;
  for(auto varCtx : ctx->ID()) {
    vars.push_back(var{varCtx->getText(), Types.to_string(t1), size});
  }
  DEBUG_EXIT();
  return vars;
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

antlrcpp::Any CodeGenVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt = visit(ctx->expr());
  std::string          addr = codAt.addr;
  instructionList &    code = codAt.code;
  code =
    code ||
    instruction::PUSH(addr) ||
    instruction::RETURN();
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE1 = visit(ctx->left_expr());
  std::string           addr1 = codAtsE1.addr;
  // std::string           offs1 = codAtsE1.offs;
  instructionList &     code1 = codAtsE1.code;
  // TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  CodeAttribs     && codAtsE2 = visit(ctx->expr());
  std::string           addr2 = codAtsE2.addr;
  // std::string           offs2 = codAtsE2.offs;
  instructionList &     code2 = codAtsE2.code;
  // TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());
  code = code1 || code2 || instruction::LOAD(addr1, addr2);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = visit(ctx->expr());
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  // For now we only visit the first block of code of an if then else statement
  instructionList &&   code2 = visit(ctx->statements(0));
  std::string label = codeCounters.newLabelIF();
  std::string labelEndIf = "endif"+label;
  code = code1 || instruction::FJUMP(addr1, labelEndIf) ||
         code2 || instruction::LABEL(labelEndIf);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = visit(ctx->expr());
  // Conditional code
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  // Statements block code
  instructionList &&   code2 = visit(ctx->statements());
  std::string label = codeCounters.newLabelWHILE();
  std::string labelBeginWhile = "beginwhile"+label;
  std::string labelEndWhile = "endwhile"+label;
  code =  instruction::LABEL(labelBeginWhile) ||
          code1 ||
          instruction::FJUMP(addr1, labelEndWhile) ||
          code2 ||
          instruction::UJUMP(labelBeginWhile) ||
          instruction::LABEL(labelEndWhile);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  // std::string name = ctx->ident()->ID()->getSymbol()->getText();
  std::string name = ctx->function_call()->ID()->getText();
  code = instruction::CALL(name);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitfuncCallExpr(AslParser::FuncCallExprContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string name = ctx->function_call()->ID()->getText();
  code = instruction::CALL(name);
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
  std::string         offs1 = codAt1.offs;
  instructionList &   code1 = codAt1.code;
  instructionList &    code = code1;
  TypesMgr::TypeId tid = getTypeDecor(ctx->expr());

  if(Types.isFloatTy(tid)) {
    code = code || instruction::WRITEF(addr1); 
  } else if (Types.isIntegerTy(tid) or Types.isBooleanTy(tid)) {
    code = code || instruction::WRITEI(addr1);
  } else if (Types.isCharacterTy(tid)) {
    code = code || instruction::WRITEC(addr1);
  } else if (Types.isArrayTy(tid)){
    // TODO
  }
  
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

antlrcpp::Any CodeGenVisitor::visitLeftExprIdent(AslParser::LeftExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLeftExprArray(AslParser::LeftExprArrayContext *ctx) {
  //This is wrong cause we are only accessing the array identifier and not the index
  //We will change this later
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt = visit(ctx->expr());
  DEBUG_EXIT();
  return codAt;
}

antlrcpp::Any CodeGenVisitor::visitBinaryOperation(AslParser::BinaryOperationContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr; // addr expr1
  instructionList &   code1 = codAt1.code; // code eval expr1
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;  // addr expr2
  instructionList &   code2 = codAt2.code; // code eval expr2
  instructionList &&   code = code1 || code2;
  
  std::string res_temp = "%"+codeCounters.newTEMP();
  if(ctx->AND()) {
    code = code || instruction::AND(res_temp, addr1, addr2);
  } else if(ctx->OR()) {
    code = code || instruction::OR(res_temp, addr1, addr2);
  }
  CodeAttribs codAts(res_temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitBinaryOperationUnary(AslParser::BinaryOperationUnaryContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr());
  std::string         addr1 = codAt1.addr;
  instructionList &    code = codAt1.code;
  std::string res_temp = "%"+codeCounters.newTEMP();
  code = code || instruction::NOT(res_temp, addr1);
  CodeAttribs codAts(res_temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr; // addr expr1
  instructionList &   code1 = codAt1.code; // code eval expr1
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;  // addr expr2
  instructionList &   code2 = codAt2.code; // code eval expr2
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId  t = getTypeDecor(ctx);

  // Type coercion
  if(Types.isFloatTy(t)) {
    if(Types.isIntegerTy(t1)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, addr1);
      addr1 = temp;
    } else if(Types.isIntegerTy(t2)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, addr2);
      addr2 = temp;
    }
  }

  std::string res_temp = "%"+codeCounters.newTEMP();
  if (ctx->MUL()) {
    if (Types.isFloatTy(t)) {
      code = code || instruction::FMUL(res_temp, addr1, addr2);
    } else {
      code = code || instruction::MUL(res_temp, addr1, addr2);
    }
  } else if (ctx->DIV()) {
    if (Types.isFloatTy(t)) {
      code = code || instruction::FDIV(res_temp, addr1, addr2);
    } else {
      code = code || instruction::DIV(res_temp, addr1, addr2);
    }
  //} else if (ctx->MOD()) (todo) {
  } else if (ctx->PLUS()) {
    if (Types.isFloatTy(t)) {
      code = code || instruction::FADD(res_temp, addr1, addr2);
    } else {
      code = code || instruction::ADD(res_temp, addr1, addr2);
    }
  } else { ///if (ctx->MINUS())
    if (Types.isFloatTy(t)) {
      code = code || instruction::FSUB(res_temp, addr1, addr2);
    } else {
      code = code || instruction::SUB(res_temp, addr1, addr2);
    }
  }

  CodeAttribs codAts(res_temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitArithmeticUnary(AslParser::ArithmeticUnaryContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr());
  std::string         addr1 = codAt1.addr;
  instructionList &    code = codAt1.code;
  
  std::string res_temp = "%"+codeCounters.newTEMP();
  if(ctx->MINUS()) {
    code = code || instruction::NEG(res_temp, addr1);
  }
  
  CodeAttribs codAts(res_temp, "", code);
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
  TypesMgr::TypeId  t = getTypeDecor(ctx);

  // Type coercion
  if(Types.isFloatTy(t)) {
    if(Types.isIntegerTy(t1)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, addr1);
      addr1 = temp;
    } else if(Types.isIntegerTy(t2)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, addr2);
      addr2 = temp;
    }
  }

  std::string temp_res = "%"+codeCounters.newTEMP();
  if(ctx->EQUAL()) {
    code = code || instruction::EQ(temp_res, addr1, addr2);
  } else if(ctx->NEQUAL()) {
    std::string temp_1 = "%"+codeCounters.newTEMP();
    code = code || instruction::EQ(temp_1, addr1, addr2);
    code = code || instruction::NOT(temp_res, temp_1);
  } else if(ctx->GT()) {
    std::string temp_1 = "%"+codeCounters.newTEMP();
    code = code || instruction::LE(temp_1, addr1, addr2);
    code = code || instruction::NOT(temp_res, temp_1);
  } else if(ctx->GE()) {
    std::string temp_1 = "%"+codeCounters.newTEMP();
    code = code || instruction::LT(temp_1, addr1, addr2);
    code = code || instruction::NOT(temp_res, temp_1);
  } else if(ctx->LT()) {
    code = code || instruction::LT(temp_res, addr1, addr2);
  } else if(ctx->LE()) {
    code = code || instruction::LE(temp_res, addr1, addr2);
  }
  CodeAttribs codAts(temp_res, "", code);
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
  if(Types.isFloatTy(getTypeDecor(ctx))) {
    code = instruction::FLOAD(temp, ctx->getText());
  } else if(Types.isIntegerTy(getTypeDecor(ctx))) {
    code = instruction::ILOAD(temp, ctx->getText());
  } else if(Types.isBooleanTy(getTypeDecor(ctx))) {
    if (ctx->getText() == "true") {
      code = instruction::ILOAD(temp, "1");
    } else {
      code = instruction::ILOAD(temp, "0");
    }
  } else if(Types.isCharacterTy(getTypeDecor(ctx))) {
    code = instruction::CLOAD(temp, ctx->getText());
  }
  CodeAttribs codAts(temp, "", code);
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
