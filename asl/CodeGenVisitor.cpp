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
  TypesMgr::TypeId functionType = Symbols.getGlobalFunctionType(ctx->ID()->getText());
  TypesMgr::TypeId returnType = Types.getFuncReturnType(functionType);
  setCurrentFunctionTy(returnType);
  if (not Types.isVoidTy(returnType)) {
    subr.add_param("_result", Types.to_string(returnType), false);
  }
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
  TypesMgr::TypeId t = getTypeDecor(ctx->type());
  param p;
  if (Types.isArrayTy(t)) {
    p = param{ctx->ID()->getText(), Types.to_string(Types.getArrayElemType(t)), true};
  } else {
    p = param{ctx->ID()->getText(), Types.to_string(t), false};
  }
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
    if (Types.isArrayTy(t1)) {
      vars.push_back(var{varCtx->getText(), Types.to_string(Types.getArrayElemType(t1)), size});
    } else {
      vars.push_back(var{varCtx->getText(), Types.to_string(t1), size});
    }
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
  TypesMgr::TypeId returnType = getCurrentFunctionTy();
  TypesMgr::TypeId tid = getTypeDecor(ctx->expr());
  if (Types.isFloatTy(returnType) and Types.isIntegerTy(tid)) {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::FLOAT(temp, addr);
    addr = temp;
  }
  if (Types.isIntegerTy(returnType) or Types.isBooleanTy(returnType)) {
    code = code || instruction::ILOAD("_result", addr);
  } else if (Types.isFloatTy(returnType)) {
    code = code || instruction::FLOAD("_result", addr);
  } else if (Types.isCharacterTy(returnType)) {
    code = code || instruction::CLOAD("_result", addr);
  }
  code = code || instruction::RETURN();
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
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  CodeAttribs     && codAtsE2 = visit(ctx->expr());
  std::string           addr2 = codAtsE2.addr;
  std::string           offs2 = codAtsE2.offs;
  instructionList &     code2 = codAtsE2.code;
  TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());
  code = code1 || code2;
  
  // Handle offset
  if (offs1 != "") {
    addr1 = addr1+"["+offs1+"]";
  }
  if (offs2 != "") {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::LOADX(temp, addr2, offs2);
    addr2 = temp;
  }

  if (Types.isFloatTy(tid1) and Types.isIntegerTy(tid2)) {
    code = code || instruction::FLOAT(addr1, addr2);
  } else {
    code = code || instruction::LOAD(addr1, addr2);
  }
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
  if (ctx->statements().size() == 1) {
    std::string labelEndIf = "endif"+codeCounters.newLabelIF();
    code = code1 || instruction::FJUMP(addr1, labelEndIf) || code2 || instruction::LABEL(labelEndIf);
  } else {
    instructionList &&  code3 = visit(ctx->statements(1));
    std::string labelElse = "else"+codeCounters.newLabelIF();
    std::string labelEndIf = "endif"+codeCounters.newLabelIF();
    code =  code1 || instruction::FJUMP(addr1, labelElse) ||
            code2 || instruction::FJUMP(addr1, labelEndIf) ||
            instruction::LABEL(labelElse) ||
            code3 || instruction::LABEL(labelEndIf);
  }
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
  std::string name = ctx->function_call()->ID()->getText();
  TypesMgr::TypeId funcType = Symbols.getGlobalFunctionType(name);
  TypesMgr::TypeId returnType = Types.getFuncReturnType(funcType);
  if (not Types.isVoidTy(returnType)) {
    code = code || instruction::PUSH(); // Space for return value
  }
  std::vector<TypesMgr::TypeId> param_types = Types.getFuncParamsTypes(funcType);
  for(uint i=0; i<ctx->function_call()->expr().size(); i++) {
    auto expr = ctx->function_call()->expr(i);
    CodeAttribs && codAt = visit(expr);
    std::string addr = codAt.addr;
    std::string offs = codAt.offs; // TODO
    TypesMgr::TypeId exprType = getTypeDecor(expr);
    //Type coercion
    if (Types.isIntegerTy(exprType) and Types.isFloatTy(param_types[i])) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || codAt.code || instruction::FLOAT(temp, addr);
      addr = temp;
    } else {
      code = code || codAt.code;
    }
    // Passing by reference
    if (Types.isArrayTy(exprType)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::ALOAD(temp, addr);
      addr = temp;
    }
    code = code || instruction::PUSH(addr);
  }
  code = code || instruction::CALL(name);
  for(uint i=0; i<ctx->function_call()->expr().size(); i++) {
    code = code || instruction::POP();
  }
  if (not Types.isVoidTy(returnType)) {
    code = code || instruction::POP();
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitFuncCallExpr(AslParser::FuncCallExprContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string name = ctx->function_call()->ID()->getText();
  TypesMgr::TypeId funcType = Symbols.getGlobalFunctionType(name);
  code = instruction::PUSH();
  std::vector<TypesMgr::TypeId> param_types = Types.getFuncParamsTypes(funcType);
  for(uint i=0; i<ctx->function_call()->expr().size(); i++){
    auto expr = ctx->function_call()->expr(i);
    CodeAttribs && codAt = visit(expr);
    std::string addr = codAt.addr;
    std::string offs = codAt.offs;
    TypesMgr::TypeId exprType = getTypeDecor(expr);
    //Type coercion
    if (Types.isIntegerTy(exprType) and Types.isFloatTy(param_types[i])) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || codAt.code || instruction::FLOAT(temp, addr);
      addr = temp;
    } else {
      code = code || codAt.code;
    }
    // Passing by reference
    if (Types.isArrayTy(exprType)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::ALOAD(temp, addr);
      addr = temp;
    }
    code = code || instruction::PUSH(addr);
  }
  code = code || instruction::CALL(name);
  for(uint i=0; i<ctx->function_call()->expr().size(); i++) {
    code = code || instruction::POP();
  }
  std::string res_temp = "%"+codeCounters.newTEMP();
  code = code || instruction::POP(res_temp);
  CodeAttribs codAts(res_temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAtsE = visit(ctx->left_expr());
  std::string          addr = codAtsE.addr;
  std::string          offs = codAtsE.offs;
  instructionList &    code1 = codAtsE.code;
  instructionList &     code = code1;
  TypesMgr::TypeId type = getTypeDecor(ctx->left_expr());
  if (offs == "") {
    if (Types.isFloatTy(type)) {
      code = code || instruction::READF(addr);
    } else if (Types.isIntegerTy(type) or Types.isBooleanTy(type)) {
      code = code || instruction::READI(addr);
    } else if (Types.isCharacterTy(type)) {
      code = code || instruction::READC(addr);
    }
  } else {
    std::string temp = "%"+codeCounters.newTEMP();
    if (Types.isFloatTy(type)) {
      code = code || instruction::READF(temp) || instruction::XLOAD(addr, offs, temp);
    } else if (Types.isIntegerTy(type) or Types.isBooleanTy(type)) {
      code = code || instruction::READI(temp) || instruction::XLOAD(addr, offs, temp);
    } else if (Types.isCharacterTy(type)) {
      code = code || instruction::READC(temp) || instruction::CLOAD(addr+"["+offs+"]", temp);
    }
  }
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

  if (offs1 != "") {
    std::string temp = "%"+codeCounters.newTEMP();
    if (Types.isFloatTy(tid) or Types.isIntegerTy(tid) or Types.isBooleanTy(tid)) {
      code = code || instruction::LOADX(temp, addr1, offs1);
    } else if (Types.isCharacterTy(tid)) {
      code = code || instruction::CLOAD(temp, addr1+"["+offs1+"]");
    }
    addr1 = temp;
  }
  if(Types.isFloatTy(tid)) {
    code = code || instruction::WRITEF(addr1); 
  } else if (Types.isIntegerTy(tid) or Types.isBooleanTy(tid)) {
    code = code || instruction::WRITEI(addr1);
  } else if (Types.isCharacterTy(tid)) {
    code = code || instruction::WRITEC(addr1);
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

antlrcpp::Any CodeGenVisitor::visitWriteChar(AslParser::WriteCharContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string c = ctx->CHARVAL()->getText();
  std::string parsed = c.substr(1, c.size()-2);
  code = code || instruction::WRITES("\"" + parsed + "\"");
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
  CodeAttribs && codAt1 = visit(ctx->ident());
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs && codAt2 = visit(ctx->expr());
  std::string         addr2 = codAt2.addr;
  std::string         offs2 = codAt2.offs;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  std::string offs = "";
  if (Symbols.isParameterClass(addr1)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::ILOAD(temp, addr1);
      addr1 = temp;
  }
  if (offs2 == "") {
    offs = addr2;
  } else {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::LOADX(temp, addr2, offs2);
    offs = temp;
  }
  CodeAttribs codAts(addr1, offs, code);
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
  std::string         offs1 = codAt1.offs; // offs expr1
  instructionList &   code1 = codAt1.code; // code eval expr1
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;  // addr expr2
  std::string         offs2 = codAt2.offs;  // offs expr2
  instructionList &   code2 = codAt2.code; // code eval expr2
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId  t = getTypeDecor(ctx);

  // Offset handling
  if (offs1 != "") {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::LOADX(temp, addr1, offs1);
    addr1 = temp;
  }
  if (offs2 != "") {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::LOADX(temp, addr2, offs2);
    addr2 = temp;
  }

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
  } else if (ctx->MOD()) {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::DIV(temp, addr1, addr2);
    code = code || instruction::MUL(temp, temp, addr2);
    code = code || instruction::SUB(res_temp, addr1, temp);
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
    if (Types.isFloatTy(getTypeDecor(ctx))) {
      code = code || instruction::FNEG(res_temp, addr1);
    } else {
      code = code || instruction::NEG(res_temp, addr1);
    }
  }
  
  CodeAttribs codAts(res_temp, "", code);
  DEBUG_EXIT();
  return codAts;
}


antlrcpp::Any CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  std::string         offs1 = codAt1.offs;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  std::string         offs2 = codAt2.offs;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));

  // Offset handling
  if (offs1 != "") {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::LOADX(temp, addr1, offs1);
    addr1 = temp;
  }
  if (offs2 != "") {
    std::string temp = "%"+codeCounters.newTEMP();
    code = code || instruction::LOADX(temp, addr2, offs2);
    addr2 = temp;
  }

  // Type coercion
  if(Types.isFloatTy(t1) or Types.isFloatTy(t2)) {
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
  if(Types.isFloatTy(t1) or Types.isFloatTy(t2)) {
    if(ctx->EQUAL()) {
      code = code || instruction::FEQ(temp_res, addr1, addr2);
    } else if(ctx->NEQUAL()) {
      std::string temp_1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FEQ(temp_1, addr1, addr2);
      code = code || instruction::NOT(temp_res, temp_1);
    } else if(ctx->GT()) {
      std::string temp_1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLE(temp_1, addr1, addr2);
      code = code || instruction::NOT(temp_res, temp_1);
    } else if(ctx->GE()) {
      std::string temp_1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLT(temp_1, addr1, addr2);
      code = code || instruction::NOT(temp_res, temp_1);
    } else if(ctx->LT()) {
      code = code || instruction::FLT(temp_res, addr1, addr2);
    } else if(ctx->LE()) {
      code = code || instruction::FLE(temp_res, addr1, addr2);
    }
  } else {
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
