#include "ArgParseStandalone.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <map>
#include <string>
#include <vector>
#include <fstream>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;

// ==================
// Lexer
// ==================

// The Lexer returns tokens [0-255] if it is an unknown charcter, otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number
static std::istream* input_stream = nullptr;

char GetChar() {
    return input_stream->get();
}

/// gettok - Return the next token from standard input.
static int gettok() {
    static int LastChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar)) {
        LastChar = GetChar();
    }

    // Alphanumeric Tokens
    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = GetChar()))) {
            IdentifierStr += LastChar;
        }
        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern") {
            return tok_extern;
        }
        return tok_identifier;
    }

    // Number tokens
    if (isdigit(LastChar) || LastChar == '.') {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = GetChar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    // Comments
    if (LastChar == '#') {
        do {
            LastChar = GetChar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) {
            return gettok();
        }
    }

    // Check for end of file. Don't eat the EOF.
    if (LastChar == EOF) {
        return tok_eof;
    }

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = GetChar();
    return ThisChar;
}

//======================
// Abstract Syntax Tree (AST) (aka Parse Tree)
//======================

namespace {

/// ExprAST - Base class for all expression nodes.
class ExprAST {
    public:
        virtual ~ExprAST() = default;

        virtual Value* codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST: public ExprAST {
    double Val;

    public:
        NumberExprAST(double Val): Val(Val) {}

        Value* codegen() override;
};

/// VariableExprAST - Expression class for a binary operator
class VariableExprAST: public ExprAST {
    std::string Name;

    public:
        VariableExprAST(const std::string& Name): Name(Name) {}

        Value* codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST: public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
    
    public:
        BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                      std::unique_ptr<ExprAST> RHS)
            : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

        Value* codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST: public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string& Callee,
                    std::vector<std::unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(std::move(Args)) {}

        Value* codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;

    public:
        PrototypeAST(const std::string& Name, std::vector<std::string> Args)
            : Name(Name), Args(std::move(Args)) {}

        Function* codegen();
        const std::string& getName() const {
            return Name;
        }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

    public:
        FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                    std::unique_ptr<ExprAST> Body)
            : Proto(std::move(Proto)), Body(std::move(Body)) {}

        Function* codegen();
};

}

//===============
// Parser
//===============

/// CurTok/getNextToken - Provide a simple token buffer. CurTok is the current
/// token the parser is looking at. getNextToken reads another token from the
/// Lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() {
    return CurTok = gettok();
}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
    if (!isascii(CurTok)) {
        return -1;
    }
    
    // Make sure it's a declared binop.
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) {
        return -1;
    }
    return TokPrec;
}

/// LogError* - These are Little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char* Str) {
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char* Str) {
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consume the number
    return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // eat (.
    auto V = ParseExpression();
    if (!V) {
        return nullptr;
    }

    if (CurTok != ')') {
        return LogError("expected ')'");
    }
    getNextToken(); // eat ).
    return V;
}

/// identifier expr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;

    getNextToken(); // eat identifier.

    if (CurTok != '(') { // Simple variable ref.
        return std::make_unique<VariableExprAST>(IdName);
    }

    // Call.
    getNextToken();    // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression()) {
                Args.push_back(std::move(Arg));
            } else {
                return nullptr;
            }

            if (CurTok == ')') {
                break;
            }

            if (CurTok != ',') {
                return LogError("Expected ')' or ',' in argument list");
            }
            getNextToken();
        }
    }

    // Eat the ')'.
    getNextToken();

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        default: {
            return LogError("unknown token when expecting an expression");
        }
        case tok_identifier: {
            return ParseIdentifierExpr();
        }
        case tok_number: {
            return ParseNumberExpr();
        }
        case '(': {
            return ParseParenExpr();
        }
    }
}


/// expression
///   ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS) {
        return nullptr;
    }

    return ParseBinOpRHS(0, std::move(LHS));
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
    // If this is a binop, find its precedence.
    while (true) {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec) {
            return LHS;
        }

        // Okay, we know this is a binop.
        int BinOp = CurTok;
        getNextToken(); // eat binop

        // Parse the primary expression after the binary operator.
        auto RHS = ParsePrimary();
        if (!RHS) {
            return nullptr;
        }

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }

        //Merge LHS/RHS.
        LHS =
            std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}


/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier) {
        return LogErrorP("Expected function name in prototype");
    }

    std::string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(') {
        return LogErrorP("Expected '(' in prototype");
    }

    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier) {
        ArgNames.push_back(IdentifierStr);
    }
    if (CurTok != ')') {
        return LogErrorP("Expected ')' in prototype");
    }

    // success.
    getNextToken(); // eat ')'.

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition
///   ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken(); // eat def.
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }

    if (auto E = ParseExpression()) {
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto E = ParseExpression()) {
        // Make an anonymous proto.
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                    std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken(); // eat extern.
    return ParsePrototype();
}

//===========
// Code Generation
//===========

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value*> NamedValues;

Value* LogErrorV(const char* Str) {
    LogError(Str);
    return nullptr;
}

Value* NumberExprAST::codegen() {
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value* VariableExprAST::codegen() {
    // Look this variable up in the function.
    Value* V = NamedValues[Name];
    if (!V) {
        return LogErrorV("Unknown variable name");
    }
    return V;
}

Value* BinaryExprAST::codegen() {
    Value* L = LHS->codegen();
    Value* R = RHS->codegen();
    if (!L || !R) {
        return nullptr;
    }

    switch (Op) {
        case '+': {
            return Builder->CreateFAdd(L, R, "addtmp");
        }
        case '-': {
            return Builder->CreateFSub(L, R, "subtmp");
        }
        case '*': {
            return Builder->CreateFMul(L, R, "multmp");
        }
        case '<': {
            L = Builder->CreateFCmpULT(L, R, "cmptmp");
            return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
        }
        default: {
            return LogErrorV("invalid binary operator");
        }
    }
}

Value* CallExprAST::codegen() {
    // Look up the name in the global module table.
    Function* CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF) {
        return LogErrorV("Unknown function referenced");
    }

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size()) {
        return LogErrorV("Incorrect # arguments passed");
    }

    std::vector<Value*> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back()) {
            return nullptr;
        }
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function* PrototypeAST::codegen() {
    // Make the function type: double(double, double) etc.
    std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
    FunctionType* FT =
        FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

    Function* F =
        Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto& Arg: F->args()) {
        Arg.setName(Args[Idx++]);
    }

    return F;
}

Function* FunctionAST::codegen() {
    // First, check for an existing function from a previous 'extern' declaration.
    Function* TheFunction = TheModule->getFunction(Proto->getName());

    if (!TheFunction) {
        TheFunction = Proto->codegen();
    }

    if (!TheFunction) {
        return nullptr;
    }

    // Create a new basic block to start insertion into.
    BasicBlock* BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);
    
    // Record the function arguments in the NamedValues map
    NamedValues.clear();
    for (auto& Arg: TheFunction->args()) {
        NamedValues[std::string(Arg.getName())] = &Arg;
    }

    if (Value* RetVal = Body->codegen()) {
        // Finish off the function.
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        verifyFunction(*TheFunction);

        return TheFunction;
    }

    // Error reading body, remove function.
    TheFunction->eraseFromParent();
    return nullptr;
}

//============
// Top-Level parsing and JIT Driver
//============

static void InitializeModule() {
    // Open a new context and module.
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        if (auto* FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read function definition:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        if (auto* FnIR = ProtoAST->codegen()) {
            fprintf(stderr, "Read extern:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr()) {
        if (auto* FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read top-level expression:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");

            // Remove anonymous expression.
            FnIR->eraseFromParent();
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
static void MainLoop(bool using_cin) {
    while (true) {
        if (using_cin) {
            fprintf(stderr, "ready> ");
        }
        switch (CurTok) {
            case tok_eof: {
                return;
            }
            case ';': { // ignore top-level semicolons.
                getNextToken();
                break;
            }
            case tok_def: {
                HandleDefinition();
                break;
            }
            case tok_extern: {
                HandleExtern();
                break;
            }
            default: {
                HandleTopLevelExpression();
                break;
            }
        }
    }
}

//========================
// Driver section
//========================

int main(int argc, char** argv) {
    // Install standard binary operators.
    // 1 is Lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    auto Parser = ArgParse::ArgParser("Kaleidoscope Compiler");
    std::string source_filepath = "";
    Parser.AddArgument("--source", "Path to source file", &source_filepath);
    if (Parser.ParseArgs(argc, argv) < 0) {
        std::cerr << "Error parsing arguments" << std::endl;
        return -1;
    }

    std::unique_ptr<std::istream> is_pointer = nullptr;
    bool using_cin = false;
    if (source_filepath == "") {
        using_cin = true;
        input_stream = &std::cin;
    } else {
        // Copy file to if_pointer
        std::ifstream* ifs = new std::ifstream(source_filepath, std::ifstream::in);
        is_pointer.reset(static_cast<std::istream*>(ifs));
        input_stream = is_pointer.get();
    }

    if (using_cin) {
        fprintf(stderr, "ready> ");
    }
    getNextToken();

    // Make the module, which holds all the code.
    InitializeModule();

    // Run the main "interpreter Loop" now.
    MainLoop(using_cin);

    // Print out all of the generated code.
    TheModule->print(errs(), nullptr);

    return 0;
}
