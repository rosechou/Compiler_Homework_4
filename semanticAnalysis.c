#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 assignment document. //
int g_anyErrorOccur = 0;

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclarationNode(AST_NODE* declarationNode);
void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);


typedef enum ErrorMsgKind
{
    SYMBOL_IS_NOT_TYPE,
    SYMBOL_REDECLARE,
    SYMBOL_UNDECLARED,
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,
    EXCESSIVE_ARRAY_DIM_DECLARATION,
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    RETURN_TYPE_UNMATCH,
    INCOMPATIBLE_ARRAY_DIMENSION,
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT,
    PASS_ARRAY_TO_SCALAR,
    PASS_SCALAR_TO_ARRAY
} ErrorMsgKind;



void printErrorMsgSpecial(AST_NODE* node1, char* name2, ErrorMsgKind errorMsgKind) 
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);

    char* name1 = NULL;
    if(node1->nodeType == IDENTIFIER_NODE)
    {
        name1 = node1->semantic_value.identifierSemanticValue.identifierName;
    } 
    else
    {
        name1 = (char*) "expression";
    }

    switch(errorMsgKind)
    {
    case PASS_SCALAR_TO_ARRAY:
        printf("Scalar %s passed to array parameter '%s'.\n", name1, name2);
        break;
    case PASS_ARRAY_TO_SCALAR:
        printf("Array %s passed to scalar parameter '%s'.\n", name1, name2);
        break;
    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }

}


void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind) 
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);

    char* id = NULL;
    if(node->nodeType == IDENTIFIER_NODE)
    {
        id = node->semantic_value.identifierSemanticValue.identifierName;
    } 
    else
    {
        id = (char*) "expression";
    }

    switch(errorMsgKind)
    {
    case SYMBOL_UNDECLARED:
        printf("ID %s undeclared.\n", id);
        break;
    case SYMBOL_REDECLARE:
        printf("ID %s redeclared.\n", id);
        break;
    case TOO_FEW_ARGUMENTS:
        printf("too few arguments to function %s.\n", id);
        break;
    case TOO_MANY_ARGUMENTS:
        printf("too many arguments to function %s.\n", id);
        break;
    case RETURN_TYPE_UNMATCH:
        printf("Incompatible return type.\n");
        break;
    case INCOMPATIBLE_ARRAY_DIMENSION:
        printf("Incompatible array dimensions.\n");
        break;
    case ARRAY_SUBSCRIPT_NOT_INT:
        printf("Array subscript is not an integer.\n");
        break;
    
    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
}

void semanticAnalysis(AST_NODE *root)
{
    processProgramNode(root);
}

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2)
{
    if(dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}

void processProgramNode(AST_NODE *programNode) {
    AST_NODE* child = programNode->child;

    programNode->dataType = NONE_TYPE;

    while (child) 
    {
        if (child->nodeType == VARIABLE_DECL_LIST_NODE) {
            processGeneralNode(child);
        } else {
            processDeclarationNode(child);
        }

        if (child->dataType == ERROR_TYPE) {
            programNode->dataType = ERROR_TYPE;
        }

        child = child->rightSibling;
    }
}


void processDeclarationNode(AST_NODE* declarationNode) 
{
    AST_NODE* typeNode = declarationNode->child;
    processTypeNode(typeNode);

    if (typeNode->dataType == ERROR_TYPE)
    {
        declarationNode->dataType = ERROR_TYPE;
        return;
    }

    switch (declarationNode->semantic_value.declSemanticValue.kind) 
    {
        case VARIABLE_DECL:
            return declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 0);
        case TYPE_DECL:
            return declareIdList(declarationNode, TYPE_ATTRIBUTE, 0);
        case FUNCTION_DECL:
            return declareFunction(declarationNode);
        case FUNCTION_PARAMETER_DECL:
            return declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 1);
    }
}

void processTypeNode(AST_NODE* idNodeAsType)
{
    IdentifierSemanticValue* val = &idNodeAsType->semantic_value.identifierSemanticValue;
    char* id = val->identifierName;
    SymbolTableEntry* entry = retrieveSymbol(id);

    if(!entry)
    {
        printErrorMsg(idNodeAsType, SYMBOL_UNDECLARED);
        idNodeAsType->dataType = ERROR_TYPE;
    }
    else if(entry -> attribute -> attributeKind != TYPE_ATTRIBUTE)
    {
        printErrorMsg(idNodeAsType, SYMBOL_IS_NOT_TYPE);
        idNodeAsType->dataType = ERROR_TYPE;
    }
    else
    {
        val -> symbolTableEntry = entry;
        TypeDescriptor* typeDesc = entry->attribute->attr.typeDescriptor;
            switch (typeDesc->kind) 
            {
                case SCALAR_TYPE_DESCRIPTOR:
                        idNodeAsType->dataType = typeDesc->properties.dataType;
                        break;
                case ARRAY_TYPE_DESCRIPTOR:
                        idNodeAsType->dataType = typeDesc->properties.arrayProperties.elementType;
                        break;
            }
    }
}


void checkInitialType(AST_NODE* idNode, AST_NODE* declarationNode) 
{
    AST_NODE *right = idNode->child;
    processExprRelatedNode(right);

    int flag = 0;
    switch (right->dataType) 
    {
        case INT_PTR_TYPE: case FLOAT_PTR_TYPE:
            printErrorMsg(idNode, TRY_TO_INIT_ARRAY);
            break;
        case CONST_STRING_TYPE:
            printErrorMsg(idNode, NOT_ASSIGNABLE);
            break;
        case VOID_TYPE: case NONE_TYPE: case ERROR_TYPE:
            break;
        case INT_TYPE: case FLOAT_TYPE:
            flag = 1;
            break;
    }

    if (!flag) 
    {
        declarationNode->dataType = ERROR_TYPE;
    }
}


void declareIdList(AST_NODE* declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize)
{
    AST_NODE* typeNode = declarationNode->child;
    TypeDescriptor *typeDesc= typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
    if((isVariableOrTypeAttribute == VARIABLE_ATTRIBUTE) && 
       (typeDesc->kind == SCALAR_TYPE_DESCRIPTOR) &&
       (typeDesc->properties.dataType == VOID_TYPE))
    {
        printErrorMsg(typeNode, VOID_VARIABLE);
        typeNode->dataType = ERROR_TYPE;
        return;
    }
    AST_NODE* traverse= typeNode->rightSibling;
    while(traverse)
    {
        if(declaredLocally(traverse->semantic_value.identifierSemanticValue.identifierName))
        {
            printErrorMsg(traverse, SYMBOL_REDECLARE);
            traverse->dataType = ERROR_TYPE;
            declarationNode->dataType = ERROR_TYPE;
        }
        else
        {
            SymbolAttribute* attribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
            attribute->attributeKind = isVariableOrTypeAttribute;
            switch(traverse->semantic_value.identifierSemanticValue.kind)
            {
                case NORMAL_ID:
                    attribute->attr.typeDescriptor = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
                    break;
                case ARRAY_ID:
                    if((isVariableOrTypeAttribute == TYPE_ATTRIBUTE) && (typeDesc->kind == SCALAR_TYPE_DESCRIPTOR) && (typeDesc->properties.dataType == VOID_TYPE))
                    {
                        printErrorMsg(traverse, TYPEDEF_VOID_ARRAY);
                        traverse->dataType = ERROR_TYPE;
                        declarationNode->dataType = ERROR_TYPE;
                        break;
                    }
                    attribute->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
                    processDeclDimList(traverse, attribute->attr.typeDescriptor, ignoreArrayFirstDimSize);
                    if(traverse->dataType == ERROR_TYPE)
                    {
                        free(attribute->attr.typeDescriptor);
                        declarationNode->dataType = ERROR_TYPE;
                    }
                    else if(typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR)
                    {
                        attribute->attr.typeDescriptor->properties.arrayProperties.elementType = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType;
                    }
                    else if(typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR)
                    {
                        int typearrayDim = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.dimension;
                        int idArrayDim = attribute->attr.typeDescriptor->properties.arrayProperties.dimension;
                        if((typearrayDim + idArrayDim) > MAX_ARRAY_DIMENSION)
                        {
                            printErrorMsg(traverse, EXCESSIVE_ARRAY_DIM_DECLARATION);
                            free(attribute->attr.typeDescriptor);
                            traverse->dataType = ERROR_TYPE;
                            declarationNode->dataType = ERROR_TYPE;
                        }   
                        else
                        {
                            attribute->attr.typeDescriptor->properties.arrayProperties.elementType = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.elementType;
                            attribute->attr.typeDescriptor->properties.arrayProperties.dimension = typearrayDim + idArrayDim;
                            int type_index = 0;
                            int id_index = 0;
                            for(type_index = 0, id_index = idArrayDim; id_index < idArrayDim + typearrayDim; ++type_index, ++id_index)
                            {
                                attribute->attr.typeDescriptor->properties.arrayProperties.sizeInEachDimension[id_index] = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.sizeInEachDimension[type_index];
                            }
                        }
                    }
                    break;
                case WITH_INIT_ID:
                    if(typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR)
                    {
                        printErrorMsg(traverse, TRY_TO_INIT_ARRAY);
                        traverse->dataType = ERROR_TYPE;
                        declarationNode->dataType = ERROR_TYPE;
                    }
                    else
                    {
                        attribute->attr.typeDescriptor = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
                    }
                    break;
                default:
                    printf("Unhandle case in void declareIdList\n");
                    traverse->dataType = ERROR_TYPE;
                    declarationNode->dataType = ERROR_TYPE;
                    break;
            }
            if(traverse->dataType == ERROR_TYPE)
            {
                free(attribute);
                declarationNode->dataType = ERROR_TYPE;
            }
            else
            {
                traverse->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(traverse->semantic_value.identifierSemanticValue.identifierName, attribute);
            }
        }
        traverse = traverse->rightSibling;
    }
}

void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode)
{
    if(assignOrExprRelatedNode->nodeType==STMT_NODE)
    {
        if(assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind == ASSIGN_STMT)
        {
            checkAssignmentStmt(assignOrExprRelatedNode);
        }
        else if(assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT)
        {
            checkFunctionCall(assignOrExprRelatedNode); 
        }   
    }
    else
    {
        processExprRelatedNode(assignOrExprRelatedNode);    
    }
}

void checkWhileStmt(AST_NODE* whileNode)
{
    /*the child of whilenode is the stmt in ()*/
    if(whileNode->child->nodeType == EXPR_NODE)
    {
        processExprNode(whileNode->child);  
    }
    else if(whileNode->child->nodeType == IDENTIFIER_NODE)
    {
        /*too many arguement*/
        processVariableRValue(whileNode->child);/*change arguement*/
    }
    else if(whileNode->child->nodeType == STMT_NODE)
    {
        checkFunctionCall(whileNode->child);
    }
    else
    {
        processConstValueNode(whileNode->child);
    }
    /*to access content inside of brace*/
    processBlockNode(whileNode->child->rightSibling);
}


void checkForStmt(AST_NODE* forNode)
{
    AST_NODE* init = forNode->child;
    AST_NODE* cond = init->rightSibling;
    AST_NODE* loop = cond->rightSibling;
    processGeneralNode(init);
    processGeneralNode(cond);
    processGeneralNode(loop);
    processBlockNode(loop->rightSibling);
}



void checkAssignmentStmt(AST_NODE* assignmentNode) 
{
    AST_NODE* left = assignmentNode->child;
    AST_NODE* right = left->rightSibling;

    processVariableLValue(left);
    if (left->dataType == ERROR_TYPE) 
    {
        assignmentNode->dataType = ERROR_TYPE;
    }
    processExprRelatedNode(right);
    if (right->dataType == ERROR_TYPE) 
    {
        assignmentNode->dataType = ERROR_TYPE;
    }

    if (right->dataType == INT_PTR_TYPE || right->dataType == FLOAT_PTR_TYPE) 
    {
        printErrorMsg(right, INCOMPATIBLE_ARRAY_DIMENSION);
        assignmentNode->dataType = ERROR_TYPE;
    }
    if (right->dataType == CONST_STRING_TYPE) 
    {
        printErrorMsg(right, NOT_ASSIGNABLE);
        assignmentNode->dataType = ERROR_TYPE;
    }
    assignmentNode->dataType = left->dataType;
    return;
}

void checkIfStmt(AST_NODE* ifNode)
{
    AST_NODE* cond = ifNode->child;
    AST_NODE* block = cond->rightSibling;
    AST_NODE* else_block = block->rightSibling;
    
    checkAssignOrExpr(cond);
    processStmtNode(block);
    processStmtNode(else_block);
}

void checkWriteFunction(AST_NODE* functionCallNode)
{
    AST_NODE* func_name = functionCallNode->child;
    AST_NODE* para_list = func_name->rightSibling;
    AST_NODE* actual_para = para_list->child;
    int regular_count=0;
    int actual_count = 0;
    SymbolTableEntry* entry = retrieveSymbol(functionCallNode->child->semantic_value.identifierSemanticValue.identifierName);

    while((entry!=NULL)&&(entry->attribute->attributeKind != FUNCTION_SIGNATURE))
    {
        entry = entry->sameNameInOuterLevel;/*TODO: check pointer in symbolTable*/
    }
    
    if(entry == NULL)
    {
        printErrorMsgSpecial(functionCallNode->child, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, SYMBOL_UNDECLARED);
        return; 
    }

    regular_count = entry->attribute->attr.functionSignature->parametersCount;
    while(actual_para != NULL)
    {
        actual_count++;
        actual_para = actual_para->rightSibling;    
    }   
    
    if(regular_count < actual_count)
    {
        printErrorMsgSpecial(functionCallNode, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, TOO_MANY_ARGUMENTS);
        return;
    }
    else(regular_count > actual_count)
    ;{
        printErrorMsgSpecial(functionCallNode, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, TOO_FEW_ARGUMENTS);
        return;
    }   
    functionCallNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
}


void checkFunctionCall(AST_NODE* functionCallNode) 
{
    AST_NODE* func_name = functionCallNode->child;
    char* id = func_name->semantic_value.identifierSemanticValue.identifierName;

    if (strcmp(id, SYMBOL_TABLE_SYS_LIB_WRITE) == 0) 
    {
        return checkWriteFunction(functionCallNode);
    }

    SymbolTableEntry* entry = retrieveSymbol(id);
    AST_NODE* para_list = func_name->rightSibling;
    AST_NODE* actual_para = para_list->child;
    Parameter* parameter;

    if (!entry) 
    {
        printErrorMsg(func_name, SYMBOL_UNDECLARED);
        functionCallNode->dataType = ERROR_TYPE;
    } 
    else if (entry->attribute->attributeKind != FUNCTION_SIGNATURE) 
    {
        printErrorMsg(func_name, NOT_FUNCTION_NAME);
        functionCallNode->dataType = ERROR_TYPE;
    }

    processGeneralNode(para_list);
    parameter = entry->attribute->attr.functionSignature->parameterList;

    while (actual_para && parameter) 
    {
        if (actual_para->dataType == ERROR_TYPE) 
        {
            functionCallNode->dataType = ERROR_TYPE;
        }
        checkParameterPassing(parameter, actual_para);

        actual_para = actual_para->rightSibling;
        parameter = parameter->next;
    }

    if (actual_para) 
    {
        printErrorMsg(func_name, TOO_MANY_ARGUMENTS);
    } 
    else if (parameter) 
    {
        printErrorMsg(func_name, TOO_FEW_ARGUMENTS);
    }
    return;
}


void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter) 
{
    int pass_array = actualParameter->dataType == INT_PTR_TYPE || actualParameter->dataType == FLOAT_PTR_TYPE;

    if (actualParameter->dataType == CONST_STRING_TYPE) 
    {
        printErrorMsg(actualParameter, NOT_ASSIGNABLE);
        return;
    }

    if (formalParameter->type->kind == SCALAR_TYPE_DESCRIPTOR && pass_array) 
    {
        printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_ARRAY_TO_SCALAR);
        actualParameter->dataType = ERROR_TYPE;
    } 
    else if (formalParameter->type->kind == ARRAY_TYPE_DESCRIPTOR && !pass_array) 
    {
        printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_SCALAR_TO_ARRAY);
        actualParameter->dataType = ERROR_TYPE;
    }
}


void processExprRelatedNode(AST_NODE* exprRelatedNode)
{
    switch(exprRelatedNode->nodeType)
    {
        case CONST_VALUE_NODE:
            processConstValueNode(exprRelatedNode);
            break;
        case IDENTIFIER_NODE:
            processVariableRValue(exprRelatedNode);
            break;
        case EXPR_NODE:
            processExprNode(exprRelatedNode);
            break;
        case STMT_NODE:
            checkFunctionCall(exprRelatedNode);
            break;
        default:
            printf("unhandled case\n");
            exprRelatedNode->dataType = ERROR_TYPE;
            break;
    }   
}

void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue)
{
    if(exprOrConstNode->nodeType == CONST_VALUE_NODE)
    {
        if(exprOrConstNode->dataType == INT_TYPE)
        {
            if(fValue)
            {
                *fValue = exprOrConstNode->semantic_value.const1->const_u.intval;
            }
            else
            {
                *iValue = exprOrConstNode->semantic_value.const1->const_u.intval;
            }
        }
        else
        {
            *fValue = exprOrConstNode->semantic_value.const1->const_u.fval;
        }
    }
    else
    {
        if(exprOrConstNode->dataType == INT_TYPE)
        {
            if(fValue)
            {
                *fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
            }
            else
            {
                *iValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
            }
        }
        else
        {
            *fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.fValue;

        }
    }
}

void evaluateExprValue(AST_NODE* exprNode)
{
    if(exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION)
    {
        AST_NODE* left = exprNode->child;
        AST_NODE* right = exprNode->rightSibling;
        if(left->dataType == INT_TYPE && right->dataType == INT_TYPE)
        {
                int l_value = 0;
                int r_value = 0;
                getExprOrConstValue(left, &l_value, NULL);
                getExprOrConstValue(right, &r_value, NULL);
                exprNode->dataType = INT_TYPE; /*const folding*/

                switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp) //there is l_value and r_value
                {
                    case BINARY_OP_ADD:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value + r_value);
                        break;
                    case BINARY_OP_SUB:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value - r_value);
                        break;
                    case BINARY_OP_MUL:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value * r_value);
                        break;
                    case BINARY_OP_DIV:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value / r_value);
                        break;
                    case BINARY_OP_EQ:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value == r_value);
                        break;
                    case BINARY_OP_GE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value >= r_value);
                        break;
                    case BINARY_OP_LE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value <= r_value);
                        break;
                    case BINARY_OP_NE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value != r_value);
                        break;
                    case BINARY_OP_GT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value > r_value);
                        break;
                    case BINARY_OP_LT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value < r_value);
                        break;
                    case BINARY_OP_AND:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value && r_value);
                        break;
                    case BINARY_OP_OR:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value || r_value);
                        break;
                    default:
                        printf("unhandled case\n");
                        break;
                }
        return;
        }
        else
        {
                float l_value = 0.0;
                float r_value = 0.0;
                getExprOrConstValue(left, NULL, &l_value);
                getExprOrConstValue(right, NULL, &r_value);
                exprNode->dataType = FLOAT_TYPE; /*const folding*/

                switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp)
                {
                    case BINARY_OP_ADD:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value + r_value);
                        break;
                    case BINARY_OP_SUB:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value - r_value);
                        break;
                    case BINARY_OP_MUL:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value * r_value);
                        break;
                    case BINARY_OP_DIV:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value / r_value);
                        break;
                    case BINARY_OP_EQ:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value == r_value);
                        break;
                    case BINARY_OP_GE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value >= r_value);
                        break;
                    case BINARY_OP_LE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value <= r_value);
                        break;
                    case BINARY_OP_NE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value != r_value);
                        break;
                    case BINARY_OP_GT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value > r_value);
                        break;
                    case BINARY_OP_LT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value < r_value);
                        break;
                    case BINARY_OP_AND:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value && r_value);
                        break;
                    case BINARY_OP_OR:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value || r_value);
                        break;
                    default:
                        printf("unhandled case\n");
                        break;
                }
            return;
        }
    }
    else
    {
        AST_NODE* node = exprNode->child;
        if(node->dataType == INT_TYPE)
        {
            int value=0;
            getExprOrConstValue(node, &value, NULL);
            exprNode->dataType = INT_TYPE;
            switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp)
            {
                case UNARY_OP_POSITIVE:
                    exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = value;
                    break;
                case UNARY_OP_NEGATIVE:
                    exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = -(value);
                    break;
                case UNARY_OP_LOGICAL_NEGATION:
                    exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = !(value);
                    break;
                default:
                    printf("unhandled case\n");
                    break;
            }
        }
        else
        {
            float value = 0.0;
            getExprOrConstValue(node, NULL, &value);
            exprNode->dataType = FLOAT_TYPE;
            switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp)
            {
                case UNARY_OP_POSITIVE:
                    exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = value;
                    break;
                case UNARY_OP_NEGATIVE:
                    exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = -(value);
                    break;
                case UNARY_OP_LOGICAL_NEGATION:
                    exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = !(value);
                    break;
                default:
                    printf("unhandled case\n");
                    break;
            }
        }
    }

}


int isConstExpr(AST_NODE* exprNode) 
{
    return (exprNode->nodeType == CONST_VALUE_NODE) ||
          (exprNode->nodeType == EXPR_NODE && exprNode->semantic_value.exprSemanticValue.isConstEval);
}


void processExprNode(AST_NODE* exprNode) 
{
    if (exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION) 
    {
        AST_NODE* left = exprNode->child;
        AST_NODE* right = left->rightSibling;

        processExprRelatedNode(left);
        processExprRelatedNode(right);

        if (left->dataType == CONST_STRING_TYPE || right->dataType == CONST_STRING_TYPE)
         {
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (left->dataType == INT_PTR_TYPE || left->dataType == FLOAT_PTR_TYPE || right->dataType == INT_PTR_TYPE || right->dataType == FLOAT_PTR_TYPE) 
        {
            printErrorMsg(left, INCOMPATIBLE_ARRAY_DIMENSION);
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (left->dataType != INT_TYPE && left->dataType != FLOAT_TYPE && right->dataType != INT_TYPE && right->dataType != FLOAT_TYPE) 
        {
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        switch (exprNode->semantic_value.exprSemanticValue.op.binaryOp) 
        {
            case BINARY_OP_EQ: case BINARY_OP_NE: case BINARY_OP_GE: case BINARY_OP_LE: case BINARY_OP_GT: case BINARY_OP_LT: case BINARY_OP_AND: case BINARY_OP_OR:
                exprNode->dataType = INT_TYPE;  
                break;
            default:
                exprNode->dataType = getBiggerType(left->dataType, right->dataType);
                break;
        }

        if (isConstExpr(left) && isConstExpr(right)) 
        {
            evaluateExprValue(exprNode);
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }
    } 
    else 
    {
        AST_NODE* node= exprNode->child;
        processExprRelatedNode(node);

        if (node->dataType == CONST_STRING_TYPE) 
        {
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
        }

        if (node->dataType == INT_PTR_TYPE || node->dataType == FLOAT_PTR_TYPE) 
        {
            printErrorMsg(node, INCOMPATIBLE_ARRAY_DIMENSION);
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (node->dataType != INT_TYPE && node->dataType != FLOAT_TYPE) 
        {
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (exprNode->semantic_value.exprSemanticValue.op.unaryOp == UNARY_OP_LOGICAL_NEGATION) 
        {
            exprNode->dataType = INT_TYPE;
        } 
        else 
        {
            exprNode->dataType = node->dataType;
        }

        if (isConstExpr(exprNode)) 
        {
            evaluateExprValue(exprNode);
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }
    }
}


void processVariableLValue(AST_NODE* idNode) 
{
    SymbolTableEntry *entry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
    idNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
    if (!entry) 
    {
        printErrorMsg(idNode, SYMBOL_UNDECLARED);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    SymbolAttributeKind symKind = entry->attribute->attributeKind;

    if (symKind == TYPE_ATTRIBUTE) 
    {
        printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    } 
    else if (symKind == FUNCTION_SIGNATURE) 
    {
        printErrorMsg(idNode, IS_FUNCTION_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    TypeDescriptor *typeDesc = (idNode)->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;

    switch (idNode->semantic_value.identifierSemanticValue.kind) 
    {
        case NORMAL_ID:
            if (typeDesc->kind == ARRAY_TYPE_DESCRIPTOR) 
            {
                printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
                idNode->dataType = ERROR_TYPE;
            } 
            else 
            {
                idNode->dataType = typeDesc->properties.dataType;
            }
            break;
        case ARRAY_ID: 
            if (typeDesc->kind == SCALAR_TYPE_DESCRIPTOR) 
            {
                printErrorMsg(idNode, NOT_ARRAY);
                idNode->dataType = ERROR_TYPE;
                break;
            }

            int dim = 0;
            AST_NODE *dimNode = idNode->child;
            while (dimNode) 
            {
                dim++;
                processExprRelatedNode(dimNode);
                if (dimNode->dataType == ERROR_TYPE) 
                {
                    idNode->dataType = ERROR_TYPE;
                } 
                else if (dimNode->dataType == FLOAT_TYPE) 
                {
                    printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                    idNode->dataType = ERROR_TYPE;
                }
                dimNode = dimNode->rightSibling;
            }

            if (dim == typeDesc->properties.arrayProperties.dimension) 
            {
                idNode->dataType = typeDesc->properties.arrayProperties.elementType;
            } 
            else 
            {
                printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
                idNode->dataType = ERROR_TYPE;
            }
            break;
        
        default: 
            break;
    }
}

void processVariableRValue(AST_NODE* idNode)
{
    SymbolTableEntry* entry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
    idNode->semantic_value.identifierSemanticValue.symbolTableEntry=entry;
    if (!entry)
    {
        printErrorMsg(idNode, SYMBOL_UNDECLARED);
        idNode->dataType=ERROR_TYPE;
    }

    SymbolAttributeKind kind = entry->attribute->attributeKind;

    if(kind == FUNCTION_SIGNATURE)
    {
        printErrorMsg(idNode, IS_FUNCTION_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }else if(kind == TYPE_ATTRIBUTE)
    {
        printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }
    TypeDescriptor* typdes= idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;

    switch(idNode->semantic_value.identifierSemanticValue.kind)
    {
        case NORMAL_ID:
            if(typdes->kind == ARRAY_TYPE_DESCRIPTOR)
            {
                if(typdes->properties.arrayProperties.elementType == INT_TYPE)
                {
                    idNode->dataType = INT_PTR_TYPE;
                }
                else if(typdes->properties.arrayProperties.elementType == FLOAT_TYPE)
                {
                    idNode->dataType = FLOAT_PTR_TYPE;
                }
            }
            else if(typdes->kind == SCALAR_TYPE_DESCRIPTOR)
            {
                idNode->dataType = typdes->properties.dataType;
            }
            break;

        case ARRAY_ID:
            if(typdes->kind == SCALAR_TYPE_DESCRIPTOR)
            {
                printErrorMsg(idNode, NORMAL_ID);
                idNode->dataType = ERROR_TYPE;
            }
            else if(typdes->kind == ARRAY_TYPE_DESCRIPTOR)
            {
                int dim = 0;
                AST_NODE* dimNode = idNode->child;
                while(dimNode)
                {
                    dim++;
                    processExprRelatedNode(dimNode);
                    if(dimNode->dataType == ERROR_TYPE)
                    {
                        idNode->dataType = ERROR_TYPE;
                    }
                    else if(dimNode ->dataType == FLOAT_TYPE)
                    {
                        printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                        idNode->dataType = ERROR_TYPE;
                    }
                    dimNode= dimNode ->rightSibling;
                }

                ArrayProperties* arrpro = &typdes->properties.arrayProperties;
                int dim_check = arrpro->dimension;

                if(dim == dim_check)
                {
                    idNode->dataType = arrpro->elementType;
                }   
                else if(dim < dim_check)/*array slice*/
                {
                    if(arrpro->elementType == INT_TYPE)
                    {
                        idNode->dataType = INT_PTR_TYPE;
                    }
                    else if(arrpro->elementType == FLOAT_TYPE)
                    {
                        idNode->dataType = FLOAT_PTR_TYPE;
                    }
                    
                }
                else
                {
                    printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
                    idNode->dataType=ERROR_TYPE;
                }
            }
            break;
    }
}


void processConstValueNode(AST_NODE* constValueNode)
{
    switch(constValueNode->semantic_value.const1->const_type)
    {
        case INTEGERC:
            constValueNode->dataType = INT_TYPE;
            constValueNode->semantic_value.exprSemanticValue.constEvalValue.iValue = constValueNode->semantic_value.const1->const_u.intval;
            break;
        case FLOATC:
            constValueNode->dataType = FLOAT_TYPE;
            constValueNode->semantic_value.exprSemanticValue.constEvalValue.fValue = constValueNode->semantic_value.const1->const_u.fval;
            break;
        case STRINGC:
            constValueNode->dataType = CONST_STRING_TYPE;
            break;
        default:
            constValueNode->dataType = ERROR_TYPE;
            break;
    }
}


void checkReturnStmt(AST_NODE* returnNode)
{
    AST_NODE* parent = returnNode->parent;
    AST_NODE* type;
    AST_NODE* child = returnNode->child;

    while(parent)
    {
        if(parent->nodeType == DECLARATION_NODE)
        {
            if(parent->semantic_value.declSemanticValue.kind == FUNCTION_DECL)
            {
                break;
            }
        }
        parent = parent->parent;
    }

    type = parent->child;
    if(type->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType == VOID_TYPE)
    {
        if(child != NULL)
        {
            printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
        }
        return;
    }
    else if(child->nodeType == EXPR_NODE)
    {
        processExprNode(child);
    }
    else if(child->nodeType == IDENTIFIER_NODE)
    {
        processVariableRValue(child);/*change arguement*/
    }
    else if(child->nodeType == STMT_NODE)
    {
        checkFunctionCall(child);
    }
    else
    {
        processConstValueNode(child);
    }

    if(type->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType!= child->dataType)
    {
        printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
    }
}


void processBlockNode(AST_NODE* blockNode)
{
    openScope();
    AST_NODE* child = blockNode->child;
    while(child != NULL)
    {
        processGeneralNode(child);
        child = child->rightSibling;
    }
    closeScope();   
}


void processStmtNode(AST_NODE* stmtNode)
{
    if(stmtNode->nodeType == NUL_NODE)
    {
        return;
    }
    else if(stmtNode->nodeType == BLOCK_NODE)
    {
        return processBlockNode(stmtNode);
    }

    switch(stmtNode->semantic_value.stmtSemanticValue.kind)
    {
        case ASSIGN_STMT:
            return checkAssignmentStmt(stmtNode);
        case WHILE_STMT:
            return checkWhileStmt(stmtNode);
        case FOR_STMT:
            return checkForStmt(stmtNode);
        case IF_STMT:
            return checkIfStmt(stmtNode);
        case FUNCTION_CALL_STMT:
            return checkFunctionCall(stmtNode);
        case RETURN_STMT:
            return checkReturnStmt(stmtNode);
        default:
            stmtNode->dataType = ERROR_TYPE;
            return;
    }
}


void processGeneralNode(AST_NODE *node)
{
    AST_NODE* child = node->child;
    switch(node->nodeType)
    {
        case VARIABLE_DECL_LIST_NODE:
            while(child!=NULL)
            {
                processDeclarationNode(child);
                if(child->dataType == ERROR_TYPE)
                {
                    node->dataType = ERROR_TYPE;
                }
                child = child->rightSibling;
            }
            break;
        case STMT_LIST_NODE:
            while(child!=NULL)
            {
                processStmtNode(child);
                if(child->dataType == ERROR_TYPE)
                {
                    node->dataType = ERROR_TYPE;
                }
                child = child->rightSibling;
            }
            break;
        case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
            while(child!=NULL)
            {
                checkAssignOrExpr(child);
                if(child->dataType == ERROR_TYPE)
                {
                    node->dataType = ERROR_TYPE;
                }
                child = child->rightSibling;
            }
            break;
        case NONEMPTY_RELOP_EXPR_LIST_NODE:
            while(child!=NULL)
            {
                processExprRelatedNode(child);
                if(child->dataType == ERROR_TYPE)
                {
                    node->dataType = ERROR_TYPE;
                }
                child = child->rightSibling;
            }
            break;
        case NUL_NODE:
            break;
        default:
            puts("unhandled case.");
            node->nodeType = ERROR_TYPE;
            break;
    }
}

void processDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize)
{
    AST_NODE* list = idNode->child;
    AST_NODE* dim = list;
    typeDescriptor->kind = ARRAY_TYPE_DESCRIPTOR;
    int dimension = 0;
    if((dim->nodeType == NUL_NODE)&&(ignoreFirstDimSize))
    {
        typeDescriptor->properties.arrayProperties.sizeInEachDimension[dimension] =0;
        dimension++;
        dim = list->rightSibling;
    }
    while(dim)
    {
        if(dimension>=MAX_ARRAY_DIMENSION)
        {
            printErrorMsg(list->parent, EXCESSIVE_ARRAY_DIM_DECLARATION);
            idNode->dataType = ERROR_TYPE;
            break;
        }
        processExprRelatedNode(dim);
        if(dim->dataType == ERROR_TYPE)
        {
            idNode->dataType = ERROR_TYPE;
        }
        else if(dim->dataType == FLOAT_TYPE)
        {
            printErrorMsg(dim->parent, ARRAY_SUBSCRIPT_NOT_INT);
            idNode->dataType = ERROR_TYPE;
        }
        else
        {
            typeDescriptor->properties.arrayProperties.sizeInEachDimension[dimension] = dim->semantic_value.exprSemanticValue.constEvalValue.iValue;
        }
        dimension++;
        dim = dim->rightSibling;
    }
    typeDescriptor->properties.arrayProperties.dimension = dimension;
}


void declareFunction (AST_NODE *declarationNode) {
    AST_NODE *id    = declarationNode->child->rightSibling;
    AST_NODE *para = declarationNode->child->rightSibling->rightSibling->child;
    AST_NODE *block = declarationNode->child->rightSibling->rightSibling->rightSibling;
    processTypeNode(declarationNode->child);
    SymbolTableEntry *check;
    SymbolAttribute *attribute;
    Parameter *current = NULL, *head = NULL, *prev = NULL;
    int count = 0;

    if (declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry == NULL)
        return;

    check = retrieveSymbol(id->semantic_value.identifierSemanticValue.identifierName);

    while (check != NULL) 
    {
        if (check->attribute->attributeKind == FUNCTION_SIGNATURE)
            break;
        check = check->sameNameInOuterLevel;
    }

    if (check != NULL) 
    {
        printErrorMsgSpecial (id, id->semantic_value.identifierSemanticValue.identifierName, SYMBOL_REDECLARE);
        return;
    }

    attribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    attribute->attributeKind = FUNCTION_SIGNATURE;
    openScope(); 
    while (para != NULL) 
    {
        count++;
        processDeclarationNode(para);
        current = (Parameter*)malloc(sizeof(Parameter));
        if (head == NULL)
            head = current;
        else
            prev->next = current;
        current->parameterName = para->child->rightSibling->semantic_value.identifierSemanticValue.identifierName;
        current->next = NULL;
        current->type = para->child->rightSibling->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
        para = para->rightSibling;
        prev = current;
        current = current->next;
    }
    attribute->attr.functionSignature = (FunctionSignature *)malloc(sizeof(FunctionSignature));
    attribute->attr.functionSignature->parametersCount = count;
    attribute->attr.functionSignature->parameterList = head;
    attribute->attr.functionSignature->returnType = declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType;
    id->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(id->semantic_value.identifierSemanticValue.identifierName, attribute);
    processBlockNode(block);
    closeScope(); 
    id->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(id->semantic_value.identifierSemanticValue.identifierName, attribute);
}